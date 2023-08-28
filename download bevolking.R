# Sinds augustus 2023 is de bevolkingstabel niet meer direct op te vragen vanaf de servers van het CBS. Er kan alleen een volledige datadump worden gedownload,
# maar deze is nogal groot. (Overdreven voor dagelijks gebruik.) Dit script haalt van een gewenste regio de meest logische getallen op en slaat deze op voor later gebruik.
#
# Let op: uitvoeren kan even duren en heeft flink wat werkgeheugen en opslagruimte nodig.
# Bij foutmeldingen zul je waarschijnlijk IT lief aan moeten kijken om meer werkgeheugen te krijgen.
#
# In theorie zou het downloaden en uitpakken van de bestanden automatisch moeten kunnen, maar bij mijn testruns ging dit nooit goed.
# Voor nu is de oplossing daarom om via de browser de bestanden te downloaden (https://datasets.cbs.nl/csv/CBS/nl/03759ned) en deze uit te pakken in de submap 'bevolking'.
# Let op: map wordt naderhand leeggemaakt om ruimte te besparen! Het zipbestand kan wel gewoon bewaard worden.

library(cbsodataR)
library(this.path)
library(tidyverse)

setwd(dirname(this.path()))

# Selecteer de gewenste regio's om te exporteren uit de dataset. Voor het gemak kan met de onderstaande code gemakkelijk een GGD-regio worden geselecteerd.
regios = cbs_get_data("85067NED") %>% select(RegioS, Naam_2, Code_14, Naam_15) %>% rename(Gemeente=Naam_2, GGDregio=Code_14, GGDnaam=Naam_15)

regios.gewenst = data.frame(Code=str_trim(regios$RegioS[str_detect(regios$GGDnaam, "Noord- en Oost-Gelderland")]),
                            Naam=str_trim(regios$Gemeente[str_detect(regios$GGDnaam, "Noord- en Oost-Gelderland")]))
regios.gewenst = bind_rows(regios.gewenst, data.frame(Code="NL01"))

# Geef hier de gewenste jaren op. Als dit een enkel getal is, en het getal is kleiner dan 1970, wordt er aangenomen dat dit het aantal afgelopen jaren specificeert.
# Bijvoorbeeld: jaar.gewenst = 3 betekent laatste 3 jaar, jaar.gewenst = c(2020, 2021) betekent precies wat het lijkt.
jaar.gewenst = 3




#
# Vanaf hier niks aanpassen!
#



# metadata van de tabel
meta = cbs_get_meta("03759ned")

# zou leuk zijn als dit ooit gaat werken
#download.file("https://datasets.cbs.nl/csv/CBS/nl/03759ned", "cbs_tabel.zip")
#unzip("cbs_tabel.zip", exdir="bevolking")
#file.remove("cbs_tabel.zip")

# RIP geheugen en processor, ga maar even koffie halen
data = read.table("bevolking/Observations.csv", sep=";", header=T)
table(data$RegioS)

# gelijk de onnodige informatie verwijderen, zodat we de computer niet meer horen kraken
# N.B.: bewust niet met tidyverse, omdat deze meer rekenkracht vraagt, en dat kunnen we de arme computer nu niet aandoen
data = data[data$RegioS %in% regios.gewenst$Code,]

# gewenste jaren achterhalen
if (length(jaar.gewenst) == 1 && jaar.gewenst < 1970) {
  laatstejaren = sort(meta$Perioden$Title, decreasing=T)[1:jaar.gewenst]
  jaren = meta$Perioden$Key[meta$Perioden$Title %in% laatstejaren]
} else {
  jaren = meta$Perioden$Key[meta$Perioden$Title %in% as.character(jaar.gewenst)]
}

for (jaar in jaren) {
  data.jaar = data[data$Perioden == jaar & data$ValueAttribute != "Impossible",] %>%
    select(-StringValue, -ValueAttribute, -Id)
  
  # toevoegen labels
  data.jaar = data.jaar %>%
    mutate(Measure=case_when(Measure == "M000352" ~ "Bevolking1Jan",
                             TRUE ~ "BevolkingGem")) %>%
    mutate(Value=as.numeric(Value)) %>%
    pivot_wider(names_from=Measure, values_from=Value) %>%
    left_join(meta$Geslacht %>% select(Key, Title) %>% mutate(Key=str_trim(Key)) %>% rename(Geslacht_label=Title), by=c("Geslacht"="Key")) %>%
    left_join(meta$Leeftijd %>% select(Key, Title) %>% mutate(Key=str_trim(Key)) %>% rename(Leeftijd_label=Title) %>% mutate(Key=as.numeric(Key)), by=c("Leeftijd"="Key")) %>% # we moeten Key omzetten naar numeriek, want logica
    left_join(meta$BurgerlijkeStaat %>% select(Key, Title) %>% mutate(Key=str_trim(Key)) %>% rename(BurgerlijkeStaat_label=Title), by=c("BurgerlijkeStaat"="Key")) %>%
    left_join(meta$RegioS %>% select(Key, Title)  %>% mutate(Key=str_trim(Key)) %>% rename(RegioS_label=Title), by=c("RegioS"="Key")) %>%
    left_join(meta$Perioden %>% select(Key, Title) %>% mutate(Key=str_trim(Key)) %>% rename(Perioden_label=Title), by=c("Perioden"="Key"))
  
  write.csv(data.jaar, paste0("bevolking_totaal_", str_sub(jaar, end=4), ".csv"))
  
  write.csv(data.jaar %>%
              filter(BurgerlijkeStaat_label == "Totaal burgerlijke staat", Geslacht_label == "Totaal mannen en vrouwen", Leeftijd_label == "Totaal") %>%
              group_by(RegioS, RegioS_label) %>%
              summarize(n=sum(Bevolking1Jan, na.rm=T)),
            paste0("bevolking_perregio_", str_sub(jaar, end=4), ".csv"))
}

files = list.files("bevolking/", pattern=".*.csv")
for (file in files) {
  file.remove(paste0("bevolking/", file))
}

