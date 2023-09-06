#
#
# Om een overzicht van onze inwoners te krijgen halen we jaarlijks data op van het RIVM en CBS.
# Dit script combineert vaccinatiegegevens en inwonersdata, en maakt daar een aantal overzichtstabellen
# en grafieken van.
#
#


library(tidyverse)
library(this.path)
library(openxlsx)
library(cbsodataR)
library(tmap)
library(maptiles)
library(sf)

setwd(dirname(this.path()))

# wrapper voor sprintf, voor makkelijk weergeven
printf = function (...) cat(paste(sprintf(...),"\n"))

# GGD NOG kleuren
nog_colors = c("#1D1756", "#D1005D", "#2F6594", "#D3DFF1", "#FFBDDB")
nog_palette = c("#FAE6EF","#F6CCDF","#ED99BE","#E3669E","#D1005D")

# metadata ophalen voor het zoeken van gemeentes
gemeenten.nog = "Aalten, Apeldoorn, Berkelland, Bronckhorst, Brummen, Doetinchem, Elburg, Epe, Ermelo, Harderwijk, Hattem, Heerde, Lochem, Montferland, Nunspeet, Oldebroek, Oost Gelre, Oude IJsselstreek, Putten, Voorst, Winterswijk, Zutphen"
gemeenten.nog = str_trim(unlist(str_split(gemeenten.nog, ",")))
meta = cbs_get_meta("85372NED")
gemeenten = meta$WijkenEnBuurten %>%
  filter(str_starts(Key, "GM")) %>%
  filter(Title %in% gemeenten.nog)
buurten = meta$WijkenEnBuurten %>%
  filter(str_sub(Key, end=6) %in% str_trim(str_replace(gemeenten$Key, "GM", "BU"))) %>%
  left_join(gemeenten %>% select(Municipality, Title) %>% rename(Gemeente=Title), by="Municipality", keep=F)

# kaartdata eenmalig inladen
# deze is beschikbaar vanaf de website van het CBS: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen
# let op: de laagnamen kunnen wisselen tussen versies
# zie st_layers(bestandsnaam) voor de correcte naam
kaartdata.gemeente = st_read("../../../../Documenten/kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer = "cbs_gemeente_2021_gegeneraliseerd")
kaartdata.gemeente = kaartdata.gemeente %>%
  filter(statcode %in% str_trim(gemeenten$Key))
kaartdata.buurt = st_read("../../../../Documenten/kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer = "cbs_buurt_2020_gegeneraliseerd")
kaartdata.buurt = kaartdata.buurt %>%
  filter(statcode %in% str_trim(buurten$Key))

kaart = function (data, var, title, legend.title, textvar = NA, breaks=NA) {
  #box = st_bbox(data)
  bg = maptiles::get_tiles(data, crop=T, provider="Esri.WorldStreetMap", zoom=10)
  
  if (length(var) == 1) {
    result = tm_shape(bg) + tm_rgb() +
      tm_shape(data) +
      tm_fill(col = var,
              palette= nog_palette,
              alpha=0.4,
              breaks = quantile(data[[var]], c(seq(0,1,0.2)), na.rm = T),
              title=legend.title,
              legend.show=T,
              legend.format=list(fun=function(x) { if (all(x >= 0) && all(x <= 1)) return(sprintf("%.0f%%", x*100)) else return(round(x, digits=0)) },
                                 text.separator="tot"),
              legend.position=c("top", "right"),
              legend.title.fontface = "bold") +
      tm_borders(alpha=0.5, col=nog_colors[2]) +
      tm_layout(main.title=title,
                frame=F)
  } else {
    result = tm_shape(bg) + tm_rgb() + tm_shape(data) +
      tm_fill(col = var,
              palette=nog_palette,
              alpha=0.4,
              #breaks = breaks,
              title=legend.title) +
      tm_borders(alpha=0.5, col=nog_colors[2]) +
      tm_facets(nrow = 2, free.scales = F) +
      tm_layout(legend.outside=T,
                legend.format=list(fun=function(x) { if (all(x >= 0) && all(x <= 1)) return(sprintf("%.0f%%", x*100)) else return(round(x, digits=0)) },
                                   text.separator="tot"),
                legend.outside.position="right",
                legend.title.fontface = "bold",
                main.title=title,
                frame=F)
  }
  
  if (!is.na(textvar)) {
    result = result + tm_text(text=textvar, size="AREA", size.lowerbound=0.8, print.tiny=T)
  }
  
  result
}

##
## opleidingen
##
opleiding.gemeente = cbs_get_data("85372NED", WijkenEnBuurten=gemeenten$Key, Marges="MW00000") %>% cbs_add_label_columns()
# de buurten zijn te groot om in één keer te doen; loopen
opleiding.buurt = lapply(unique(buurten$Municipality), function (gemcode) {
  return(cbs_get_data("85372NED", WijkenEnBuurten=buurten$Key[buurten$Municipality == gemcode], Marges="MW00000") %>% cbs_add_label_columns())
})
opleiding.buurt = bind_rows(opleiding.buurt) %>%
  left_join(buurten %>% select(Key, Gemeente), by=c("WijkenEnBuurten"="Key"))
opleiding.perbuurt = opleiding.buurt %>%
  mutate(Opleidingsniveau_label = str_trim(Opleidingsniveau_label)) %>%
  group_by(Gemeente, WijkenEnBuurten, WijkenEnBuurten_label, Opleidingsniveau_label) %>%
  summarise(n=sum(Bevolking15Tot75Jaar_2)) %>%
  pivot_wider(names_from=Opleidingsniveau_label, values_from=n) %>%
  relocate(Hoog, .after=Middelbaar)
opleiding.pergemeente = opleiding.gemeente %>%
  mutate(Opleidingsniveau_label = str_trim(Opleidingsniveau_label)) %>%
  group_by(WijkenEnBuurten_label, Opleidingsniveau_label) %>%
  summarise(n=sum(Bevolking15Tot75Jaar_2)) %>%
  pivot_wider(names_from=Opleidingsniveau_label, values_from=n) %>%
  relocate(Hoog, .after=Middelbaar) %>%
  mutate(Perc.Laag=Laag/(Laag+Middelbaar+Hoog),
         Perc.Middelbaar=Middelbaar/(Laag+Middelbaar+Hoog),
         Perc.Hoog=Hoog/(Laag+Middelbaar+Hoog))


##
## kerncijfers per buurt
##
kerncijfers.buurt = lapply(unique(buurten$Municipality), function (gemcode) {
  return(cbs_get_data("85039NED", WijkenEnBuurten=buurten$Key[buurten$Municipality == gemcode]) %>% cbs_add_label_columns() %>% select(-MeestVoorkomendePostcode_114))
})
kerncijfers.buurt = bind_rows(kerncijfers.buurt)

inwoners.buurt = kerncijfers.buurt %>%
  select(WijkenEnBuurten, WijkenEnBuurten_label, Gemeentenaam_1, AantalInwoners_5:SterfteRelatief_27, GemiddeldeHuishoudensgrootte_32, Bevolkingsdichtheid_33,
         Koopwoningen_40:HuurwoningenTotaal_41) %>%
  mutate(Gemeentenaam_1=str_trim(Gemeentenaam_1))
colnames(inwoners.buurt) = str_replace(colnames(inwoners.buurt), "_\\d+$", "")
# combineren met opleidingsdata
inwoners.buurt = inwoners.buurt %>%
  left_join(opleiding.perbuurt, by=c("WijkenEnBuurten", "WijkenEnBuurten_label", "Gemeentenaam"="Gemeente"), keep=F)

##
## inwoners per leeftijd 
##

# de brontabel is niet meer beschikbaar op de servers van het CBS
# deze kan, indien gewenst, met een apart script worden ingelezen (zie github)
meta = cbs_get_meta("03759NED")
jaar.inwoners = as.numeric(max(meta$Perioden$Title))
inwoners.leeftijd = read.csv(paste0("../../../../Documenten/CBS data/bevolking_totaal_", jaar.inwoners, ".csv")) %>%
  filter(RegioS %in% str_trim(gemeenten$Key))
inwoners.leeftijd = inwoners.leeftijd %>%
  filter(BurgerlijkeStaat_label == "Totaal burgerlijke staat") %>%
  select(Geslacht_label, Leeftijd_label, RegioS, RegioS_label, Bevolking1Jan) %>%
  mutate(Leeftijd=as.numeric(str_match(Leeftijd_label, "(\\d+) jaar$")[,2]))
inwoners.pergroep = inwoners.leeftijd %>%
  filter(!is.na(Leeftijd), Geslacht_label=="Totaal mannen en vrouwen") %>%
  mutate(Leeftijd_cat=cut(Leeftijd, c(0, 18, 44, 65, Inf), include.lowest=T)) %>%
  group_by(Leeftijd_cat) %>%
  summarise(n=sum(Bevolking1Jan, na.rm=T)) %>%
  mutate(perc=n/sum(n)*100)

##
## herkomst migranten
##
meta = cbs_get_meta("85458NED")
jaar.migranten = last(sort(meta$Perioden$Title))
data.migr = cbs_get_data("85458NED", Geslacht="T001038", Perioden=meta$Perioden$Key[meta$Perioden$Title == jaar.migranten], RegioS=str_trim(gemeenten$Key)) %>% cbs_add_label_columns()

# totalen per gemeente, zodat we een percentage kunnen berekenen
totalen.migr = data.migr %>%
  filter(Leeftijd == "10000", Herkomstland == "T001040", Geboorteland == "T001638") # alle leeftijden, alle landen

perc.migr = data.migr %>%
  filter(Leeftijd == "10000", Herkomstland != "T001040", Geboorteland == "T001638") %>% # alle herkomstlanden, maar geboorteland maakt niet uit
  group_by(RegioS_label, Herkomstland_label) %>%
  summarize(n=sum(Bevolking_1, na.rm=T)) %>%
  left_join(totalen.migr %>% select(RegioS_label, Bevolking_1), by="RegioS_label") %>%
  mutate(perc=n/Bevolking_1*100)

##
## vaccinatiegraad
##
vacc.graad = cbs_get_data("50117NED", catalog="RIVM", RegioS=str_trim(gemeenten$Key))
vacc.graad = vacc.graad %>%
  cbs_add_label_columns() %>%
  mutate(Jaar=as.numeric(str_extract(Perioden, "\\d+")),
         Regio=str_trim(RegioS)) %>%
  rename(Populatie=Populatie_1, Gevaccineerden=Gevaccineerden_2, Vaccinatiegraad=Vaccinatiegraad_3)
jaar.vacc = max(vacc.graad$Jaar, na.rm=T) # nieuwste cohort

##
## gezondheid per buurt
##
meta = cbs_get_meta("50090NED", catalog="RIVM")
jaar.gezondheid = as.numeric(max(meta$Perioden$Title))
gezondheid.buurt = lapply(unique(buurten$Municipality), function (gemcode) {
  return(cbs_get_data("50090NED", catalog="RIVM", WijkenEnBuurten=buurten$Key[buurten$Municipality == gemcode], Perioden=paste0(jaar.gezondheid, "JJ00"), Marges="MW00000") %>% cbs_add_label_columns())
})
gezondheid.buurt = bind_rows(gezondheid.buurt) %>% rename(ErnstigeGeluidhinderWeg50KmMinUur=ErnstigeGeluidhinderWeg50KmUur_42, ErnstigeGeluidhinderWeg50KmPlusUur=ErnstigeGeluidhinderWeg50KmUur_43)
# er zijn twee "ErnstigeGeluidhinderWeg50KmUur"-variabelen; die moeten we hernoemen om conflicten te voorkomen
colnames(gezondheid.buurt) = str_replace(colnames(gezondheid.buurt), "_\\d+$", "")

gezondheid.gemeentes = cbs_get_data("50090NED", catalog="RIVM", WijkenEnBuurten=gemeenten$Key, Perioden="2020JJ00", Marges="MW00000") %>%
  cbs_add_label_columns() %>%
  rename(ErnstigeGeluidhinderWeg50KmMinUur=ErnstigeGeluidhinderWeg50KmUur_42, ErnstigeGeluidhinderWeg50KmPlusUur=ErnstigeGeluidhinderWeg50KmUur_43)
colnames(gezondheid.gemeentes) = str_replace(colnames(gezondheid.buurt), "_\\d+$", "")

##
## zorgdeclaraties
##

# deze is beschikbaar vanaf de website van Vektis: https://www.vektis.nl/open-data
zorgkosten = read.csv("../../../../Documenten/CBS data/Vektis gemeente 2021.csv", sep=";")
zorgkosten = zorgkosten %>%
  mutate(gemeentenaam=str_to_lower(gemeentenaam)) %>%
  left_join(gemeenten %>% select(Key, Title) %>% mutate(gemeentenaam=str_to_lower(Title)) %>% rename(Gemeentecode=Key, Gemeente=Title), by="gemeentenaam", keep=F) %>%
  filter(!is.na(Gemeentecode))

##
## levensverwachting
##
levensverwachting.gezondheid = cbs_get_data("50108NED", catalog="RIVM", RegioS=c(str_trim(gemeenten$Key), "GG1413"), Marges="MW00000") %>% cbs_add_label_columns()
levensverwachting = cbs_get_data("50104NED", catalog="RIVM", RegioS=c(str_trim(gemeenten$Key), "GG1413"), Marges="MW00000", Geslacht="T001038", Perioden="2017G400") %>% cbs_add_label_columns()

##
## werknemers per branche
##
meta = cbs_get_meta("84916NED")
beroepen = cbs_get_data("84916NED", RegioS=str_trim(gemeenten$Key)) %>%
  cbs_add_label_columns() %>%
  mutate(WerkzameBeroepsbevolking_1=WerkzameBeroepsbevolking_1*1000) # aantallen zijn in duizenden

##
## algemene dingetjes
##
graphname = function (naam) { return(sprintf("IZB overzicht/graphs/%s_%s.png", gemeentenaam, naam)) }

bronnen = read.table(text=paste0("Element;Bron;Jaar\nOpleidingsniveau;CBS;2021\nKerncijfers wijken;CBS;2021\nInwonersaantallen;CBS;", jaar.inwoners, "\nMigranten en herkomst;CBS;", jaar.migranten, "\nGezondheid en leefstijl;RIVM;", jaar.gezondheid, "\nVaccinatiegraad;RIVM;", jaar.vacc, "\nZorgdeclaraties;Vektis;2021\nLevensverwachting;RIVM;2020"), header=T, sep=";")

##
## overzichtssheet per gemeente
##
titelStyle = createStyle(fontSize=16)
headerStyle = createStyle(border="TopBottomLeftRight", textDecoration="bold", fgFill=nog_colors[2], fontColour="#ffffff", halign="center")
kopStyle = createStyle(textDecoration="bold")
percStyle = createStyle(numFmt="PERCENTAGE", halign="center")
geldStyle = createStyle(numFmt="CURRENCY", halign="center")
numStyle = createStyle(numFmt="0.00", halign="center")
for (i in 1:nrow(gemeenten)) {
  gemeentecode = gemeenten$Key[i]
  gemeentenaam = gemeenten$Title[i]
  
  wb = createWorkbook()
  
  addWorksheet(wb, "Overzicht")
  setColWidths(wb, "Overzicht", cols=1, widths=20)
  writeData(wb, "Overzicht", sprintf("Overzicht %s", gemeentenaam), startCol=1, startRow=1)
  addStyle(wb, "Overzicht", titelStyle, cols=1, rows=1)
  writeData(wb, "Overzicht", sprintf("Dit document geeft een overzicht van de demografie en leefomstandigheden in %s op 1 januari van de jaren 2020-2023, afhankelijk van de bron.", gemeentenaam),
            startCol=1, startRow=3)
  writeData(wb, "Overzicht", "Let op! De data is geschat op basis van het BRP en kan enigszins afwijken van de realiteit. Daarnaast zijn vanwege de privacy aantallen afgerond op de dichtstbijzijnde vijfvoud.",
            startCol=1, startRow=4)
  
  # basis demografie
  inwoners.gemeente = inwoners.buurt %>%
    filter(Gemeentenaam == gemeentenaam) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  displaydata = inwoners.gemeente %>% select(AantalInwoners:k_65JaarOfOuder) %>% mutate(across(Mannen:k_65JaarOfOuder, ~.x/AantalInwoners))
  colnames(displaydata) = c("Aantal inwoners", "Mannen", "Vrouwen", "0 t/m 14", "15 t/m 24", "25 t/m 44", "45 t/m 64", "65+")
  writeData(wb, "Overzicht", "Demografie", startCol=1, startRow=6)
  addStyle(wb, "Overzicht", kopStyle, cols=1, rows=6, stack=T)
  writeData(wb, "Overzicht", displaydata, startCol=1, startRow=7, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", percStyle, cols=2:ncol(displaydata), rows=8, gridExpand=T, stack=T)
  
  # huwelijkse staat en afkomst
  displaydata = inwoners.gemeente %>% select(Ongehuwd:OverigNietWesters) %>% mutate(across(everything(), ~.x/inwoners.gemeente$AantalInwoners))
  names = str_replace_all(colnames(displaydata), "([a-z]{1})([A-Z]{1})", "\\1 \\2")
  displaydata = t(displaydata) %>% data.frame(Perc=.) %>% mutate("Groep"=names) %>% relocate(Groep)
  writeData(wb, "Overzicht", "Huwelijkse staat en afkomst", startCol=1, startRow=10)
  addStyle(wb, "Overzicht", kopStyle, cols=1, rows=10, stack=T)
  writeData(wb, "Overzicht", displaydata[1:4,], startCol=1, startRow=11, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", percStyle, cols=2, rows=11+1:4, gridExpand=T, stack=T)
  writeData(wb, "Overzicht", displaydata[5:nrow(displaydata),], startCol=1, startRow=17, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", percStyle, cols=2, rows=17+(1:nrow(displaydata)-4), gridExpand=T, stack=T)
  
  # opleidingsniveau
  opleiding = opleiding.gemeente %>%
    filter(WijkenEnBuurten_label == gemeentenaam) %>%
    select(Opleidingsniveau_label, Bevolking15Tot75Jaar_2) %>%
    rename(Niveau=Opleidingsniveau_label, Perc=Bevolking15Tot75Jaar_2) %>%
    mutate(Perc=Perc/sum(Perc, na.rm=T))
    
  writeData(wb, "Overzicht", "Opleiding", startCol=4, startRow=10)
  addStyle(wb, "Overzicht", kopStyle, cols=4, rows=10, stack=T)
  writeData(wb, "Overzicht", opleiding, startCol=4, startRow=11, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", percStyle, cols=5, rows=12:15, gridExpand=T, stack=T)
  
  # zorgkosten
  zorgkosten.overzicht = zorgkosten %>%
    filter(Gemeentecode == gemeentecode) %>%
    mutate(max_leeftijd=as.numeric(str_match(leeftijdsklasse, "\\d+ t/m (\\d+) jaar")[,2]),
           leeftijdsklasse=case_when(max_leeftijd < 20 ~ "Jongeren (20-)",
                                     max_leeftijd < 65 ~ "Volwassenen",
                                     TRUE ~ "Ouderen (65+)")) %>%
    group_by(leeftijdsklasse) %>%
    summarize(across(aantal_verzekerdejaren:kosten_overig, ~sum(., na.rm=T)))
  zorgkosten.pj = data.frame(Leeftijd=zorgkosten.overzicht$leeftijdsklasse, Kosten=rowSums(zorgkosten.overzicht[3:ncol(zorgkosten.overzicht)]) / zorgkosten.overzicht$aantal_verzekerdejaren)
  zorgkosten.pj = zorgkosten.pj[c(1,3,2),] # ouderen onderaan
  
  writeData(wb, "Overzicht", "Zorgkosten p.p.p.j.", startCol=4, startRow=16)
  addStyle(wb, "Overzicht", kopStyle, cols=4, rows=16, stack=T)
  writeData(wb, "Overzicht", zorgkosten.pj, startCol=4, startRow=17, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", geldStyle, cols=5, rows=18:(18+nrow(zorgkosten.pj)), gridExpand=T, stack=T)
  
  # levensverwachting
  levensverwachting.gemeente = matrix(nrow=3, ncol=2)
  colnames(levensverwachting.gemeente) = c(gemeentenaam, "GGD NOG")
  rownames(levensverwachting.gemeente) = c("Goed ervaren gezondheid", "Zonder lichamelijke beperkingen", "Levensverwachting")
  levensverwachting.gemeente[1,1] = levensverwachting.gezondheid$GoedErvarenGezondheid_1[levensverwachting.gezondheid$RegioS_label == gemeentenaam]
  levensverwachting.gemeente[1,2] = levensverwachting.gezondheid$GoedErvarenGezondheid_1[levensverwachting.gezondheid$RegioS == "GG1413"]
  levensverwachting.gemeente[2,1] = levensverwachting.gezondheid$ZonderLichamelijkeBeperkingen_2[levensverwachting.gezondheid$RegioS_label == gemeentenaam]
  levensverwachting.gemeente[2,2] = levensverwachting.gezondheid$ZonderLichamelijkeBeperkingen_2[levensverwachting.gezondheid$RegioS == "GG1413"]
  levensverwachting.gemeente[3,1] = levensverwachting$Levensverwachting_1[levensverwachting$RegioS_label == gemeentenaam & levensverwachting$Leeftijd_label == "0 jaar"]
  levensverwachting.gemeente[3,2] = levensverwachting$Levensverwachting_1[levensverwachting$Leeftijd_label == "0 jaar" & levensverwachting$RegioS == "GG1413"]
  
  writeData(wb, "Overzicht", "Levensverwachting", startCol=4, startRow=22)
  addStyle(wb, "Overzicht", kopStyle, cols=4, rows=22, stack=T)
  writeData(wb, "Overzicht", levensverwachting.gemeente, startCol=4, startRow=23, headerStyle=headerStyle, borders="surrounding", rowNames=T)
  addStyle(wb, "Overzicht", numStyle, cols=5:6, rows=23:(23+nrow(levensverwachting.gemeente)), gridExpand=T, stack=T)
  
  # beroepen
  beroepen.gemeente = beroepen %>%
    filter(RegioS_label == gemeentenaam, Beroepsklasse_label != "Totaal") %>%
    select(Beroepsklasse_label, WerkzameBeroepsbevolking_1) %>%
    rename(Sector=Beroepsklasse_label, Aantal=WerkzameBeroepsbevolking_1) %>%
    mutate(Perc=Aantal/sum(Aantal, na.rm=T)) %>%
    select(-Aantal)
  
  writeData(wb, "Overzicht", "Beroepen", startCol=1, startRow=26)
  addStyle(wb, "Overzicht", kopStyle, cols=1, rows=26, stack=T)
  writeData(wb, "Overzicht", beroepen.gemeente, startCol=1, startRow=27, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Overzicht", percStyle, cols=2:3, rows=28:(28+nrow(beroepen.gemeente)), gridExpand=T, stack=T)
  
  # kaartje, voor de leuk
  png(graphname("kaart"))
  print(tm_shape(kaartdata.gemeente %>% mutate(gemeente=ifelse(statnaam == gemeentenaam, gemeentenaam, ""))) +
    tm_fill(col = "gemeente",
            palette = nog_colors[c(5,2)],
            legend.show=F) +
    tm_text(text="gemeente", size="AREA", size.lowerbound=0.8, print.tiny=T) +
    tm_layout(frame=F))
  dev.off()
  insertImage(wb, "Overzicht", graphname("kaart"), startRow=11, startCol=8, width=5, height=5)
  
  # demografie en leefstijl
  addWorksheet(wb, "Leefstijl")
  setColWidths(wb, "Leefstijl", cols=1:15, widths=20)
  
  gezondheid.gemeente = gezondheid.buurt %>%
    filter(str_trim(Gemeentenaam) == gemeentenaam) %>%
    select(Leeftijd_label, WijkenEnBuurten_label, ErvarenGezondheidGoedZeerGoed:BrozeGezondheidSocialeDomein) %>%
    rename(Buurt=WijkenEnBuurten_label) %>%
    mutate(across(ErvarenGezondheidGoedZeerGoed:BrozeGezondheidSocialeDomein, ~.x/100))
  
  displaydata = gezondheid.gemeente %>%
    filter(Leeftijd_label == "18 jaar of ouder") %>%
    select(Buurt, VoldoetAanBeweegrichtlijn:OvermatigeDrinker)
  writeData(wb, "Leefstijl", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Leefstijl", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  
  # leeftijden inwoners
  addWorksheet(wb, "Leeftijden")
  setColWidths(wb, "Leeftijden", cols=1:7, widths=12)
  
  inwoners.gemeente = inwoners.leeftijd %>%
    filter(Geslacht_label != "Totaal mannen en vrouwen", !is.na(Leeftijd), RegioS_label == gemeentenaam) %>%
    arrange(Leeftijd)
  inwoners.gemeente.totaal = sum(inwoners.gemeente$Bevolking1Jan, na.rm=T)
  
  displaydata = inwoners.gemeente %>%
    rename(n=Bevolking1Jan) %>%
    select(Leeftijd, n, Geslacht_label) %>%
    mutate(perc=n/inwoners.gemeente.totaal) %>%
    pivot_wider(names_from="Geslacht_label", values_from=c("n", "perc")) %>%
    relocate(perc_Mannen, .after=n_Mannen) %>%
    rename(Mannen=n_Mannen, Vrouwen=n_Vrouwen, percM=perc_Mannen, percV=perc_Vrouwen)
  writeData(wb, "Leeftijden", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Leeftijden", percStyle, cols=c(3,5), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  
  # leeftijden weergeven in grafiek
  # om een tweezijdige grafiek te maken moeten we ??n van de waardes flippen, in dit geval zetten we mannen op negatief
  displaydata = inwoners.gemeente %>%
    mutate(Leeftijd_cat=(cut(Leeftijd, breaks=seq(-1, max(Leeftijd)+5, 5), labels=F)-1)*5) %>%
    group_by(Leeftijd_cat, Geslacht_label) %>%
    summarise(perc=sum(Bevolking1Jan, na.rm=T)/inwoners.gemeente.totaal*100) %>%
    mutate(perc=ifelse(Geslacht_label == "Mannen", -1*perc, perc))
  range.x = pretty(displaydata$perc)
  png(graphname("leeftijden"))
  print(ggplot(displaydata, aes(x=Leeftijd_cat, y=perc, group=Geslacht_label, fill=Geslacht_label)) +
    geom_bar(stat="identity", width=3) +
    coord_flip() +
    scale_y_continuous(breaks = range.x, 
                       labels = abs(range.x)) +
    scale_fill_manual(values=c(nog_colors[1], nog_colors[2]),
                      name="",
                      breaks=c("Mannen", "Vrouwen"),
                      labels=c("Mannen", "Vrouwen")) +
    labs(x = "Leeftijd", y = "Percentage (%)", title = "Verdeling leeftijden") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))
  dev.off()
  insertImage(wb, "Leeftijden", graphname("leeftijden"), startRow=3, startCol=7, width=5, height=5)
  
  # migranten en herkomst
  addWorksheet(wb, "Migranten")
  setColWidths(wb, "Migranten", cols=1, widths=30)
  
  displaydata = perc.migr %>%
    filter(RegioS_label == gemeentenaam) %>%
    ungroup() %>%
    select(-c(RegioS_label, Bevolking_1)) %>%
    rename(Regio=Herkomstland_label) %>%
    mutate(perc=perc/100)
  writeData(wb, "Migranten", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Migranten", percStyle, cols=3, rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  
  png(graphname("herkomst_migranten"))
  print(ggplot(perc.migr %>%
           filter(RegioS_label == gemeentenaam, Herkomstland_label %in% c("Nederland", "Europa (exclusief Nederland)", "Afrika", "Amerika", "Azië", "Oceanië")) %>%
           mutate(perc_label = sprintf("%0.1f%%", perc)),
         aes(x=Herkomstland_label, y=n, fill=Herkomstland_label)) +
    geom_col() +
    geom_text(aes(label=perc_label), vjust=0) +
    labs(x="Regio van herkomst", y="Aantal", title="Herkomst inwoners") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90),
          legend.position = "none"))
  dev.off()
  insertImage(wb, "Migranten", graphname("herkomst_migranten"), startRow=2, startCol=5, width=5, height=5)
  
  # opleiding
  addWorksheet(wb, "Opleiding")
  setColWidths(wb, "Opleiding", cols=1, widths=30)
  
  displaydata = opleiding.perbuurt %>%
    filter(Gemeente == gemeentenaam) %>%
    ungroup() %>%
    select(-Gemeente) %>%
    rename(Buurt=WijkenEnBuurten_label) %>%
    mutate(n=Laag+Middelbaar+Hoog,
           Laag=Laag/n,
           Middelbaar=Middelbaar/n,
           Hoog=Hoog/n)
  writeData(wb, "Opleiding", displaydata[,-1], startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Opleiding", percStyle, cols=2:4, rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  
  
  kaartdata = kaartdata.buurt %>% filter(statcode %in% displaydata$WijkenEnBuurten) %>%
    left_join(displaydata, by=c("statcode"="WijkenEnBuurten")) %>%
    mutate(Buurt.hr=ifelse(!str_detect(Buurt, "Verspreide"),"",str_sub(as.character(Buurt), start=18)))
  
  png(graphname("opleiding"))
  print(kaart(kaartdata, var="Hoog", title="Percentage hoogopgeleiden", legend.title="Percentage"))
  dev.off()
  insertImage(wb, "Opleiding", graphname("opleiding"), startRow=2, startCol=7, width=5, height=5)
  
  png(graphname("opleiding_heatmap"))
  print(ggplot(inwoners.buurt %>%
           filter(Gemeentenaam == gemeentenaam) %>%
           pivot_longer(Laag:Hoog, values_to="n", names_to="Opleiding") %>%
           mutate(perc=n/AantalInwoners*100,
                  Opleiding=factor(Opleiding, levels=c("Laag", "Middelbaar", "Hoog"), ordered=T)),
         aes(x=Opleiding, y=WijkenEnBuurten_label, fill=perc)) +
    geom_tile(color="white", lwd=1) +
    scale_fill_gradient(low=nog_palette[1], high=nog_palette[5], name="Percentage", labels=function (x) { return(sprintf("%.0f%%", x)) }) +
    labs(x="", y="", title="Opleidingsniveau per buurt") +
    theme_minimal())
  dev.off()
  insertImage(wb, "Opleiding", graphname("opleiding_heatmap"), startRow=2, startCol=13, width=5, height=5)
  
  # vaccinatiegraad
  addWorksheet(wb, "Vaccinaties")
  setColWidths(wb, "Vaccinaties", cols=1, widths=30)  
  
  vacc.graad.gemeente = vacc.graad %>%
    filter(RegioS_label == gemeentenaam)
  
  displaydata = vacc.graad.gemeente %>%
    select(Vaccinaties_label, Jaar, Vaccinatiegraad) %>%
    rename(Vaccinatie=Vaccinaties_label) %>%
    mutate(Vaccinatiegraad=Vaccinatiegraad/100) %>%
    pivot_wider(names_from="Jaar", values_from="Vaccinatiegraad")
  writeData(wb, "Vaccinaties", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Vaccinaties", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  
  ylim = vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties %in% c("A028769", "A028770", "A028771", "A028772")]
  ylim = ylim[!is.na(ylim)]
  ylim.diff = max(ylim)-min(ylim)
  png(graphname("vacc_1"))
  plot(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028769"], # DKTP
       vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028769"],
       type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (2 jr)", xlab="Jaar", ylab="Percentage (%)", ylim=c(min(ylim)-(ylim.diff), min(c(max(ylim)+(ylim.diff), 100)))) 
  grid(nx=NA, ny=NULL)
  par(new=T)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028769"], # DKTP
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028769"], col=nog_colors[1], lwd=1)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028770"], # Hib
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028770"], col=nog_colors[2], lwd=2)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028771"], # BMR
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028771"], col=nog_colors[3], lwd=2)
  legend("bottomleft", legend=c("DKTP", "Hib", "BMR"), fill=nog_colors[1:3], bty="n")
  dev.off()
  insertImage(wb, "Vaccinaties", graphname("vacc_1"), startRow=nrow(displaydata)+3, startCol=1, width=5, height=5)
  
  ylim = vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties %in% c("A028772", "A028773", "A028774", "A028775")]
  ylim = ylim[!is.na(ylim)]
  ylim.diff = max(ylim)-min(ylim)
  png(graphname("vacc_2"))
  plot(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028772"], # MenACWY
       vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028772"],
       type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (2 jr)", xlab="Jaar", ylab="Percentage (%)", ylim=c(min(ylim)-(ylim.diff), min(c(max(ylim)+(ylim.diff), 100)))) 
  grid(nx=NA, ny=NULL)
  par(new=T)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028772"], # MenACWY
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028772"], col=nog_colors[1], lwd=1)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028773"], # Pneumo
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028773"], col=nog_colors[2], lwd=2)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028774"], # HepB
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028774"], col=nog_colors[3], lwd=2)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028775"], # volledig
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028775"], col=nog_colors[4], lwd=2)
  legend("bottomleft", legend=c("MenACWY", "Pneumokokken", "HepB", "Volledig"), fill=nog_colors[1:4], bty="n")
  dev.off()
  insertImage(wb, "Vaccinaties", graphname("vacc_2"), startRow=nrow(displaydata)+3, startCol=6, width=5, height=5)
  
  ylim = vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties %in% c("A028778", "A028779", "A028780", "A049175")]
  ylim = ylim[!is.na(ylim)]
  ylim.diff = max(ylim)-min(ylim)
  png(graphname("vacc_3"))
  plot(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028778"], # DKTP volledig
       vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028778"],
       type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (12+ jr)", xlab="Jaar", ylab="Percentage (%)",
       ylim=c(max(c(min(ylim)-(ylim.diff), 0)), min(c(max(ylim)+(ylim.diff), 100)))) 
  grid(nx=NA, ny=NULL)
  par(new=T)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028778"], # DKTP volledig
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028778"], col=nog_colors[1], lwd=1)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028779"], # BMR
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028779"], col=nog_colors[2], lwd=2)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A028780"], # HPV
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A028780"], col=nog_colors[3], lwd=2)
  lines(vacc.graad.gemeente$Jaar[vacc.graad.gemeente$Vaccinaties == "A049175"], # MenACWY
        vacc.graad.gemeente$Vaccinatiegraad[vacc.graad.gemeente$Vaccinaties == "A049175"], col=nog_colors[4], lwd=2)
  legend("bottomleft", legend=c("DKTP", "BMR", "HPV", "MenACWY"), fill=nog_colors[1:4], bty="n")
  dev.off()
  insertImage(wb, "Vaccinaties", graphname("vacc_3"), startRow=nrow(displaydata)+3, startCol=13, width=5, height=5)
  
  # gezondheid
  addWorksheet(wb, "Gezondheid")
  setColWidths(wb, "Gezondheid", cols=1, widths=30)
  
  
  displaydata = gezondheid.gemeente %>%
    filter(Leeftijd_label == "18 jaar of ouder") %>%
    select(Buurt, EenOfMeerLangdurigeAandoeningen:BeperkingInBewegen)
  writeData(wb, "Gezondheid", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Gezondheid", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)
  setColWidths(wb, "Gezondheid", cols=2:ncol(displaydata), widths=20)
  
  # geografische indeling
  addWorksheet(wb, "Verschillen per buurt")
  setColWidths(wb, "Verschillen per buurt", cols=1, widths=20)
  
  inwoners.gemeente = inwoners.buurt %>%
    filter(Gemeentenaam == gemeentenaam) %>%
    select(-WijkenEnBuurten, -Gemeentenaam) %>%
    mutate(across(Mannen:OverigNietWesters, ~.x/AantalInwoners),
           Koopwoningen=Koopwoningen/100,
           HuurwoningenTotaal=HuurwoningenTotaal/100, # dit zijn al percentages
           Laag=Laag/(Laag+Middelbaar+Hoog),
           Middelbaar=Middelbaar/(Laag+Middelbaar+Hoog),
           Hoog=Hoog/(Laag+Middelbaar+Hoog)) %>%
    relocate(Laag, Middelbaar, Hoog, .after=Vrouwen) %>%
    rename(Laag.Opl=Laag, Middelbaar.Opl=Middelbaar, Hoog.Opl=Hoog, Huurwoningen=HuurwoningenTotaal,
           Buurt=WijkenEnBuurten_label)
  colnames(inwoners.gemeente) = str_replace(colnames(inwoners.gemeente), "k_", "")
  writeData(wb, "Verschillen per buurt", inwoners.gemeente, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Verschillen per buurt", percStyle, cols=c(3:23, ncol(inwoners.gemeente)-(1:0)), rows=2:(nrow(inwoners.gemeente)+1), gridExpand=T, stack=T)
  
  # zorgkosten
  addWorksheet(wb, "Zorgkosten")
  
  zorgkosten.leeftijd = zorgkosten %>%
    filter(Gemeentecode == gemeentecode) %>%
    group_by(geslacht, leeftijdsklasse) %>%
    summarize(across(where(is.numeric), ~sum(.x))) %>%
    select(-aantal_bsn)
  displaydata =  matrix(rowSums(zorgkosten.leeftijd[,-c(1:3)])/zorgkosten.leeftijd$aantal_verzekerdejaren, nrow=19, ncol=2)
  colnames(displaydata) = c("M", "V")
  rownames(displaydata) = str_replace(zorgkosten.leeftijd$leeftijdsklasse[1:19], " jaar", "")
  
  png(graphname("zorgkosten"))
  barplot(t(displaydata), main="Zorgkosten per persoon per jaar", ylab="Kosten (€)", beside=T, col=nog_colors[1:2],
          legend.text=c("Mannen", "Vrouwen"), args.legend=c(x="topleft"), las=2)
  dev.off()
  insertImage(wb, "Zorgkosten", graphname("zorgkosten"), startRow=2, startCol=2, width=5, height=5)
  
  zorgkosten.gemeente = zorgkosten %>%
    group_by(Gemeente) %>%
    summarize(across(where(is.numeric), ~sum(.x))) %>%
    select(-aantal_bsn)
  zorgkosten.gemeente$Bedrag = rowSums(zorgkosten.gemeente[,-c(1:2)])/zorgkosten.gemeente$aantal_verzekerdejaren
  displaydata = zorgkosten.gemeente[c("Gemeente", "Bedrag")]
  
  kaartdata = kaartdata.gemeente %>% filter(statnaam %in% displaydata$Gemeente) %>%
    left_join(displaydata, by=c("statnaam"="Gemeente")) %>%
    mutate(display=ifelse(statnaam == gemeentenaam, gemeentenaam, ""))
  
  png(graphname("zorgkosten_kaart"))
  print(kaart(kaartdata, var="Bedrag", title="Gemiddelde zorgkosten p.p.p.j.", legend.title="Bedrag (€)", textvar="display"))
  dev.off()
  insertImage(wb, "Zorgkosten", graphname("zorgkosten_kaart"), startRow=2, startCol=8, width=5, height=5)
  
  addWorksheet(wb, "Zorgkosten data")
  setColWidths(wb, "Zorgkosten data", cols=1, widths=20)
  
  zorgkosten.gemeente = zorgkosten %>%
    filter(Gemeentecode == gemeentecode) %>%
    select(-gemeentenaam, -Gemeentecode, -Gemeente)
  
  writeData(wb, "Zorgkosten data", zorgkosten.gemeente, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
  addStyle(wb, "Zorgkosten data", geldStyle, cols=5:ncol(zorgkosten.gemeente), rows=2:(nrow(zorgkosten.gemeente)+1), gridExpand=T, stack=T)
  
  # bronvermelding
  addWorksheet(wb, "Bronvermelding")
  setColWidths(wb, "Bronvermelding", cols=1, widths=20)
  writeData(wb, "Bronvermelding", "Bronvermelding", startRow=1, startCol=1)
  addStyle(wb, "Bronvermelding", titelStyle, cols=1, rows=1)
  
  writeData(wb, "Bronvermelding", bronnen, startCol=1, startRow=3)
  
  saveWorkbook(wb, paste0("IZB overzicht/", gemeentenaam, ".xlsx"), overwrite=T)
  printf("Overzicht van %s opgeslagen. (%d/%d)", gemeentenaam, i, nrow(gemeenten)+1)
}

##
## overzichtssheet voor de regio
##
wb = createWorkbook()

gemeentenaam = "NOG" # voor de grafieken

addWorksheet(wb, "Overzicht")
setColWidths(wb, "Overzicht", cols=1, widths=20)
writeData(wb, "Overzicht", "Overzicht GGD NOG", startCol=1, startRow=1)
addStyle(wb, "Overzicht", titelStyle, cols=1, rows=1)
writeData(wb, "Overzicht", sprintf("Dit document geeft een overzicht van de demografie en leefomstandigheden in %s op 1 januari 2020 of 2021, afhankelijk van de bron.", gemeentenaam),
          startCol=1, startRow=3)
writeData(wb, "Overzicht", "Let op! De data is geschat op basis van het BRP en kan enigszins afwijken van de realiteit. Daarnaast zijn vanwege de privacy aantallen afgerond op de dichtstbijzijnde vijfvoud.",
          startCol=1, startRow=4)

# basis demografie
inwoners.nog = inwoners.buurt %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
displaydata = inwoners.nog %>% select(AantalInwoners:k_65JaarOfOuder) %>% mutate(across(Mannen:k_65JaarOfOuder, ~.x/AantalInwoners))
colnames(displaydata) = c("Aantal inwoners", "Mannen", "Vrouwen", "0 t/m 14", "15 t/m 24", "25 t/m 44", "45 t/m 64", "65+")
writeData(wb, "Overzicht", "Demografie", startCol=1, startRow=6)
addStyle(wb, "Overzicht", kopStyle, cols=1, rows=6, stack=T)
writeData(wb, "Overzicht", displaydata, startCol=1, startRow=7, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", percStyle, cols=2:ncol(displaydata), rows=8, gridExpand=T, stack=T)

# huwelijkse staat en afkomst
displaydata = inwoners.nog %>% select(Ongehuwd:OverigNietWesters) %>% mutate(across(everything(), ~.x/inwoners.nog$AantalInwoners))
names = str_replace_all(colnames(displaydata), "([a-z]{1})([A-Z]{1})", "\\1 \\2")
displaydata = t(displaydata) %>% data.frame(Perc=.) %>% mutate("Groep"=names) %>% relocate(Groep)
writeData(wb, "Overzicht", "Huwelijkse staat en afkomst", startCol=1, startRow=10)
addStyle(wb, "Overzicht", kopStyle, cols=1, rows=10, stack=T)
writeData(wb, "Overzicht", displaydata[1:4,], startCol=1, startRow=11, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", percStyle, cols=2, rows=11+1:4, gridExpand=T, stack=T)
writeData(wb, "Overzicht", displaydata[5:nrow(displaydata),], startCol=1, startRow=17, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", percStyle, cols=2, rows=17+(1:nrow(displaydata)-4), gridExpand=T, stack=T)

# opleidingsniveau
opleiding = opleiding.gemeente %>%
  select(Opleidingsniveau_label, Bevolking15Tot75Jaar_2) %>%
  rename(Niveau=Opleidingsniveau_label) %>%
  group_by(Niveau) %>%
  summarize(Perc=sum(Bevolking15Tot75Jaar_2)) %>%
  mutate(Perc=Perc/sum(Perc, na.rm=T))

writeData(wb, "Overzicht", "Opleiding", startCol=4, startRow=10)
addStyle(wb, "Overzicht", kopStyle, cols=4, rows=10, stack=T)
writeData(wb, "Overzicht", opleiding, startCol=4, startRow=11, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", percStyle, cols=5, rows=12:15, gridExpand=T, stack=T)

# zorgkosten
zorgkosten.overzicht = zorgkosten %>%
  mutate(max_leeftijd=as.numeric(str_match(leeftijdsklasse, "\\d+ t/m (\\d+) jaar")[,2]),
         leeftijdsklasse=case_when(max_leeftijd < 20 ~ "Jongeren (20-)",
                                   max_leeftijd < 65 ~ "Volwassenen",
                                   TRUE ~ "Ouderen (65+)")) %>%
  group_by(leeftijdsklasse) %>%
  summarize(across(aantal_verzekerdejaren:kosten_overig, ~sum(., na.rm=T)))
zorgkosten.pj = data.frame(Leeftijd=zorgkosten.overzicht$leeftijdsklasse, Kosten=rowSums(zorgkosten.overzicht[3:ncol(zorgkosten.overzicht)]) / zorgkosten.overzicht$aantal_verzekerdejaren)
zorgkosten.pj = zorgkosten.pj[c(1,3,2),] # ouderen onderaan

writeData(wb, "Overzicht", "Zorgkosten p.p.p.j.", startCol=4, startRow=16)
addStyle(wb, "Overzicht", kopStyle, cols=4, rows=16, stack=T)
writeData(wb, "Overzicht", zorgkosten.pj, startCol=4, startRow=17, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", geldStyle, cols=5, rows=18:(18+nrow(zorgkosten.pj)), gridExpand=T, stack=T)

# levensverwachting
levensverwachting.nog = matrix(nrow=3, ncol=1)
colnames(levensverwachting.nog) = c("GGD NOG")
rownames(levensverwachting.nog) = c("Goed ervaren gezondheid", "Zonder lichamelijke beperkingen", "Levensverwachting")
levensverwachting.nog[1] = levensverwachting.gezondheid$GoedErvarenGezondheid_1[levensverwachting.gezondheid$RegioS == "GG1413"]
levensverwachting.nog[2] = levensverwachting.gezondheid$ZonderLichamelijkeBeperkingen_2[levensverwachting.gezondheid$RegioS == "GG1413"]
levensverwachting.nog[3] = levensverwachting$Levensverwachting_1[levensverwachting$Leeftijd_label == "0 jaar" & levensverwachting$RegioS == "GG1413"]

writeData(wb, "Overzicht", "Levensverwachting", startCol=4, startRow=22)
addStyle(wb, "Overzicht", kopStyle, cols=4, rows=22, stack=T)
writeData(wb, "Overzicht", levensverwachting.nog, startCol=4, startRow=23, headerStyle=headerStyle, borders="surrounding", rowNames=T)
addStyle(wb, "Overzicht", numStyle, cols=5, rows=23:(23+nrow(levensverwachting.nog)), gridExpand=T, stack=T)

# beroepen
beroepen.nog = beroepen %>%
  filter(Beroepsklasse_label != "Totaal") %>%
  select(Beroepsklasse_label, WerkzameBeroepsbevolking_1) %>%
  rename(Sector=Beroepsklasse_label) %>%
  group_by(Sector) %>%
  summarize(Aantal=sum(WerkzameBeroepsbevolking_1, na.rm=T)) %>%
  mutate(Perc=Aantal/sum(Aantal, na.rm=T)) %>%
  select(-Aantal)

writeData(wb, "Overzicht", "Beroepen", startCol=1, startRow=26)
addStyle(wb, "Overzicht", kopStyle, cols=1, rows=26, stack=T)
writeData(wb, "Overzicht", beroepen.gemeente, startCol=1, startRow=27, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Overzicht", percStyle, cols=2:3, rows=28:(28+nrow(beroepen.gemeente)), gridExpand=T, stack=T)

# kaartje, voor de leuk
png(graphname("kaart"))
print(tm_shape(kaartdata.gemeente) +
        tm_fill(col = "statnaam",
                palette = nog_colors,
                legend.show=F) +
        tm_layout(frame=F))
dev.off()
insertImage(wb, "Overzicht", graphname("kaart"), startRow=11, startCol=8, width=5, height=5)

# demografie en leefstijl
addWorksheet(wb, "Leefstijl")
setColWidths(wb, "Leefstijl", cols=1:20, widths=20)

gezondheid.nog = gezondheid.buurt %>%
  select(Leeftijd_label, WijkenEnBuurten_label, Gemeentenaam, ErvarenGezondheidGoedZeerGoed:BrozeGezondheidSocialeDomein) %>%
  rename(Buurt=WijkenEnBuurten_label) %>%
  mutate(across(ErvarenGezondheidGoedZeerGoed:BrozeGezondheidSocialeDomein, ~.x/100))

displaydata = gezondheid.nog %>%
  filter(Leeftijd_label == "18 jaar of ouder") %>%
  select(Gemeentenaam, Buurt, VoldoetAanBeweegrichtlijn:OvermatigeDrinker)
writeData(wb, "Leefstijl", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Leefstijl", percStyle, cols=3:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

# leeftijden inwoners
addWorksheet(wb, "Leeftijden")
setColWidths(wb, "Leeftijden", cols=1:7, widths=12)

inwoners.nog = inwoners.leeftijd %>%
  filter(Geslacht_label != "Totaal mannen en vrouwen", !is.na(Leeftijd)) %>%
  group_by(Geslacht_label, Leeftijd) %>%
  summarize(n=sum(Bevolking1Jan)) %>%
  arrange(Leeftijd)
inwoners.nog.totaal = sum(inwoners.nog$n, na.rm=T)

displaydata = inwoners.nog %>%
  select(Leeftijd, n, Geslacht_label) %>%
  mutate(perc=n/inwoners.nog.totaal) %>%
  pivot_wider(names_from="Geslacht_label", values_from=c("n", "perc")) %>%
  relocate(perc_Mannen, .after=n_Mannen) %>%
  rename(Mannen=n_Mannen, Vrouwen=n_Vrouwen, percM=perc_Mannen, percV=perc_Vrouwen)
writeData(wb, "Leeftijden", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Leeftijden", percStyle, cols=c(3,5), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

# leeftijden weergeven in grafiek
# om een tweezijdige grafiek te maken moeten we ??n van de waardes flippen, in dit geval zetten we mannen op negatief
displaydata = inwoners.nog %>%
  mutate(Leeftijd_cat=(cut(Leeftijd, breaks=seq(-1, max(Leeftijd)+5, 5), labels=F)-1)*5) %>%
  group_by(Leeftijd_cat, Geslacht_label) %>%
  summarise(perc=sum(n, na.rm=T)/inwoners.nog.totaal*100) %>%
  mutate(perc=ifelse(Geslacht_label == "Mannen", -1*perc, perc))
range.x = pretty(displaydata$perc)
png(graphname("leeftijden"))
print(ggplot(displaydata, aes(x=Leeftijd_cat, y=perc, group=Geslacht_label, fill=Geslacht_label)) +
        geom_bar(stat="identity", width=3) +
        coord_flip() +
        scale_y_continuous(breaks = range.x, 
                           labels = abs(range.x)) +
        scale_fill_manual(values=c(nog_colors[1], nog_colors[2]),
                          name="",
                          breaks=c("Mannen", "Vrouwen"),
                          labels=c("Mannen", "Vrouwen")) +
        labs(x = "Leeftijd", y = "Percentage (%)", title = "Verdeling leeftijden") +
        theme_minimal() + 
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5)))
dev.off()
insertImage(wb, "Leeftijden", graphname("leeftijden"), startRow=3, startCol=7, width=5, height=5)

# migranten en herkomst
addWorksheet(wb, "Migranten")

png(graphname("herkomst_migranten"))
print(ggplot(perc.migr %>%
               filter(Herkomstland_label %in% c("Nederland", "Europa (exclusief Nederland)", "Afrika", "Amerika", "Azië", "Oceanië")) %>%
               group_by(Herkomstland_label) %>%
               summarize(n=sum(n, na.rm=T), Bevolking_1=sum(Bevolking_1, na.rm=T)) %>%
               mutate(perc=n/Bevolking_1*100,
                      perc_label = sprintf("%0.1f%%", perc)),
             aes(x=Herkomstland_label, y=n, fill=Herkomstland_label)) +
        geom_col() +
        geom_text(aes(label=perc_label), vjust=0) +
        labs(x="Regio van herkomst", y="Aantal", title="Herkomst inwoners") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90),
              legend.position = "none"))
dev.off()
insertImage(wb, "Migranten", graphname("herkomst_migranten"), startRow=2, startCol=1, width=5, height=5)

png(graphname("herkomst_migranten_gesplitst"), width=800, height=500)
print(ggplot(perc.migr %>%
               filter(Herkomstland_label %in% c("Nederland", "Europa (exclusief Nederland)", "Afrika", "Amerika", "Azië", "Oceanië")) %>%
               mutate(perc_label = sprintf("%0.1f%%", perc)),
             aes(x=RegioS_label, y=perc, fill=Herkomstland_label)) +
        geom_col(position="dodge") +
        labs(x="Gemeente", y="Percentage", title="Herkomst inwoners", fill="Herkomst") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=90),
              legend.position="bottom"))
dev.off()
insertImage(wb, "Migranten", graphname("herkomst_migranten_gesplitst"), startRow=2, startCol=8, width=8, height=5)

addWorksheet(wb, "Migranten data")
setColWidths(wb, "Migranten data", cols=1, widths=30)

displaydata = perc.migr %>%
  select(-c(n, Bevolking_1)) %>%
  rename(Gemeente=RegioS_label, Regio=Herkomstland_label) %>%
  mutate(perc=perc/100) %>%
  pivot_wider(names_from=Gemeente, values_from=perc)
writeData(wb, "Migranten data", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Migranten data", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

# opleiding
addWorksheet(wb, "Opleiding")
setColWidths(wb, "Opleiding", cols=1, widths=30)

displaydata = opleiding.pergemeente %>%
  rename(Gemeente=WijkenEnBuurten_label) %>%
  mutate(n=Laag+Middelbaar+Hoog) %>%
  select(Gemeente, n, starts_with("Perc."))
colnames(displaydata) = str_replace(colnames(displaydata), "Perc.", "")
writeData(wb, "Opleiding", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Opleiding", percStyle, cols=3:5, rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

kaartdata = kaartdata.gemeente %>% filter(statnaam %in% displaydata$Gemeente) %>%
  left_join(displaydata, by=c("statnaam"="Gemeente"))

png(graphname("opleiding"))
print(kaart(kaartdata, var="Hoog", title="Percentage hoogopgeleiden", legend.title="Percentage"))
dev.off()
insertImage(wb, "Opleiding", graphname("opleiding"), startRow=2, startCol=7, width=5, height=5)

png(graphname("opleiding_heatmap"))
print(ggplot(displaydata %>%
         pivot_longer(Laag:Hoog, values_to="perc", names_to="Opleiding") %>%
         mutate(Opleiding=factor(Opleiding, levels=c("Laag", "Middelbaar", "Hoog"), ordered=T),
                perc=perc*100,
                perc.hr=sprintf("%.1f%%", perc)),
       aes(x=Opleiding, y=Gemeente, fill=perc)) +
  geom_tile(color="white", lwd=1) +
  scale_fill_gradient(low=nog_palette[1], high=nog_palette[5], name="Percentage", labels=function (x) { return(sprintf("%.0f%%", x)) }) +
  labs(x="", y="", title="Opleidingsniveau") +
  theme_minimal())
dev.off()
insertImage(wb, "Opleiding", graphname("opleiding_heatmap"), startRow=2, startCol=14, width=5, height=5)

# vaccinatiegraad
addWorksheet(wb, "Vaccinaties")

vacc.graad.nog = vacc.graad %>%
  group_by(Vaccinaties, Vaccinaties_label, Jaar) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  mutate(Vaccinatiegraad=Gevaccineerden/Populatie*100)

ylim = vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties %in% c("A028769", "A028770", "A028771", "A028772")]
ylim = ylim[!is.na(ylim)]
ylim.diff = max(ylim)-min(ylim)
png(graphname("vacc_1"))
plot(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028769"], # DKTP
     vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028769"],
     type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (2 jr)", xlab="Jaar", ylab="Percentage (%)", ylim=c(min(ylim)-(ylim.diff), min(c(max(ylim)+(ylim.diff), 100)))) 
grid(nx=NA, ny=NULL)
par(new=T)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028769"], # DKTP
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028769"], col=nog_colors[1], lwd=1)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028770"], # Hib
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028770"], col=nog_colors[2], lwd=2)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028771"], # BMR
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028771"], col=nog_colors[3], lwd=2)
legend("bottomleft", legend=c("DKTP", "Hib", "BMR"), fill=nog_colors[1:3], bty="n")
dev.off()
insertImage(wb, "Vaccinaties", graphname("vacc_1"), startRow=2, startCol=1, width=5, height=5)

ylim = vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties %in% c("A028772", "A028773", "A028774", "A028775")]
ylim = ylim[!is.na(ylim)]
ylim.diff = max(ylim)-min(ylim)
png(graphname("vacc_2"))
plot(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028772"], # MenACWY
     vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028772"],
     type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (2 jr)", xlab="Jaar", ylab="Percentage (%)", ylim=c(min(ylim)-(ylim.diff), min(c(max(ylim)+(ylim.diff), 100)))) 
grid(nx=NA, ny=NULL)
par(new=T)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028772"], # MenACWY
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028772"], col=nog_colors[1], lwd=1)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028773"], # Pneumo
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028773"], col=nog_colors[2], lwd=2)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028774"], # HepB
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028774"], col=nog_colors[3], lwd=2)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028775"], # volledig
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028775"], col=nog_colors[4], lwd=2)
legend("bottomleft", legend=c("MenACWY", "Pneumokokken", "HepB", "Volledig"), fill=nog_colors[1:4], bty="n")
dev.off()
insertImage(wb, "Vaccinaties", graphname("vacc_2"), startRow=2, startCol=7, width=5, height=5)

ylim = vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties %in% c("A028778", "A028779", "A028780", "A049175")]
ylim = ylim[!is.na(ylim)]
ylim.diff = max(ylim)-min(ylim)
png(graphname("vacc_3"))
plot(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028778"], # DKTP volledig
     vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028778"],
     type="l", col=nog_colors[1], lwd=2, main="Vaccinatiegraad (12+ jr)", xlab="Jaar", ylab="Percentage (%)",
     ylim=c(max(c(min(ylim)-(ylim.diff), 0)), min(c(max(ylim)+(ylim.diff), 100)))) 
grid(nx=NA, ny=NULL)
par(new=T)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028778"], # DKTP volledig
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028778"], col=nog_colors[1], lwd=1)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028779"], # BMR
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028779"], col=nog_colors[2], lwd=2)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A028780"], # HPV
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A028780"], col=nog_colors[3], lwd=2)
lines(vacc.graad.nog$Jaar[vacc.graad.nog$Vaccinaties == "A049175"], # MenACWY
      vacc.graad.nog$Vaccinatiegraad[vacc.graad.nog$Vaccinaties == "A049175"], col=nog_colors[4], lwd=2)
legend("bottomleft", legend=c("DKTP", "BMR", "HPV", "MenACWY"), fill=nog_colors[1:4], bty="n")
dev.off()
insertImage(wb, "Vaccinaties", graphname("vacc_3"), startRow=2, startCol=13, width=5, height=5)

png(graphname("vacc_heatmap"), width=800, height=800)
jaar = max(vacc.graad$Jaar)
print(ggplot(vacc.graad %>%
               mutate(label=case_when(str_detect(Vaccinaties_label, "DKTP.*2 jaar") ~ "DKTP 2 jr",
                                      str_detect(Vaccinaties_label, "Hib vol") ~ "Hib 2 jr",
                                      str_detect(Vaccinaties_label, "BMR.*2 jaar") ~ "BMR 2 jr",
                                      str_detect(Vaccinaties_label, "MenC.*2 jaar") ~ "MenC 2 jr",
                                      str_detect(Vaccinaties_label, "Pneumo.*2 jaar") ~ "Pneu 2 jr",
                                      str_detect(Vaccinaties_label, "Hep.*B.*2 jaar") ~ "HepB 2 jr",
                                      str_detect(Vaccinaties_label, "D.*TP.*10 jaar") ~ "DKTP 10 jr",
                                      str_detect(Vaccinaties_label, "BMR.*10 jaar") ~ "BMR 10 jr",
                                      str_detect(Vaccinaties_label, "HPV") ~ "HPV",
                                      str_detect(Vaccinaties_label, "MenACWY.*15 jaar") ~ "MenC 15 jr")) %>%
               filter(Jaar == jaar, !is.na(label)) %>%
               mutate(label=factor(label, levels=c("DKTP 2 jr", "Hib 2 jr", "BMR 2 jr", "MenC 2 jr",
                                                   "Pneu 2 jr", "HepB 2 jr", "DKTP 10 jr", "BMR 10 jr",
                                                   "HPV", "MenC 15 jr"),
                                   ordered=T)),
             aes(x=label, y=RegioS_label, fill=Vaccinatiegraad)) +
  geom_tile(color="white", lwd=1) +
  scale_fill_gradient(low=nog_palette[1], high=nog_palette[5], name="Percentage", labels=function (x) { return(sprintf("%.0f%%", x)) }) +
  labs(x="", y="", title=sprintf("Vaccinatiegraad in %d", jaar)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90)))
dev.off()
insertImage(wb, "Vaccinaties", graphname("vacc_heatmap"), startRow=27, startCol=1, width=5, height=5)

addWorksheet(wb, "Vaccinaties data")
setColWidths(wb, "Vaccinaties data", cols=1:2, widths=30)  

displaydata = vacc.graad %>%
  select(RegioS_label, Vaccinaties_label, Jaar, Vaccinatiegraad) %>%
  rename(Vaccinatie=Vaccinaties_label) %>%
  mutate(Vaccinatiegraad=Vaccinatiegraad/100) %>%
  rename(Gemeente=RegioS_label) %>%
  pivot_wider(names_from="Jaar", values_from="Vaccinatiegraad")
writeData(wb, "Vaccinaties data", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Vaccinaties data", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

# gezondheid
addWorksheet(wb, "Gezondheid")
setColWidths(wb, "Gezondheid", cols=1, widths=20)

displaydata = gezondheid.gemeentes %>%
  filter(Leeftijd_label == "18 jaar of ouder") %>%
  select(Gemeentenaam, EenOfMeerLangdurigeAandoeningen:BeperkingInBewegen) %>%
  mutate(across(where(is.numeric), ~.x/100))
writeData(wb, "Gezondheid", displaydata, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Gezondheid", percStyle, cols=2:ncol(displaydata), rows=2:(nrow(displaydata)+1), gridExpand=T, stack=T)

# geografische indeling
addWorksheet(wb, "Verschillen per buurt")
setColWidths(wb, "Verschillen per buurt", cols=1, widths=20)

inwoners.nog = inwoners.buurt %>%
  select(-WijkenEnBuurten) %>%
  mutate(across(Mannen:OverigNietWesters, ~.x/AantalInwoners),
         Koopwoningen=Koopwoningen/100,
         HuurwoningenTotaal=HuurwoningenTotaal/100, # dit zijn al percentages
         Laag=Laag/(Laag+Middelbaar+Hoog),
         Middelbaar=Middelbaar/(Laag+Middelbaar+Hoog),
         Hoog=Hoog/(Laag+Middelbaar+Hoog)) %>%
  relocate(Laag, Middelbaar, Hoog, .after=Vrouwen) %>%
  rename(Laag.Opl=Laag, Middelbaar.Opl=Middelbaar, Hoog.Opl=Hoog, Huurwoningen=HuurwoningenTotaal,
         Buurt=WijkenEnBuurten_label)
colnames(inwoners.nog) = str_replace(colnames(inwoners.nog), "k_", "")
writeData(wb, "Verschillen per buurt", inwoners.nog, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Verschillen per buurt", percStyle, cols=c(4:24, ncol(inwoners.nog)-(1:0)), rows=2:(nrow(inwoners.nog)+1), gridExpand=T, stack=T)

# zorgkosten
addWorksheet(wb, "Zorgkosten")

zorgkosten.leeftijd = zorgkosten %>%
  group_by(geslacht, leeftijdsklasse) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  select(-aantal_bsn)
displaydata =  matrix(rowSums(zorgkosten.leeftijd[,-c(1:3)])/zorgkosten.leeftijd$aantal_verzekerdejaren, nrow=19, ncol=2)
colnames(displaydata) = c("M", "V")
rownames(displaydata) = str_replace(zorgkosten.leeftijd$leeftijdsklasse[1:19], " jaar", "")

png(graphname("zorgkosten"))
barplot(t(displaydata), main="Zorgkosten per persoon per jaar", ylab="Kosten (€)", beside=T, col=nog_colors[1:2],
        legend.text=c("Mannen", "Vrouwen"), args.legend=c(x="topleft"), las=2)
dev.off()
insertImage(wb, "Zorgkosten", graphname("zorgkosten"), startRow=2, startCol=2, width=5, height=5)

zorgkosten.gemeente = zorgkosten %>%
  group_by(gemeentenaam) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  select(-aantal_bsn) %>%
  mutate(gemeentenaam=str_to_sentence(gemeentenaam))
zorgkosten.gemeente$Bedrag = rowSums(zorgkosten.gemeente[,-c(1:2)])/zorgkosten.gemeente$aantal_verzekerdejaren
displaydata = zorgkosten.gemeente[c("gemeentenaam", "Bedrag")]

kaartdata = kaartdata.gemeente %>% filter(statnaam %in% displaydata$gemeentenaam) %>%
  left_join(displaydata, by=c("statnaam"="gemeentenaam"))

png(graphname("zorgkosten_kaart"))
print(kaart(kaartdata, var="Bedrag", title="Gemiddelde zorgkosten p.p.p.j.", legend.title="Bedrag (€)"))
dev.off()
insertImage(wb, "Zorgkosten", graphname("zorgkosten_kaart"), startRow=2, startCol=8, width=5, height=5)

addWorksheet(wb, "Zorgkosten data")
setColWidths(wb, "Zorgkosten data", cols=1, widths=20)

zorgkosten.nog = zorgkosten %>%
  select(-gemeentenaam, -Gemeentecode) %>%
  relocate(Gemeente)

writeData(wb, "Zorgkosten data", zorgkosten.nog, startCol=1, startRow=1, headerStyle=headerStyle, borders="surrounding")
addStyle(wb, "Zorgkosten data", geldStyle, cols=6:ncol(zorgkosten.nog), rows=2:(nrow(zorgkosten.nog)+1), gridExpand=T, stack=T)

# bronvermelding
addWorksheet(wb, "Bronvermelding")
setColWidths(wb, "Bronvermelding", cols=1, widths=20)
writeData(wb, "Bronvermelding", "Bronvermelding", startRow=1, startCol=1)
addStyle(wb, "Bronvermelding", titelStyle, cols=1, rows=1)

writeData(wb, "Bronvermelding", bronnen, startCol=1, startRow=3)

saveWorkbook(wb, paste0("IZB overzicht/", gemeentenaam, ".xlsx"), overwrite=T)
printf("Overzicht van %s opgeslagen. (%d/%d)", gemeentenaam, nrow(gemeenten)+1, nrow(gemeenten)+1)
