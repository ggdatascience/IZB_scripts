library(tidyverse)
library(pdftools)
library(ggdnog)
library(DBI)
library(odbc)
library(readxl)
library(this.path)

setwd(dirname(this.path()))

base_path = "C:/Users/a.dijkstra/OneDrive - Het Servicecentrum/Vertrouwelijke documenten/1_Bestrijden van infectieziekten/Documenten HPZone/Legionella in waterleiding/2024/"

#files = list.files(pattern=".*\\.pdf")
files = list.files(base_path, pattern=".*\\.pdf", recursive=T)
excelsheet = read_excel(paste0(base_path, "Meldingen Legionella in waterleiding - 2024.xlsx"))
colnames(excelsheet) = str_replace_all(str_to_lower(colnames(excelsheet)), c("kve.*"="kve", "plaats.*"="plaats", ".*datum.*"="monsterdatum"))
excelsheet$postcode = str_replace(excelsheet$postcode, " ", "")
if (is.character(excelsheet$monsterdatum)) excelsheet$monsterdatum = openxlsx::convertToDate(excelsheet$monsterdatum)

# de legionellameldingen van IL&T hebben een vaste structuur
# op pagina 1 begint het met "Melding legionella (witruimte) Registratienummer (witruimte) (nummer)"
# op pagina 1 staan verder gegevens van de melder
# op pagina 2 staan gegevens van de melding
# pagina 3 bevat de daadwerkelijke meting
# globaal bestaat de pagina uit deze opmaak:
# "variabelenaam (witruimte) antwoord"
# hierbij kan de variabelenaam op meerdere regels staan, dus soms is deze niet volledig
meldingen = data.frame()
for (filename in files) {
  #filename = paste0("C:/Users/a.dijkstra/OneDrive - Het Servicecentrum/Vertrouwelijke documenten/1_Bestrijden van infectieziekten/Documenten HPZone/Legionella in waterleiding/2024/11 November/", filename)
  # er zitten wat foute bestanden van 0 kB in - deze willen we negeren
  data = NULL
  tryCatch({ data = pdf_text(paste0(base_path, filename)) %>% str_split("\n") %>% unlist() },
           error=function (e) { printf("Fout tijdens het lezen van %s: %s", filename, e) })
  if (is.null(data)) next
  
  # er zijn twee types rapporten, met globaal dezelfde indeling
  # < midden 2023: blauw formulier wat begint met 'Eigen kopie, niet opsturen'
  # > midden 2023: rood formulier wat begint met 'Melding legionella'
  if (!str_starts(data[1], "Melding legionella") && !str_starts(data[1], "Eigen kopie, niet opsturen")) {
    printf("Bestand %s bevat geen legionellamelding van IL&T, genegeerd.", filename)
    next
  }
  
  indeling2023 = str_starts(data[1], "Melding legionella")
  
  if (indeling2023) {
    # registratienummer zit in regel 1, na de titel
    regnr = str_match(data[1], "Registratienummer\\s+(.*)")[,2]
    melddatum = str_match(data[3], "Ingediend op\\s+([0-9-]+)")[,2] %>% dmy()
  } else {
    # registratienummer zit op regel 5, op een losse regel
    regnr = str_extract(data[5], "M-.*")
    # melddatum zit op regel 8, ook op een losse regel
    melddatum = str_extract(data[8], "[0-9-]+") %>% dmy()
  }
  
  
  # alles hierna kunnen we gewoon en masse doen
  # we willen alleen splitsen op melder en melding, en daar is makkelijk op te zoeken:
  # - indeling2023: 2. Melding
  # - !indeling2023: Gegevens locatie danwel Gegevens besmette accommodatie
  matches = str_match(data[1:str_which(data, ifelse(indeling2023, "2. Melding", "Gegevens locatie|Gegevens besmette"))], "^(\\w[a-zA-Z() -]+)\\s{2,}(.*)")
  vars = matches[,2] %>% str_trim()
  vals = matches[,3] %>% str_trim()
  melder = data.frame(vars=vars[!is.na(vars)], vals=vals[!is.na(vars)])
  
  matches = str_match(data[str_which(data, ifelse(indeling2023, "2. Melding", "Gegevens locatie|Gegevens besmette")):length(data)], "^(\\w[a-zA-Z() -]+)\\s{2,}(.*)")
  vars = matches[,2] %>% str_trim()
  vals = matches[,3] %>% str_trim()
  melding = data.frame(vars=vars[!is.na(vars)], vals=vals[!is.na(vars)])
  
  # de gegevens kunnen onder melding of melder staan, waarbij we de voorkeur geven voor melding
  find_val = function (var) {
    if (sum(str_detect(melding$vars, var)) == 1) {
      return(melding$vals[str_which(melding$vars, var)])
    } else if (sum(str_detect(melder$vars, var)) == 1) {
      return(melder$vals[str_which(melder$vars, var)])
    } else {
      if (sum(str_detect(melding$vars, var)) > 1) {
        warning(sprintf("Let op: variabele %s is meer dan 1x aangetroffen in de meldingsdata.", var))
        return(melding$vals[first(str_which(melding$vars, var))])
      } else if (sum(str_detect(melder$vars, var)) > 1) {
        warning(sprintf("Let op: variabele %s is meer dan 1x aangetroffen in de meldersdata.", var))
        return(melder$vals[first(str_which(melder$vars, var))])
      }
      return(NA)
    }
  }

  # speciaal geval: het type legionella staat op een losse regel; direct na de regel "Hoogst gemeten waarde"
  # format van die regel: [type] in kve/l (beide indelingen) - tenzij er door de lengte opgeschoven wordt en de in kve/l wegvalt...
  # daarnaast is er een uitzonderingsgeval: in sommige oude formulieren staat "Over welke soort(en) ...", gevolgd door het type in een aparte kolom
  if (any(str_detect(data, "Hoogst gemeten"))) {
    #if (length(str_which(data, "Hoogst gemeten")) > 1) browser()
    r = str_which(data, "Hoogst gemeten")
    # regels zitten niet per se achter elkaar; we willen de regel zelf, en de eerstvolgende 2 die beginnen met een letter
    r1 = c()
    for (r_cur in r) {
      r_next = str_which(data, "^\\w")
      r1 = c(r1, first(r_next[r_next > r_cur]))
    }
    zin = paste(str_sub(data[r], end=first(str_locate(data[r], "\\s{2,}")[,1])) %>% str_trim(), data[r1], data[r1+1])
    matches = str_match(zin, regex("Legionella (.*?) in", ignore_case=T))
    typering = NA
    if (sum(!is.na(matches[,2])) > 0) {
      typering = suppressWarnings(str_to_sentence(matches[!is.na(matches[,2]),2], locale="nl") %>% str_trim())
    }
    
    waardes = str_match(data[r], "(\\d+) kve")
    kve = waardes[,2]
  } else if (any(str_detect(data, fixed("Over welke soort(en)")))) {
    # er kan een gek teken voor staan; alles behalve tekst verwijderen
    typering = find_val("Over welke soort") %>% str_extract("[a-zA-Z -]+") %>% str_replace("Legionella", "") %>% str_trim() %>%
      str_to_sentence(locale="nl")
    # de kve staat dan ook op een gekke plek
    kve = str_extract(data, "\\d+ kve/l")
    kve = kve[first(which(!is.na(kve)))] %>% str_extract("\\d+")
  }
  if (any(is.na(typering))) browser()
  typering = str_replace(typering, fixed("- "), "-")
  
  # adres kan opgegeven zijn in straat en huisnummer (indien locatie melder != locatie overschrijding), waarbij de postcode en plaats dan 2x voorkomen
  # in de melding; locatie en melder (indeling2023), of als tweede adresregel (!indeling2023)
  # of, wanneer locatie melder == locatie overschrijding, dan is het gegeven als adres (geheel) en 1x de postcode
  if (any(str_detect(data, "Gegevens besmette")) || find_val("Is het adres van de locatie") == "Nee") {
    adres = ifelse(indeling2023, paste(find_val("Straat"), find_val("Huisnummer")), melding$vals[first(str_which(melding$vars, "Adres"))])
    postcode = melding$vals[first(str_which(melding$vars, "Postcode"))]
    plaats = melding$vals[first(str_which(melding$vars, "Plaats"))]
  } else {
    adres = find_val("Adres")
    postcode = find_val("Postcode")
    plaats = find_val("Plaats")
  }
  postcode = str_replace_all(postcode, " ", "")
  
  meldingen = bind_rows(meldingen, data.frame(regnr=regnr, melddatum=melddatum, handelsnaam=find_val(ifelse(indeling2023, "Handelsnaam", "Naam organisatie")),
                                              adres=adres, postcode=postcode, plaats=plaats, land=find_val("Land"),
                                              kve=kve, typering=typering,
                                              type_instelling=find_val("Tot welke categorie"), file=filename))
}

meldingen %>%
  group_by(regnr) %>%
  summarize(n=n(), files=str_c(file, collapse=", ")) %>%
  filter(n > 1)

meldingen_join = meldingen %>% left_join(excelsheet %>% select(postcode, kve, monsterdatum) %>% mutate(kve=as.character(kve)),
                                         by=c("postcode", "kve")) %>%
  select(-file) %>%
  mutate(diff_data=abs(melddatum - as.Date(monsterdatum))) %>%
  group_by(regnr, typering, kve)
# het komt regelmatig voor dat de combinatie PC6 en KVE niet uniek is; in dat geval willen we alleen de dichtstbij liggende datum bewaren
group_index = group_indices(meldingen_join)
dupl = group_index[duplicated(group_index)]
printf("%d duplicaten gevonden", length(dupl))
for (d in dupl) {
  print(meldingen_join[group_index == d,])
}

meldingen_join = meldingen_join %>%
  arrange(diff_data) %>%
  summarize(across(everything(), ~first(.x)))
#meldingen_join = meldingen %>% select(-file) %>% group_by(regnr, typering, kve) %>% summarize(across(everything(), ~first(.x)))

# database bijwerken
conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0\\odbnog", Database="HPZone")

prev = dbGetQuery(conn, "SELECT regnr FROM leg_meldingen")
insert = meldingen_join[!meldingen_join$regnr %in% prev,] %>% select(-diff_data) %>% mutate(kve=as.numeric(kve), monsterdatum=as.Date(monsterdatum))
result = dbAppendTable(conn, "leg_meldingen", insert)
printf("%d rijen toegevoegd.", nrow(insert))

dbDisconnect(conn)


