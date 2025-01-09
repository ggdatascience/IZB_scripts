################################################################################
# 
# Automatische verwerking legionellameldingen vanuit IL&T.
#
# Er komen in de IZB-mailbox berichten binnen vanuit IL&T met meldingen van
# overschrijdingen van legionellawaarden in bijv. waterleidingen en koeltorens.
# Deze worden geautomatiseerd verstuurd, en kunnen dus makkelijk aan onze kant
# automatisch worden ingelezen.
#
# Een trigger in Power Automate slaat automatisch de bijlagen bij deze mails
# op in de map 'Legionella melding - Outlook'. Dit script leest meerdere keren
# per dag die bestanden uit, verwerkt de daarin opgeslagen data, slaat het op
# in de database, en verplaatst de bestanden naar de juiste map.
#
# Globaal stappenplan:
# - Loop door bestanden in de map heen.
# - Indien formulier IL&T:
#   -> Verwerk data
#   -> Sla formulier op met juiste naam in de juiste map
#       > [jaartal]/[maand]/[datum]_[bedrijfsnaam]_formulier_[meldingsnummer].pdf
#   -> Sla bijlagen op in de juiste map
#       > [jaartal]/[maand]/[datum]_[bedrijfsnaam]_bijlage_[meldingsnummer].pdf
#
# Om dit script in een andere organisatie op te zetten zijn de volgende stappen nodig:
#
# 1. Maak een logische mappenstructuur. Binnen NOG is hier gekozen voor:
#    (hoofdmap) > Legionella in waterleiding > Jaar > Maand > Documenten
#    Het script bevindt zich in (hoofdmap) > Legionella in waterleiding, waar
#    eveneens de bijlagen in een submap geplaatst worden.
# 2. Maak een Power Automate flow aan.
#   - Trigger
#       -> Mail afkomstig van 'noreply@ilent.nl', EN
#       -> Onderwerp begint met 'Legionella melding', EN
#       -> Bevat bijlage met extensie '.pdf'
#   - Actie
#       -> Download bijlagen, en plaats deze in een centrale map op Sharepoint.
#       -> Optioneel: verplaats mail binnen mailbox naar map met afgehandelde meldingen
# 3. Maak een database of bedenk zelf een ander uitvoerformaat; Excel is ook geschikt,
#    maar dan moet dit script uiteraard aangepast worden. Als er geen database beschikbaar
#    is, dan is SQLite waarschijnlijk de makkelijkste optie. Hiervoor hoeft alleen de
#    verbinding en de gebruikte package te worden aangepast naar RSQLite.
#   - De gebruikte datastructuur (in T-SQL) is meegeleverd in leg_meldingen.sql.
# 4. Automatiseer de verwerking door dit script uit te voeren. Dit kan als extra
#    actie binnen Power Automate, als job in SQL Server Agent, of simpelweg via
#    Windows Taakplanner. Tip: uitvoeren van R vanaf de command line is als volgt:
#   - "...:\Program Files\R\R-[4.3.3]versie]\bin\x64\R.exe" -f "[locatie]\automatisering_legionellameldingen.R"
# 5. Optioneel: geografisch overzicht van casuistiek en meldingen kan met bijgevoegde
#    Rmarkdown.
#
################################################################################

library(tidyverse)
library(pdftools)
library(ggdnog)
library(DBI)
library(odbc)
library(readxl)
library(this.path)

setwd(dirname(this.path()))

## log levels
ERR = 1
WARN = 2
MSG = 3
DEBUG = 4

msg = function (msg, ..., level = DEBUG) {
  # schrijf naar logbestand
  if (level > MSG) {
    if (is.character(msg)) {
      cat(sprintf(paste0("[%s - %s] ", msg, "\n"), Sys.time(), deparse(substitute(level)), ...), file = "log.txt", append=T)
    } else {
      cat(sprintf(paste0("[%s - %s] ", msg, "\n"), Sys.time(), deparse(substitute(level))), file = "log.txt", append=T)
    }
  }
  
  # geen gewenst level of level hoog genoeg; printen
  if (level == ERR) {
    if (is.character(msg))
      stop(sprintf(msg, ...))
    else
      stop(msg)
  } else if (level == WARN) {
    if (is.character(msg))
      message(sprintf(msg, ...))
    else
      message(msg)
  } else if (level == MSG) {
    if (is.character(msg))
      message(sprintf(msg, ...))
    else
      message(msg)
  } else {
    if (is.character(msg))
      printf(paste0(ifelse(level == DEBUG, "[DEBUG] ", ""), msg), ...)
    else
      print(msg)
  }
}

#files = list.files(pattern=".*\\.pdf")
files = list.files("Legionella melding - Outlook", pattern="Samenvatting.*\\.pdf", recursive=T)


meldingen = data.frame()
for (filename in files) {
  #filename = paste0("C:/Users/a.dijkstra/OneDrive - Het Servicecentrum/Vertrouwelijke documenten/1_Bestrijden van infectieziekten/Documenten HPZone/Legionella in waterleiding/2024/11 November/", filename)
  # er zitten wat foute bestanden van 0 kB in - deze willen we negeren
  data = NULL
  tryCatch({ data = pdf_text(paste0("Legionella melding - Outlook/", filename)) %>% str_split("\n") %>% unlist() },
           error=function (e) { msg("Fout tijdens het lezen van %s: %s", filename, e, level=ERR) })
  if (is.null(data)) next
  
  # er zijn twee types rapporten, met globaal dezelfde indeling
  # < midden 2023: blauw formulier wat begint met 'Eigen kopie, niet opsturen'
  # > midden 2023: rood formulier wat begint met 'Melding legionella'
  if (!str_starts(data[1], "Melding legionella") && !str_starts(data[1], "Eigen kopie, niet opsturen")) {
    msg("Bestand %s bevat geen legionellamelding van IL&T, genegeerd.", filename, level=DEBUG)
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
                                              type_instelling=find_val("Tot welke categorie"), file=filename,
                                              appendix=str_sub(find_val("Analyserapportage"), start=3)))
}

if (nrow(meldingen) <= 0) {
  # geografisch overzicht van meldingen en casuistiek
  rmarkdown::render("overzicht legionella.Rmd", rmarkdown::html_document())
  
  stop("Geen verdere verwerking nodig.")
}

# database bijwerken
conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0\\odbnog", Database="HPZone")

prev = dbGetQuery(conn, "SELECT regnr FROM leg_meldingen")
insert = meldingen[!meldingen$regnr %in% prev,]

if (nrow(insert) != nrow(meldingen)) {
  # dit is gek - er zou geen mail moeten zijn als er geen nieuwe melding was
  msg("Aantallen komen niet overeen: %d (invoegen) vs. %d (meldingen)", nrow(insert), nrow(meldingen), level=WARN)
  msg("Gevonden bestaande melding(en): %s", str_c(prev[prev %in% meldingen$regnr], collapse=", "), level=WARN)
}

result = dbAppendTable(conn, "leg_meldingen", insert %>% select(-c(file, appendix)))
msg("%d rijen toegevoegd.", nrow(insert), level=MSG)

dbDisconnect(conn)

if (nrow(insert) > 0) {
  for (i in 1:nrow(insert)) {
    jaar = year(insert$melddatum[i]) %>% as.character()
    maand = format(insert$melddatum[i], "%m %B")
    
    if (!dir.exists(jaar)) {
      dir.create(jaar)
    }
    
    if (!dir.exists(paste0(jaar, "/", maand))) {
      dir.create(paste0(jaar, "/", maand))
    }
    
    # formulier
    file.rename(paste0("Legionella melding - Outlook/", insert$file[i]),
                sprintf("%s/%s/%s_%s_formulier_%s.pdf", jaar, maand, insert$melddatum[i], insert$handelsnaam[i], insert$regnr[i]))
    # appendix?
    if (!is.na(insert$appendix[i])) {
      file.rename(paste0("Legionella melding - Outlook/", insert$appendix[i]),
                  sprintf("%s/%s/%s_%s_bijlage_%s.pdf", jaar, maand, insert$melddatum[i], insert$handelsnaam[i], insert$regnr[i]))
    }
  }
  
  msg("%d formulieren verplaatst.", nrow(insert), level=MSG)
}

# geografisch overzicht van meldingen en casuistiek
rmarkdown::render("overzicht legionella.Rmd", rmarkdown::html_document())
