# registratiecijfers Nivel tbv team IZB

library(pdftools)
library(tidyverse)
library(this.path)
library(lubridate)
library(RODBC)
library(xml2)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

# verbinding met de ODB
conn = odbcDriverConnect("Driver={SQL Server};Server=az-odb0\\odbnog")
sqlQuery(conn, "USE ODBNOG")

# GGD-regio's worden apart opgeslagen
ggd = sqlFetch(conn, "ggd")

# ophalen laatste publicaties van het Nivel
page = read_html("https://www.nivel.nl/nl/resultaten-van-onderzoek/actuele-cijfers-ziekten-per-week")

# de pagina bestaat uit een hele lijst elementen, we zoeken specifiek een <h3> met daarin een <span class="red">, gevolgd door een <ol>
# hierin staan steeds twee links: een algemene, en eentje gesplitst per GGD
h3 = xml_find_all(page, "//div/h3/span[@class='red']")
h3 = sapply(h3, function (el) {
  if (str_detect(xml_text(el), "Meest actuele"))
    return(el)
  return(NA)
})
h3 = h3[!is.na(h3)]
# nu zoeken we de <a> met GGD in de href
links = xml_find_all(page, paste0(xml_path(h3[[1]]), "/../../ol/li/a[contains(@href,'GGD')]"))
datafiles = bind_rows(lapply(links, function (el) {
  periode = str_match(xml_text(el), "(\\d{4}).*(\\d{1,2})")
  return(data.frame(jaar=as.numeric(periode[,2]), week=as.numeric(periode[,3]), link=xml_attr(el, "href")))
})) %>% arrange(jaar, week)

for (d in 1:nrow(datafiles)) {
  printf("Verwerking van week %d-%d...", datafiles$jaar[d], datafiles$week[d])
  
  data = pdf_text(paste0("https://www.nivel.nl", datafiles$link[d]))
  
  # datum rapport ophalen; dit is relevant voor het omschrijven van weeknummers
  datum = dmy(str_match(data[1], "bijgewerkt op:\\s+(\\d{1,2} \\w+ \\d+)")[,2], locale="nl") # de ODB is ingesteld op Engels; forceren op NL
  jaar = year(datum)
  
  # vanaf de derde pagina is het steeds een pagina met cijfers voor de laatste 3 weken, dan een pagina met leeftijdsgroepen (dus 2 per soort aandoening)
  week = NULL
  data.weken = data.frame()
  data.leeftijden = data.frame()
  for (i in 3:length(data)) {
    # regel 1: aandoening
    # regel 2: skip
    # regel 3: categorieën of weken
    text.header = str_sub(data[i], end=str_locate_all(data[i], "\\n")[[1]][3,1])
    # soms is er per ongeluk een extra pagina; dan overslaan
    if (str_starts(text.header, "Beschrijving populatie")) next
    
    disease = str_sub(text.header, end=str_locate(text.header, "\\n")[1]-1)
    categories = str_trim(str_sub(text.header, start=str_locate_all(text.header, "\\n")[[1]][2,1])) %>% str_replace_all("\\s{2,}",";") %>% str_split(";") %>% unlist()
    text.page = paste0("regio", str_sub(data[i], start=str_locate_all(data[i], "\\n")[[1]][4,1]+1)) %>%
      str_replace_all("(\\s*?)100.000(\\s*?)", ";per100k;") %>% # per 100.000 doet rare dingen en is onhandig met variabelenamen
      str_replace_all("[\\s]{2,}", ";") %>%
      str_replace_all("####", "0") %>% # missende waardes worden soms vervangen door ####
      # sommige witruimtes zijn alsnog maar 1 spatie; alles zonder ( of - er direct na vervangen door ;
      str_replace_all("(\\d+) (\\d+)", "\\1;\\2") %>%
      # regio NOG of VGGM gooit alles door de war, daar komt één cijfer een regel naar voren
      str_replace("\\)((;[0-9,.]+)+)\\sVeiligheids([^;0-9]+)", ")\nVeiligheids\\3;\\1;") %>%
      # soms komt een hele serie verder naar voren
      str_replace("(GGD Noord- en Oost-Gelderland(?:;[0-9.,() -]+){12})((?:;[0-9.,() -]+)+)\\sVeiligheids([^;0-9]+)", "\\1\nVeiligheids\\3\\2") %>%
      # VGGM is te lang voor de tabel, en soms valt de splitsing met de volgende cel daardoor weg
      # we kunnen dit een beetje bruteforcen door (tekst met daarin 'Veiligheids- en Gezondheids' gevolgd door cijfer) te vervangen door (tekst);(cijfer)
      str_replace("(Veiligheids- en Gezondheids[^;0-9]+)(\\d+)","\\1;\\2") %>%
      str_replace("Nederland;(.*?)\\n.*", "Nederland;\\1") %>% # de regel na Nederland is onzin; overslaan
      str_replace_all(";;", ";") %>% # zorgen dat decimalen werken
      str_replace_all(",", ".")
    data.page = read.table(text=text.page, header=T, sep=";")
    
    # zorgen dat de meest recente week bekend is
    if (is.null(week) && str_detect(categories[1], "week")) {
      week = as.numeric(str_extract(categories[1], "\\d+"))
    }
    
    # het zijn 3 groepen van 3 variabelen; aantal, per 100k, prevalentieratio
    # die laatste moet even herschreven worden
    for (j in 1:3) {
      category = categories[j]
      data.cat = data.page[,c(1, (1:3)+(j-1)*3+1)]
      colnames(data.cat) = c("regio", "aantal", "per100k", "SPR")
      data.cat$SPR.point = as.numeric(str_match(data.cat$SPR, "^[0-9.]+"))
      data.cat$SPR.lower = as.numeric(str_match(data.cat$SPR, "\\(([0-9.]+) -")[,2])
      data.cat$SPR.upper = as.numeric(str_match(data.cat$SPR, "- ([0-9.]+)\\)")[,2])
      data.cat$aandoening = disease
      data.cat = data.cat %>% select(-SPR) %>% relocate(aandoening)
      if (str_detect(category, "week")) {
        data.cat$week = as.numeric(str_extract(category, "\\d+"))
        data.cat$jaar = jaar
        
        # is de week > 48 en is de datum van het rapport vroeg in het jaar? jaar -1 doen
        if (as.numeric(str_extract(category, "\\d+")) > 48 && yday(datum) < 40) {
          data.cat$jaar = jaar - 1
        }
        
        
        data.weken = bind_rows(data.weken, data.cat)
      } else if (str_detect(category, "jaar")) {
        data.cat$cat = category
        data.cat$week = week
        data.cat$jaar = jaar
        
        # is de week > 48 en is de datum van het rapport vroeg in het jaar? jaar -1 doen
        if (week > 48 && yday(datum) < 40) {
          data.cat$jaar = jaar - 1
        }
        
        data.leeftijden = bind_rows(data.leeftijden, data.cat)
      }
    }
  }
  
  # veiligheidsregio's toevoegen
  data.weken$regio = str_trim(data.weken$regio)
  data.leeftijden$regio = str_trim(data.leeftijden$regio)
  regios = unique(data.weken$regio)
  ggd$regionaam = NA
  data.weken$ggd_id = NA
  for (i in 1:nrow(ggd)) {
    ggd$regionaam[i] = regios[str_which(regios, ggd$ggd_zoekterm[i])]
    data.weken$ggd_id[data.weken$regio == ggd$regionaam[i]] = ggd$ggd_id[i]
    data.leeftijden$ggd_id[data.leeftijden$regio == ggd$regionaam[i]] = ggd$ggd_id[i]
  }
  
  # nu per week invoegen in de database
  data.weken = data.weken %>% group_by(jaar, week)
  for (i in 1:n_groups(data.weken)) {
    rijen.week = group_rows(data.weken)[[i]]
    data.week = data.weken[rijen.week,]
    
    # SPR waardes bij Nederland zijn leegL
    data.week$SPR.upper[is.na(data.week$SPR.upper)] = 0
    data.week$SPR.lower[is.na(data.week$SPR.lower)] = 0
    
    count = sqlQuery(conn, sprintf("SELECT COUNT(*) FROM nivel_zorgregistraties WHERE jaar = %d AND week = %d",
                                   max(data.week$jaar), max(data.week$week)))
    if (count > 0) {
      # week bestaat al, updaten
      update = sprintf("UPDATE nivel_zorgregistraties
             SET aantal=%0.2f, per100k=%0.1f, prevalentie_ratio=%0.1f, SPR_upper=%0.1f, SPR_lower=%0.1f, updated=GETDATE()
             WHERE aandoening='%s' AND ggd_regio=%d AND jaar=%d AND week=%d", data.week$aantal, data.week$per100k, data.week$SPR.point,
                       data.week$SPR.upper, data.week$SPR.lower, data.week$aandoening, data.week$ggd_id, data.week$jaar, data.week$week)
      
      sqlQuery(conn, paste0("BEGIN TRANSACTION;
    ", str_c(update, collapse=";\n"), "
             COMMIT TRANSACTION;"))
    } else {
      values = sprintf("('%s', %d, %d, %d, %d, %0.2f, %0.1f, %0.1f, %0.1f, GETDATE())", data.week$aandoening, data.week$ggd_id, data.week$jaar, data.week$week,
                       data.week$aantal, data.week$per100k, data.week$SPR.point, data.week$SPR.upper, data.week$SPR.lower)
      sqlQuery(conn, paste0("INSERT INTO nivel_zorgregistraties(aandoening, ggd_regio, jaar, week, aantal, per100k, prevalentie_ratio, SPR_upper, SPR_lower, created)
             VALUES", str_c(values, collapse=", ")))
    }
  }
  
  # eveneens per leeftijdsgroep toevoegen
  data.leeftijden = data.leeftijden %>% group_by(cat)
  for (i in 1:n_groups(data.leeftijden)) {
    rijen.leeftijd = group_rows(data.leeftijden)[[i]]
    data.leeftijd = data.leeftijden[rijen.leeftijd,]
    
    # SPR waardes bij Nederland zijn leegL
    data.leeftijd$SPR.upper[is.na(data.leeftijd$SPR.upper)] = 0
    data.leeftijd$SPR.lower[is.na(data.leeftijd$SPR.lower)] = 0
    
    count = sqlQuery(conn, sprintf("SELECT COUNT(*) FROM nivel_zorgregistraties_leeftijd WHERE jaar = %d AND week = %d AND leeftijd = '%s'",
                                   max(data.leeftijd$jaar), max(data.leeftijd$week), unique(data.leeftijd$cat)))
    if (count > 0) {
      # week bestaat al, updaten
      update = sprintf("UPDATE nivel_zorgregistraties_leeftijd
             SET aantal=%0.2f, per100k=%0.1f, prevalentie_ratio=%0.1f, SPR_upper=%0.1f, SPR_lower=%0.1f, updated=GETDATE()
             WHERE aandoening='%s' AND ggd_regio=%d AND jaar=%d AND week=%d AND leeftijd='%s'", data.leeftijd$aantal, data.leeftijd$per100k, data.leeftijd$SPR.point,
                       data.leeftijd$SPR.upper, data.leeftijd$SPR.lower, data.leeftijd$aandoening, data.leeftijd$ggd_id, data.leeftijd$jaar, data.leeftijd$week, data.leeftijd$cat)
      
      sqlQuery(conn, paste0("BEGIN TRANSACTION;
    ", str_c(update, collapse=";\n"), "
             COMMIT TRANSACTION;"))
    } else {
      values = sprintf("('%s', '%s', %d, %d, %d, %d, %0.2f, %0.1f, %0.1f, %0.1f, GETDATE())", data.leeftijd$aandoening, data.leeftijd$cat, data.leeftijd$ggd_id, data.leeftijd$jaar, data.leeftijd$week,
                       data.leeftijd$aantal, data.leeftijd$per100k, data.leeftijd$SPR.point, data.leeftijd$SPR.upper, data.leeftijd$SPR.lower)
      sqlQuery(conn, paste0("INSERT INTO nivel_zorgregistraties_leeftijd(aandoening, leeftijd, ggd_regio, jaar, week, aantal, per100k, prevalentie_ratio,
                                                                       SPR_upper, SPR_lower, created)
             VALUES", str_c(values, collapse=", ")))
    }
  }
  
  printf("Verwerking afgerond.")
}

odbcClose(conn)
