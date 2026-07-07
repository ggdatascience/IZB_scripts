# registratiecijfers Nivel tbv team IZB

library(tidyverse)
library(this.path)
library(lubridate)
library(DBI)
library(odbc)
library(xml2)
library(openxlsx)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

NullOrQuotes = function (val, fmt=NA) {
  fmt = rep(coalesce(fmt, "%s"), length(val))
  return(case_when(is.na(val) | is.null(val) ~ "NULL",
                   is.numeric(val) & !is.na(fmt) ~ sprintf(fmt, val),
                   is.numeric(val) ~ as.character(val),
                   is.character(val) ~ as.character(dbQuoteString(conn, as.character(val))),
                   TRUE ~ paste0("'", str_replace_all(val, fixed("'"), "''"), "'")))
}

# omschrijven van ICPC-codes naar ouderwetse aandoeningen, voor compatibiliteit
icpc_dict = c("A03"="Koorts", "A76"="Andere virusziekte met exantheem", "H71"="Otitis media acuta / myringitis",
              "R72"="Streptokokken-angina/roodvonk", "R81"="Pneumonie", "S72"="Scabies/andere aandoening door mijten",
              "S84"="Impetigo/impetiginisatie", "GE"="Braken, diarree of veronderstelde gastro-intestinale infectie",
              "ARI"="Acute respiratoire infecties excl. pneumonie (R74/R75/R77/R78/R80)", "OOGONTST"="Oogontstekingen")

# verbinding met de ODB
conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0\\odbnog", Database="ODBNOG")

# GGD-regio's worden apart opgeslagen
ggd = dbGetQuery(conn, "SELECT * FROM ggd")

# ophalen laatste publicaties van het Nivel
page = read_html("https://www.nivel.nl/nl/resultaten-van-onderzoek/actuele-cijfers-ziekten-per-week")

# de pagina bestaat uit een hele lijst elementen, we zoeken specifiek een <h3> met daarin een <span class="red">, gevolgd door een <ul>
# hierin staan steeds twee links: een algemene, en eentje gesplitst per GGD
h3 = xml_find_all(page, "//div/h3/span[@class='red']")
if (length(h3) > 1) {
  h3 = sapply(h3, function (el) {
    if (str_detect(xml_text(el), "Meest actuele"))
      return(el)
    return(NA)
  })
}
h3 = h3[!is.na(h3)]
# nu zoeken we de <a> met GGD in de href
links = xml_find_all(page, paste0(xml_path(h3[[1]]), "/../../ul/li/a[contains(@href,'GGD')]"))
datafiles = bind_rows(lapply(links, function (el) {
  periode = str_match(xml_text(el), "(\\d{4}).*?(\\d+)")
  return(data.frame(jaar=as.numeric(periode[,2]), week=as.numeric(periode[,3]), link=xml_attr(el, "href")))
})) %>% arrange(jaar, week)

for (d in 1:nrow(datafiles)) {
  if (!str_ends(datafiles$link[d], "xlsx")) next
  
  printf("Verwerking van week %d-%d...", datafiles$jaar[d], datafiles$week[d])
  
  tmpname = paste0("temp", d, ".xlsx")
  download.file(paste0("https://www.nivel.nl", datafiles$link[d]), tmpname, mode="wb")
  
  sheets = getSheetNames(tmpname)
  
  data_totaal = data.frame()
  for (sheet in sheets) {
    if (sheet == "Voorblad") next
    
    # de tabbladen hebben een identieke opmaak, behalve Populatie
    if (sheet == "Populatie") {
      # Populatie:
      # - A: GGD
      # - C: week x
      # - D: populatie week x
      # - F: week x-1
      # - G: populatie week x-1, enz.
      # Weeknummer is de kolomnaam van de populatiekolom
      data = read.xlsx(tmpname, sheet=sheet, startRow=3)
      
      populatie = list()
      for (i in 1:3) {
        week = colnames(data)[(i*2)+1] %>% as.numeric()
        jaar = datafiles$jaar[d]
        
        # is de week > 48 en is de datum van het rapport vroeg in het jaar? jaar -1 doen
        if (week > 48 && datafiles$week[d] < 5) {
          jaar = jaar - 1
        }
        
        data_week = data[-1,c(1, i*2, (i*2)+1)]
        colnames(data_week) = c("GGD", "n_praktijken", "n_inwoners")
        
        populatie[[i]] = cbind(data_week, week=week, jaar=jaar)
      }
      populatie = bind_rows(populatie) %>%
        mutate(n_praktijken = as.numeric(n_praktijken),
               n_inwoners = as.numeric(n_inwoners))
    } else {
      # Overige tabbladen:
      # - A: GGD
      # - C: totaal per 100.000 week x
      # - D: 0-4 jaar
      # - E: 5-14 jaar
      # - F: 15-64 jaar
      # - G: 65+
      # - I: totaal per 100.000 week x-1, enz.
      # Weeknummer is de kolomnaam van de 0-4 kolom
      header = read.xlsx(tmpname, sheet=sheet, rows=4)
      data = read.xlsx(tmpname, sheet=sheet, startRow=5)
      
      data_formatted = list()
      for (i in 1:3) {
        # startkolommen zijn 2, 7, 12
        start_col = (i-1)*5+2
        # weken bevinden zich in de kolomkop van kolom 3, 8, 13, maar vanwege het inlezen als losse header is het kolom 2, 4, 6
        week = colnames(header)[i*2] %>% as.numeric()
        jaar = datafiles$jaar[d]
        
        # is de week > 48 en is de datum van het rapport vroeg in het jaar? jaar -1 doen
        if (week > 48 && datafiles$week[d] < 5) {
          jaar = jaar - 1
        }
        
        data_week = data[,c(1, start_col:(start_col+4))]
        colnames(data_week) = c("GGD", "per100k_totaal", "per100k_0 - 4 jaar", "per100k_5 - 14 jaar", "per100k_15 - 64 jaar", "per100k_65+")
        
        data_formatted[[i]] = cbind(data_week, week=week, jaar=jaar)
      }
      data_formatted = bind_rows(data_formatted) %>%
        cbind(aandoening=unname(icpc_dict[sheet]))
      data_totaal = bind_rows(data_totaal, data_formatted)
    }
  }
  data_totaal = data_totaal %>%
    left_join(populatie, by=c("GGD", "week", "jaar")) %>%
    mutate(aantal=round(per100k_totaal/100000*n_inwoners))
  
  data_totaal$ggd_id = NA
  regios = unique(data_totaal$GGD)
  for (i in 1:nrow(ggd)) {
    regionaam = regios[str_which(regios, ggd$ggd_zoekterm[i])]
    data_totaal$ggd_id[data_totaal$GGD == regionaam] = ggd$ggd_id[i]
  }
  
  # nu per week invoegen in de database
  data_totaal = data_totaal %>% group_by(jaar, week)
  for (i in 1:n_groups(data_totaal)) {
    rijen_week = group_rows(data_totaal)[[i]]
    data_week = data_totaal[rijen_week,]
    
    data_leeftijd = data_week %>%
      select(-per100k_totaal) %>%
      pivot_longer(starts_with("per100k_"), names_to="leeftijdsgroep", names_prefix="per100k_", values_to="per100k")
    
    count = dbGetQuery(conn, sprintf("SELECT COUNT(*) FROM nivel_zorgregistraties WHERE jaar = %d AND week = %d",
                                   max(data_week$jaar), max(data_week$week)))
    if (all(count > 0)) {
      # week bestaat al, updaten
      update = sprintf("UPDATE nivel_zorgregistraties
             SET aantal=%s, per100k=%s, updated=GETDATE()
             WHERE aandoening='%s' AND ggd_regio=%d AND jaar=%d AND week=%d", NullOrQuotes(data_week$aantal), NullOrQuotes(data_week$per100k_totaal, "%.2f"),
                       data_week$aandoening, data_week$ggd_id, data_week$jaar, data_week$week)
      
      dbExecute(conn, paste0("BEGIN TRANSACTION;
      ", str_c(update, collapse=";\n"), ";
               COMMIT TRANSACTION;"))
      printf("Week %d-%d: %d rijen aangepast.", first(data_week$jaar), first(data_week$week), length(update))
      
      # per leeftijdsgroep
      update = sprintf("UPDATE nivel_zorgregistraties_leeftijd
             SET per100k=%s, updated=GETDATE()
             WHERE aandoening='%s' AND ggd_regio=%d AND jaar=%d AND week=%d AND leeftijd='%s'", NullOrQuotes(data_leeftijd$per100k, "%.2f"), data_leeftijd$aandoening,
                       data_leeftijd$ggd_id, data_leeftijd$jaar, data_leeftijd$week, data_leeftijd$leeftijdsgroep)
      
      for (u in seq(1, length(update), 500)) {
        end = min(u + 499, length(update))
        dbExecute(conn, paste0("BEGIN TRANSACTION;
      ", str_c(update[u:end], collapse=";\n"), ";
               COMMIT TRANSACTION;"))
      }
      printf("Week %d-%d: %d rijen aangepast voor de leeftijdstabel.", first(data_week$jaar), first(data_week$week), length(update))
    } else {
      # totaal
      values = sprintf("('%s', %d, %d, %d, %s, %s, GETDATE())", data_week$aandoening, data_week$ggd_id, data_week$jaar, data_week$week,
                       NullOrQuotes(data_week$aantal), NullOrQuotes(data_week$per100k_totaal))
      dbExecute(conn, paste0("INSERT INTO nivel_zorgregistraties(aandoening, ggd_regio, jaar, week, aantal, per100k, created)
             VALUES", str_c(values, collapse=", ")))
      printf("Week %d-%d: %d rijen ingevoegd.", first(data_week$jaar), first(data_week$week), length(values))
      
      # per leeftijdsgroep
      values = sprintf("('%s', '%s', %d, %d, %d, %s, GETDATE())",
                       data_leeftijd$aandoening, data_leeftijd$leeftijdsgroep, data_leeftijd$ggd_id, data_leeftijd$jaar, data_leeftijd$week,
                       NullOrQuotes(data_leeftijd$per100k))
      
      for (u in seq(1, length(values), 500)) {
        end = min(u + 499, length(values))
        dbExecute(conn, paste0("INSERT INTO nivel_zorgregistraties_leeftijd(aandoening, leeftijd, ggd_regio, jaar, week, per100k, created)
             VALUES", str_c(values[u:end], collapse=", ")))
      }
      printf("Week %d-%d: %d rijen ingevoegd voor de leeftijdstabel.", first(data_week$jaar), first(data_week$week), length(values))
    }
  }
  
  printf("Verwerking afgerond.")
}

dbDisconnect(conn)

# tempbestanden verwijderen
temps = list.files(pattern=".xlsx$")
file.remove(temps)