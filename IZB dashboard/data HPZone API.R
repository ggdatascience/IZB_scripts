################################################################################
# Dit script loopt door de exports van HPZone heen en voegt deze toe aan een   #
# database, zodat de data gebruikt kan worden in een dashboard.                #
#                                                                              #
# Gebruik is simpel:                                                           #
# - Exporteer de cases, situations en enquiries van het afgelopen jaar vanuit  #
#   HPZone met de opties 'All core values' en 'Excel' aangevinkt.              #
# - Plaats de drie bestanden in de submap data.                                #
# - Voer dit bestand uit.                                                      #
# - Verwijder, indien gewenst, de originele Excelbestanden.                    #
################################################################################

library(tidyverse)
library(lubridate)
library(this.path)
library(HPZoneAPI)
library(DBI)
library(odbc)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

# jaren om op te halen
# mogelijke waardes: jaartallen of "cur" voor huidig (en voorgaand indien recent na jaarwisseling) jaar
years = "cur"

HPZone_setup()

conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0\\odbnog", Database="HPZone")

# eenmalig: invoeren PC4 naar gemeente en gemeentenamen
if (F) {
  gemeentes = read_xlsx("Gemeenten alfabetisch 2022.xlsx")
  res = dbExecute(conn, "INSERT INTO [Algemeen].[dbo].[gemeente](gemeentecode, gemeentenaam) VALUES(?, ?)",
                  params=list(gemeentes$Gemeentecode, str_trim(gemeentes$Gemeentenaam)))
  printf("%d gemeentes ingevoegd.", res)

  data = read.csv("../../../Documenten/CBS data/pc4_gemeente.csv")
  # er zijn een paar dubbele postcodes
  # hierbij nemen we aan dat het grootste aantal inwoners de juiste gemeente is (de anderen zijn meestal 1-5 adressen)
  data = data %>%
    arrange(desc(aantal)) %>%
    group_by(PC4) %>%
    summarize(gemeentecode=first(Gemeente2022))
  for (i in seq(1, nrow(data), 500)) {
    res = dbExecute(conn, "INSERT INTO [Algemeen].[dbo].[PC4_gemeente](PC4, gemeentecode, jaar) VALUES(?, ?, '2022-01-01')",
                    params=list(data$PC4[i:min(nrow(data),i+499)], data$gemeentecode[i:min(nrow(data),i+499)]))

    printf("%d PC4's ingevoegd.", res)
  }
}

infec.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_infectie]")
diag.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_diagnose]")
groepen.db = dbGetQuery(conn, "SELECT * FROM [dbo].[infectieziekte]")
agents.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_agent]")

# eenmalig: invoeren infecties, diagnoses, enz.
if (F) {
  inf = read.table("infecties_hpzone.csv", sep=",", header=T, encoding="UTF-8")
  colnames(inf)[1] = "Name" # deze doet een beetje gek
  diag = read.table("diagnoses_hpzone.csv", sep=",", header=T, encoding="UTF-8")
  colnames(diag)[1] = "Name"
  agents = read.table("agents_hpzone.csv", sep=",", header=T, encoding="UTF-8")
  colnames(agents)[1] = "Name" # deze doet een beetje gek

  # infecties invoegen
  infec.hpzone = inf$Name
  infec.missend = infec.hpzone[!infec.hpzone %in% infec.db$infec_naam]
  if (length(infec.missend) > 0)
    dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_infectie](inf_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, infec.missend)), collapse=", ")))
  infec.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_infectie]")

  # groepen infectieziektes invoegen
  groepen.hpzone = unique(diag$Mapped.Notifiable.Disease[str_length(diag$Mapped.Notifiable.Disease) > 0])
  groepen.missend = groepen.hpzone[!groepen.hpzone %in% groepen.db$groep_naam]
  if (length(groepen.missend) > 0)
    dbExecute(conn, paste0("INSERT INTO [dbo].[infectieziekte](groep_naam, groep_meldingsplichtig) VALUES", str_c(sprintf("(%s, 1)", dbQuoteString(conn, groepen.missend)), collapse=", ")))
  groepen.db = dbGetQuery(conn, "SELECT * FROM [dbo].[infectieziekte]")

  # diagnoses invoegen
  diag.hpzone = diag$Name
  diag.missend = diag.hpzone[!diag.hpzone %in% diag.db$dx_naam]
  if (length(diag.missend) > 0) {
    diag.insert = data.frame(name=diag.missend, groep_naam=diag$Mapped.Notifiable.Disease[diag$Name %in% diag.missend]) %>%
      left_join(groepen.db, by="groep_naam")
    dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_diagnose](dx_naam, dx_groep) VALUES",
                           str_c(sprintf("(%s, %s)", dbQuoteString(conn, diag.insert$name),
                                         ifelse(is.na(diag.insert$groep_id), "NULL", as.character(diag.insert$groep_id))), collapse=", ")))
  }
  diag.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_diagnose]")

  # agents invoegen
  agents.hpzone = agents$Name
  agents.missend = agents.hpzone[!agents.hpzone %in% agents.db$ag_naam]
  if (length(agents.missend) > 0) {
    agents.insert = data.frame(name=agents.missend,
                               dx_naam=agents$Default.Diagnosis[agents$Name %in% agents.missend],
                               inf_naam=agents$Associated.Infection[agents$Name %in% agents.missend]) %>%
      left_join(diag.db, by="dx_naam") %>%
      left_join(infec.db, by="inf_naam")
    dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_agent](ag_naam, ag_infectie, ag_standaarddiagnose) VALUES",
                           str_c(sprintf("(%s, %s, %s)", dbQuoteString(conn, agents.insert$name),
                                         ifelse(is.na(agents.insert$inf_id), "NULL", as.character(agents.insert$inf_id)),
                                         ifelse(is.na(agents.insert$dx_id), "NULL", as.character(agents.insert$dx_id))), collapse=", ")))
  }
  agents.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_agent]")
}

NullOrQuotes = function (val) {
  return(case_when(is.na(val) | is.null(val) ~ "NULL",
                   is.numeric(val) ~ as.character(val),
                   TRUE ~ paste0("'", str_replace_all(val, fixed("'"), "''"), "'")))
}

if (any(years == "cur")) {
  years = years[years != "cur"]

  years = c(years, year(Sys.Date()))
  if (month(Sys.Date()) < 5) {
    years = c(years, year(Sys.Date())-1)
  }
}

# alles wrappen in een blok, zodat er bij fouten gestopt wordt
{
  # cases eerst
  #last_update = dbGetQuery(conn, "SELECT MAX(updated) FROM vw_cases")[[1]]
  #if (is.na(last_update)) last_update = ymd_hms("1990-01-01 01:01:01")
  
  hpz_months = c()
  for (cur_year in years) {
    hpz_months = c(hpz_months, sprintf("%s-%02d", cur_year, 1:12))
  }

  for (cur_month in hpz_months) {
    printf("Cases voor maand %s", cur_month)
    if (ymd(paste0(cur_month, "-01")) > Sys.Date()) {
      printf("Maand nog niet begonnen; overslaan.")
      next
    }
    
    month_begin = paste0(cur_month, "-01")
    month_end = paste0(cur_month, "-", days_in_month(ym(cur_month)))
    
    fields = "Entered_by, ABR, Age_in_years, Agent, Case_creation_date, Case_manager, Case_number, Confidence, Date_closed, Date_of_death, Date_of_onset, Datum_definitief_in_osiris, Datum_geautomatiseerd_in_osiris, Datum_gefiatteerd_in_osiris, Datum_gewist, Datum_melding_aan_de_ggd, Diagnosis, Gender, Hospitalised, Infection, Laboratorium_waar_de_casus_gediagnosticeerd_is, Osirisnummer, Postcode, Oorspronkelijke_bron_van_de_melding, Principal_contextual_setting, Recent_travel_to_another_country, Status_van_de_melding, Vaccinated_in_respect_to_the_diagnosis, Vaccination_date"
    data = HPZone_request_paginated(paste0('cases(where: { and: [ { Case_creation_date: { gte: "', month_begin, '" } }, { Case_creation_date: { lte: "', month_end, '" } } ]  }) { items { ', fields, ' } }'), scope="extended") %>%
      HPZone_convert_dates() %>%
      distinct() # ja, zelfs de API geeft soms dubbele cases...
    # tijdelijk, zodat de datastructuur blijft kloppen
    data = data %>%
      rename(Investigating_officer="Entered_by")

    # medewerkers
    medewerkers.hpzone = c(data$Case_manager, data$Investigating_officer) %>% unique()
    medewerkers.hpzone = medewerkers.hpzone[!is.na(medewerkers.hpzone)]
    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")

    medewerkers.missend = medewerkers.hpzone[!medewerkers.hpzone %in% medewerkers.db$mdw_naam]
    if (length(medewerkers.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_medewerker](mdw_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, medewerkers.missend)), collapse=", ")))

    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")

    # context
    context.hpzone = unique(data$Principal_contextual_setting)
    context.hpzone = context.hpzone[!is.na(context.hpzone)]
    context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")

    context.missend = context.hpzone[!context.hpzone %in% context.db$ctx_naam]
    if (length(context.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_context](ctx_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, context.missend)), collapse=", ")))

    context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")

    # om onbekende redenen verandert HPZone soms de namen van de diagnose
    # zo is er bijvoorbeeld "Hepatitis B, chronic" zichtbaar onder Reference > Clinical > Diagnoses, maar heet deze in de export "Chronic Hepatitis B"
    # en dan moeten we ook nog gsub gebruiken, omdat str_replace geen \\L ondersteunt...
    data$Diagnosis = gsub("^(Acute|Chronic) hep\\w+ (\\w)", "Hepatitis \\2, \\L\\1", data$Diagnosis, perl=T)
    # er is geen acute hepatitis A of E in de lijst, maar natuurlijk zit dit wel in de data
    data$Diagnosis = str_replace(data$Diagnosis, "Hepatitis (A|E).*", "Hepatitis \\1")
    # streptokokken zijn ook gek
    data$Diagnosis = str_replace(data$Diagnosis, "Streptococcal septicae.*", "Streptococcal Group A septicaemia")
    data$Diagnosis = str_replace(data$Diagnosis, "Streptococcal septic art.*", "Streptococcal Group A septic arthritis")
    data$Diagnosis = str_replace(data$Diagnosis, "Streptococcal (toxic shock syndrome|meningitis)", "Streptococcal Group A \\1")
    data$Diagnosis = str_replace(data$Diagnosis, fixed("Streptococcal pneumonia (Group A)"), "Streptococcal Group A pneumonia")
    # en natuurlijk Haemophilus ook
    data$Diagnosis = str_replace(data$Diagnosis, "Haemophilus influenzae, spec.*", "Haemophilus influenzae infection, specified")
    # en waarom zou dit dan wel logisch zijn?
    data$Diagnosis = str_replace(data$Diagnosis, "Malaria$", "Malaria, unspecified")
    # of dit?
    data$Diagnosis = str_replace(data$Diagnosis, "(Avian) (Influenza)", "\\2, \\1")

    # wat blijkt... dit gebeurt ook met infecties
    # waarom zijn we niet verbaasd?
    # "Group A Streptococcal infection (non-invasive)" "Streptococcal Group A infection, non-invasive"  "Streptococcal Infection: Group A" --> Streptococcal Group A infection, non-invasive or unspecified
    data$Infection = str_replace(data$Infection, "Group A Streptococcal infection \\(non-invasive\\)|Streptococcal Group A infection, non-invasive|treptococcal Infection: Group A", "Streptococcal Group A infection, non-invasive or unspecified")

    # nu de casuistiek importeren
    # het is makkelijker om eerst alle IDs te combineren
    data = data %>%
      left_join(context.db, by=c("Principal_contextual_setting"="ctx_naam")) %>%
      left_join(infec.db, by=c("Infection"="inf_naam")) %>%
      left_join(diag.db, by=c("Diagnosis"="dx_naam")) %>%
      left_join(agents.db, by=c("Agent"="ag_naam")) %>%
      left_join(medewerkers.db, by=c("Case_manager"="mdw_naam")) %>%
      rename(casemdw_id=mdw_id) %>%
      left_join(medewerkers.db, by=c("Investigating_officer"="mdw_naam")) %>%
      mutate(Geslacht=case_match(Gender,
                                 "Female" ~ "F",
                                 "Male" ~ "M",
                                 "Other" ~ "X",
                                 .default="U"), # wellicht ooit nog een anders toevoegen?
             Peildatum=case_when(!is.na(Date_of_onset) ~ Date_of_onset,
                                 !is.na(Datum_melding_aan_de_ggd) ~ Datum_melding_aan_de_ggd,
                                 TRUE ~ Case_creation_date),
             PC=str_match(Postcode, "\\d+")[,1],
             Datum.verschil=abs(difftime(Datum_melding_aan_de_ggd, Date_of_onset, units="days")),
             # missende geboortedatum wordt aangegeven met datum 01-01-1900, dus als de leeftijd gelijk is aan (rapportjaar) - 1900 is deze eigenlijk missend
             Age_in_years=ifelse(Age_in_years >= year(Peildatum) - 1900, NA, Age_in_years),
             overleden=!is.na(Date_of_death),
             Osirisnummer=as.numeric(Osirisnummer))

    # een aantal sanity checks
    printf("Er zijn %d casussen met een verschil van meer dan 365 dagen tussen eerste ziektedag en meldingsdatum. Dit zijn HPZone ID's %s.",
           sum(data$Datum.verschil > 365, na.rm=T),
           str_c(sprintf("%s (%d dagen)", data$Case_number[which(data$Datum.verschil > 365)], data$Datum.verschil[which(data$Datum.verschil > 365)]), collapse=", "))
    printf("Er zijn %d casussen waarbij de eerste ziektedag na de meldingsdatum valt. Dit zijn HPZone ID's %s.",
           sum(data$Date_of_onset > data$Datum_melding_aan_de_ggd, na.rm=T), str_c(data$Case_number[which(data$Date_of_onset > data$Datum_melding_aan_de_ggd)], collapse=", "))
    printf("Er zijn %d casussen met gekke meldingsdata (> nu). Dit zijn HPZone ID's %s.",
           sum(data$Datum_melding_aan_de_ggd > Sys.Date(), na.rm=T),
           str_c(data$Case_number[which(data$Datum_melding_aan_de_ggd > Sys.Date())], collapse=", "))
    printf("Er zijn %d casussen met gekke EZD's (<= eerste melding of > nu). Dit zijn HPZone ID's %s.",
           sum(data$Date_of_onset > Sys.Date(), na.rm=T),
           str_c(data$Case_number[which(data$Date_of_onset > Sys.Date())], collapse=", "))
    printf("Er zijn %d casussen zonder melddatum. Invoerdatum aagenomen als melddatum. Dit zijn HPZone ID's %s.",
           sum(is.na(data$Datum_melding_aan_de_ggd), na.rm=T),
           str_c(data$Case_number[which(is.na(data$Datum_melding_aan_de_ggd))], collapse=", "))
    printf("Er zijn %d casussen zonder context. Dit zijn HPZone ID's %s. Hier wordt context Onbekend (%d) aangenomen.",
           sum(is.na(data$ctx_id)), str_c(data$Case_number[is.na(data$ctx_id)], collapse=", "), context.db$ctx_id[context.db$ctx_naam == "Onbekend"])
    printf("Er zijn %d casussen zonder opgegeven ziekenhuisopname. Dit zijn HPZone ID's %s. Hier wordt 'Not known' aangenomen.",
           sum(is.na(data$Hospitalised)), str_c(data$Case_number[is.na(data$Hospitalised)], collapse=", "))
    printf("Er zijn %d casussen zonder meldingsstatus in OSIRIS, en %d casussen zonder melddatum in OSIRIS. Hiervan overlappen %d gevallen.",
           sum(is.na(data$Status_van_de_melding)), sum(is.na(data$Datum.OSIRIS)), sum(is.na(data$Status_van_de_melding) & is.na(data$Datum.OSIRIS)))
    printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen datum is opgegeven. Dit zijn HPZone ID's %s. Melddatum wordt aangenomen.",
           sum(!is.na(data$Osirisnummer) & is.na(data$Datum_gefiatteerd_in_osiris)), str_c(data$Case_number[!is.na(data$Osirisnummer) & is.na(data$Datum_gefiatteerd_in_osiris)], collapse=", "))
    printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen Status_van_de_melding is opgegeven. Dit zijn HPZone ID's %s. Status wordt aangenomen als 'Gefiatteerd'.",
           sum(!is.na(data$Osirisnummer) & is.na(data$Status_van_de_melding)), str_c(data$Case_number[!is.na(data$Osirisnummer) & is.na(data$Status_van_de_melding)], collapse=", "))
    printf("Er zijn %d casussen waarbij wel een vaccinatiedatum is opgegeven, maar geen of negatieve vaccinatiestatus. Dit zijn HPZone ID's %s.",
           sum((is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee") & !is.na(data$Vaccination_date), na.rm=T), str_c(data$Case_number[which((is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee") & !is.na(data$Vaccination_date))], collapse=", "))
    printf("Let op! Al deze gevallen worden alsnog geïmporteerd. Controleer de gegevens in HPZone en voer na correctie een nieuwe export in via dit script.")

    data$ctx_id[is.na(data$ctx_id)] = context.db$ctx_id[context.db$ctx_naam == "Onbekend"]
    data$Datum_gefiatteerd_in_osiris[!is.na(data$Osirisnummer) & is.na(data$Datum_gefiatteerd_in_osiris)] = data$Case_creation_date[!is.na(data$Osirisnummer) & is.na(data$Datum_gefiatteerd_in_osiris)]
    data$Status_van_de_melding[!is.na(data$Osirisnummer) & is.na(data$Status_van_de_melding)] = "Gefiatteerd"
    data$Vaccinated_in_respect_to_the_diagnosis[!is.na(data$Vaccination_date) & (is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee")] = "Ja"

    # het kan zijn dat een bepaald geval al voorkomt in de database, in dat geval moeten we updaten
    # dit breken we op in lijsten van 500, zodat de database niet helemaal hoeft te huilen
    hpzone_ids = unique(data$Case_number)
    existing = c()
    for (i in seq(1, length(hpzone_ids), 500)) {
      result = dbGetQuery(conn, paste0("SELECT Case_number FROM [dbo].[cases] WHERE Case_number IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
      existing = c(existing, as.character(result$Case_number))
    }
    if (length(existing) > 0) {
      data.existing = data[data$Case_number %in% existing,]

      update = sprintf("UPDATE [dbo].[cases]
              SET ABR=%s, Age_in_years=%s, Agent=%s, Case_creation_date='%s', 
                  Case_manager=%s, Confidence=%s,
                  Date_closed=%s, Date_of_death=%s, Date_of_onset=%s,
                  Datum_definitief_in_osiris=%s, Datum_geautomatiseerd_in_osiris=%s,
                  Datum_gefiatteerd_in_osiris=%s, Datum_gewist=%s,
                  Datum_melding_aan_de_ggd=%s, Diagnosis=%s,
                  Gender='%s', Hospitalised=%s, Infection=%s,
                  Investigating_officer=%s, Laboratorium_waar_de_casus_gediagnosticeerd_is=%s,
                  Osirisnummer=%s, Oorspronkelijke_bron_van_de_melding=%s,
                  Postcode=%s, Principal_contextual_setting=%s,
                  Recent_travel_to_another_country=%s, Status_van_de_melding=%s,
                  Vaccinated_in_respect_to_the_diagnosis=%s, Vaccination_date=%s, updated=GETDATE()
              WHERE Case_number=%d",
              NullOrQuotes(data.existing$ABR), NullOrQuotes(data.existing$Age_in_years), NullOrQuotes(data.existing$ag_id), data.existing$Case_creation_date,
              NullOrQuotes(data.existing$casemdw_id), dbQuoteString(conn, data.existing$Confidence),
              NullOrQuotes(data.existing$Date_closed), NullOrQuotes(data.existing$Date_of_death), NullOrQuotes(data.existing$Date_of_onset),
              NullOrQuotes(data.existing$Datum_definitief_in_osiris), NullOrQuotes(data.existing$Datum_geautomatiseerd_in_osiris),
              NullOrQuotes(data.existing$Datum_gefiatteerd_in_osiris), NullOrQuotes(data.existing$Datum_gewist),
              NullOrQuotes(data.existing$Datum_melding_aan_de_ggd), NullOrQuotes(data.existing$dx_id),
              data.existing$Geslacht, NullOrQuotes(data.existing$Hospitalised), NullOrQuotes(data.existing$inf_id),
              NullOrQuotes(data.existing$mdw_id), dbQuoteString(conn, data.existing$Laboratorium_waar_de_casus_gediagnosticeerd_is),
              NullOrQuotes(data.existing$Osirisnummer), dbQuoteString(conn, data.existing$Oorspronkelijke_bron_van_de_melding),
              NullOrQuotes(data.existing$PC), NullOrQuotes(data.existing$ctx_id),
              dbQuoteString(conn, data.existing$Recent_travel_to_another_country), NullOrQuotes(data.existing$Status_van_de_melding),
              NullOrQuotes(data.existing$Vaccinated_in_respect_to_the_diagnosis), NullOrQuotes(data.existing$Vaccination_date),
              data.existing$Case_number)

      for (i in seq(1, length(update), 500)) {
        printf("- Aanpassen: rijen %d-%d/%d", i, min(length(update),i+499), length(update))
        dbBegin(conn)
        res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
        dbCommit(conn)
      }
    }
    data.insert = data[!data$Case_number %in% existing,]
    
    insert = sprintf("(%s, %s, %s, '%s',
                      %s, %d, %s,
                     %s, %s, %s,
                     %s, %s,
                     %s, %s,
                     %s, %s,
                     '%s', %s, %s,
                     %s, %s,
                     %s, %s,
                     %s, %s,
                     %s, %s,
                     %s, %s, GETDATE())",
                     NullOrQuotes(data.insert$ABR), NullOrQuotes(data.insert$Age_in_years), NullOrQuotes(data.insert$ag_id), data.insert$Case_creation_date,
                     NullOrQuotes(data.insert$casemdw_id), data.insert$Case_number, dbQuoteString(conn, data.insert$Confidence),
                     NullOrQuotes(data.insert$Date_closed), NullOrQuotes(data.insert$Date_of_death), NullOrQuotes(data.insert$Date_of_onset),
                     NullOrQuotes(data.insert$Datum_definitief_in_osiris), NullOrQuotes(data.insert$Datum_geautomatiseerd_in_osiris),
                     NullOrQuotes(data.insert$Datum_gefiatteerd_in_osiris), NullOrQuotes(data.insert$Datum_gewist),
                     NullOrQuotes(data.insert$Datum_melding_aan_de_ggd), NullOrQuotes(data.insert$dx_id),
                     data.insert$Geslacht, NullOrQuotes(data.insert$Hospitalised), NullOrQuotes(data.insert$inf_id),
                     NullOrQuotes(data.insert$mdw_id), dbQuoteString(conn, data.insert$Laboratorium_waar_de_casus_gediagnosticeerd_is),
                     NullOrQuotes(data.insert$Osirisnummer), dbQuoteString(conn, data.insert$Oorspronkelijke_bron_van_de_melding),
                     NullOrQuotes(data.insert$PC), NullOrQuotes(data.insert$ctx_id),
                     dbQuoteString(conn, data.insert$Recent_travel_to_another_country), NullOrQuotes(data.insert$Status_van_de_melding),
                     NullOrQuotes(data.insert$Vaccinated_in_respect_to_the_diagnosis), NullOrQuotes(data.insert$Vaccination_date))

    if (length(insert) > 0) {
      # opbreken in blokjes van 250, zodat niet alles direct pijn doet
      for (i in seq(1, length(insert), 250)) {
        printf("- Invoegen: rijen %d-%d/%d", i, min(length(insert),i+249), length(insert))
        dbBegin(conn)
        res = dbExecute(conn, paste0("INSERT INTO [dbo].[cases](ABR, Age_in_years, Agent, Case_creation_date,
                                     Case_manager, Case_number, Confidence,
                                     Date_closed, Date_of_death, Date_of_onset,
                                     Datum_definitief_in_osiris, Datum_geautomatiseerd_in_osiris,
                                     Datum_gefiatteerd_in_osiris, Datum_gewist,
                                     Datum_melding_aan_de_ggd, Diagnosis,
                                     Gender, Hospitalised, Infection,
                                     Investigating_officer, Laboratorium_waar_de_casus_gediagnosticeerd_is,
                                     Osirisnummer, Oorspronkelijke_bron_van_de_melding,
                                     Postcode, Principal_contextual_setting,
                                     Recent_travel_to_another_country, Status_van_de_melding,
                                     Vaccinated_in_respect_to_the_diagnosis, Vaccination_date, created) VALUES",
                                     str_c(insert[i:min(length(insert),i+249)], collapse=", ")) %>% str_replace("\n    ", ""))
        dbCommit(conn)
      }
    }
    
    #
    # situations
    #
    
    printf("Situations voor maand %s", cur_month)
    
    fields = "Location, Type, Status, Start_date, Situation_number, Situation_creation_date, Scenario, Principal_contextual_setting, Postcode, Osirisnummer, Number_potentially_at_risk, Number_of_symptomatic_cases, Number_of_fatalities, Number_hospitalised, Manager, Investigating_officer, Infectious_agent, Confidence, Closed_date, Artikel_26"
    
    data = HPZone_request_paginated(paste0('situations(where: { and: [ { Situation_creation_date: { gte: "', month_begin, '" } }, { Situation_creation_date: { lte: "', month_end, '" } } ]  }) { items { ', fields, ' } }'), scope="extended") %>%
      HPZone_convert_dates() %>%
      distinct()
    
    # medewerkers
    medewerkers.hpzone = c(data$Manager, data$Investigating_officer) %>% unique()
    medewerkers.hpzone = medewerkers.hpzone[!is.na(medewerkers.hpzone)]
    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
    
    medewerkers.missend = medewerkers.hpzone[!medewerkers.hpzone %in% medewerkers.db$mdw_naam]
    if (length(medewerkers.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_medewerker](mdw_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, medewerkers.missend)), collapse=", ")))
    
    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
    
    # scenario
    scenario.hpzone = unique(data$Scenario)
    scenario.hpzone = scenario.hpzone[!is.na(scenario.hpzone)]
    scenario.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_scenario]")
    
    scenario.missend = scenario.hpzone[!scenario.hpzone %in% scenario.db$sc_naam]
    if (length(scenario.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_scenario](sc_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, scenario.missend)), collapse=", ")))
    
    scenario.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_scenario]")
    
    # context
    context.hpzone = unique(data$Principal_contextual_setting)
    context.hpzone = context.hpzone[!is.na(context.hpzone)]
    context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")
    
    context.missend = context.hpzone[!context.hpzone %in% context.db$ctx_naam]
    if (length(context.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_context](ctx_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, context.missend)), collapse=", ")))
    
    context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")
    
    
    data = data %>%
      left_join(context.db, by=c("Principal_contextual_setting"="ctx_naam")) %>%
      left_join(agents.db, by=c("Infectious_agent"="ag_naam")) %>%
      left_join(scenario.db, by=c("Scenario"="sc_naam")) %>%
      left_join(medewerkers.db, by=c("Manager"="mdw_naam")) %>%
      rename(manager_id=mdw_id) %>%
      left_join(medewerkers.db, by=c("Investigating_officer"="mdw_naam")) %>%
      mutate(PC=str_extract(Postcode, "\\d+"))
    
    # kijken welke records al bestaan
    hpzone_ids = unique(data$Situation_number)
    existing = c()
    for (i in seq(1, length(hpzone_ids), 500)) {
      result = dbGetQuery(conn, paste0("SELECT Situation_number FROM [dbo].[situations] WHERE Situation_number IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
      existing = c(existing, as.character(result$Situation_number))
    }
    
    if (length(existing) > 0) {
      data.existing = data[data$Situation_number %in% existing,]
      update = sprintf("UPDATE [dbo].[situations]
             SET Type=%s, Status=%s, Start_date=%s,
             Situation_creation_date='%s', Scenario=%s, Principal_contextual_setting=%s,
             Postcode=%s, Osirisnummer=%s, Number_potentially_at_risk=%s,
             Number_of_symptomatic_cases=%s, Number_of_fatalities=%s,
             Number_hospitalised=%s, Manager=%s, Investigating_officer=%s,
             Infectious_agent=%s, Confidence=%s, Closed_date=%s,
             Artikel_26=%s,
             updated=GETDATE()
             WHERE Situation_number=%s",
                       NullOrQuotes(data.existing$Type), NullOrQuotes(data.existing$Status), NullOrQuotes(data.existing$Start_date),
                       data.existing$Situation_creation_date, NullOrQuotes(data.existing$sc_id), NullOrQuotes(data.existing$ctx_id),
                       NullOrQuotes(data.existing$PC), NullOrQuotes(data.existing$Osirisnummer), NullOrQuotes(data.existing$Number_potentially_at_risk),
                       NullOrQuotes(data.existing$Number_of_symptomatic_cases), NullOrQuotes(data.existing$Number_of_fatalities),
                       NullOrQuotes(data.existing$Number_hospitalised), NullOrQuotes(data.existing$manager_id), NullOrQuotes(data.existing$mdw_id),
                       NullOrQuotes(data.existing$ag_id), NullOrQuotes(data.existing$Confidence), NullOrQuotes(data.existing$Closed_date),
                       NullOrQuotes(data.existing$Artikel_26),
                       data.existing$Situation_number)
      
      for (i in seq(1, length(update), 500)) {
        printf("- Aanpassen: rijen %d-%d/%d", i, min(length(update),i+499), length(update))
        dbBegin(conn)
        res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
        dbCommit(conn)
      }
    }
    data.insert = data[!data$Situation_number %in% existing,]
    insert = sprintf("(%d, %s, %s, %s,
                   '%s', %s, %s,
                   %s, %s, %s,
                   %s, %s,
                   %s, %s, %s,
                   %s, %s, %s,
                   %s, GETDATE())",
                     data.insert$Situation_number,
                     NullOrQuotes(data.insert$Type), NullOrQuotes(data.insert$Status), NullOrQuotes(data.insert$Start_date),
                     data.insert$Situation_creation_date, NullOrQuotes(data.insert$sc_id), NullOrQuotes(data.insert$ctx_id),
                     NullOrQuotes(data.insert$PC), NullOrQuotes(data.insert$Osirisnummer), NullOrQuotes(data.insert$Number_potentially_at_risk),
                     NullOrQuotes(data.insert$Number_of_symptomatic_cases), NullOrQuotes(data.insert$Number_of_fatalities),
                     NullOrQuotes(data.insert$Number_hospitalised), NullOrQuotes(data.insert$manager_id), NullOrQuotes(data.insert$mdw_id),
                     NullOrQuotes(data.insert$ag_id), NullOrQuotes(data.insert$Confidence), NullOrQuotes(data.insert$Closed_date),
                     NullOrQuotes(data.insert$Artikel_26))
    
    if (length(insert) > 0) {
      # opbreken in blokjes van 500, zodat niet alles direct pijn doet
      for (i in seq(1, length(insert), 500)) {
        printf("- Invoegen: rijen %d-%d/%d", i, min(length(insert),i+499), length(insert))
        dbBegin(conn)
        res = dbExecute(conn, paste0("INSERT INTO situations(Situation_number, Type, Status, Start_date,
                                   Situation_creation_date, Scenario, Principal_contextual_setting,
                                   Postcode, Osirisnummer, Number_potentially_at_risk,
                                   Number_of_symptomatic_cases, Number_of_fatalities,
                                   Number_hospitalised, Manager, Investigating_officer,
                                   Infectious_agent, Confidence, Closed_date,
                                   Artikel_26, created) VALUES", str_c(insert[i:min(length(insert),i+499)], collapse=", ")))
        dbCommit(conn)
      }
    }
    
    # TODO: export KDV
    
    #
    # enquiries
    #
    
    printf("Enquiries voor maand %s", cur_month)
    
    fields = "Type_of_caller, Status, Specific_topic, Received_on, Originally_taken_by, Handled_by, Enquiry_number, Date_closed, Caller_postcode4, Broad_topic, Additional_topic"
    
    data = HPZone_request_paginated(paste0('enquiries(where: { and: [ { Received_on: { gte: "', month_begin, '" } }, { Received_on: { lte: "', month_end, '" } } ]  }) { items { ', fields, ' } }'), scope="extended") %>%
      HPZone_convert_dates(search="(Date|Received_on)") %>%
      distinct()
    
    # eerst zorgen dat alle variabelen bekend zijn
    # dit zijn: medewerker (2x), onderwerp
    
    # medewerkers
    medewerkers.hpzone = c(data$Originally_taken_by, data$Handled_by) %>% unique()
    medewerkers.hpzone = medewerkers.hpzone[!is.na(medewerkers.hpzone)]
    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
    
    medewerkers.missend = medewerkers.hpzone[!medewerkers.hpzone %in% medewerkers.db$mdw_naam]
    if (length(medewerkers.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_medewerker](mdw_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, medewerkers.missend)), collapse=", ")))
    
    medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
    
    # onderwerp
    onderwerp.hpzone = unique(data$Specific_topic)
    onderwerp.hpzone = onderwerp.hpzone[!is.na(onderwerp.hpzone)]
    onderwerp.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_onderwerp]")
    
    onderwerp.missend = onderwerp.hpzone[!onderwerp.hpzone %in% onderwerp.db$ond_naam]
    if (length(onderwerp.missend) > 0)
      dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_onderwerp](ond_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, onderwerp.missend)), collapse=", ")))
    
    onderwerp.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_onderwerp]")
    
    # sanity checks
    printf("Er zijn %d gevallen waarbij de sluitingsdatum voor de ontvangstdatum staat. Dit zijn gevallen %s.",
           sum(data$Received_on > data$Date_closed), str_c(data$Number[data$Received_on > data$Date_closed], collapse=", "))
    printf("Er zijn %d gevallen zonder Broad_topic. Dit zijn gevallen %s.",
           sum(is.na(data$Broad_topic)), str_c(data$Number[is.na(data$Broad_topic)], collapse=", "))
    printf("Er zijn %d gevallen zonder Specific_topic. Dit zijn gevallen %s.",
           sum(is.na(data$Specific_topic)), str_c(data$Number[is.na(data$Specific_topic)], collapse=", "))
    
    data = data %>%
      left_join(onderwerp.db, by=c("Specific_topic"="ond_naam")) %>%
      left_join(medewerkers.db, by=c("Originally_taken_by"="mdw_naam")) %>%
      rename(ontvanger_id=mdw_id) %>%
      left_join(medewerkers.db, by=c("Handled_by"="mdw_naam")) %>%
      mutate(PC=str_extract(Caller_postcode4, "\\d+")) 
    
    # kijken welke records al bestaan
    hpzone_ids = unique(data$Enquiry_number)
    existing = c()
    for (i in seq(1, length(hpzone_ids), 500)) {
      result = dbGetQuery(conn, paste0("SELECT Enquiry_number FROM [dbo].[enquiries] WHERE Enquiry_number IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
      existing = c(existing, as.character(result$Enquiry_number))
    }
    
    if (length(existing) > 0) {
      data.existing = data[data$Enquiry_number %in% existing,]
      update = sprintf("UPDATE [dbo].[enquiries]
               SET Type_of_caller=%s, Status=%s, Specific_topic=%s,
               Received_on=%s, Originally_taken_by=%s, Handled_by=%s,
               Date_closed=%s, Caller_postcode4=%s, Broad_topic=%s,
               Additional_topic=%s,
               updated=GETDATE()
               WHERE Enquiry_number=%d",
                       NullOrQuotes(data.existing$Type_of_caller), NullOrQuotes(data.existing$Status), NullOrQuotes(data.existing$ond_id),
                       NullOrQuotes(data.existing$Received_on), NullOrQuotes(data.existing$ontvanger_id), NullOrQuotes(data.existing$mdw_id),
                       NullOrQuotes(data.existing$Date_closed), NullOrQuotes(data.existing$Caller_postcode4), dbQuoteString(conn, data.existing$Broad_topic),
                       dbQuoteString(conn, data.existing$Additional_topic %>% as.character()),
                       data.existing$Enquiry_number)
      
      for (i in seq(1, length(update), 500)) {
        printf("- Aanpassen: rijen %d-%d/%d", i, min(length(update),i+499), length(update))
        dbBegin(conn)
        res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
        dbCommit(conn)
      }
    }
    data.insert = data[!data$Enquiry_number %in% existing,]
    insert = sprintf("(%d, %s, %s, %s,
                     %s, %s, %s,
                     %s, %s, %s,
                     %s, GETDATE())", data.insert$Enquiry_number,
                     NullOrQuotes(data.insert$Type_of_caller), NullOrQuotes(data.insert$Status), NullOrQuotes(data.insert$ond_id),
                     NullOrQuotes(data.insert$Received_on), NullOrQuotes(data.insert$ontvanger_id), NullOrQuotes(data.insert$mdw_id),
                     NullOrQuotes(data.insert$Date_closed), NullOrQuotes(data.insert$Caller_postcode4), dbQuoteString(conn, data.insert$Broad_topic),
                     dbQuoteString(conn, as.character(data.insert$Additional_topic))) # additional topic kan helemaal leeg zijn; wrappen in as.char
    
    if (length(insert) > 0) {
      # opbreken in blokjes van 500, zodat niet alles direct pijn doet
      for (i in seq(1, length(insert), 500)) {
        printf("- Invoegen: rijen %d-%d/%d", i, min(length(insert),i+499), length(insert))
        dbBegin(conn)
        res = dbExecute(conn, paste0("INSERT INTO enquiries(Enquiry_number,
              Type_of_caller, Status, Specific_topic,
              Received_on, Originally_taken_by, Handled_by,
              Date_closed, Caller_postcode4, Broad_topic,
              Additional_topic, created) VALUES", str_c(insert[i:min(length(insert),i+499)], collapse=", ")))
        dbCommit(conn)
      }
    }
  }


  printf("Omzetting naar bestanden voor het HSC dashboard wordt opgestart.")

  source("preparatie_HPZone_export.R")

  printf("Omzetting uitgevoerd.")
}

dbDisconnect(conn)
