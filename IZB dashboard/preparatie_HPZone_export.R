################################################################################
# Dit script download de meest recente data vanuit HPZone met de API.          #
#                                                                              #
# Gebruik is simpel:                                                           #
# - Vul de onderstaande configuratie in.                                       #
# - Voer dit bestand uit.                                                      #
################################################################################

# configuratiemogelijkheden

# map waar de bestanden geplaatst moeten worden
# LET OP: slash vs. backslash, gelijk aan opmerking hierboven
# zet op NA voor uitvoer in datamap
uitvoermap = "\\\\az-data1.ssc.local\\Apl_data\\DWH_Data\\Import\\NOG_HPZone\\"
# jaren om op te halen
# mogelijke waardes: jaartallen of "cur" voor huidig (en voorgaand indien recent na jaarwisseling) jaar
years = "cur"
# API details
# hiervan zijn twee delen nodig:
# - client secret (herkenbaar aan PROD-API-iets)
# - client key (herkenbaar aan onleesbaarheid)
client_id = ""
client_secret = ""


## begin script

# benodigde packages installeren/laden
pkg_req = c("tidyverse", "this.path", "devtools", "HPZoneAPI")

for (pkg in pkg_req) {
  if (system.file(package = pkg) == "") {
    if (pkg == "HPZoneAPI") {
      devtools::install_github("ggdatascience/HPZoneAPI")
    } else {
      install.packages(pkg)
    }
  }
}

library(tidyverse)
library(this.path)
library(HPZoneAPI)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

HPZone_setup(client_id, client_secret)

if (!is.na(uitvoermap) && !str_ends(uitvoermap, "\\\\") && !str_ends(uitvoermap, "/")) {
  uitvoermap = paste0(uitvoermap, "/")
}

# doorlopen benodigde jaren
if (any(years == "cur")) {
  years = years[years != "cur"]

  years = c(years, year(Sys.Date()))
  if (month(Sys.Date()) < 5) {
    years = c(years, year(Sys.Date())-1)
  }
}

output_files = c()
for (cur_year in years) {
  # cases eerst
  fields = "ABR, Age_in_years, Agent, Case_creation_date, Case_manager, Case_number, Confidence, Date_closed, Date_of_death, Date_of_onset, Datum_definitief_in_osiris, Datum_geautomatiseerd_in_osiris, Datum_gefiatteerd_in_osiris, Datum_gewist, Datum_melding_aan_de_ggd, Diagnosis, Disease, Gender, Hospitalised, Infection, Entered_by, Laboratorium_waar_de_casus_gediagnosticeerd_is, Osiris_meldingsnummer, Osirisnummer, Oorspronkelijke_bron_van_de_melding, Postcode, Principal_contextual_setting, Recent_travel_to_another_country, Status_van_de_melding, Vaccinated_in_respect_to_the_diagnosis, Vaccination_date"

  data = HPZone_request_paginated(paste0('cases(where: { and: [ { Case_creation_date: { gte: "', cur_year, '-01-01" } }, { Case_creation_date: { lte: "', cur_year, '-12-31" } } ]  }) { items { ', fields, ' } }'),
                                  scope="extended") %>%
    HPZone_convert_dates()

  data$Diagnosis = gsub("^(Acute|Chronic) hep\\w+ (\\w)", "Hepatitis \\2, \\L\\1", data$Diagnosis, perl=T)
  # er staat geen acute hepatitis A of E in de lijst, maar natuurlijk zit dit wel in de data
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


  # de melddatum in OSIRIS staat in verschillende velden; samenvoegen voor duidelijkheid
  data = data %>%
    mutate(Datum.OSIRIS=case_when(!is.na(Datum_definitief_in_osiris) ~ Datum_definitief_in_osiris,
                                  !is.na(Datum_gefiatteerd_in_osiris) ~ Datum_gefiatteerd_in_osiris))

  # sanity checks
  printf("Er zijn %d casussen waarbij de eerste ziektedag na de meldingsdatum valt. Dit zijn HPZone ID's %s.",
         sum(data$Date_of_onset > data$Datum_melding_aan_de_ggd, na.rm=T), str_c(data$Case_number[which(data$Date_of_onset > data$Datum_melding_aan_de_ggd)], collapse=", "))
  printf("Er zijn %d casussen met gekke meldingsdata (> nu). Dit zijn HPZone ID's %s.",
         sum(data$Datum_melding_aan_de_ggd > Sys.Date(), na.rm=T),
         str_c(data$Case_number[which(data$Datum_melding_aan_de_ggd > Sys.Date())], collapse=", "))
  printf("Er zijn %d casussen met gekke EZD's (> nu). Dit zijn HPZone ID's %s.",
         sum(data$Date_of_onset > Sys.Date(), na.rm=T),
         str_c(data$Case_number[which(data$Date_of_onset > Sys.Date())], collapse=", "))
  printf("Er zijn %d casussen zonder melddatum. Invoerdatum aagenomen als melddatum. Dit zijn HPZone ID's %s.",
         sum(is.na(data$Datum_melding_aan_de_ggd), na.rm=T),
         str_c(data$Case_number[which(is.na(data$Datum_melding_aan_de_ggd))], collapse=", "))
  printf("Er zijn %d casussen zonder context. Dit zijn HPZone ID's %s. Hier wordt context Onbekend aangenomen.",
         sum(is.na(data$Principal_contextual_setting)), str_c(data$Case_number[is.na(data$Principal_contextual_setting)], collapse=", "))
  printf("Er zijn %d casussen zonder opgegeven ziekenhuisopname. Dit zijn HPZone ID's %s. Hier wordt 'Not known' aangenomen.",
         sum(is.na(data$Hospitalised)), str_c(data$Case_number[is.na(data$Hospitalised)], collapse=", "))
  printf("Er zijn %d casussen zonder meldingsstatus in OSIRIS, en %d casussen zonder melddatum in OSIRIS. Hiervan overlappen %d gevallen.",
         sum(is.na(data$Status_van_de_melding)), sum(is.na(data$Datum.OSIRIS)), sum(is.na(data$Status_van_de_melding) & is.na(data$Datum.OSIRIS)))
  printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen datum is opgegeven. Dit zijn HPZone ID's %s. Melddatum wordt aangenomen.",
         sum(!is.na(data$Osiris_meldingsnummer) & is.na(data$Datum_gefiatteerd_in_osiris)), str_c(data$Case_number[!is.na(data$Osiris_meldingsnummer) & is.na(data$Datum_gefiatteerd_in_osiris)], collapse=", "))
  printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen status van de melding is opgegeven. Dit zijn HPZone ID's %s. Status wordt aangenomen als 'Gefiatteerd'.",
         sum(!is.na(data$Osiris_meldingsnummer) & is.na(data$Status_van_de_melding)), str_c(data$Case_number[!is.na(data$Osiris_meldingsnummer) & is.na(data$Status_van_de_melding)], collapse=", "))
  printf("Er zijn %d casussen waarbij wel een vaccinatiedatum is opgegeven, maar geen of negatieve vaccinatiestatus. Dit zijn HPZone ID's %s.",
         sum((is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee") & !is.na(data$Vaccination_date), na.rm=T), str_c(data$Case_number[which((is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee") & !is.na(data$Vaccination_date))], collapse=", "))
  printf("Let op! Al deze gevallen worden alsnog geïmporteerd. Controleer de gegevens in HPZone en voer na correctie een nieuwe export in via dit script.")

  data$Principal_contextual_setting[is.na(data$Principal_contextual_setting)] = "Onbekend"
  data$Datum_gefiatteerd_in_osiris[!is.na(data$Osiris_meldingsnummer) & is.na(data$Datum_gefiatteerd_in_osiris)] = data$Case_creation_date[!is.na(data$Osiris_meldingsnummer) & is.na(data$Datum_gefiatteerd_in_osiris)]
  data$Status_van_de_melding[!is.na(data$Osiris_meldingsnummer) & is.na(data$Status_van_de_melding)] = "Gefiatteerd"
  data$Vaccinated_in_respect_to_the_diagnosis[!is.na(data$Vaccination_date) & (is.na(data$Vaccinated_in_respect_to_the_diagnosis) | data$Vaccinated_in_respect_to_the_diagnosis == "Nee")] = "Ja"
  data$Hospitalised[data$Hospitalised == -1] = NA

  # omzetten naar gewenst formaat
  data = data %>%
    mutate(peildatum=case_when(!is.na(Date_of_onset) ~ Date_of_onset,
                               !is.na(Datum_melding_aan_de_ggd) ~ Datum_melding_aan_de_ggd,
                               TRUE ~ Case_creation_date),
           melddatum=case_when(!is.na(Datum_melding_aan_de_ggd) ~ Datum_melding_aan_de_ggd,
                               TRUE ~ Case_creation_date),
           geslacht=case_when(Gender == "Female" ~ "F",
                              Gender == "Male" ~ "M",
                              TRUE ~ "U"),
           PC=Postcode,
           ziekenhuisopname=Hospitalised,
           gemeld=case_when(!is.na(Datum_gefiatteerd_in_osiris) ~ Datum_gefiatteerd_in_osiris,
                            !is.na(Datum_definitief_in_osiris) ~ Datum_definitief_in_osiris)) %>%
    rename(hpzone_id="Case_number", meldorganisatie="Oorspronkelijke_bron_van_de_melding",
           leeftijd="Age_in_years", agent="Agent", infectie="Infection",
           diagnose="Diagnosis", diagnosezekerheid="Confidence", antibioticaresistentie="ABR",
           buitenland="Recent_travel_to_another_country", eersteziektedag="Date_of_onset",
           context="Principal_contextual_setting", overlijden="Date_of_death",
           vaccinatie="Vaccinated_in_respect_to_the_diagnosis", vaccinatiedatum="Vaccination_date",
           statusmelding="Status_van_de_melding", medewerker="Entered_by",
           casemanager="Case_manager") %>%
    select(hpzone_id, peildatum, melddatum, meldorganisatie, geslacht, leeftijd, PC,
           agent, infectie, diagnose, diagnosezekerheid, antibioticaresistentie, buitenland,
           eersteziektedag, context, ziekenhuisopname, overlijden, vaccinatie, vaccinatiedatum,
           gemeld, statusmelding, medewerker, casemanager) %>%
    rename(postcode=PC) %>%
    # missende geboortedatum wordt aangegeven met datum 01-01-1900, dus als de leeftijd gelijk is aan (rapportjaar) - 1900 is deze eigenlijk missend
    mutate(leeftijd=ifelse(as.numeric(leeftijd) >= year(peildatum) - 1900, NA, as.numeric(leeftijd)))

  write.csv(data, sprintf("export_cases_%s.csv", cur_year), na="", row.names=F)
  output_files = c(output_files, sprintf("export_cases_%s.csv", cur_year))

  printf("Bestand opgeslagen als export_cases_%s.csv.", cur_year)



  # situations
  fields = "Type, Status, Start_date, Situation_number, Situation_creation_date, Scenario, Principal_contextual_setting, Postcode, Osirisnummer, Number_potentially_at_risk, Number_of_symptomatic_cases, Number_of_fatalities, Number_hospitalised, Manager, Location, Investigating_officer, Infectious_agent, Confidence, Closed_date, Artikel_26"
  data = HPZone_request_paginated(paste0('situations(where: { and: [ { Situation_creation_date: { gte: "', cur_year, '-01-01" } }, { Situation_creation_date: { lte: "', cur_year, '-12-31" } } ]  }) { items { ', fields, ' } }'),
                                  scope="standard") %>%
    HPZone_convert_dates()

  # omzetten naar gewenst formaat
  data = data %>%
    mutate(datum=case_when(!is.na(Start_date) ~ Start_date,
                           TRUE ~ Situation_creation_date),
           artikel26=Artikel_26,
           PC=Postcode,
           melding=!is.na(Osirisnummer),
           risiconiveau=1) %>%
    rename(hpzone_id="Situation_number", invoerdatum="Situation_creation_date",
           status=Status, type=Type, agent="Infectious_agent", scenario=Scenario,
           zekerheid="Confidence",
           context="Principal_contextual_setting", medewerker="Investigating_officer",
           casemanager="Manager", aantal_symptomatisch="Number_of_symptomatic_cases",
           aantal_risico="Number_potentially_at_risk", aantal_ziekenhuis="Number_hospitalised",
           aantal_overleden="Number_of_fatalities") %>%
    select(hpzone_id, datum, invoerdatum, status, type, agent, scenario, zekerheid,
           risiconiveau, artikel26, context, PC, melding, medewerker, casemanager,
           aantal_symptomatisch, aantal_risico, aantal_ziekenhuis, aantal_overleden) %>%
    rename(postcode=PC)

  write.csv(data, sprintf("export_situations_%s.csv", cur_year), na="", row.names=F)
  output_files = c(output_files, sprintf("export_situations_%s.csv", cur_year))

  printf("Bestand opgeslagen als export_situations_%s.csv.", cur_year)



  # enquiries
  fields = "Type_of_caller, Status, Specific_topic, Received_on, Originally_taken_by, Handled_by, Enquiry_number, Date_closed, Caller_postcode4, Broad_topic, Additional_topic"
  data = HPZone_request_paginated(paste0('enquiries(where: { and: [ { Received_on: { gte: "', cur_year, '-01-01" } }, { Received_on: { lte: "', cur_year, '-12-31" } } ]  }) { items { ', fields, ' } }'),
                                  scope="standard") %>%
    HPZone_convert_dates(search="(dat(e|um))|Received_on")

  printf("Er zijn %d gevallen waarbij de sluitingsdatum voor de ontvangstdatum staat. Dit zijn gevallen %s.",
         sum(data$Received_on > data$`Date closed`), str_c(data$Number[data$Received_on > data$`Date closed`], collapse=", "))
  printf("Er zijn %d gevallen zonder broad topic. Dit zijn gevallen %s.",
         sum(is.na(data$Broad_topic)), str_c(data$Number[is.na(data$Broad_topic)], collapse=", "))
  printf("Er zijn %d gevallen zonder specific topic. Dit zijn gevallen %s.",
         sum(is.na(data$Specific_topic)), str_c(data$Number[is.na(data$Specific_topic)], collapse=", "))

  data = data %>%
    mutate(PC=Caller_postcode4,
           geslacht="U") %>%
    rename(hpzone_id="Enquiry_number", startdatum="Received_on", einddatum="Date_closed",
           ontvanger="Originally_taken_by", medewerker="Handled_by", status=Status,
           typebeller="Type_of_caller", categorie="Broad_topic",
           onderwerp="Specific_topic", onderwerpopen="Additional_topic") %>%
    select(hpzone_id, startdatum, einddatum, ontvanger, status, PC, geslacht,
           typebeller, categorie, onderwerp, onderwerpopen) %>%
    rename(postcode=PC)

  write.csv(data, sprintf("export_enquiries_%s.csv", cur_year), na="", row.names=F, fileEncoding="UTF-8")
  output_files = c(output_files, sprintf("export_enquiries_%s.csv", cur_year))

  printf("Bestand opgeslagen als export_enquiries_%s.csv.", cur_year)
}

if (!is.na(uitvoermap)) {
  for (f in output_files) {
    file.rename(f, paste0(uitvoermap, f))
  }
  printf("Bestanden verplaatst naar %s: %s.", uitvoermap, str_c(output_files, collapse=", "))
}
