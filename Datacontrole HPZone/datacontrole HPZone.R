library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)
library(HPZoneAPI)
library(ggdnog)
library(this.path)
library(knitr)
library(Microsoft365R)

outlook = get_business_outlook()

setwd(dirname(this.path()))

conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0.ssc.local\\odbnog", Database="HPZone")

startdatum = floor_date(Sys.Date() - months(1), unit="month")
einddatum = ceiling_date(Sys.Date() - months(1), unit="month")
#startdatum = ymd("2026-07-01")
#einddatum = ceiling_date(startdatum, unit="month")

mdw = read.table(text="email,zoekterm
adres@ggd.nl,Jan
adres2@ggd.nl,Manon", sep=",", header=T)


cases = HPZone_request("cases", "all", where=c("Case_creation_date", ">=", as.character(startdatum), "Case_creation_date", "<", as.character(einddatum))) %>%
  HPZone_convert_dates()

cases_odb = dbGetQuery(conn, paste0("SELECT * FROM vw_cases WHERE hpzone_id IN (", str_c(cases$Case_number, collapse=", "), ")"))

cases = cases %>%
  left_join(cases_odb %>% select(hpzone_id, groep), by=c("Case_number"="hpzone_id")) %>%
  mutate(inf=coalesce(groep, Infection))

problemen = data.frame()

probleem = function (IDs, field, note) {
  if (length(IDs) == 0) return()
  if (all(is.na(IDs))) return()
  problemen <<- bind_rows(problemen, data.frame(ID=IDs, veld=field, melding=note))
}

# checks:
# - geen infectie
# - EZD na melddatum
# - geen status melding OSIRIS
# - geen context
# - geen postcode
# - geen reisgeschiedenis
# - meldingsdatum > nu
# - Datum OSIRIS en status van de melding
# - vaccinatiedatum maar geen status, of vice versa

inf_missing = cases %>%
  filter(is.na(Infection))
probleem(inf_missing$Case_number, "Infectie", "Geen infectie opgegeven")

ctx_missing = cases %>%
  filter(is.na(Principal_contextual_setting))
probleem(ctx_missing$Case_number, "Context", "Geen context opgegeven")

pc_missing = cases %>%
  filter(is.na(Postcode))
probleem(pc_missing$Case_number, "Postcode", "Geen postcode opgegeven")

ezd_fout = cases %>%
  filter(Date_of_onset > Datum_melding_aan_de_ggd)
probleem(ezd_fout$Case_number, "EZD", "Ingevoerde EZD ligt na meldingsdatum")

ezd_telaat = cases %>%
  mutate(Datum_verschil=Date_of_onset - Datum_melding_aan_de_ggd %>% as.numeric() %>% abs()) %>%
  filter(Datum_verschil > 60)
probleem(ezd_fout$Case_number, "Melddatum", "Meldingsdatum opvallend lang (> 60d) na of voor EZD")

# rabies excluderen; die heeft als 't goed is geen EZD
ezd_missing = cases %>%
  filter(is.na(Date_of_onset), Infection != "Rabies")
probleem(ezd_missing$Case_number, "EZD", "Geen EZD opgegeven*")

md_missing = cases %>%
  filter(is.na(Datum_melding_aan_de_ggd))
probleem(md_missing$Case_number, "Melddatum", "Geen melddatum opgegeven")

zh_missing = cases %>%
  filter(is.na(Hospitalised))
probleem(zh_missing$Case_number, "Ziekenhuisopname", "Geen ziekenhuisopname opgegeven")

status_missing = cases %>%
  filter(is.na(Status_van_de_melding))
probleem(status_missing$Case_number, "Status melding", "Geen status van de melding opgegeven")

# mogelijke mismatches:
# gefiatteerd, maar lege datum
# definitief, maar lege datum
# wel datum, geen status
status_osiris_mismatch_fiat = cases %>%
  filter(is.na(Datum_gefiatteerd_in_osiris), Status_van_de_melding %in% c("Gefiatteerd", "Definitief"))
probleem(status_osiris_mismatch_fiat$Case_number, "Datum gefiatteerd", "Geen datum gefiatteerd opgegeven, maar wel aangemerkt als gefiatteerd/definitief")
status_osiris_mismatch_definitief = cases %>%
  filter(is.na(Datum_definitief_in_osiris), Status_van_de_melding == "Definitief")
probleem(status_osiris_mismatch_definitief$Case_number, "Datum definitief", "Geen datum definitief opgegeven, maar wel aangemerkt als definitief")
status_osiris_mismatch_datum = cases %>%
  filter(!is.na(Datum_definitief_in_osiris) | !is.na(Datum_gefiatteerd_in_osiris), is.na(Status_van_de_melding))
probleem(status_osiris_mismatch_datum$Case_number, "Status melding", "Status melding is niet opgegeven, maar er is wel een datum definitief of gefiatteerd ingevuld")

osirisnr_missing = cases %>%
  filter(is.na(Osirisnummer), Confidence != "Discarded", Status_van_de_melding != "Niet meldingsplichtig", Status_van_de_melding != "Overgedragen")
probleem(osirisnr_missing$Case_number, "Osirisnummer", "Geen Osirisnummer opgegeven")

vacc_onlogisch = cases %>%
  filter((is.na(Vaccinated_in_respect_to_the_diagnosis) | Vaccinated_in_respect_to_the_diagnosis == "Nee") &
           !is.na(Vaccination_date))
probleem(vacc_onlogisch$Case_number, "Vaccinatiestatus", "Vaccinatiestatus mist of is 'Nee', maar er is wel een vaccinatiedatum opgegeven")

vacc_datum = cases %>%
  filter((!is.na(Vaccinated_in_respect_to_the_diagnosis) & Vaccinated_in_respect_to_the_diagnosis != "Nee" & Vaccinated_in_respect_to_the_diagnosis != "Onbekend") &
           is.na(Vaccination_date))
probleem(vacc_datum$Case_number, "Vaccinatiedatum", "Er is geen vaccinatiedatum opgegeven")

reis_mist = cases %>%
  filter(is.na(Recent_travel_to_another_country))
probleem(reis_mist$Case_number, "Reisgeschiedenis", "Reisgeschiedenis is niet ingevuld")

# registratie typering
# sinds oktober 2026 toegevoegd vanuit BRMS
# - CPE: Molecular Typing Data - Voorbeeld: OXA-048 
# - hep A: Molecular Typing Data - Voorbeeld: 1A of 1B 
# - Hib: Serogroup/Serotype (deze vinden wij niet relevant, en staat dus niet in het script)
# - iGAS: Molecular Typing Data
# - legionella: uitgebreider, zie onder
# - meningokokken: Serogroup
# - MRSA: Molecular Typing Data
# - pneumokokken: Molecular Typing Data - Voorbeeld: 4 of 9A of 19A of 23B
# - STEC: uitgebreider, zie onder
typering_mist = cases %>%
  filter((groep %in% c("CPE", "Hepatitis A", "Invasieve groep A-streptokokkeninfectie", "MRSA", "Invasieve pneumokokkenziekte") & is.na(Molecular_typing_data)) |
           (groep %in% c("Meningokokkenziekte") & is.na(Serogroup_serotype)))
probleem(typering_mist$Case_number, "Typering", "Typering niet ingevuld")

typering_fout = cases %>%
  filter(groep == "CPE", !str_detect(Molecular_typing_data, "^[a-zA-Z]+-\\d+"))
probleem(typering_fout$Case_number, "Typering", "Typering niet in juist formaat - correct voorbeeld: OXA-048")

typering_fout = cases %>%
  filter(groep == "Hepatitis A", !str_detect(Molecular_typing_data, "^\\d+[a-zA-Z]+"))
probleem(typering_fout$Case_number, "Typering", "Typering niet in juist formaat - correct voorbeeld: 1A of 1B")

typering_fout = cases %>%
  filter(groep == "Invasieve groep A-streptokokkeninfectie", !str_detect(Molecular_typing_data, "^\\d{1,2}\\.\\d{1,2}\\s(C|c)lus.+$"))
probleem(typering_fout$Case_number, "Typering", "Typering niet in juist formaat - correct voorbeeld: 4.0 Cluster EE of 87.0 Cluster E3")

typering_fout = cases %>%
  filter(groep == "Invasieve pneumokokkenziekte", !str_detect(Molecular_typing_data, "^\\d+[a-zA-Z]?$"))
probleem(typering_fout$Case_number, "Typering", "Typering niet in juist formaat - correct voorbeeld: 4 of 9A of 19A of 23B")

# voor legionella even aparte logica:
# - Organism Species bepaalt of Serogroup beschikbaar is
# - indien niet beschikbaar, dan Molecular Typing Data
# - Molecular Typing Data kan ook de sequence type bevatten, bijvoorbeeld ST:[spatie]2441 of ST:[spatie]93
typering_mist = cases %>%
  filter(groep == "Legionellose" & is.na(Organism_species))
probleem(typering_mist$Case_number, "Typering", "Organisme niet ingevuld")

typering_mist = cases %>%
  filter(groep == "Legionellose" & is.na(Serogroup_serotype))
probleem(typering_mist$Case_number, "Typering", "Serogroep niet ingevuld")

typering_mist = cases %>%
  filter(groep == "Legionellose" & is.na(Molecular_typing_data) & is.na(Serogroup_serotype))
probleem(typering_mist$Case_number, "Typering", "Serogroep niet ingevuld en molecular typing data niet ingevuld")

# STEC ook apart:
# - Serogroup invullen
# - Toxin invullen
typering_mist = cases %>%
  filter(groep == "STEC/Enterohemorragische E.coli-infectie" & is.na(Serogroup_serotype))
probleem(typering_mist$Case_number, "Typering", "Serogroep niet ingevuld")

typering_mist = cases %>%
  filter(groep == "STEC/Enterohemorragische E.coli-infectie" & is.na(Toxin))
probleem(typering_mist$Case_number, "Typering", "Toxine niet ingevuld")


# nu per medewerker een lijst genereren
# om het makkelijk te maken geeft HPZone ze niet consistent weer; het kan [voornaam achternaam] zijn, of [achternaam, voornaam]
# herschrijven!
cases$Investigating_officer = str_replace(cases$Investigating_officer, "([^,]+), (.*)", "\\2 \\1")

ranking = cases %>%
  group_by(Investigating_officer) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

ranking_inf = cases %>%
  filter(!is.na(inf)) %>%
  group_by(inf) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

template = readLines("datacontrole_template.html")
# variabelen: naam, aantal_cases, aantal_cases_mdw, num_mdw, num_mdw_totaal, aantal_fouten_totaal, aantal_fouten_mdw, fouten
for (io in unique(cases$Investigating_officer)) {
  cases_mdw = cases %>% filter(Investigating_officer == io)
  ranking_inf_mdw = cases_mdw %>%
    group_by(inf) %>%
    summarize(n=n()) %>%
    arrange(desc(n)) %>%
    mutate(perc=n/sum(n)*100)
  problemen_mdw = problemen %>%
    filter(ID %in% cases_mdw$Case_number) %>%
    arrange(ID)
  # basiselementen
  replacements = c("naam"=str_extract(io, "[^ ]+"),
                   "aantal_cases"=nrow(cases),
                   "aantal_cases_mdw"=nrow(cases_mdw),
                   "perc_cases_meldplicht"=sprintf("%.0f%%", sum(cases_mdw$Status_van_de_melding != "Niet meldingsplichtig", na.rm=T)/nrow(cases_mdw)*100),
                   "num_mdw"=which(ranking$Investigating_officer == io),
                   "num_mdw_totaal"=nrow(ranking),
                   "topinf"=ranking_inf$inf[1],
                   "topinf_n"=ranking_inf$n[1],
                   "topinf_mdw"=ranking_inf_mdw$inf[1],
                   "topinf_n_mdw"=ifelse(ranking_inf$inf[1] %in% ranking_inf_mdw$inf, ranking_inf_mdw$n[ranking_inf_mdw$inf == ranking_inf$inf[1]], 0),
                   "perc_topinf"=sprintf("%.0f%%", ranking_inf_mdw$n[1] / ranking_inf$n[ranking_inf$inf == ranking_inf_mdw$inf[1]] * 100),
                   "aantal_fouten_totaal"=nrow(problemen),
                   "aantal_fouten_mdw"=nrow(problemen_mdw),
                   "aantal_cases_fouten"=length(unique(problemen_mdw$ID)),
                   "fouten"="test",
                   "mdw_inf"="test",
                   "datum"=strftime(Sys.Date(), "%d-%m-%Y"),
                   "begindatum"=strftime(startdatum, "%d-%m-%Y"),
                   "einddatum"=strftime(einddatum, "%d-%m-%Y"),
                   "logo"=image_uri("logo_nog.png"))
  replacements["mdw_inf"] = paste0("<tr><td>", ranking_inf_mdw$inf, "</td><td>", ranking_inf_mdw$n, "</td><td>", round(ranking_inf_mdw$perc), "%</td></tr>") %>%
    str_c(collapse="\r\n")
  # foutentabel?
  if (nrow(problemen_mdw) > 0) {
    foutentabel = "<table id=\"fouten\"><colgroup><col /></colgroup>\r\n"
    foutentabel = paste0(foutentabel, "<tr><th>HPZone ID</th><th>Veld</th><th>Probleem</th></tr>\r\n")
    for (i in 1:nrow(problemen_mdw)) {
      foutentabel = paste0(foutentabel, "<tr><td>", problemen_mdw$ID[i], "</td><td>", problemen_mdw$veld[i], "</td><td>", problemen_mdw$melding[i], "</td></tr>\r\n")
    }
    foutentabel = paste0(foutentabel, "</table>\r\n")
    if (any(str_detect(problemen_mdw$melding, fixed("*")))) {
      foutentabel = paste0(foutentabel, "<br /><span style=\"font-style: italic;\">* Let op: een ontbrekende EZD kan in sommige gevallen correct zijn. Er is vanuit de data geen onderscheid te maken tussen 'Nvt' en een leeg veld.</span><br />\r\n")
    }
  } else {
    foutentabel = "Er zijn deze maand geen problemen gevonden! Goed gedaan!"
  }
  replacements["fouten"] = foutentabel
  replacements = setNames(replacements, paste0("\\{", names(replacements), "\\}"))
  write(template %>% str_replace_all(replacements), "test.html")
  
  mdw_lookup = str_detect(io, mdw$zoekterm)
  if (sum(mdw_lookup) != 1) {
    printf("Let op! Medewerker %s niet gevonden - resultaten niet gemaild.", io)
    outlook$create_email(subject="Medewerker niet gevonden")$
      set_recipients(to="epi@ggd.nl")$
      set_body(sprintf("Let op! Medewerker %s niet gevonden. Casuistiekcontrole niet gemaild.", io))$
      send()
  } else {
    outlook$create_email(subject=sprintf("Casuïstiekcontrole %s", strftime(startdatum, "%B %Y")), content_type="html")$
      set_recipients(to=mdw$email[mdw_lookup])$
      set_body(template %>% str_replace_all(replacements))$
      send()
  }
}

dbDisconnect(conn)

