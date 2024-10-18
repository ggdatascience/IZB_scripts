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
library(this.path)
library(readxl)
library(DBI)
library(odbc)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

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
                   TRUE ~ paste0("'", str_replace_all(val, fixed("'"), "''"), "'")))
}

# alles wrappen in een blok, zodat er bij fouten gestopt wordt
{
  # doorlopen bestanden in de datamap
  files = list.files("./data/", pattern=".*?\\.xlsx")
  
  for (file in files) {
    printf("Verwerking bestand %d: %s", which(files == file), file)
    # enquiries worden soms gek geëxporteerd, waardoor R ze niet kan openen
    # er wordt dan geklaagd over iets van een unicodekarater
    # openen en opslaan in Excel lost dit op, maar is irritant; automatiseren!
    tryCatch({ data = read_excel(paste0("./data/", file)) }, error=function(e) { 
      if (str_detect(e$message, "Unicode")) {
        printf("Let op! Bestand %s kon niet geopend worden. Er wordt een poging gedaan om het bestand automatisch opnieuw op te slaan met Excel.", file)
        filename = paste0(dirname(this.path()), "/data/", file) %>%
          str_replace_all(fixed("/"), "\\") # het omzetscript wil een pad met \, R geeft /
        system(sprintf("excelsave.bat \"%s\" ", filename))
        Sys.sleep(2)
        printf("Poging 2 wordt gestart. Mocht er zo een foutmelding verschijnen (Column `mdw_id` doesn't exist.), voer dan het script opnieuw uit.")
        # vanuit deze functie moeten we assign gebruiken, omdat anders de variabele data alleen lokaal wordt overschreven
        assign("data", read_excel(paste0("./data/", file)), envir=.GlobalEnv)
      }
      else
        print(e)
    })
    
    # om te controleren of er geen gekke data aanwezig is willen we weten of 
    # data in dit bestand voor hun invoerdatum zijn (dat kan, muv HepB, niet)
    if ("Time entered" %in% colnames(data))
      startdatum = min(data$`Time entered`, na.rm=T)
    else if ("Received on" %in% colnames(data))
      startdatum = min(data$`Received on`, na.rm=T)
    else
      startdatum = Sys.Date()
    jaar = year(startdatum)
    
    # er zijn, als het goed is, drie soorten bestanden:
    # - cases -> te herkennen aan 'Case Number' en 'Time entered'
    # - situations -> te herkennen aan 'Identification Number (Internal)' en 'Time entered'
    # - enquiries -> te herkennen aan 'Number' en 'Received on'
    if ("Case Number" %in% colnames(data) && "Time entered" %in% colnames(data)) {
      # eerst zorgen dat alle variabelen bekend zijn
      # dit zijn: context, medewerkers, diagnose, infectie
      
      # medewerkers
      medewerkers.hpzone = c(data$`Case Manager`, data$`Investigating Officer`) %>% unique()
      medewerkers.hpzone = medewerkers.hpzone[!is.na(medewerkers.hpzone)]
      medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
      
      medewerkers.missend = medewerkers.hpzone[!medewerkers.hpzone %in% medewerkers.db$mdw_naam]
      if (length(medewerkers.missend) > 0)
        dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_medewerker](mdw_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, medewerkers.missend)), collapse=", ")))
      
      medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
      
      # context
      context.hpzone = unique(data$`Principal Contextual Setting`)
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
        left_join(context.db, by=c("Principal Contextual Setting"="ctx_naam")) %>%
        left_join(infec.db, by=c("Infection"="inf_naam")) %>%
        left_join(diag.db, by=c("Diagnosis"="dx_naam")) %>%
        left_join(agents.db, by=c("Agent"="ag_naam")) %>%
        left_join(medewerkers.db, by=c("Case Manager"="mdw_naam")) %>%
        rename(casemdw_id=mdw_id) %>%
        left_join(medewerkers.db, by=c("Investigating Officer"="mdw_naam")) %>%
        mutate(Geslacht=case_when(Gender == "Female" ~ "F",
                                  Gender == "Male" ~ "M",
                                  Gender == "Other" ~ "X",
                                  TRUE ~ "U"), # wellicht ooit nog een anders toevoegen?
               PC=str_match(ifelse(!is.na(Postcode), Postcode, `Post District`), "\\d+")[,1],
               Datum.OSIRIS=case_when(!is.na(`Datum gefiatteerd in Osiris`) ~ `Datum gefiatteerd in Osiris`,
                                      !is.na(`Datum definitief in Osiris`) ~ `Datum gefiatteerd in Osiris`),
               Peildatum=case_when(!is.na(`Date of Onset`) ~ `Date of Onset`,
                                   !is.na(`Datum melding aan de GGD`) ~ `Datum melding aan de GGD`,
                                   TRUE ~ `Time entered`),
               Melddatum=case_when(!is.na(`Datum melding aan de GGD`) ~ `Datum melding aan de GGD`,
                                   TRUE ~ `Time entered`),
               Datum.verschil=abs(difftime(`Datum melding aan de GGD`, `Date of Onset`, units="days")),
               # missende geboortedatum wordt aangegeven met datum 01-01-1900, dus als de leeftijd gelijk is aan (rapportjaar) - 1900 is deze eigenlijk missend
               `Age in Years (at date of onset)`=as.numeric(`Age in Years (at date of onset)`),
               `Age in Years (at date of onset)`=ifelse(`Age in Years (at date of onset)` >= year(Peildatum) - 1900, NA, `Age in Years (at date of onset)`))
      
      # een aantal sanity checks
      printf("Er zijn %d casussen met een verschil van meer dan 365 dagen tussen eerste ziektedag en meldingsdatum. Dit zijn HPZone ID's %s.",
             sum(data$Datum.verschil > 365, na.rm=T),
             str_c(sprintf("%s (%d dagen)", data$`Case Number`[which(data$Datum.verschil > 365)], data$Datum.verschil[which(data$Datum.verschil > 365)]), collapse=", "))
      printf("Er zijn %d casussen waarbij de eerste ziektedag na de meldingsdatum valt. Dit zijn HPZone ID's %s.",
             sum(data$`Date of Onset` > data$`Datum melding aan de GGD`, na.rm=T), str_c(data$`Case Number`[which(data$`Date of Onset` > data$`Datum melding aan de GGD`)], collapse=", "))
      printf("Er zijn %d casussen met gekke meldingsdata (<= jaar databestand of > nu). Dit zijn HPZone ID's %s.",
             sum(data$`Datum melding aan de GGD` < jaar | data$`Datum melding aan de GGD` > Sys.Date(), na.rm=T),
             str_c(data$`Case Number`[which(data$`Datum melding aan de GGD` < jaar | data$`Datum melding aan de GGD` > Sys.Date())], collapse=", "))
      printf("Er zijn %d casussen met gekke EZD's (<= eerste melding of > nu). Dit zijn HPZone ID's %s.",
             sum(data$`Date of Onset` < jaar | data$`Date of Onset` > Sys.Date(), na.rm=T),
             str_c(data$`Case Number`[which(data$`Date of Onset` < jaar | data$`Date of Onset` > Sys.Date())], collapse=", "))
      printf("Er zijn %d casussen zonder melddatum. Invoerdatum aagenomen als melddatum. Dit zijn HPZone ID's %s.",
             sum(is.na(data$`Datum melding aan de GGD`), na.rm=T),
             str_c(data$`Case Number`[which(is.na(data$`Datum melding aan de GGD`))], collapse=", "))
      printf("Er zijn %d casussen zonder context. Dit zijn HPZone ID's %s. Hier wordt context Onbekend (%d) aangenomen.",
             sum(is.na(data$ctx_id)), str_c(data$`Case Number`[is.na(data$ctx_id)], collapse=", "), context.db$ctx_id[context.db$ctx_naam == "Onbekend"])
      printf("Er zijn %d casussen zonder opgegeven ziekenhuisopname. Dit zijn HPZone ID's %s. Hier wordt 'Not known' aangenomen.",
             sum(is.na(data$Hospitalised)), str_c(data$`Case Number`[is.na(data$Hospitalised)], collapse=", "))
      printf("Er zijn %d casussen zonder meldingsstatus in OSIRIS, en %d casussen zonder melddatum in OSIRIS. Hiervan overlappen %d gevallen.",
             sum(is.na(data$`Status van de melding`)), sum(is.na(data$Datum.OSIRIS)), sum(is.na(data$`Status van de melding`) & is.na(data$Datum.OSIRIS)))
      printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen datum is opgegeven. Dit zijn HPZone ID's %s. Melddatum wordt aangenomen.",
             sum(!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)), str_c(data$`Case Number`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)], collapse=", "))
      printf("Er zijn %d casussen waarbij wel een meldingsnummer is aangemaakt, maar geen status van de melding is opgegeven. Dit zijn HPZone ID's %s. Status wordt aangenomen als 'Gefiatteerd'.",
             sum(!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Status van de melding`)), str_c(data$`Case Number`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Status van de melding`)], collapse=", "))
      printf("Er zijn %d casussen waarbij wel een vaccinatiedatum is opgegeven, maar geen of negatieve vaccinatiestatus. Dit zijn HPZone ID's %s.",
             sum((is.na(data$`Vaccinated in respect to the diagnosis`) | data$`Vaccinated in respect to the diagnosis` == "Nee") & !is.na(data$`Vaccination Date (if relevant)`), na.rm=T), str_c(data$`Case Number`[which((is.na(data$`Vaccinated in respect to the diagnosis`) | data$`Vaccinated in respect to the diagnosis` == "Nee") & !is.na(data$`Vaccination Date (if relevant)`))], collapse=", "))
      printf("Let op! Al deze gevallen worden alsnog geïmporteerd. Controleer de gegevens in HPZone en voer na correctie een nieuwe export in via dit script.")
      
      data$ctx_id[is.na(data$ctx_id)] = context.db$ctx_id[context.db$ctx_naam == "Onbekend"]
      data$Hospitalised = case_match(data$Hospitalised, "Yes" ~ 1, "No" ~ 0, .default=NA)
      data$`Datum gefiatteerd in Osiris`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)] = data$`Time entered`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)]
      data$`Status van de melding`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Status van de melding`)] = "Gefiatteerd"
      data$`Vaccinated in respect to the diagnosis`[!is.na(data$`Vaccination Date (if relevant)`) & (is.na(data$`Vaccinated in respect to the diagnosis`) | data$`Vaccinated in respect to the diagnosis` == "Nee")] = "Ja"
      
      # het kan zijn dat een bepaald geval al voorkomt in de database, in dat geval moeten we updaten
      # dit breken we op in lijsten van 500, zodat de database niet helemaal hoeft te huilen
      hpzone_ids = unique(data$`Case Number`)
      existing = c()
      for (i in seq(1, length(hpzone_ids), 500)) {
        result = dbGetQuery(conn, paste0("SELECT hpzone_id FROM [dbo].[casus] WHERE hpzone_id IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
        existing = c(existing, as.character(result$hpzone_id))
      }
      if (length(existing) > 0) {
        data.existing = data[data$`Case Number` %in% existing,]
        update = sprintf("UPDATE [dbo].[casus]
               SET peildatum='%s', melddatum='%s', meldorganisatie=%s, geslacht='%s', leeftijd=%s, postcode=%s, agent=%s, infectie=%s, diagnose=%d, diagnosezekerheid=%s,
               buitenland=%s, eersteziektedag=%s, context=%d, ziekenhuisopname=%s, overlijden=%s, vaccinatie=%s, vaccinatiedatum=%s, gemeld=%s, statusmelding=%s, medewerker=%s,
               casemanager=%s, antibioticaresistentie=%s, updated=GETDATE()
               WHERE hpzone_id=%s", data.existing$Peildatum, data.existing$Melddatum, dbQuoteString(conn, data.existing$`Oorspronkelijke bron van de melding`),
                         data.existing$Geslacht, NullOrQuotes(data.existing$`Age in Years (at date of onset)`), NullOrQuotes(data.existing$PC), NullOrQuotes(data.existing$ag_id), NullOrQuotes(data.existing$inf_id),
                         data.existing$dx_id, dbQuoteString(conn, data.existing$Confidence), NullOrQuotes(data.existing$`Recent travel to another country`), NullOrQuotes(data.existing$`Date of Onset`),
                         data.existing$ctx_id, NullOrQuotes(data.existing$Hospitalised),
                         NullOrQuotes(data.existing$`Date of death (where appropriate)`), NullOrQuotes(data.existing$`Vaccinated in respect to the diagnosis`),
                         NullOrQuotes(data.existing$`Vaccination Date (if relevant)`),
                         NullOrQuotes(data.existing$Datum.OSIRIS), NullOrQuotes(data.existing$`Status van de melding`),
                         NullOrQuotes(data.existing$mdw_id), NullOrQuotes(data.existing$casemdw_id), NullOrQuotes(data.existing$ABR), data.existing$`Case Number`)
        
        for (i in seq(1, length(update), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
          dbCommit(conn)
          
          printf("%d rijen aangepast.", length(i:min(length(update),i+499)))
        }
      }
      data.insert = data[!data$`Case Number` %in% existing,]
      insert = sprintf("(%s, '%s', '%s', %s, '%s', %s, %s, %s, %s, %d, %s, %s, %s, %s, %d, %s, %s, %s, %s, %s, %s, %s, %s, GETDATE())", data.insert$`Case Number`, data.insert$Peildatum, data.insert$Melddatum, dbQuoteString(conn, data.insert$`Oorspronkelijke bron van de melding`),
                       data.insert$Geslacht, NullOrQuotes(data.insert$`Age in Years (at date of onset)`), NullOrQuotes(data.insert$PC), NullOrQuotes(data.insert$ag_id), NullOrQuotes(data.insert$inf_id), data.insert$dx_id,
                       dbQuoteString(conn, data.insert$Confidence), NullOrQuotes(data.insert$ABR), NullOrQuotes(data.insert$`Recent travel to another country`), NullOrQuotes(data.insert$`Date of Onset`),
                       data.insert$ctx_id, NullOrQuotes(data.insert$Hospitalised),
                       NullOrQuotes(data.insert$`Date of death (where appropriate)`), NullOrQuotes(data.insert$`Vaccinated in respect to the diagnosis`),
                       NullOrQuotes(data.insert$`Vaccination Date (if relevant)`),
                       NullOrQuotes(data.insert$Datum.OSIRIS), NullOrQuotes(data.insert$`Status van de melding`),
                       NullOrQuotes(data.insert$mdw_id), NullOrQuotes(data.insert$casemdw_id))
      
      if (length(insert) > 0) {
        # opbreken in blokjes van 500, zodat niet alles direct pijn doet
        for (i in seq(1, length(insert), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, paste0("INSERT INTO casus(hpzone_id, peildatum, melddatum, meldorganisatie, geslacht, leeftijd, postcode, agent, infectie, diagnose,
               diagnosezekerheid, antibioticaresistentie, buitenland, eersteziektedag, context, ziekenhuisopname, overlijden, vaccinatie,
                                       vaccinatiedatum, gemeld, statusmelding, medewerker, casemanager, created) VALUES", str_c(insert[i:min(length(insert),i+499)], collapse=", ")))
          dbCommit(conn)
          printf("%d rijen toegevoegd.", res)
        }
      }
    } else if ("Time entered" %in% colnames(data) && "Identification Number (Internal)" %in% colnames(data)) {
      # situations
      # relevante velden:
      # type, scenario (vullen met infectious agent), confidence. (met punt!), risk level, postcode, princ. context,
      # gemeld in OSIRIS, OSIRISnummer, manager, invest. officer, time entered, artikel 26,
      # number potentially at risk, number of symptomatic cases, number hospitalized, number of fatalities
      
      # eerst controleren of de context, scenario en infectious agent al bekend zijn
      
      # medewerkers
      medewerkers.hpzone = c(data$Manager, data$`Investigating Officer`) %>% unique()
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
      context.hpzone = unique(data$`Principal Contextual Setting`)
      context.hpzone = context.hpzone[!is.na(context.hpzone)]
      context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")
      
      context.missend = context.hpzone[!context.hpzone %in% context.db$ctx_naam]
      if (length(context.missend) > 0)
        dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_context](ctx_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, context.missend)), collapse=", ")))
      
      context.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_context]")
      
      # sanity checks
      printf("Er zijn %d gevallen waarbij 'gemeld in OSIRIS' op 'Yes' staat, maar waar geen meldingsnummer is toegevoegd. Dit zijn gevallen %s.",
             sum(data$`Gemeld in Osiris` == "Yes" & is.na(data$Osirisnummer)), str_c(data$`Identification Number (Internal)`[data$`Gemeld in Osiris` == "Yes" & is.na(data$Osirisnummer)], collapse=", "))
      printf("Er zijn %d gevallen waarbij 'gemeld in OSIRIS' op 'No' staat, maar waar wel een meldingsnummer is toegevoegd. Dit zijn gevallen %s.",
             sum(data$`Gemeld in Osiris` == "No" & !is.na(data$Osirisnummer)), str_c(data$`Identification Number (Internal)`[data$`Gemeld in Osiris` == "No" & !is.na(data$Osirisnummer)], collapse=", "))
      
      data = data %>%
        left_join(context.db, by=c("Principal Contextual Setting"="ctx_naam")) %>%
        left_join(agents.db, by=c("Infectious Agent"="ag_naam")) %>%
        left_join(scenario.db, by=c("Scenario"="sc_naam")) %>%
        left_join(medewerkers.db, by=c("Manager"="mdw_naam")) %>%
        rename(manager_id=mdw_id) %>%
        left_join(medewerkers.db, by=c("Investigating Officer"="mdw_naam")) %>%
        mutate(PC=str_extract(Postcode, "\\d+"),
               Melding.OSIRIS=case_when(!is.na(`Gemeld in Osiris`) ~ `Gemeld in Osiris`,
                                      !is.na(Osirisnummer) ~ "Yes"),
               Peildatum=case_when(!is.na(`Start Date`) ~ `Start Date`,
                                   TRUE ~ `Time entered`))
      
      # kijken welke records al bestaan
      hpzone_ids = unique(data$`Identification Number (Internal)`)
      existing = c()
      for (i in seq(1, length(hpzone_ids), 500)) {
        result = dbGetQuery(conn, paste0("SELECT hpzone_id FROM [dbo].[situatie] WHERE hpzone_id IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
        existing = c(existing, as.character(result$hpzone_id))
      }
      
      #test = dbGetQuery(conn, "SELECT * FROM [dbo].[situatie]")
      
      if (length(existing) > 0) {
        data.existing = data[data$`Identification Number (Internal)` %in% existing,]
        update = sprintf("UPDATE [dbo].[situatie]
               SET datum='%s', invoerdatum='%s', status='%s', type='%s', agent=%s, scenario=%s, zekerheid=%s,
                risiconiveau=%s, artikel26=%s, context=%s, postcode=%s, melding='%s', medewerker=%s, manager=%s,
                aantal_symptomatisch=%s, aantal_risico=%s, aantal_ziekenhuis=%s, aantal_overleden=%s, updated=GETDATE()
               WHERE hpzone_id=%s", data.existing$Peildatum, data.existing$`Time entered`, data.existing$Status, data.existing$Type, NullOrQuotes(data.existing$ag_id), NullOrQuotes(data.existing$sc_id),
                         NullOrQuotes(data.existing$Confidence.), NullOrQuotes(data.existing$`Risk Level`), ifelse(data.existing$`Artikel 26` == "Yes", 1, 0), NullOrQuotes(data.existing$ctx_id),
                         NullOrQuotes(data.existing$PC), ifelse(data.existing$Melding.OSIRIS == "Yes", 1, 0),
                         NullOrQuotes(data.existing$mdw_id), NullOrQuotes(data.existing$manager_id),
                         NullOrQuotes(data.existing$`Number of symptomatic cases`), NullOrQuotes(data.existing$`Number potentially at risk`),
                         NullOrQuotes(data.existing$`Number hospitalised`), NullOrQuotes(data.existing$`Number of fatalities`),
                         data.existing$`Identification Number (Internal)`)
        
        for (i in seq(1, length(update), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
          dbCommit(conn)
          
          printf("%d rijen aangepast.", length(i:min(length(update),i+499)))
        }
      }
      data.insert = data[!data$`Identification Number (Internal)` %in% existing,]
      insert = sprintf("('%s', '%s', '%s', '%s', '%s', %s, %s,
                %s, %s, %s, %s, %s, %s, %s,
                %s, %s, %s, %s, %s, GETDATE())", data.insert$`Identification Number (Internal)`, data.insert$Peildatum, data.insert$`Time entered`, data.insert$Status, data.insert$Type, NullOrQuotes(data.insert$ag_id), NullOrQuotes(data.insert$sc_id),
                       NullOrQuotes(data.insert$Confidence.), NullOrQuotes(data.insert$`Risk Level`), ifelse(data.insert$`Artikel 26` == "Yes", 1, 0), NullOrQuotes(data.insert$ctx_id),
                       NullOrQuotes(data.insert$PC), ifelse(data.insert$Melding.OSIRIS == "Yes", 1, 0),
                       NullOrQuotes(data.insert$mdw_id), NullOrQuotes(data.insert$manager_id),
                       NullOrQuotes(data.insert$`Number of symptomatic cases`), NullOrQuotes(data.insert$`Number potentially at risk`),
                       NullOrQuotes(data.insert$`Number hospitalised`), NullOrQuotes(data.insert$`Number of fatalities`))
      
      if (length(insert) > 0) {
        # opbreken in blokjes van 500, zodat niet alles direct pijn doet
        for (i in seq(1, length(insert), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, paste0("INSERT INTO situatie(hpzone_id, datum, invoerdatum, status, type, agent, scenario, zekerheid,
                risiconiveau, artikel26, context, postcode, melding, medewerker, manager,
                aantal_symptomatisch, aantal_risico, aantal_ziekenhuis, aantal_overleden, created) VALUES", str_c(insert[i:min(length(insert),i+499)], collapse=", ")))
          dbCommit(conn)
          printf("%d rijen toegevoegd.", res)
        }
      }
    } else if ("Number" %in% colnames(data) && "Received on" %in% colnames(data)) {
      # enquiries
      # eerst zorgen dat alle variabelen bekend zijn
      # dit zijn: medewerker (2x), onderwerp
      
      # medewerkers
      medewerkers.hpzone = c(data$`Originally taken by`, data$`Handled by`) %>% unique()
      medewerkers.hpzone = medewerkers.hpzone[!is.na(medewerkers.hpzone)]
      medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
      
      medewerkers.missend = medewerkers.hpzone[!medewerkers.hpzone %in% medewerkers.db$mdw_naam]
      if (length(medewerkers.missend) > 0)
        dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_medewerker](mdw_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, medewerkers.missend)), collapse=", ")))
      
      medewerkers.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_medewerker]")
      
      # onderwerp
      onderwerp.hpzone = unique(data$`Specific Topic`)
      onderwerp.hpzone = onderwerp.hpzone[!is.na(onderwerp.hpzone)]
      onderwerp.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_onderwerp]")
      
      onderwerp.missend = onderwerp.hpzone[!onderwerp.hpzone %in% onderwerp.db$ond_naam]
      if (length(onderwerp.missend) > 0)
        dbExecute(conn, paste0("INSERT INTO [dbo].[hpz_onderwerp](ond_naam) VALUES", str_c(sprintf("(%s)", dbQuoteString(conn, onderwerp.missend)), collapse=", ")))
      
      onderwerp.db = dbGetQuery(conn, "SELECT * FROM [dbo].[hpz_onderwerp]")
      
      # sanity checks
      printf("Er zijn %d gevallen waarbij de sluitingsdatum voor de ontvangstdatum staat. Dit zijn gevallen %s.",
             sum(data$`Received on` > data$`Date closed`), str_c(data$Number[data$`Received on` > data$`Date closed`], collapse=", "))
      printf("Er zijn %d gevallen zonder broad topic. Dit zijn gevallen %s.",
             sum(is.na(data$`Broad Topic`)), str_c(data$Number[is.na(data$`Broad Topic`)], collapse=", "))
      printf("Er zijn %d gevallen zonder specific topic. Dit zijn gevallen %s.",
             sum(is.na(data$`Specific Topic`)), str_c(data$Number[is.na(data$`Specific Topic`)], collapse=", "))
      
      data = data %>%
        left_join(onderwerp.db, by=c("Specific Topic"="ond_naam")) %>%
        left_join(medewerkers.db, by=c("Originally taken by"="mdw_naam")) %>%
        rename(ontvanger_id=mdw_id) %>%
        left_join(medewerkers.db, by=c("Handled by"="mdw_naam")) %>%
        mutate(PC=str_extract(`Caller's Post District`, "\\d+"),
               Geslacht=case_when(Gender == "Female" ~ "F",
                                  Gender == "Male" ~ "M",
                                  TRUE ~ "U")) # wellicht ooit nog een anders toevoegen?)
      
      # kijken welke records al bestaan
      hpzone_ids = unique(data$Number)
      existing = c()
      for (i in seq(1, length(hpzone_ids), 500)) {
        result = dbGetQuery(conn, paste0("SELECT hpzone_id FROM [dbo].[vraag] WHERE hpzone_id IN (", str_c(hpzone_ids[i:min(length(hpzone_ids),i+500)], collapse=", "), ")"))
        existing = c(existing, as.character(result$hpzone_id))
      }
      
      if (length(existing) > 0) {
        data.existing = data[data$Number %in% existing,]
        update = sprintf("UPDATE [dbo].[vraag]
               SET startdatum='%s', einddatum=%s, ontvanger=%s, medewerker=%s, status=%s, postcode=%s, geslacht='%s', typebeller=%s,
               categorie=%s, onderwerp=%s, onderwerpopen=%s, 
               updated=GETDATE()
               WHERE hpzone_id=%s", data.existing$`Received on`, NullOrQuotes(data.existing$`Date closed`), data.existing$ontvanger_id, data.existing$mdw_id,
                         NullOrQuotes(data.existing$Status), NullOrQuotes(data.existing$PC), data.existing$Geslacht, NullOrQuotes(data.existing$`Type of Caller`),
                         NullOrQuotes(data.existing$`Broad Topic`), NullOrQuotes(data.existing$ond_id), NullOrQuotes(data.existing$`Additional Topic`),
                         data.existing$Number)
        
        for (i in seq(1, length(update), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, str_c(update[i:min(length(update),i+499)], collapse=";\n"))
          dbCommit(conn)
          
          printf("%d rijen aangepast.", length(i:min(length(update),i+499)))
        }
      }
      data.insert = data[!data$Number %in% existing,]
      insert = sprintf("(%s, '%s', %s, %s, %s, %s, %s, '%s', %s, %s, %s, %s, GETDATE())",
                       data.insert$Number, data.insert$`Received on`, NullOrQuotes(data.insert$`Date closed`),
                       data.insert$ontvanger_id, data.insert$mdw_id, NullOrQuotes(data.insert$Status), NullOrQuotes(data.insert$PC),
                       data.insert$Geslacht, NullOrQuotes(data.insert$`Type of Caller`), NullOrQuotes(data.insert$`Broad Topic`),
                       NullOrQuotes(data.insert$ond_id), NullOrQuotes(data.insert$`Additional Topic`))
      
      if (length(insert) > 0) {
        # opbreken in blokjes van 500, zodat niet alles direct pijn doet
        for (i in seq(1, length(insert), 500)) {
          dbBegin(conn)
          res = dbExecute(conn, paste0("INSERT INTO vraag(hpzone_id, startdatum, einddatum, ontvanger, medewerker, status, postcode, geslacht, typebeller, categorie, onderwerp,
      onderwerpopen, created) VALUES", str_c(insert[i:min(length(insert),i+499)], collapse=", ")))
          dbCommit(conn)
          printf("%d rijen toegevoegd.", res)
        }
      }
    }
    
    printf("Verwerking %s afgerond.", file)
  }
  
  printf("Omzetting naar bestanden voor het HSC dashboard wordt opgestart.")
  
  source("preparatie_HPZone_export.R")
  
  printf("Omzetting uitgevoerd.")
}

dbDisconnect(conn)