################################################################################
# Dit script loopt door de exports van HPZone heen en verwijdert persoonlijke  #
# informatie, zodat de bestanden veilig gedeeld kunnen worden voor het         #
# bovenregionale dashboard.                                                    #
#                                                                              #
# Gebruik is simpel:                                                           #
# - Exporteer de cases, situations en enquiries van het afgelopen jaar vanuit  #
#   HPZone met de opties 'All core values' en 'Excel' aangevinkt.              #
# - Plaats de drie bestanden in dezelfde map als dit bestand, of de opgegeven  #
#   datamap.                                                                   #
# - Voer dit bestand uit.                                                      #
# - Verwijder, indien gewenst, de originele Excelbestanden.                    #
################################################################################

# configuratiemogelijkheden
# wel of niet de Excelbestanden verwijderen na afloop? (T voor ja, F voor nee)
bestanden_verwijderen_na_afloop = T
# map waar de bestanden in staan - "./" is de map waarin het script zich bevindt
# "../data" betekent één map omhoog en dan de map data
# LET OP: Windows werkt met een backward slash (C:\Gebruikers\Blabla), in R moet dit ofwel een forward slash zijn (C:/Gebruikers/Blabla)
# of verdubbeld worden (C:\\Gebruikers\\Blabla)
datamap = "./data/"
# map waar de bestanden geplaatst moeten worden
# LET OP: slash vs. backslash, gelijk aan opmerking hierboven
# zet op NA voor uitvoer in datamap
uitvoermap = "\\\\az-data1.ssc.local\\Apl_data\\DWH_Data\\Import\\NOG_HPZone\\"


## begin script

# benodigde packages installeren/laden
pkg_req = c("tidyverse", "this.path", "readxl")

for (pkg in pkg_req) {
  if (system.file(package = pkg) == "") {
    install.packages(pkg)
  }
}

library(tidyverse)
library(this.path)
library(readxl)

setwd(paste0(dirname(this.path()), datamap))

printf = function (...) cat(paste(sprintf(...),"\n"))

if (!is.na(uitvoermap) && !str_ends(uitvoermap, "\\\\") && !str_ends(uitvoermap, "/")) {
  uitvoermap = paste0(uitvoermap, "/")
}

# doorlopen bestanden in de datamap
files = list.files(datamap, pattern=".*?\\.xlsx")
handled_files = c()
output_files = c()

for (file in files) {
  printf("Verwerking bestand %d: %s", which(files == file), file)
  # enquiries worden soms gek geëxporteerd, waardoor R ze niet kan openen
  # er wordt dan geklaagd over iets van een unicodekarater
  # openen en opslaan in Excel lost dit op, maar is irritant; automatiseren!
  tryCatch({ data = read_excel(file) }, error=function(e) { 
    if (str_detect(e$message, "Unicode")) {
      filename = paste0(dirname(this.path()), "/", file) %>%
        str_replace_all(fixed("/"), "\\") # het omzetscript wil een pad met \, R geeft /
      system(sprintf("excelsave.bat \"%s\" ", filename))
      data = read_excel(paste0("./", file))
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
  
  # er zijn, als het goed is, drie soorten bestanden:
  # - cases -> te herkennen aan 'Case Number' en 'Time entered'
  # - situations -> te herkennen aan 'Identification Number (Internal)' en 'Time entered'
  # - enquiries -> te herkennen aan 'Number' en 'Received on'
  
  if ("Case Number" %in% colnames(data) && "Time entered" %in% colnames(data)) {
    # cases
    
    # om onbekende redenen verandert HPZone soms de namen van de diagnose
    # zo is er bijvoorbeeld "Hepatitis B, chronic" zichtbaar onder Reference > Clinical > Diagnoses, maar heet deze in de export "Chronic Hepatitis B"
    # en dan moeten we ook nog gsub gebruiken, omdat str_replace geen \\L ondersteunt... zwaar leven
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
      mutate(Datum.OSIRIS=case_when(!is.na(`Datum gefiatteerd in Osiris`) ~ `Datum gefiatteerd in Osiris`,
                                    !is.na(`Datum definitief in Osiris`) ~ `Datum gefiatteerd in Osiris`))
    
    # sanity checks
    printf("Er zijn %d casussen waarbij de eerste ziektedag na de meldingsdatum valt. Dit zijn HPZone ID's %s.",
           sum(data$`Date of Onset` > data$`Datum melding aan de GGD`, na.rm=T), str_c(data$`Case Number`[which(data$`Date of Onset` > data$`Datum melding aan de GGD`)], collapse=", "))
    printf("Er zijn %d casussen met gekke meldingsdata (> nu). Dit zijn HPZone ID's %s.",
           sum(data$`Datum melding aan de GGD` > Sys.Date(), na.rm=T),
           str_c(data$`Case Number`[which(data$`Datum melding aan de GGD` > Sys.Date())], collapse=", "))
    printf("Er zijn %d casussen met gekke EZD's (<= eerste melding of > nu). Dit zijn HPZone ID's %s.",
           sum(data$`Date of Onset` < startdatum | data$`Date of Onset` > Sys.Date(), na.rm=T),
           str_c(data$`Case Number`[which(data$`Date of Onset` < startdatum | data$`Date of Onset` > Sys.Date())], collapse=", "))
    printf("Er zijn %d casussen zonder melddatum. Invoerdatum aagenomen als melddatum. Dit zijn HPZone ID's %s.",
           sum(is.na(data$`Datum melding aan de GGD`), na.rm=T),
           str_c(data$`Case Number`[which(is.na(data$`Datum melding aan de GGD`))], collapse=", "))
    printf("Er zijn %d casussen zonder context. Dit zijn HPZone ID's %s. Hier wordt context Onbekend aangenomen.",
           sum(is.na(data$`Principal Contextual Setting`)), str_c(data$`Case Number`[is.na(data$`Principal Contextual Setting`)], collapse=", "))
    printf("Er zijn %d casussen zonder opgegeven ziekenhuisopname. Dit zijn HPZone ID's %s. Hier wordt 'Nee' aangenomen.",
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
    
    data$`Principal Contextual Setting`[is.na(data$`Principal Contextual Setting`)] = "Onbekend"
    data$`Datum gefiatteerd in Osiris`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)] = data$`Time entered`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Datum gefiatteerd in Osiris`)]
    data$`Status van de melding`[!is.na(data$`Osiris Meldingsnummer`) & is.na(data$`Status van de melding`)] = "Gefiatteerd"
    data$`Vaccinated in respect to the diagnosis`[!is.na(data$`Vaccination Date (if relevant)`) & (is.na(data$`Vaccinated in respect to the diagnosis`) | data$`Vaccinated in respect to the diagnosis` == "Nee")] = "Ja"
    
    # omzetten naar gewenst formaat
    data = data %>%
      mutate(peildatum=case_when(!is.na(`Date of Onset`) ~ `Date of Onset`,
                                 !is.na(`Datum melding aan de GGD`) ~ `Datum melding aan de GGD`,
                                 TRUE ~ `Time entered`),
             melddatum=case_when(!is.na(`Datum melding aan de GGD`) ~ `Datum melding aan de GGD`,
                                 TRUE ~ `Time entered`),
             geslacht=case_when(Gender == "Female" ~ "F",
                                Gender == "Male" ~ "M",
                                TRUE ~ "U"),
             PC=ifelse(!is.na(Postcode), Postcode, `Post District`),
             ziekenhuisopname=case_when(Hospitalised == "Yes" ~ 1,
                                        Hospitalised == "No" ~ 0),
             gemeld=case_when(!is.na(`Datum gefiatteerd in Osiris`) ~ `Datum gefiatteerd in Osiris`,
                              !is.na(`Datum definitief in Osiris`) ~ `Datum definitief in Osiris`)) %>%
      rename(hpzone_id="Case Number", meldorganisatie="Oorspronkelijke bron van de melding",
             leeftijd="Age in Years (at date of onset)", agent="Agent", infectie="Infection",
             diagnose="Diagnosis", diagnosezekerheid="Confidence", antibioticaresistentie="ABR",
             buitenland="Recent travel to another country", eersteziektedag="Date of Onset",
             context="Principal Contextual Setting", overlijden="Date of death (where appropriate)",
             vaccinatie="Vaccinated in respect to the diagnosis", vaccinatiedatum="Vaccination Date (if relevant)",
             statusmelding="Status van de melding", medewerker="Investigating Officer",
             casemanager="Case Manager") %>%
      select(hpzone_id, peildatum, melddatum, meldorganisatie, geslacht, leeftijd, PC,
             agent, infectie, diagnose, diagnosezekerheid, antibioticaresistentie, buitenland,
             eersteziektedag, context, ziekenhuisopname, overlijden, vaccinatie, vaccinatiedatum,
             gemeld, statusmelding, medewerker, casemanager) %>%
      rename(postcode=PC)
    
    write.csv(data, sprintf("export_cases_%s.csv", startdatum), na="", row.names=F)
    handled_files = c(handled_files, file)
    output_files = c(output_files, sprintf("export_cases_%s.csv", startdatum))
    
    printf("Bestand opgeslagen als export_cases_%s.csv.", startdatum)
  } else if ("Time entered" %in% colnames(data) && "Identification Number (Internal)" %in% colnames(data)) {
    # situations
    
    # sanity checks
    printf("Er zijn %d gevallen waarbij 'gemeld in OSIRIS' op 'Yes' staat, maar waar geen meldingsnummer is toegevoegd. Dit zijn gevallen %s.",
           sum(data$`Gemeld in Osiris` == "Yes" & is.na(data$Osirisnummer)), str_c(data$`Identification Number (Internal)`[data$`Gemeld in Osiris` == "Yes" & is.na(data$Osirisnummer)], collapse=", "))
    printf("Er zijn %d gevallen waarbij 'gemeld in OSIRIS' op 'No' staat, maar waar wel een meldingsnummer is toegevoegd. Dit zijn gevallen %s.",
           sum(data$`Gemeld in Osiris` == "No" & !is.na(data$Osirisnummer)), str_c(data$`Identification Number (Internal)`[data$`Gemeld in Osiris` == "No" & !is.na(data$Osirisnummer)], collapse=", "))
    
    # omzetten naar gewenst formaat
    data = data %>%
      mutate(datum=case_when(!is.na(`Start Date`) ~ `Start Date`,
                                 TRUE ~ `Time entered`),
             artikel26=ifelse(`Artikel 26` == "Yes", 1, 0),
             PC=Postcode,
             melding.OSIRIS=case_when(!is.na(`Gemeld in Osiris`) ~ `Gemeld in Osiris`,
                                      !is.na(Osirisnummer) ~ "Yes"),
             melding=ifelse(melding.OSIRIS == "Yes", 1, 0)) %>%
      rename(hpzone_id="Identification Number (Internal)", invoerdatum="Time entered",
             status=Status, type=Type, agent="Infectious Agent", scenario=Scenario,
             zekerheid="Confidence.", risiconiveau="Risk Level",
             context="Principal Contextual Setting", medewerker="Investigating Officer",
             casemanager="Manager", aantal_symptomatisch="Number of symptomatic cases",
             aantal_risico="Number potentially at risk", aantal_ziekenhuis="Number hospitalised",
             aantal_overleden="Number of fatalities") %>%
      select(hpzone_id, datum, invoerdatum, status, type, agent, scenario, zekerheid,
             risiconiveau, artikel26, context, PC, melding, medewerker, casemanager,
             aantal_symptomatisch, aantal_risico, aantal_ziekenhuis, aantal_overleden) %>%
      rename(postcode=PC)
    
    write.csv(data, sprintf("export_situations_%s.csv", startdatum), na="", row.names=F)
    handled_files = c(handled_files, file)
    output_files = c(output_files, sprintf("export_situations_%s.csv", startdatum))
    
    printf("Bestand opgeslagen als export_situations_%s.csv.", startdatum)
  } else if ("Number" %in% colnames(data) && "Received on" %in% colnames(data)) {
    # enquiries
    
    # sanity checks
    printf("Er zijn %d gevallen waarbij de sluitingsdatum voor de ontvangstdatum staat. Dit zijn gevallen %s.",
           sum(data$`Received on` > data$`Date closed`), str_c(data$Number[data$`Received on` > data$`Date closed`], collapse=", "))
    printf("Er zijn %d gevallen zonder broad topic. Dit zijn gevallen %s.",
           sum(is.na(data$`Broad Topic`)), str_c(data$Number[is.na(data$`Broad Topic`)], collapse=", "))
    printf("Er zijn %d gevallen zonder specific topic. Dit zijn gevallen %s.",
           sum(is.na(data$`Specific Topic`)), str_c(data$Number[is.na(data$`Specific Topic`)], collapse=", "))
    
    data = data %>%
      mutate(PC=`Caller's Post District`,
             geslacht=case_when(Gender == "Female" ~ "F",
                                Gender == "Male" ~ "M",
                                Gender == "Other" ~ "X",
                                TRUE ~ "U")) %>%
      rename(hpzone_id="Number", startdatum="Received on", einddatum="Date closed",
             ontvanger="Originally taken by", medewerker="Handled by", status=Status,
             typebeller="Type of Caller", categorie="Broad Topic",
             onderwerp="Specific Topic", onderwerpopen="Additional Topic") %>%
      select(hpzone_id, startdatum, einddatum, ontvanger, status, PC, geslacht,
             typebeller, categorie, onderwerp, onderwerpopen) %>%
      rename(postcode=PC)
    
    write.csv(data, sprintf("export_enquiries_%s.csv", startdatum), na="", row.names=F, fileEncoding="UTF-8")
    handled_files = c(handled_files, file)
    output_files = c(output_files, sprintf("export_enquiries_%s.csv", startdatum))
    
    printf("Bestand opgeslagen als export_enquiries_%s.csv.", startdatum)
  } else {
    printf("Onbekend formaat gevonden in %s; bestand genegeerd.", file)
  }
}

if (length(handled_files) > 0 && bestanden_verwijderen_na_afloop) {
  file.remove(handled_files)
  printf("De volgende bestanden zijn verwijderd na verwerking: %s", str_c(handled_files, collapse=", "))
}

if (!is.na(uitvoermap)) {
  for (f in output_files) {
    file.rename(f, paste0(uitvoermap, f))
  }
  printf("Bestanden verplaatst naar %s: %s.", uitvoermap, str_c(output_files, collapse=", "))
}
