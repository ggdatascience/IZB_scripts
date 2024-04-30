library(tidyverse)
library(sf)
library(tmap)
library(openxlsx)
library(lubridate)
library(cbsodataR)
library(this.path)
library(leaflet)

setwd(dirname(this.path()))

printf = function (...) cat(paste(sprintf(...),"\n"))

nog_colors = c("#1D1756", "#D1005D", "#2F6594", "#D3DFF1", "#FFBDDB")
nog_palette = c("#FAE6EF","#F6CCDF","#ED99BE","#E3669E","#D1005D")

# wijk per gemeente opvragen
gemeenten.nog = "Aalten, Apeldoorn, Berkelland, Bronckhorst, Brummen, Doetinchem, Elburg, Epe, Ermelo, Harderwijk, Hattem, Heerde, Lochem, Montferland, Nunspeet, Oldebroek, Oost Gelre, Oude IJsselstreek, Putten, Voorst, Winterswijk, Zutphen"
gemeenten.nog = str_trim(unlist(str_split(gemeenten.nog, ",")))
meta = cbs_get_meta("85372NED")
gemeenten = meta$WijkenEnBuurten %>%
  filter(str_starts(Key, "GM")) %>%
  filter(Title %in% gemeenten.nog) %>%
  mutate(Key=str_trim(Key))
wijken = meta$WijkenEnBuurten %>%
  filter(str_sub(Key, end=6) %in% str_trim(str_replace(gemeenten$Key, "GM", "WK"))) %>%
  left_join(gemeenten %>% select(Municipality, Title) %>% rename(Gemeente=Title), by="Municipality", keep=F) %>%
  mutate(Key=str_trim(Key))
buurten = meta$WijkenEnBuurten %>%
  filter(str_sub(Key, end=6) %in% str_trim(str_replace(gemeenten$Key, "GM", "BU"))) %>%
  left_join(gemeenten %>% select(Municipality, Title) %>% rename(Gemeente=Title), by="Municipality", keep=F)

# inladen sheet RIVM
data = data.frame()
sheets = getSheetNames("Vaccgraad_GGD Noord- en Oost Gelderland_2020tm2023.xlsx")
for (sheet in sheets) {
  if (!str_detect(sheet, "\\d+")) next
  jaar = str_extract(sheet, "\\d+") %>% as.numeric()
  
  # de sheet is weer lekker onleesbaar
  # in rij 14 staat de vaccinatienaam, in rij 15 de serie, in rij 16 de leeftijd, en in rij 17 het geboortejaar
  # helaas staan deze niet altijd netjes onder elkaar; bijv B14 = DKTP, B15 = Primaire serie, maar H14 = leeg en H15 = Basisimmuun
  # daarnaast bevat kolom 1 in de tabel zowel gemeentenamen als postcodes, maar in de header niks (dus alles +1 doen)
  header = read.xlsx("Vaccgraad_GGD Noord- en Oost Gelderland_2020tm2023.xlsx", sheet=sheet, rows=13:17, skipEmptyCols=F)
  data.sheet = read.xlsx("Vaccgraad_GGD Noord- en Oost Gelderland_2020tm2023.xlsx", sheet=sheet, startRow=18, skipEmptyCols=F)
  
  # plan van aanpak: loop door de header, stel vaccinatienaam samen uit huidige kolom of eerste vorige kolom met waarde,
  # pak de bijbehorende kolom uit de data en excludeer alles met een naam die niet 4 cijfers is
  for (i in 1:ncol(header)) {
    # skip indien het geboortejaar leeg is, dan hebben we er niks aan
    if (is.na(header[4, i])) next
    
    # vaccinnaam kan een aantal kolommen eerder staan
    vacc.naam = header[1, i]
    j = 1
    while (is.na(vacc.naam)) {
      vacc.naam = header[1, i-j]
      j = j + 1
    }
    # serie idem
    vacc.serie = header[2, i]
    j = 1
    while (is.na(vacc.serie)) {
      vacc.serie = header[2, i-j]
      j = j + 1
    }
    # en natuurlijk is leeftijdsgroep ook lastig; die zit in colnames
    vacc.leeftijdsgroep = colnames(header)[i]
    j = 1
    while (str_detect(vacc.leeftijdsgroep, "X\\d+")) {
      vacc.leeftijdsgroep = colnames(header[i-j])
      j = j + 1
    }
    vacc.leeftijd = str_extract(header[3, i], "\\d+") %>% as.numeric()
    vacc.geboortejaar = str_extract(header[4, i], "\\d+") %>% as.numeric()
    
    # de structuur van de sheet eronder is steeds:
    # kolom k: aantal individuen
    # kolom k+1: aantal gevaccineerd
    # kolom k+2: percentage
    data.subset = data.sheet[,c(1, i, i+1, i+2)] %>%
      filter(str_detect(Rijlabels, "\\d+"))
    colnames(data.subset) = c("PC4", "n", "n.vacc", "perc")
    data.subset = data.subset %>%
      mutate(naam=vacc.naam, serie=vacc.serie, vaccid=paste0(vacc.naam, " - ", vacc.leeftijdsgroep, " - ", vacc.serie), leeftijd=vacc.leeftijd, geboortejaar=vacc.geboortejaar, jaar=jaar, .before=1)
    
    data = bind_rows(data, data.subset)
  }
}

# er zijn jaren (sinds COVID-19) waarin de vaccinatiegraad op 2 manieren wordt weergegeven; met en zonder afkapgrens
# wij willen alleen de waardes zonder afkapgrens, dus is.na(leeftijd)
doublures = data %>%
  group_by(vaccid, jaar, PC4) %>%
  group_keys()
doublures$n = data %>%
  group_by(vaccid, jaar, PC4) %>%
  group_size()
doublures = doublures %>% filter(n > 1)
removals = c()
for (i in 1:nrow(doublures)) {
  removals = c(removals, which(data$vaccid == doublures$vaccid[i] & data$jaar == doublures$jaar[i] & data$PC4 == doublures$PC4[i] & !is.na(data$leeftijd)))
}
data = data[-removals,]

# PC4 omzetten naar wijk
pc6 = read.csv("../../../../Documenten - NOG W Openbare data/Kaarten/pc6hnr20220801_gwb.csv", sep=";")
pc6 = pc6 %>%
  mutate(PC4=str_sub(PC6, end=4)) %>%
  group_by(PC4, Buurt2022, Wijk2022, Gemeente2022) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(PC4) %>%
  summarize(Buurt=first(Buurt2022), Wijk=first(Wijk2022), Gemeente=first(Gemeente2022)) #%>%
  #left_join(gemeenten %>% mutate(Key=str_trim(Key)), by=c("GMKey"="Key")) %>%
  #rename(Gemeente=Title)

data = data %>%
  left_join(pc6, by="PC4") %>%
  mutate(Wijk=sprintf("WK%06d", as.numeric(Wijk)), Buurt=sprintf("BU%0d", as.numeric(Buurt)), Gemeente=sprintf("GM%04d", Gemeente)) %>%
  left_join(gemeenten %>% select(Key, Title) %>% rename(Gemeentenaam=Title), by=c("Gemeente"="Key")) %>%
  left_join(wijken %>% select(Key, Title) %>% rename(Wijknaam=Title), by=c("Wijk"="Key")) %>%
  left_join(buurten %>% select(Key, Title) %>% rename(Buurtnaam=Title), by=c("Buurt"="Key")) %>%
  mutate(label=sprintf("%s (%s) - %.1f%%", PC4, Gemeentenaam, perc*100),
         popup=sprintf("Vaccinatiegraad: %d/%d (%.1f%%)<br><br>Locatie: %s (%s)<br>Buurt: %s<br>Wijk: %s", n.vacc, n, perc*100, PC4, Gemeentenaam, Buurtnaam, Wijknaam))


# kaarten maken
kaartdata.gemeente = st_read("../../../../Documenten - NOG W Openbare data/Kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer="cbs_gemeente_2022_gegeneraliseerd")
kaartdata.wijk = st_read("../../../../Documenten - NOG W Openbare data/Kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer="cbs_wijk_2021_gegeneraliseerd")
kaartdata.buurt = st_read("../../../../Documenten - NOG W Openbare data/Kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer="cbs_buurt_2020_gegeneraliseerd")
kaartdata.pc4 = st_read("../../../../Documenten - NOG W Openbare data/Kaarten/pc4_arcgis.shp")

# grote versie per type, zodat de postcodes leesbaar zijn
for (vacctype in unique(data$vaccid[data$jaar == 2023])) {
  data.subset = data %>%
    filter(jaar == 2023, vaccid == vacctype) %>%
    mutate(label=sprintf("%s (%s)\n%.1f%%", PC4, Gemeentenaam, perc*100))
  if (nrow(data.subset) == 0) next
  
  png(sprintf("graphs/PC4 %s.png", str_replace(vacctype, "/|\\*", "")), width=5000, height=3000)
  print(tm_shape(kaartdata.pc4 %>% right_join(data.subset, by="PC4") %>% st_make_valid()) +
    tm_fill(col="perc",
            palette=nog_palette,
            breaks=quantile(data.subset$perc, seq(0, 1, 0.2), na.rm=T),
            title="Vaccinatiegraad",
            textNA="Geen data",
            legend.show=T,
            legend.format=list(fun=function(x) { return(sprintf("%.0f%%", x*100)) },
                               text.separator="tot")) +
    tm_borders(alpha=0.5, col=nog_colors[2]) +
    tm_text(text="label", size="AREA", size.lowerbound=0.8, print.tiny=T) +
    tm_layout(main.title=sprintf("Vaccinatiegraad %s in 2023", vacctype),
              frame=F,
              legend.outside=T))
  dev.off()
}

# grote versie met verschil in vaccinatiegraad
for (vacctype in unique(data$vaccid[data$jaar == 2023])) {
  data.subset = data %>%
    filter(jaar %in% c(2022, 2023), vaccid == vacctype) %>%
    arrange(jaar) %>%
    group_by(PC4, vaccid, Gemeentenaam, Wijknaam, Buurtnaam) %>%
    summarize(verschil=last(perc)-first(perc)) %>%
    mutate(label=sprintf("%s (%s)\n%.1f%%", PC4, Gemeentenaam, verschil*100))
  if (nrow(data.subset) == 0) next
  
  png(sprintf("graphs/afname %s.png", str_replace(vacctype, "/|\\*", "")), width=5000, height=3000)
  print(tm_shape(kaartdata.pc4 %>% right_join(data.subset, by="PC4") %>% st_make_valid()) +
          tm_fill(col="verschil",
                  palette=nog_palette,
                  breaks=quantile(data.subset$verschil, seq(0, 1, 0.2), na.rm=T),
                  title="Verschil vaccinatiegraad (procentpunt)",
                  textNA="Geen data",
                  legend.show=T,
                  legend.format=list(fun=function(x) { return(sprintf("%.0f%%", x*100)) },
                                     text.separator="tot")) +
          tm_borders(alpha=0.5, col=nog_colors[2]) +
          tm_text(text="label", size="AREA", size.lowerbound=0.8, print.tiny=T) +
          tm_layout(main.title=sprintf("Verschil vaccinatiegraad %s in 2023 t.o.v. 2022", vacctype),
                    frame=F,
                    legend.outside=T))
  dev.off()
}

# verloop per wijk
data.wijk = data %>%
  group_by(vaccid, jaar, Gemeentenaam, Wijknaam) %>%
  summarize(Wijk=first(Wijk), n=sum(n, na.rm=T), n.vacc=sum(n.vacc, na.rm=T), perc=n.vacc/n*100) %>%
  arrange(jaar)

# grote versie per wijk
for (vacctype in unique(data$vaccid[data$jaar == 2023])) {
  data.subset = data.wijk %>%
    filter(jaar == 2023, vaccid == vacctype) %>%
    mutate(label=sprintf("%s (%s)\n%.1f%%", Wijknaam, Gemeentenaam, perc))
  if (nrow(data.subset) == 0) next
  
  png(sprintf("graphs/wijk %s.png", str_replace(vacctype, "/|\\*", "")), width=4000, height=2400)
  print(tm_shape(kaartdata.wijk %>% right_join(data.subset, by=c("statcode"="Wijk")) %>% st_make_valid()) +
          tm_fill(col="perc",
                  palette=nog_palette,
                  breaks=quantile(data.subset$perc, seq(0, 1, 0.2), na.rm=T),
                  title="Vaccinatiegraad",
                  textNA="Geen data",
                  legend.show=T,
                  legend.format=list(fun=function(x) { return(sprintf("%.0f%%", x)) },
                                     text.separator="tot")) +
          tm_borders(alpha=0.5, col=nog_colors[2]) +
          tm_text(text="label", size="AREA", size.lowerbound=0.8, print.tiny=T) +
          tm_layout(main.title=sprintf("Vaccinatiegraad %s in 2023", vacctype),
                    frame=F,
                    legend.outside=T))
  dev.off()
}

# kleinere versie per type, met gemeentegrenzen
for (vacctype in unique(data$vaccid[data$jaar == 2023])) {
  data.subset = data %>%
    filter(jaar == 2023, vaccid == vacctype) 
  if (nrow(data.subset) == 0) next
  
  png(sprintf("graphs/gemeente %s.png", str_replace(vacctype, "/|\\*", "")), width=900, height=600)
  print(tm_shape(kaartdata.pc4 %>% right_join(data.subset, by="PC4") %>% st_make_valid()) +
          tm_fill(col="perc",
                  palette=nog_palette,
                  breaks=quantile(data.subset$perc, seq(0, 1, 0.2), na.rm=T),
                  title="Vaccinatiegraad",
                  textNA="Geen data",
                  legend.show=T,
                  legend.format=list(fun=function(x) { return(sprintf("%.0f%%", x*100)) },
                                     text.separator="tot")) +
          #tm_borders(alpha=0.5, col=nog_colors[2]) +
        tm_shape(kaartdata.gemeente %>% filter(statnaam %in% data.subset$Gemeentenaam)) +
          tm_borders(alpha=0.8, col=nog_colors[2]) +
          tm_text(text="statnaam", size="AREA", size.lowerbound=0.8, print.tiny=T) +
          tm_layout(main.title=sprintf("Vaccinatiegraad %s in 2023", vacctype),
                    frame=F,
                    legend.outside=T))
  dev.off()
}

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

for (vacctype in unique(data.wijk$vaccid)) {
  for (gem in unique(data.wijk$Gemeentenaam)) {
    data.subset = data.wijk %>%
      filter(vaccid == vacctype, Gemeentenaam == gem)
    
    graph = ggplot(data.subset, aes(x=jaar, y=perc)) +
      geom_line(color=nog_colors[2]) +
      scale_x_continuous(breaks=integer_breaks()) +
      facet_wrap(vars(Wijknaam)) +
      labs(title=sprintf("Vaccinatiegraad in %s", gem), subtitle=vacctype, x="Jaar", y="Percentage gevaccineerd (%)") +
      theme(axis.text.x=element_text(angle=90))
    ggsave(sprintf("%s - %s.png", gem, str_replace(vacctype, "/|\\*", "")), graph, path="graphs/per wijk")
  }
}

# heatmap met toe- en afname per vaccinatie
# eerst per vaccinatie
for (vacctype in unique(data$vaccid[data$jaar == 2023])) {
  data.subset = data.wijk %>%
    ungroup() %>%
    filter(vaccid == vacctype) %>%
    group_by(vaccid, Gemeentenaam, Wijknaam) %>%
    arrange(vaccid, Gemeentenaam, Wijknaam, jaar) %>%
    mutate(prev=dplyr::lag(perc), verschil=perc-prev,
           Wijknaam.hr=paste0(Gemeentenaam, " - ", Wijknaam)) %>%
    filter(!is.na(verschil))
  if (nrow(data.subset) == 0) next
  
  graph = ggplot(data.subset, aes(x=jaar, y=Wijknaam.hr, fill=verschil)) +
    geom_tile() +
    scale_fill_gradient2(high="blue", mid="white", low="red") +
    labs(title="Verschil in vaccinatiegraad door de jaren heen", subtitle=vacctype, x=NULL, y=NULL, fill="Verschil") +
    theme_minimal()
  ggsave(sprintf("heatmap vacc %s.png", str_replace(vacctype, "/|\\*", "")), graph, path="graphs/", bg="white", width=2000, height=2400, units="px")
}

# dan per wijk
for (wijk in unique(data.wijk$Wijknaam)) {
  data.subset = data.wijk %>%
    ungroup() %>%
    filter(Wijknaam == wijk) %>%
    arrange(jaar) %>%
    group_by(vaccid, Gemeentenaam, Wijknaam) %>%
    arrange(vaccid, Gemeentenaam, Wijknaam, jaar) %>%
    mutate(prev=lag(perc), verschil=(perc-prev)) %>%
    filter(!is.na(verschil))
  if (nrow(data.subset) == 0) next
  
  graph = ggplot(data.subset, aes(x=jaar, y=vaccid, fill=verschil)) +
    geom_tile() +
    scale_fill_gradient2(high="blue", mid="white", low="red") +
    labs(title="Verschil in vaccinatiegraad door de jaren heen", subtitle=wijk, x=NULL, y=NULL, fill="Verschil") +
    theme_minimal()
  ggsave(sprintf("heatmap wijk %s - %s.png", first(data.subset$Gemeentenaam), wijk), graph, path="graphs/", bg="white", width=2000, height=2400, units="px")
}

# overzicht van het afgelopen jaar
data.subset = data.wijk %>%
  ungroup() %>%
  filter(jaar %in% c(2022, 2023)) %>%
  arrange(jaar) %>%
  group_by(vaccid, Gemeentenaam, Wijknaam) %>%
  arrange(vaccid, Gemeentenaam, Wijknaam, jaar) %>%
  mutate(prev=lag(perc), verschil=(perc-prev),
         Wijknaam.hr=paste0(Gemeentenaam, " - ", Wijknaam)) %>%
  filter(!is.na(verschil))
graph = ggplot(data.subset, aes(x=vaccid, y=Wijknaam.hr, fill=verschil)) +
  geom_tile() +
  scale_fill_gradient2(high="blue", mid="white", low="red") +
  labs(title="Verschil in vaccinatiegraad door de jaren heen", x=NULL, y=NULL, fill="Verschil") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90))
ggsave("heatmap afname 2023.png", graph, path="graphs/", bg="white", width=3000, height=3000, units="px")


# output = leaflet() %>%
#   addTiles()
# 
# for (vacctype in unique(data$vaccid)) {
#   data.subset = data %>% filter(jaar == 2023, vaccid == vacctype)
#   vaccinatiegraadpalette = colorBin(nog_palette, data.subset$perc, bins=5)
#   
#   output = output %>%
#     addPolygons(data=kaartdata.pc4 %>%
#                   right_join(data.subset, by="PC4") %>%
#                   st_transform(4326),
#                 fillColor=~vaccinatiegraadpalette(perc),
#                 fillOpacity=0.6,
#                 color=nog_colors[2],
#                 label=~label,
#                 popup=~popup,
#                 group=vacctype)
# }
#   
# output = output %>%
#   addPolygons(data=kaartdata.gemeente %>% filter(statnaam %in% gemeenten$Title) %>% st_transform(4326),
#               color="black",
#               group="Gemeente") %>%
#   addPolygons(data=kaartdata.wijk %>% filter(statnaam %in% wijken$Title) %>% st_transform(4326),
#               color="black",
#               group="Wijk") %>%
#   addPolygons(data=kaartdata.buurt %>% filter(statnaam %in% buurten$Title) %>% st_transform(4326),
#               color="black",
#               group="Buurt") %>%
#   hideGroup(unique(data$vaccid)) %>%
#   addLayersControl(baseGroups=c("Gemeente", "Wijk", "Buurt"), overlayGroups=unique(data$vaccid))
# 
