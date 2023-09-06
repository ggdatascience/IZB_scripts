#
#
# Kaarten met een overzicht van verschillende risicofactoren in onze regio.
# Data is afkomstig uit (semi)openbare bronnen en wordt visueel weergegeven in fancy grafieken.
#
#

library(tidyverse)
library(this.path)
library(openxlsx)
library(cbsodataR)
library(tmap)
library(maptiles)
library(sf)

setwd(dirname(this.path()))

# wrapper voor sprintf, voor makkelijk weergeven
printf = function (...) cat(paste(sprintf(...),"\n"))

# GGD NOG kleuren
nog_colors = c("#1D1756", "#D1005D", "#2F6594", "#D3DFF1", "#FFBDDB")
nog_palette = c("#FAE6EF","#F6CCDF","#ED99BE","#E3669E","#D1005D")
blue_palette = c("#ccf9ff", "#7ce8ff", "#55d0ff", "#00acdf", "#0080bf")

# metadata ophalen voor het zoeken van gemeentes
gemeenten.nog = "Aalten, Apeldoorn, Berkelland, Bronckhorst, Brummen, Doetinchem, Elburg, Epe, Ermelo, Harderwijk, Hattem, Heerde, Lochem, Montferland, Nunspeet, Oldebroek, Oost Gelre, Oude IJsselstreek, Putten, Voorst, Winterswijk, Zutphen"
gemeenten.nog = str_trim(unlist(str_split(gemeenten.nog, ",")))

# kaartdata eenmalig inladen
kaartdata.gemeente = st_read("kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer = "cbs_gemeente_2022_gegeneraliseerd")

kaartdata.ggd = st_read("kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer="cbs_ggdregio_2021_gegeneraliseerd")

kaartdata.buurten = st_read("kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer = "cbs_buurt_2020_gegeneraliseerd")

kaartdata.pc4 = st_read("kaarten/pc4_arcgis.shp")

###
### Regioindelingen zorg
###

meta = cbs_get_meta("85385NED")
data.regios = cbs_get_data("85385NED") %>% cbs_add_label_columns()
data.regios$RegioS = str_trim(data.regios$RegioS)
data.regios$RegioS_label = str_trim(data.regios$RegioS_label)
# Naam_15 = GGD

roaz = read.table("CBS data/roazregios.csv", sep=";", header=T) %>%
  mutate(gemeentecode=sprintf("GM%04.0f", CBS))

png("kaarten IZB/regioindeling.png", width=1200, height=1200)
tm_shape(kaartdata.gemeente %>% right_join(roaz %>% filter(str_detect(ROAZ.regio.s, "Euregio|Oost|Zwolle|Midden-Nederland")), by=c("statcode"="gemeentecode"))) +
  tm_fill(col="ROAZ.regio.s",
          palette=nog_colors[2:4],
          title="ROAZ-regio") +
  #tm_text(text="ROAZ.regio.s", size="AREA", size.lowerbound=0.8, print.tiny=T) +
tm_shape(kaartdata.ggd  %>% filter(str_detect(statnaam, "Gelderland|IJssel|Twente|Utrecht|Drenthe"))) +
  tm_text(text="statnaam", size="AREA", size.lowerbound=0.8, print.tiny=T) +
  tm_borders(col=nog_colors[1],
             lwd=2) +
  tm_layout(main.title="Regioindeling Oost-Nederland",
            main.title.size=2,
            frame=F,
            scale=1.5)
dev.off()

# verder hebben we de kaartdata van de andere gemeentes niet meer nodig
kaartdata.gemeente = kaartdata.gemeente %>%
  filter(statnaam %in% gemeenten.nog)


###
### Zorgkosten, leeftijd
###
zorgkosten = read.csv("CBS data/Vektis gemeente 2020.csv", sep=";") %>%
  filter(gemeentenaam %in% str_to_upper(gemeenten.nog)) %>%
  mutate(gemeentenaam=str_to_title(gemeentenaam, locale="nl"))

zorgkosten.pj = zorgkosten %>%
  group_by(gemeentenaam) %>%
  summarize(across(where(is.numeric), ~sum(.x, na.rm=T))) %>%
  rowwise() %>%
  transmute(gemeentenaam=gemeentenaam,
            kosten=sum(c_across(kosten_medisch_specialistische_zorg:kosten_overig), na.rm=T),
            verzekerdejaren=aantal_verzekerdejaren,
            kostenpj=kosten/verzekerdejaren,
            tekst=sprintf("%s\n€ %s", gemeentenaam, format(kostenpj, digits=1, big.mark=".")))

zorgkosten.totaal = zorgkosten %>%
  summarize(across(where(is.numeric), ~sum(.x, na.rm=T))) %>%
  pivot_longer(starts_with("kosten"), names_to="Categorie", values_to="Kosten") %>%
  mutate(Kosten.pp=format(Kosten/aantal_verzekerdejaren, big.mark=".", decimal.mark=",", digits=2, nsmall=2),
         Kosten=format(Kosten, big.mark=".", decimal.mark=",")) %>%
  select(-aantal_bsn, -aantal_verzekerdejaren) %>%
  mutate(Categorie=str_to_sentence(str_replace_all(Categorie, "_", " ") %>% str_replace_all("kosten ", "")) %>% str_replace_all("ggz", "GGZ"))

png("kaarten IZB/zorgkosten_kaart.png", width=1200, height=1200)
tm_shape(kaartdata.gemeente %>% left_join(zorgkosten.pj, by=c("statnaam"="gemeentenaam"))) +
  tm_fill(col = "kostenpj",
          palette= nog_palette,
          alpha=1.0,
          breaks = quantile(zorgkosten.pj$kostenpj, c(seq(0,1,0.2)), na.rm = T),
          legend.show=F) +
  tm_borders(alpha=0.8, col="black") +
  tm_text(text="tekst", size="AREA", size.lowerbound=0.8, print.tiny=T) +
  tm_layout(main.title="",
            main.title.size=2,
            frame=F,
            scale=1.5)
dev.off()

zorgkosten.gemeente = zorgkosten %>%
  group_by(gemeentenaam, leeftijdsklasse) %>%
  summarize(across(where(is.numeric), ~sum(.x, na.rm=T))) %>%
  rowwise() %>%
  transmute(gemeentenaam=gemeentenaam,
            leeftijdsklasse=leeftijdsklasse,
            kosten=sum(c_across(kosten_medisch_specialistische_zorg:kosten_overig), na.rm=T),
            verzekerdejaren=aantal_verzekerdejaren,
            kostenpj=kosten/verzekerdejaren)

png("kaarten IZB/zorgkosten_heatmap.png", width=1000, height=1000)
ggplot(zorgkosten.gemeente, aes(x=leeftijdsklasse, y=fct_rev(gemeentenaam), fill=kostenpj)) +
  scale_fill_gradient(low=nog_palette[1], high=nog_palette[5], name="Kosten", labels=function (x) { return(paste0("€ ", format(x, big.mark="."))) }) +
  geom_tile(color="white", lwd=1) +
  labs(title="", x="", y="") +
  theme_minimal() +
  theme(text=element_text(size=16),
        title=element_text(size=20),
        axis.text = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle=90))
dev.off()

printf("Volgens de verzekeringsdata waren er in dit jaar %d verzekerden bekend, met een totaal van € %.0f aan zorgkosten.", sum(zorgkosten$aantal_bsn, na.rm=T), sum(zorgkosten.gemeente$kosten))


##
## Inwoners en SES
##

inwoners.gemeente.leeftijd = read.csv("CBS data/inwoners_gemeente_per_leeftijd_2022.csv")
inwoners.gemeente = inwoners.gemeente.leeftijd %>%
  group_by(Gemeente) %>%
  summarize(n=sum(n, na.rm=T))
inwoners.gemeente.leeftijdscategorie = inwoners.gemeente.leeftijd %>%
  mutate(Leeftijdscategorie=cut(Leeftijd, breaks=c(seq(0, 90, 5), Inf), labels=c(sprintf("%d t/m %d jaar", seq(0, 89, 5), seq(0, 89, 5)+4), "90+"), right=F)) %>%
  group_by(Gemeente, Leeftijdscategorie) %>%
  summarize(inwoners=sum(n, na.rm=T)) %>%
  left_join(inwoners.gemeente, by="Gemeente") %>%
  mutate(inwoners=inwoners/n*100)
png("kaarten IZB/inwoners_leeftijd.png", width=1000, height=1000, bg="transparent")
ggplot(inwoners.gemeente.leeftijdscategorie, aes(x=Leeftijdscategorie, y=fct_rev(Gemeente), fill=inwoners)) +
  scale_fill_gradient(low="#e8e8ee", high=nog_colors[1], name="Percentage", labels=function (x) { return(paste0(format(x, small.mark=","), "%")) }) +
  geom_tile(color="white", lwd=1) +
  labs(title="Aantal inwoners", x="", y="") +
  theme_minimal() +
  theme(text=element_text(size=16),
        title=element_text(size=20),
        axis.text = element_text(size=16, face="bold"),
        axis.text.x = element_text(angle=90))
dev.off()

# SES-WOA
meta = cbs_get_meta("85163NED")
gemeenten = meta$WijkenEnBuurten %>%
  filter(str_starts(Key, "GM")) %>%
  filter(Title %in% gemeenten.nog)
buurten = meta$WijkenEnBuurten %>%
  filter(str_sub(Key, end=6) %in% str_trim(str_replace(gemeenten$Key, "GM", "BU"))) %>%
  left_join(gemeenten %>% select(Municipality, Title) %>% rename(Gemeente=Title), by="Municipality", keep=F)
seswoa = lapply(unique(buurten$Municipality), function (gemcode) {
  return(cbs_get_data("85163NED", WijkenEnBuurten=buurten$Key[buurten$Municipality == gemcode], Perioden="2019JJ00") %>% cbs_add_label_columns())
})
seswoa = bind_rows(seswoa)
bevolkingsdichtheid = lapply(unique(buurten$Municipality), function (gemcode) {
  return(cbs_get_data("85318NED", WijkenEnBuurten=buurten$Key[buurten$Municipality == gemcode], select=c("WijkenEnBuurten", "Bevolkingsdichtheid_33")) %>% cbs_add_label_columns())
})
bevolkingsdichtheid = bind_rows(bevolkingsdichtheid)

kaartdata.buurten = kaartdata.buurten %>% filter(statcode %in% buurten$Key)
png("kaarten IZB/SESWOA_kaart.png", width=1200, height=1200)
tm_shape(kaartdata.buurten %>% left_join(seswoa, by=c("statcode"="WijkenEnBuurten")) %>% left_join(bevolkingsdichtheid, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill(col=c("GemiddeldeScore_29", "Bevolkingsdichtheid_33"),
          palette=nog_palette,
          alpha=1.0,
          colorNA=NULL,
          showNA=F,
          legend.show=T) +
  tm_shape(kaartdata.gemeente) +
  tm_borders(alpha=0.8, col=nog_colors[2]) +
  tm_text(text="statnaam", size="AREA", size.lowerbound=0.8, print.tiny=T) +
  tm_layout(main.title="SES-WOA score",
            main.title.size=2,
            frame=F,
            scale=1.5)
dev.off()

png("kaarten IZB/SESWOA_overlay_kaart.png", width=1200, height=1200)
tm_shape(kaartdata.buurten %>% right_join(seswoa, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill(col = "GemiddeldeScore_29",
          palette= nog_palette,
          alpha=1.0,
          colorNA=NULL,
          showNA=F,
          breaks = quantile(seswoa$GemiddeldeScore_29, c(seq(0,1,0.2)), na.rm = T),
          legend.show=T,
          title="SES-WOA score (hoger is 'beter')") +
tm_shape(kaartdata.buurten %>% right_join(bevolkingsdichtheid, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill(col = "Bevolkingsdichtheid_33",
          palette=blue_palette,
          alpha=0.5,
          colorNA=NULL,
          showNA=F,
          breaks = quantile(bevolkingsdichtheid$Bevolkingsdichtheid_33, c(seq(0,1,0.2)), na.rm = T),
          legend.show=T,
          title="Inwoners per km2") +
tm_shape(kaartdata.gemeente) +
  tm_borders(alpha=0.8, col=nog_colors[2]) +
  tm_text(text="statnaam", size="AREA", size.lowerbound=0.8, print.tiny=T) +
  tm_layout(main.title="SES-WOA score",
            main.title.size=2,
            frame=F,
            scale=1.5)
dev.off()

###
### Agrarische bedrijven
###
data.kvk = read.xlsx("../Vertrouwelijke documenten - Team Infectieziektenbestrijding/7_Kennis en onderzoek/Adressen bedrijven regio NOG/adressenlijst juni 2023.xlsx") %>%
  # twee adressen aanwezig in de lijst; wij willen VA (vestigingsadres) i.p.v. CA (contactadres)
  mutate(postcode=paste0(PCVA_CIJF, PCVA_LTRS),
         huisnummer=str_extract(STRVA, "\\d+(.*?)$"))

# landbouw en veehouderij beginnen allemaal met 011 t/m 015, zie https://sbi.cbs.nl/CBS.TypeerModule.TypeerServiceWebAPI/content/angular/app/#/tree
agrarisch = data.kvk %>%
  filter(str_starts(SBI, "011") | str_starts(SBI, "012") | str_starts(SBI, "013") | str_starts(SBI, "014") | str_starts(SBI, "015"),
         W_P_TOTAAL > 1) %>%
  mutate(SBI_groep=case_when(str_starts(SBI, "011") | str_starts(SBI, "012") | str_starts(SBI, "013") ~ "Teelt van gewassen",
                             str_starts(SBI, "014") ~ "Fokken en houden van dieren",
                             str_starts(SBI, "015") ~ "Combinatie"))

kaartdata.pc6 = st_read("kaarten/PC6.shp") %>%
  filter(PC6 %in% agrarisch$postcode) %>%
  st_make_valid()

bg = maptiles::get_tiles(kaartdata.pc6, crop=T, provider="Esri.WorldStreetMap", zoom=10)

png("kaarten IZB/agrarische bedrijven.png", width=1000, height=700)
tm_shape(bg) + tm_rgb() +
tm_shape(kaartdata.pc6 %>% full_join(agrarisch, by=c("PC6"="postcode"))) +
  tm_bubbles(size="W_P_TOTAAL", col="SBI_groep", title.size="Aantal werkenden", title.col="Branche", scale=2) +
  tm_layout(legend.outside=T,
            legend.text.size=1.1,
            legend.title.size=1.5,
            main.title="Overzicht agrarische bedrijven regio NOG",
            frame=F)
dev.off()

###
### Huisartsenpraktijken
###

huisartsen = data.kvk %>%
  # huisartsen, tandartsen, etc. beginnen allemaal met 862
  filter(str_starts(SBI, "862"))

# er kunnen meerdere huisartsen gevestigd zijn op één adres
# in dat geval nemen we aan dat het gaat om een samenwerkingsverband
huisartsen.zelfdepc = huisartsen %>%
  group_by(postcode, huisnummer, SBI)

for (rows in group_rows(huisartsen.zelfdepc)) {
  if (length(rows) == 1) next
  
  printf("%d huisartsen op postcode %s: %s", length(rows), first(huisartsen.zelfdepc$postcode[rows]),
         str_c(lapply(rows, function(row) { return(paste0(huisartsen.zelfdepc$HN1X30[row], " (", huisartsen.zelfdepc$STRVA[row] , ")")) }), collapse=", "))
}

huisartsen = huisartsen.zelfdepc %>%
  summarize(postcode=first(postcode),
            huisnummer=first(huisnummer),
            namen=str_c(HN45, collapse=", "),
            adres=paste0(first(STRVA), " ", first(PCPLVA)),
            werkzamepersonen=sum(W_P_TOTAAL),
            SBI_OMSCHR=first(SBI_OMSCHR)) %>%
  mutate(werknemers_klasse=cut(werkzamepersonen, c(0, 5, 10, 50, Inf), labels=F, include.lowest=T))

write.xlsx(huisartsen, "kaarten IZB/huisartsen.xlsx")

kaartdata.pc6 = st_read("kaarten/PC6.shp") %>%
  filter(PC6 %in% huisartsen$postcode) %>%
  st_make_valid()

bg = maptiles::get_tiles(kaartdata.pc6, crop=T, provider="Esri.WorldStreetMap", zoom=10)

png("kaarten IZB/huisartsen.png", width=1000, height=700)
tm_shape(bg) + tm_rgb() +
  tm_shape(kaartdata.pc6 %>% left_join(huisartsen, by=c("PC6"="postcode"))) +
  tm_symbols(size="werknemers_klasse", sizes.legend.labels=c("0-5", "6-10", "11-50", "50+"),
             col="SBI_OMSCHR", title.size="Aantal werkenden", title.col="Branche", scale=2) +
  tm_layout(legend.outside=T,
            legend.text.size=1.1,
            legend.title.size=1.5,
            main.title="Overzicht zorgverleners regio NOG",
            frame=F)
dev.off()

###
### Werknemers per locatie en bedrijfstak
###
SBI = read.xlsx("indeling_SBI.xlsx")
werkadressen = data.kvk %>%
  mutate(SBI_prefix=str_sub(SBI, end=2)) %>%
  left_join(SBI, by="SBI_prefix") %>%
  group_by(postcode, cat_naam) %>%
  arrange(desc(W_P_TOTAAL)) %>%
  summarize(bedrijfsnaam=str_c(HN1X30, collapse=", "), werknemers=sum(W_P_TOTAAL, na.rm=T)) %>%
  mutate(bedrijfsnaam_1000=ifelse(werknemers >= 1000, bedrijfsnaam, NA),
         bedrijfsnaam_1000=ifelse(str_length(bedrijfsnaam_1000) > 20, paste0(str_sub(bedrijfsnaam_1000, end=17), "..."), bedrijfsnaam_1000))
  
kaartdata.pc6 = st_read("kaarten/PC6.shp") %>%
  filter(PC6 %in% werkadressen$postcode) %>%
  st_make_valid()

bg = maptiles::get_tiles(kaartdata.pc6, crop=T, provider="Esri.WorldStreetMap", zoom=11)

png("kaarten IZB/werklocaties.png", width=1000, height=700)
tm_shape(bg) + tm_rgb() +
  tm_shape(kaartdata.pc6 %>% left_join(werkadressen %>% filter(werknemers >= 5), by=c("PC6"="postcode"))) +
  tm_symbols(size="werknemers", col="cat_naam", title.size="Aantal werkenden", title.col="Branche", scale=2) +
  tm_text("bedrijfsnaam_1000", just="top", auto.placement=T) +
  tm_layout(legend.outside=T,
            legend.text.size=1.1,
            legend.title.size=1.5,
            main.title="Overzicht werknemers per locatie in de regio NOG",
            frame=F)
dev.off()

###
### Migratieachtergrond
###

# er zijn twee tabellen beschikbaar; eentje met land van herkomst (85458NED), en eentje met generatie (84910NED)
# een combinatie van die twee geeft waarschijnlijk het beste inzicht
meta = cbs_get_meta("85458NED")
codes.nog = meta$RegioS$Key[meta$RegioS$Title %in% gemeenten.nog]
data.migr = cbs_get_data("85458NED", Geslacht="T001038", Perioden=meta$Perioden$Key[meta$Perioden$Title == last(sort(meta$Perioden$Title))], RegioS=codes.nog) %>% cbs_add_label_columns()

# totalen per gemeente, zodat we een percentage kunnen berekenen
totalen.migr = data.migr %>%
  filter(Leeftijd == "10000", Herkomstland == "T001040", Geboorteland == "T001638") # alle leeftijden, alle landen

perc.migr = data.migr %>%
  filter(Leeftijd == "10000", Herkomstland != "T001040", Geboorteland == "T001638") %>% # alle herkomstlanden, maar geboorteland maakt niet uit
  group_by(RegioS_label, Herkomstland_label) %>%
  summarize(n=sum(Bevolking_1, na.rm=T)) %>%
  left_join(totalen.migr %>% select(RegioS_label, Bevolking_1), by="RegioS_label") %>%
  mutate(perc=n/Bevolking_1*100)

png("kaarten IZB/herkomst_migranten.png", width=800, height=800)
ggplot(perc.migr %>% filter(Herkomstland_label %in% c("Europa (exclusief Nederland)", "Afrika", "Amerika", "Azië", "Oceanië")), aes(x=Herkomstland_label, y=RegioS_label, fill=perc)) +
  geom_tile(color="white", lwd=1) +
  labs(x="Regio van herkomst", y="Gemeente", fill="Percentage", title="Percentage migranten per gemeente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))
dev.off()

###
### Opvanglocaties OekraÃ¯ne
###
pc4_gemeente = read.csv("CBS data/pc4_gemeente.csv") %>%
  # er zijn een paar dubbele postcodes
  # hierbij nemen we aan dat het grootste aantal inwoners de juiste gemeente is (de anderen zijn meestal 1-5 adressen)
  arrange(desc(aantal)) %>%
  group_by(PC4) %>%
  summarize(gemeentecode=first(Gemeente2022))
opvanglocaties = read.xlsx("kaarten IZB/Opvanglocaties VNOG 15-6.xlsx", sheet="Dataset Oekraine") %>%
  mutate(PC4=str_match(Postcode, "\\d+")[,1])

png("kaarten IZB/opvang_Oekraine_kaart.png", width=1200, height=1200, bg="transparent")
tm_shape(kaartdata.gemeente) +
  tm_fill(col="statnaam",
          palette=nog_palette[1:4],
          legend.show=F) +
  tm_borders(alpha=0.8, col="black") +
  tm_text(text="statnaam", size="AREA", size.lowerbound=0.8, print.tiny=T) +
  tm_layout(main.title="Opvanglocaties OekraÃ¯ne",
            main.title.size=2,
            frame=F,
            scale=1.5) +
tm_shape(kaartdata.pc4 %>% inner_join(opvanglocaties, by="PC4")) +
  tm_bubbles(size="Max..Aanbod.aantal.personen",
             col="Geschikt.(multi)",
             palette=c("white", "yellow", "orange",  "limegreen", "darkgreen", "purple"),
             title.size="Maximaal aanbod",
             title.col="Stap")
dev.off()