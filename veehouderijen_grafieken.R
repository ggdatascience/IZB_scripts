library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
library(maptiles)
library(cbsodataR)
library(ggdnog)
library(this.path)

setwd(dirname(this.path()))

gemeenten = cbs_get_data("85067NED") %>% cbs_add_label_columns() %>%
  rename(gemcode=RegioS, gemnaam=RegioS_label, ggdcode=Code_14, ggdnaam=Naam_15) %>%
  select(gemcode, gemnaam, ggdcode, ggdnaam) %>%
  mutate(across(where(is.character), ~str_trim(.x)))

kaartdata.buurt = st_read(dir_maps("cbsgebiedsindelingen2023.gpkg"), layer="buurt_gegeneraliseerd")
kaartdata.wijk = st_read(dir_maps("cbsgebiedsindelingen2023.gpkg"), layer="wijk_gegeneraliseerd")
kaartdata.gemeente = st_read(dir_maps("cbsgebiedsindelingen_2022_v1.gpkg"), layer="cbs_gemeente_2022_gegeneraliseerd") #%>%
  #filter(statnaam %in% gemeenten.nog)

pc6 = read.csv(dir_maps("pc6hnr20220801_gwb.csv"), sep=";") %>%
  mutate(Buurt=sprintf("BU%08d", Buurt2022),
         Wijk=sprintf("WK%06d", Wijk2022),
         Gemeente=sprintf("GM%04d", Gemeente2022)) %>%
  select(-ends_with("2022"))

meta = cbs_get_meta("85618NED")
dichtheid = cbs_get_data("85618NED", select=c("WijkenEnBuurten", "Bevolkingsdichtheid_34", "OppervlakteLand_113")) %>%
  cbs_add_label_columns() %>%
  rename(dichtheid=Bevolkingsdichtheid_34, opp=OppervlakteLand_113) %>%
  mutate(WijkenEnBuurten=str_trim(WijkenEnBuurten))

data = st_read("GIS_bestand_LBT2021_gpkg/LBT_2021.gpkg") %>%
  #filter(gemeente %in% gemeenten.nog) %>%
  st_transform(4326) #%>% st_centroid() 

data.sum = data %>%
  as.data.frame() %>% # om de geom te kunnen verwijderen moeten we het type wijzigen
  select(-geom) %>%
  left_join(pc6, by=c("pc6"="PC6", "hnr"="Huisnummer")) %>%
  group_by(Buurt, Wijk, Gemeente) %>%
  summarize(across(Rundvee:Aant_dieren_tot, ~sum(.x, na.rm=T)), Aant_veehouders=n())

#bg = maptiles::get_tiles(kaartdata.gemeente, crop=T, provider="Esri.WorldStreetMap", zoom=10)

# bevolkingsdichtheid heel Nederland
png("output/bevolkingsdichtheid NL.png", width=900, height=990)
tm_shape(kaartdata.buurt %>% left_join(dichtheid, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill("dichtheid",
          title=expression('Bevolkingsdichtheid '(km^-2)),
          breaks=quantile(dichtheid$dichtheid[str_starts(dichtheid$WijkenEnBuurten, "BU")], seq(0, 1, 0.2), na.rm=T),
          palette="Blues",
          legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
  tm_layout(frame=F,
            scale=1.5)
dev.off()
# bevolkingsdichtheid KON-regio
png("output/bevolkingsdichtheid KON.png", width=900, height=990)
tm_shape(kaartdata.buurt %>%
           filter(gm_code %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")]) %>%
           left_join(dichtheid, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill("dichtheid",
          title=expression('Bevolkingsdichtheid '(km^-2)),
          breaks=quantile(dichtheid$dichtheid[str_starts(dichtheid$WijkenEnBuurten, "BU")], seq(0, 1, 0.2), na.rm=T),
          palette="Blues",
          legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
tm_shape(kaartdata.gemeente %>% filter(statcode %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")])) +
  tm_borders() +
  tm_text("statnaam", size="AREA") +
  tm_layout(frame=F,
            scale=1.5)
dev.off()

vars = colnames(data.sum)[-c(1:3,48)]
# heel Nederland
for (var in vars) {
  data.cur = data.sum[,c("Buurt", var)] %>%
    left_join(dichtheid, by=c("Buurt"="WijkenEnBuurten")) %>%
    rename(n=!!var) %>%
    mutate(n=ifelse(n == 0, NA, n),
           rel=n/opp)
  
  png(sprintf("output/NL_%s.png", var), width=1800, height=990)
  print(tmap_arrange(tm_shape(kaartdata.buurt %>% left_join(data.cur, by=c("statcode"="Buurt"))) +
    tm_fill(col="n",
            title=paste0(str_replace_all(var, c("Gei_"="Geiten - ",
                                                "Ove_"="Overig - ",
                                                "Plm_"="Pluimvee - ",
                                                "Run_"="Runderen - ",
                                                "Sch_"="Schapen - ",
                                                "Var_"="Varkens - ",
                                                "_"=" - ")), " (n)"),
            breaks=quantile(data.cur$n, seq(0, 1, 0.2), na.rm=T),
            palette="Reds",
            textNA="Geen dieren",
            legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
    tm_layout(frame=F,
              scale=1.5),
    tm_shape(kaartdata.buurt %>%
               left_join(data.cur, by=c("statcode"="Buurt"))) +
      tm_fill(col="rel",
              title=expression('Relatief '(km^-2)),
              breaks=quantile(data.cur$rel, seq(0, 1, 0.2), na.rm=T),
              palette="Greens",
              textNA="Geen dieren",
              legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
      tm_layout(frame=F,
                scale=1.5)),
    nrow=1)
  dev.off()
}
# heel Nederland
for (var in vars) {
  data.cur = data.sum[,c("Buurt", var)] %>%
    left_join(dichtheid, by=c("Buurt"="WijkenEnBuurten")) %>%
    rename(n=!!var) %>%
    mutate(n=ifelse(n == 0, NA, n),
           rel=n/opp)
  
  png(sprintf("output/NL_%s.png", var), width=1800, height=990)
  print(tmap_arrange(tm_shape(kaartdata.buurt %>% left_join(data.cur, by=c("statcode"="Buurt"))) +
    tm_fill(col="n",
            title=paste0(str_replace_all(var, c("Gei_"="Geiten - ",
                                                "Ove_"="Overig - ",
                                                "Plm_"="Pluimvee - ",
                                                "Run_"="Runderen - ",
                                                "Sch_"="Schapen - ",
                                                "Var_"="Varkens - ",
                                                "_"=" - ")), " (n)"),
            breaks=quantile(data.cur$n, seq(0, 1, 0.2), na.rm=T),
            palette="Reds",
            textNA="Geen dieren",
            legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
    tm_layout(frame=F,
              scale=1.5),
    tm_shape(kaartdata.buurt %>%
               left_join(data.cur, by=c("statcode"="Buurt"))) +
      tm_fill(col="rel",
              title=expression('Relatief '(km^-2)),
              breaks=quantile(data.cur$rel, seq(0, 1, 0.2), na.rm=T),
              palette="Greens",
              textNA="Geen dieren",
              legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
      tm_layout(frame=F,
                scale=1.5)),
    nrow=1)
  dev.off()
}
# KON-regio
for (var in vars) {
  printf("KON: %s", var)
  data.cur = data.sum[,c("Buurt", var)] %>%
    left_join(dichtheid, by=c("Buurt"="WijkenEnBuurten")) %>%
    rename(n=!!var) %>%
    mutate(n=ifelse(n == 0, NA, n),
           rel=n/opp)
  
  png(sprintf("output/KON_%s.png", var), width=1800, height=990)
  print(tmap_arrange(tm_shape(kaartdata.buurt %>%
                                filter(gm_code %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")]) %>%
                                left_join(data.cur, by=c("statcode"="Buurt"))) +
                       tm_fill(col="n",
                               title=paste0(str_replace_all(var, c("Gei_"="Geiten - ",
                                                                   "Ove_"="Overig - ",
                                                                   "Plm_"="Pluimvee - ",
                                                                   "Run_"="Runderen - ",
                                                                   "Sch_"="Schapen - ",
                                                                   "Var_"="Varkens - ",
                                                                   "_"=" - ")), " (n)"),
                               breaks=quantile(data.cur$n, seq(0, 1, 0.2), na.rm=T),
                               palette="Reds",
                               textNA="Geen dieren",
                               legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
                     tm_shape(kaartdata.gemeente %>% filter(statcode %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")])) +
                       tm_borders() +
                       tm_text("statnaam", size="AREA", col="white") +
                       tm_layout(frame=F,
                                 scale=1.5),
                     tm_shape(kaartdata.buurt %>%
                                filter(gm_code %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")]) %>%
                                left_join(data.cur, by=c("statcode"="Buurt"))) +
                       tm_fill(col="rel",
                               title=expression('Relatief '(km^-2)),
                               breaks=quantile(data.cur$rel, seq(0, 1, 0.2), na.rm=T),
                               palette="Greens",
                               textNA="Geen dieren",
                               legend.format=list(text.separator="t/m", big.mark=".", decimal.mark=",")) +
                     tm_shape(kaartdata.gemeente %>% filter(statcode %in% gemeenten$gemcode[str_detect(gemeenten$ggdnaam, "Gelderland|Utrecht|IJsselland|Twente")])) +
                       tm_borders() +
                       tm_text("statnaam", size="AREA", col="white") +
                       tm_layout(frame=F,
                                 scale=1.5)),
        nrow=1)
  dev.off()
}



tm_shape(kaartdata.wijk %>% left_join(dichtheid, by=c("statcode"="WijkenEnBuurten"))) +
  tm_fill("dichtheid",
          breaks=quantile(dichtheid$dichtheid[str_starts(dichtheid$WijkenEnBuurten, "WK")], seq(0, 1, 0.2), na.rm=T))
