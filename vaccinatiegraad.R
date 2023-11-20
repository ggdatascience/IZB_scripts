# vaccinatiegraad per gemeente in grafieken
# op vraag van MT/communicatie

library(cbsodataR)
library(tidyverse)
library(this.path)
library(sf)
library(tmap)

setwd(dirname(this.path()))

# wrapper voor sprintf, voor makkelijk weergeven
printf = function (...) cat(paste(sprintf(...),"\n"))

# GGD NOG kleuren
nog_colors = c("#1D1756", "#D1005D", "#2F6594", "#D3DFF1", "#FFBDDB")
# omdraaien om te matchen met vorige grafieken
#nog_colors = c("#D1005D", "#8d96aa", "#1D1756", "#D3DFF1", "#FFBDDB")
nog_palette = c("#FAE6EF","#F6CCDF","#ED99BE","#E3669E","#D1005D")

meta = cbs_get_meta("50117NED", catalog="RIVM") # 50093NED = 2022, 50082NED = 2021

gemeenten.nog = "Aalten, Apeldoorn, Berkelland, Bronckhorst, Brummen, Doetinchem, Elburg, Epe, Ermelo, Harderwijk, Hattem, Heerde, Lochem, Montferland, Nunspeet, Oldebroek, Oost Gelre, Oude IJsselstreek, Putten, Voorst, Winterswijk, Zutphen"
gemeenten.nog = str_trim(unlist(str_split(gemeenten.nog, ",")))
gemeenten.yunio = gemeenten.nog[str_detect(gemeenten.nog, "Aalten|Berkelland|Bronckhorst|Doetinchem|Lochem|Montferland|Oost Gelre|Oude IJsselstreek|Winterswijk|Zutphen")]
codes.nog = data.frame(code=meta$RegioS$Key[meta$RegioS$Title %in% gemeenten.nog], naam=meta$RegioS$Title[meta$RegioS$Title %in% gemeenten.nog])
data.rivm = cbs_get_data("50117NED", catalog="RIVM", RegioS=c("NL01  ", "GG1413", codes.nog$code))
data.rivm = data.rivm %>%
  cbs_add_label_columns() %>%
  mutate(Jaar=as.numeric(str_extract(Perioden, "\\d+")),
         RegioS=str_trim(RegioS)) %>%
  rename(Populatie=Populatie_1, Gevaccineerden=Gevaccineerden_2, Vaccinatiegraad=Vaccinatiegraad_3)

data.nl = data.rivm %>% filter(RegioS == "NL01")
data.ggd = data.rivm %>% filter(RegioS == "GG1413")

kaartdata.gemeente = st_read("../../Gedeelde documenten - NOG W Openbare data/Kaarten/cbsgebiedsindelingen_2022_v1.gpkg", layer = "cbs_gemeente_2021_gegeneraliseerd")
kaartdata.gemeente = kaartdata.gemeente %>%
  filter(statcode %in% str_trim(codes.nog$code))

# speciale grafiek voor ambtelijk overleg
afname = data.ggd %>%
  group_by(Vaccinaties_label) %>%
  mutate(Verschil=Vaccinatiegraad-lag(Vaccinatiegraad))

# poging 1
png("vaccinatiegraad_AB_poging1.png", width=1000, height=700)
oldpar = par(oma=c(0,0,0,0), mar=c(2,4,2,3))
layout(matrix(c(1,2,3,1,2,3,4,5,6), nrow=3, ncol=3, byrow=T))
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")],
     type="l", col=nog_colors[2], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(80, 100), xlim=c(2016, 2023),
     main="Volledige deelname RVP")
grid(nx=NA, ny=NULL)
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "BMR basis")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "BMR basis")],
     type="l", col=nog_colors[1], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(80, 100), xlim=c(2016, 2023),
     main="BMR basisimmuun (2 jaar)")
grid(nx=NA, ny=NULL)
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")],
     type="l", col=nog_colors[3], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(0, 10), xlim=c(2016, 2023),
     main="Geen enkele vaccinatie (2 jaar)")
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig") & afname$Jaar >= 2016], col=nog_colors[2], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig") & afname$Jaar >= 2016], col=nog_colors[2], ylim=c(-5, 1), add=T)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis") & afname$Jaar >= 2016], col=nog_colors[1], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis") & afname$Jaar >= 2016], col=nog_colors[1], ylim=c(-5, 1), add=T)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1), add=T)
dev.off()

# poging 2
png("vaccinatiegraad_AB_poging2.png", width=1000, height=700)
oldpar = par(mar=c(2,4,2,3))
layout(matrix(c(1,1,2,1,1,2,1,1,2,3,3,5,4,4,5), nrow=5, ncol=3, byrow=T))
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")],
     type="l", col=nog_colors[2], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(80, 100), xlim=c(2016, 2023),
     main="Vaccinatiegraad (2 jaar)")
grid(nx=NA, ny=NULL)
lines(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")],
      col=nog_colors[2], lwd=2)
lines(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "BMR basis")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "BMR basis")],
     col=nog_colors[1], lwd=2)
legend("topright", legend=c("Volledige deelname RVP", "BMR basisimmuun"), col=nog_colors[c(2, 1)], lwd=2, lty="solid")
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")],
     type="l", col=nog_colors[3], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(0, 10), xlim=c(2016, 2023),
     main="Geen enkele vaccinatie (2 jaar)")
grid(nx=NA, ny=NULL)
#par(oma=c(2,4,1,4))
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig") & afname$Jaar >= 2016], col=nog_colors[2], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig") & afname$Jaar >= 2016], col=nog_colors[2], ylim=c(-5, 1), add=T)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis") & afname$Jaar >= 2016], col=nog_colors[1], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis") & afname$Jaar >= 2016], col=nog_colors[1], ylim=c(-5, 1), add=T)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1), add=T)
dev.off()

displaydata = matrix(nrow=2, ncol=8)
rownames(displaydata) = c("Volledige deelname RVP", "BMR basisimmuun")
displaydata[1,] = afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig") & afname$Jaar >= 2016]
displaydata[2,] = afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis") & afname$Jaar >= 2016]

barplot(displaydata, col=nog_colors[c(2,1)], ylim=c(-5, 1), beside=T)
grid(nx=NA, ny=NULL)
barplot(displaydata, col=nog_colors[c(2,1)], ylim=c(-5, 1), beside=T, add=T)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1))
grid(nx=NA, ny=NULL)
barplot(afname$Verschil[str_detect(afname$Vaccinaties_label, "Geen enkele") & afname$Jaar >= 2016], col=nog_colors[3], ylim=c(-5, 1), add=T)

# poging 3
png("vaccinatiegraad_AB_poging3.png", width=1200, height=700)
layout(matrix(c(1,1,2), nrow=3, ncol=1))
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")],
     type="l", col=nog_colors[2], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(80, 100), xlim=c(2016, 2023),
     main="Vaccinatiegraad (2 jaar)")
grid(nx=NA, ny=NULL)
lines(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")],
      col=nog_colors[2], lwd=2)
text(last(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Volledig")]), last(data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Volledig")]),
     last(afname$Verschil[str_detect(afname$Vaccinaties_label, "Volledig")]), pos=1)
lines(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "BMR basis")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "BMR basis")],
      col=nog_colors[1], lwd=2)
text(last(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "BMR basis")]), last(data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "BMR basis")]),
     last(afname$Verschil[str_detect(afname$Vaccinaties_label, "BMR basis")]), pos=1)
legend("topright", legend=c("Volledige deelname RVP", "BMR basisimmuun"), col=nog_colors[c(2, 1)], lwd=2, lty="solid")
plot(data.ggd$Jaar[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")], data.ggd$Vaccinatiegraad[str_detect(data.ggd$Vaccinaties_label, "Geen enkele")],
     type="l", col=nog_colors[3], lwd=2, xlab=NA, ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=c(0, 10), xlim=c(2016, 2023),
     main="Geen enkele vaccinatie (2 jaar)")
grid(nx=NA, ny=NULL)
dev.off()

#
# algemeen script
#

# er worden veel grafieken op identieke wijze geexporteerd; eenmalige functie
save_img = function (name, vaccination, graphdata, fig.w, fig.h, size.factor) {
  # logische y-limiet uitrekenen
  # de meeste waardes zitten tussen 80 en 100
  if (min(graphdata, na.rm=T) > 80) ylim=c(75,100)
  else if (max(graphdata, na.rm=T) < 10) ylim=c(0,10) # voor geen deelname aan RVP
  else {
    range = pretty(graphdata)
    ylim = c(min(range), max(range))
  }
  
  png(sprintf("graphs/%s_%s.png", name, vaccination), width=fig.w, height=fig.h)
  par(cex.axis=size.factor/3)
  
  # we willen niet dat er 2020.5 op de as komt te staan, dus dan moet 't handmatig
  if (nrow(graphdata) < 5) {
    plot(rownames(graphdata), graphdata[,1], col=nog_colors[1], type="l", lwd=size.factor,
         xlab=NA, ylab=NA, frame.plot=F, axes=F,
         ylim=ylim, cex=size.factor)
    grid(nx=NA, ny=NULL)
    #abline(h=ifelse(max(ylim) == 100, 90, ifelse(max(ylim) == 10, 5, sum(ylim)/2)), lwd=size.factor/2, col="lightgrey", lty=2)
    par(new=T)
    plot(rownames(graphdata), graphdata[,1], col=nog_colors[1], type="l", lwd=size.factor,
         xlab=NA, ylab=NA, frame.plot=F, axes=F,
         ylim=ylim, cex=size.factor)
    ticks = axTicks(1)
    ticks[!(ticks %in% rownames(graphdata))] = ""
    axis(1, cex=size.factor, at=axTicks(1), labels=ticks)
    axis(2, cex=size.factor)
  } else {
    plot(rownames(graphdata), graphdata[,1], col=nog_colors[1], type="l", lwd=size.factor,
         xlab=NA, ylab=NA, frame.plot=F,
         ylim=ylim, cex=size.factor)
    grid(nx=NA, ny=NULL)
    #abline(h=ifelse(max(ylim) == 100, 90, ifelse(max(ylim) == 10, 5, sum(ylim)/2)), lwd=size.factor/2, col="lightgrey", lty=2)
    par(new=T)
    plot(rownames(graphdata), graphdata[,1], col=nog_colors[1], type="l", lwd=size.factor,
         xlab=NA, ylab=NA, frame.plot=F,
         ylim=ylim, cex=size.factor)
  }
  lines(rownames(graphdata), graphdata[,2], col=nog_colors[2], lwd=size.factor)
  lines(rownames(graphdata), graphdata[,3], col=nog_colors[3], lwd=size.factor)
  #text(mean(as.numeric(rownames(graphdata))), ifelse(max(ylim) == 100, 90, ifelse(max(ylim) == 10, 5, sum(ylim)/2)), "Gewenste vaccinatiegraad",
  #     pos=3, adj=1)
  #rect(xleft=rownames(graphdata)[1], ybottom=min(ylim), xright=rownames(graphdata)[nrow(graphdata)], ytop=ifelse(max(ylim) == 100, 90, ifelse(max(ylim) == 10, 5, sum(ylim)/2)),
  #     density=5, angle=45, col="lightgrey")
  
  legend("bottomright", legend=c("Gemeente", "NOG", "NL"), fill=nog_colors[1:3], inset=c(0.02, 0.02), cex=size.factor/3)
  
  dev.off()
}

fig.h = 742/1.4
fig.w = 742
size.factor = 5.0
for (i in 1:nrow(codes.nog)) {
  code = codes.nog$code[i]
  naam = codes.nog$naam[i]
  
  data.gemeente = data.rivm %>%
    filter(RegioS == code)
  
  # DKTP 2 jr 2015-2019 (-> 2017+) - A028769
  data = data.gemeente %>%
    filter(Vaccinaties == "A028769", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 3
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028769" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028769" & data.nl$Jaar >= 2017]
  
  save_img(naam, "DKTP_2jr", graphdata, fig.w, fig.h, size.factor)
  
  # BMR 2 jr 2015-2019 (-> 2017+) - A028771
  data = data.gemeente %>%
    filter(Vaccinaties == "A028771", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 3
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028771" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028771" & data.nl$Jaar >= 2017]
  
  save_img(naam, "BMR_2jr", graphdata, fig.w, fig.h, size.factor)
  
  # MenC/ACWY 2jr 2015-2019 (-> 2017+) - A028772
  data = data.gemeente %>%
    filter(Vaccinaties == "A028772", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 3
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028772" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028772" & data.nl$Jaar >= 2017]
  
  save_img(naam, "MenACWY_2jr", graphdata, fig.w, fig.h, size.factor)
  
  # geen vaccinatie 2jr 2015-2019 (-> 2017+) - A041828
  data = data.gemeente %>%
    filter(Vaccinaties == "A041828", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 3
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A041828" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A041828" & data.nl$Jaar >= 2017]
  
  save_img(naam, "nul_vacc_2jr", graphdata, fig.w, fig.h, size.factor)
  
  # volledige deelname 2jr 2015-2019 (-> 2017+)  - A028775
  data = data.gemeente %>%
    filter(Vaccinaties == "A028775", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 3
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028775" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028775" & data.nl$Jaar >= 2017]
  
  save_img(naam, "vol_rvp_2jr", graphdata, fig.w, fig.h, size.factor)
  
  # DTP 10 jr 2007-2011 (-> 2018+) - A028778
  data = data.gemeente %>%
    filter(Vaccinaties == "A028778", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 11
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028778" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028778" & data.nl$Jaar >= 2017]
  
  save_img(naam, "DKTP_10jr", graphdata, fig.w, fig.h, size.factor)
  
  # BMR 10 jr 2007-2011 (-> 2017+) - A028779
  data = data.gemeente %>%
    filter(Vaccinaties == "A028779", Jaar >= 2017)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 11
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028778" & data.ggd$Jaar >= 2017]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028778" & data.nl$Jaar >= 2017]
  
  save_img(naam, "BMR_10jr", graphdata, fig.w, fig.h, size.factor)
  
  # HPV 14 jr 2001-2007 (-> 2015+) - A028780
  data = data.gemeente %>%
    filter(Vaccinaties == "A028780", Jaar >= 2015)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "RegioS NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 15
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = data.ggd$Vaccinatiegraad[data.ggd$Vaccinaties == "A028780" & data.ggd$Jaar >= 2015]
  graphdata[,3] = data.nl$Vaccinatiegraad[data.nl$Vaccinaties == "A028780" & data.nl$Jaar >= 2015]
  
  save_img(naam, "HPV_14jr", graphdata, fig.w, fig.h, size.factor)
  
  # MenC/ACWY 15jr 2001 (-> 2016+) - A050956 en A049175
  data = data.gemeente %>%
    filter(Vaccinaties %in% c("A050956", "A049175"), Jaar >= 2016, !is.na(Vaccinatiegraad)) %>%
    group_by(Jaar) %>%
    summarise(Vaccinatiegraad=max(Vaccinatiegraad,na.rm=T)) %>%
    arrange(Jaar)
  menc.ggd = data.ggd %>%
    filter(Vaccinaties %in% c("A050956", "A049175"), Jaar >= 2016, !is.na(Vaccinatiegraad)) %>%
    group_by(Jaar) %>%
    summarise(Vaccinatiegraad=max(Vaccinatiegraad,na.rm=T)) %>%
    arrange(Jaar)
  menc.nl = data.nl %>%
    filter(Vaccinaties %in% c("A050956", "A049175"), Jaar >= 2016, !is.na(Vaccinatiegraad)) %>%
    group_by(Jaar) %>%
    summarise(Vaccinatiegraad=max(Vaccinatiegraad,na.rm=T)) %>%
    arrange(Jaar)
  
  graphdata = matrix(NA, nrow=nrow(data), ncol=3)
  colnames(graphdata) = c(naam, "Regio NOG", "Nederland")
  rownames(graphdata) = data$Jaar - 16
  graphdata[,1] = data$Vaccinatiegraad
  graphdata[,2] = menc.ggd$Vaccinatiegraad
  graphdata[,3] = menc.nl$Vaccinatiegraad
  
  save_img(naam, "MenACWY_15jr", graphdata, fig.w, fig.h, size.factor)
}

# overzicht van de regio waarin Yunio werkt (10 gemeentes)
vacc.types = unique(data.rivm$Vaccinaties_label)
for (vacc in vacc.types) {
  leeftijd = str_match(vacc, "\\((\\d+) jaar\\)")
  if (is.na(leeftijd[1,2]))
    next
  leeftijd = as.numeric(leeftijd[1,2])
  
  data.vacc.regio = data.rivm %>%
    filter(Vaccinaties_label == vacc,
           RegioS %in% codes.nog$code[codes.nog$naam %in% gemeenten.yunio]) %>%
    group_by(Jaar) %>%
    summarize(Populatie=sum(Populatie, na.rm=T), Gevaccineerden=sum(Gevaccineerden, na.rm=T),
              Vaccinatiegraad=round(Gevaccineerden/Populatie*100,1)) %>%
    mutate(Cohort=Jaar-leeftijd-1) %>%
    slice_tail(n=5)
  data.vacc.nl = data.nl %>%
    filter(Vaccinaties_label == vacc) %>%
    mutate(Cohort=Jaar-leeftijd-1) %>%
    slice_tail(n=5)
  
  if (min(data.vacc.regio$Vaccinatiegraad, na.rm=T) > 80) ylim=c(75,100)
  else if (max(data.vacc.regio$Vaccinatiegraad, na.rm=T) < 10) ylim=c(0,10) # voor geen deelname aan RVP
  else {
    range = pretty(c(data.vacc.regio$Vaccinatiegraad, data.vacc.nl$Vaccinatiegraad))
    ylim = c(min(range), max(range))
  }
  
  png(sprintf("graphs/Yunio_%s.png", str_replace_all(vacc, "\\W+", "_")), width=800, height=600)
  plot(data.vacc.nl$Cohort, data.vacc.nl$Vaccinatiegraad, col=nog_colors[1], type="l", lwd=2,
       xlab="Geboortecohort", ylab="Vaccinatiegraad (%)", frame.plot=F, ylim=ylim, axes=F,
       main=vacc)
  grid(nx=NA, ny=NULL)
  lines(data.vacc.nl$Cohort, data.vacc.nl$Vaccinatiegraad, col=nog_colors[1], lwd=2)
  lines(data.vacc.regio$Cohort, data.vacc.regio$Vaccinatiegraad, col=nog_colors[2], lwd=2)
  abline(h=90, lty="dashed")
  legend("topright", legend=c("Nederland", "Regionaal"), fill=nog_colors[1:2])
  ticks = pretty(data.vacc.regio$Cohort)
  if (length(unique(data.vacc.regio$Cohort)) <= 2)
    ticks = unique(data.vacc.regio$Cohort)
  axis(1, at=ticks, labels=T)
  axis(2)
  dev.off()
}

# overzicht van de hele regio
vacc.types = unique(data.rivm$Vaccinaties_label)
for (vacc in vacc.types) {
  data.vacc = data.rivm %>%
    filter(Vaccinaties_label == vacc,
           RegioS %in% c("NL01", "GG1413"),
           !is.na(Vaccinatiegraad)) # sommige vaccinaties zijn pas later bijgehouden; eerdere jaren schrappen
  
  if (min(data.vacc$Vaccinatiegraad, na.rm=T) > 80) ylim=c(75,100)
  else if (max(data.vacc$Vaccinatiegraad, na.rm=T) < 10) ylim=c(0,10) # voor geen deelname aan RVP
  else {
    range = pretty(data.vacc$Vaccinatiegraad)
    ylim = c(min(range), max(range))
  }
  
  png(sprintf("graphs/NOG_%s.png", str_replace_all(vacc, "\\W+", "_")), width=800, height=600)
  plot(data.vacc$Jaar[data.vacc$RegioS_label == "Nederland"], data.vacc$Vaccinatiegraad[data.vacc$RegioS_label == "Nederland"], col=nog_colors[1], type="l", lwd=2,
       xlab=NA, ylab=NA, frame.plot=F, ylim=ylim, axes=F,
       main=vacc)
  grid(nx=NA, ny=NULL)
  lines(data.vacc$Jaar[data.vacc$RegioS_label == "Nederland"], data.vacc$Vaccinatiegraad[data.vacc$RegioS_label == "Nederland"], col=nog_colors[1], lwd=2)
  lines(data.vacc$Jaar[data.vacc$RegioS_label == "GGD Noord- en Oost-Gelderland"], data.vacc$Vaccinatiegraad[data.vacc$RegioS_label == "GGD Noord- en Oost-Gelderland"],
        col=nog_colors[2], lwd=2)
  abline(h=90, lty="dashed")
  legend("topright", legend=c("Nederland", "NOG"), fill=nog_colors[1:2])
  ticks = pretty(data.vacc$Jaar)
  if (length(unique(data.vacc$Jaar)) <= 2)
    ticks = unique(data.vacc$Jaar)
  axis(1, at=ticks, labels=T)
  axis(2)
  dev.off()
}

# kaartjes van de laatste vaccinatiegraad over de hele regio
for (vacc in vacc.types) {
  data.vacc = data.rivm %>%
    filter(Vaccinaties_label == vacc,
           !RegioS %in% c("NL01", "GG1413"))
  
  # alleen het laatste jaar voor de kaarten
  jaar = max(data.vacc$Jaar)
  data.vacc = data.vacc %>%
    filter(Jaar == jaar) %>%
    mutate(label=sprintf("%s\n%.0f%%", RegioS_label, Vaccinatiegraad))
  png(sprintf("graphs/regionaal_%s_%d.png", str_replace_all(vacc, "\\W+", "_"), jaar), width=1400, height=1000)
  print(tm_shape(kaartdata.gemeente %>% left_join(data.vacc, by=c("statcode"="RegioS"))) +
    tm_fill(col="Vaccinatiegraad",
            palette=nog_palette,
            breaks = quantile(data.vacc$Vaccinatiegraad, c(seq(0,1,0.2)), na.rm=T),
            title="Vaccinatiegraad",
            legend.show=T,
            legend.format=list(fun=function(x) { return(sprintf("%.0f%%", x)) },
                               text.separator="tot"),
            legend.position=c("top", "right"),
            legend.title.fontface = "bold") +
    tm_borders(alpha=0.5, col=nog_colors[1]) +
    tm_text(text="label", size="AREA", scale=1.5, size.lowerbound=0.8, print.tiny=T) +
    tm_layout(main.title=sprintf("Vaccinatiegraad %s in %d", vacc, jaar),
              main.title.size=2,
              legend.title.size=2,
              legend.text.size=1.5,
              frame=F))
  dev.off()
}

totaal = data.rivm %>%
  filter(RegioS %in% c("NL01", "GG1413"),
         !str_detect(Vaccinaties_label, "HPV|RVP|Geen enkele")) %>%
  group_by(RegioS_label, Jaar) %>%
  summarize(Populatie=sum(Populatie, na.rm=T), Gevaccineerden=sum(Gevaccineerden, na.rm=T)) %>%
  mutate(Vaccinatiegraad=Gevaccineerden/Populatie*100)

png("graphs/regionaal_landelijk_vaccinatiegraad.png", width=800, height=600)
plot(totaal$Jaar[totaal$RegioS_label == "Nederland"], totaal$Vaccinatiegraad[totaal$RegioS_label == "Nederland"], col=nog_colors[1], type="l", lwd=2,
     xlab=NA, ylab=NA, frame.plot=F, ylim=c(80, 100), axes=F)
grid(nx=NA, ny=NULL)
lines(totaal$Jaar[totaal$RegioS_label == "Nederland"], totaal$Vaccinatiegraad[totaal$RegioS_label == "Nederland"], col=nog_colors[1], lwd=2)
lines(totaal$Jaar[totaal$RegioS_label == "GGD Noord- en Oost-Gelderland"], totaal$Vaccinatiegraad[totaal$RegioS_label == "GGD Noord- en Oost-Gelderland"],
      col=nog_colors[2], lwd=2)
legend("topright", legend=c("Nederland", "NOG"), fill=nog_colors[1:2])
axis(1, at=pretty(totaal$Jaar), labels=T)
axis(2)
dev.off()

# afname in recente jaren berekenen
afname = data.rivm %>%
  filter(!RegioS %in% c("NL01", "GG1413")) %>%
  group_by(RegioS_label, Vaccinaties_label) %>%
  mutate(Verschil=Vaccinatiegraad-lag(Vaccinatiegraad)) %>%
  filter(Jaar == 2023)

png("graphs/regionaal_heatmap_vaccinatiegraad.png", width=1000, height=800)
ggplot(afname %>% filter(!Vaccinaties_label %in% c("D(K)TP gerevaccineerd (5 jaar)", "MenACWY volledig (cohorten 2001-2005)")),
       aes(x=RegioS_label, y=Vaccinaties_label, fill=Verschil)) +
  geom_tile() +
  scale_fill_gradient2(low="red", mid="white", high="darkblue", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90)) +
  labs(x="", y="", title="Toe- en afname vaccinatiegraad")
dev.off()

afname = data.rivm %>%
  filter(RegioS %in% c("NL01", "GG1413")) %>%
  group_by(RegioS_label, Vaccinaties_label) %>%
  mutate(Verschil=Vaccinatiegraad-lag(Vaccinatiegraad))

afname %>%
  ungroup() %>%
  filter(Jaar == max(Jaar, na.rm=T)) %>%
  arrange(Verschil) %>%
  select(Vaccinaties_label, RegioS_label, Vaccinatiegraad, Verschil)

afname %>%
  ungroup() %>%
  filter(Jaar == max(Jaar, na.rm=T),
         Vaccinatiegraad >= 90) %>%
  select(Vaccinaties_label, RegioS_label, Vaccinatiegraad, Verschil)

# valt er nog wat af te leiden uit de aantallen?
model = lm(Vaccinatiegraad ~ Jaar + Vaccinaties_label + RegioS_label, data=data.rivm %>% filter(!RegioS %in% c("NL01", "GG1413")))
summary(model)
# zoals verwacht: Nunspeet --, Apeldoorn/Elburg/Ermelo/Oldebroek/Putten/Zutphen -, Berkelland/Montferland/Oost Gelre/Oude IJsselstreek +
# HPV volledig 14 is erg laag

col.palette = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")

# samenstelling bevolking en deelname vaccinatie
inwoners = read.csv("inwoners_gemeente_per_leeftijd_2022.csv")
inwoners = inwoners %>% 
  mutate(Leeftijdscategorie=cut(Leeftijd, breaks=c(0, 3, 12, 18, 29, 39, 49, 59, 69, 79, Inf),
                                labels=c("0-3", "4-12", "13-18", "19-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                                include.lowest=T)) %>%
  group_by(Gemeente, Leeftijdscategorie) %>%
  summarise(n=sum(n))

# metadata ophalen voor het zoeken van gemeentes
meta = cbs_get_meta("84773NED")
gemeenten = meta$WijkenEnBuurten %>%
  filter(Key %in% codes.nog$code)

# opleidingen
opleiding = cbs_add_label_columns(cbs_get_data("84773NED", WijkenEnBuurten=gemeenten$Key, Marges="MW00000"))
opleiding.pergroep = opleiding %>%
  group_by(Opleidingsniveau_label, WijkenEnBuurten_label) %>%
  summarise(n=sum(Bevolking15Tot75Jaar_2))

png("vacc_vs_demografie.png", width=2200, height=1600)
par(mfrow=c(2,1), oma=c(0,0,0,4))
#layout(matrix(c(1,1,1,1,2,2,3,3,3,3), ncol=2, byrow=T))

graphdata = inwoners %>%
  pivot_wider(names_from=Gemeente, values_from=n) %>%
  mutate(across(where(is.numeric), ~.x/sum(.x)*100)) %>%
  column_to_rownames("Leeftijdscategorie") %>%
  as.matrix()
barplot(graphdata, beside=T, col=col.palette, ylab="Percentage", las=2, cex.names=1.25, cex.axis=1.25, cex=1.25)
legend("right", legend=rownames(graphdata), fill=col.palette, cex=1.25, xpd=NA, inset=c(-0.025, 0))

# graphdata = opleiding.pergroep %>%
#   pivot_wider(names_from=WijkenEnBuurten_label, values_from=n) %>%
#   column_to_rownames("Opleidingsniveau_label") %>%
#   mutate(across(everything(), ~.x/sum(.x)*100)) %>%
#   as.matrix()
# barplot(graphdata, beside=T, col=nog_colors[1:3], ylab="Percentage", las=2, cex.names=1.25, cex.axis=1.25, cex=1.25)
# legend("right", legend=rownames(graphdata), fill=nog_colors[1:3], cex=1.25, xpd=NA, inset=c(-0.025, 0))

data.dktp = data.rivm %>%
  filter(str_starts(RegioS, "GM"), Vaccinaties == "A028778", Jaar > 2010) %>% # DKTP volledig 10 jr
  group_by(RegioS_label) %>%
  summarise(DKTP=mean(Vaccinatiegraad, na.rm=T))

data.bmr = data.rivm %>%
  filter(str_starts(RegioS, "GM"), Vaccinaties == "A028779", Jaar > 2010) %>% # BMR volledig 10 jr
  group_by(RegioS_label) %>%
  summarise(BMR=mean(Vaccinatiegraad, na.rm=T))

data.hpv = data.rivm %>%
  filter(str_starts(RegioS, "GM"), Vaccinaties == "A028780", Jaar > 2010) %>% # BMR volledig 14 jr
  group_by(RegioS_label) %>%
  summarise(HPV=mean(Vaccinatiegraad, na.rm=T))

data.men = data.rivm %>%
  filter(str_starts(RegioS, "GM"), Vaccinaties == "A050956", Jaar > 2010) %>% # BMR volledig 14 jr
  group_by(RegioS_label) %>%
  summarise(MenACWY=mean(Vaccinatiegraad, na.rm=T))

graphdata = data.dktp %>%
  full_join(data.bmr, by="RegioS_label") %>%
  full_join(data.hpv, by="RegioS_label") %>%
  full_join(data.men, by="RegioS_label") %>%
  column_to_rownames("RegioS_label") %>%
  as.matrix() %>%
  t()

barplot(graphdata, beside=T, col=nog_colors[1:4], ylab="Percentage", axisnames=F, cex.axis=1.25, cex=1.25)
legend("right", legend=rownames(graphdata), fill=nog_colors[1:4], cex=1.25, xpd=NA, inset=c(-0.035, 0))

dev.off()
        