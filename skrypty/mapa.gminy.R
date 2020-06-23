library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)
library(viridis)

load("./dane/ludnosc.Rda")
load("./dane/komisje.Rda")

## wgrywamy mapę Polski
shp1 <- readOGR("./mapa/gminy", layer = "Gminy")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "JPT_KOD_JE")
#usuwamy ostatnią cyfrę z terytu shp1f
shp1f$id <- str_sub(shp1f$id, 1, 6)

## wgrywamy mapę Warszawy
shp2 <- readOGR("./mapa/Warszawa", layer = "Warsaw_AL9")
#zmieniamy format danych
shp2f <- fortify(shp2, region = "id")

temat <- theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
               panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), 
               legend.position = c(0.1, 0.1))

ggplot() +
  geom_map(data=ilosc.komisji, aes(map_id=TERYT.gminy, fill=ilosc.punktow), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "ilość lokali wyborczych",
       fill="ilość punktów")+
  scale_fill_gradient(low = "red", high = "blue")+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_map(data=filter(ilosc.komisji, TERYT.gminy %in%Warszawa), aes(map_id=TERYT.gminy, fill=ilosc.punktow), map=shp2f) + 
  geom_path(data = shp2f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  scale_fill_gradient(low = "white", high = "blue")+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

#mapa ludnosci
a <- ludnosc.gmina

png("./wykresy/gminy.ludnosc.png", units="in", width=9, height=9, res=600)
ggplot() +
  geom_map(data=a, aes(map_id=teryt, fill=ludnosc.calosc), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "ilość ludności",
       fill="ilość ludności")+
  scale_fill_viridis(values = scales::rescale(c(5000,10000,50000,100000,200000)), labels = comma, breaks= c(100000,3e5, 5e5, 7e5), option = "C", begin = 1, end = 0)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

ggplot(ludnosc.gmina, aes(x=miasto.wies, y=ludnosc.calosc))+
  geom_col(fill="blue")+
  facet_wrap(~wojewodztwo, ncol=4, scales = "free")+
  scale_y_continuous(labels = comma)+
  theme_bw()
