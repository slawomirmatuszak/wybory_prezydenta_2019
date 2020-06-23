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


## wgrywamy mapę
shp1 <- readOGR("./mapa/Powiaty", layer = "Powiaty")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "JPT_KOD_JE")

# mapa ludnosci

temat <- theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
               panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), 
               legend.position = c(0.1, 0.1))

png("./wykresy/powiaty.ludnosc.png", units="in", width=9, height=9, res=600)
ggplot() +
  geom_map(data=ludnosc.pow, aes(map_id=teryt, fill=ludnosc.calosc), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "ilość ludności",
       fill="ilość",
       caption = "Dane: GUS")+
  scale_fill_viridis(values = scales::rescale(c(30000,100000,300000, 5e5)), labels = comma, breaks= c(1e5,5e5, 1e6,15e5), option = "C", begin = 1, end = 0)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
