library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

Warszawa <- c("146502", "146503", "146504", "146505", "146506", "146507", "146508", "146509", "146510", "146511", "146512", "146513", "146514", "146515", "146516",
              "146517", "146518", "146519")

I.tura <- read.csv2("./dane/prezydent_2015_tura1.csv", encoding = "ANSI", stringsAsFactors = F)

Wawa <- I.tura %>%
  mutate(TERYT.gminy=as.character(TERYT.gminy))%>%
  mutate(TERYT.gminy = if_else(nchar(TERYT.gminy)==5, paste0("0",TERYT.gminy),TERYT.gminy))%>%
  select(1:4,7,24:37)%>%
  filter(TERYT.gminy%in%Warszawa)%>%
  summarise_if(is.numeric,sum)%>%
  select(-c(2,6))%>%
  mutate(TERYT.gminy = "146501")

I.tura <- I.tura %>%
  mutate(TERYT.gminy=as.character(TERYT.gminy))%>%
  mutate(TERYT.gminy = if_else(nchar(TERYT.gminy)==5, paste0("0",TERYT.gminy),TERYT.gminy))%>%
  select(1:4,7,24:37)%>%
  group_by(TERYT.gminy)%>%
  summarise_if(is.numeric,sum)%>%
  select(-c(2,6))%>%
  bind_rows(Wawa)

I.tura.long.gminy <- I.tura %>%
  pivot_longer(cols = c(5:15), names_to="kandydat", values_to="glosy")%>%
  group_by(TERYT.gminy)%>%
  arrange(TERYT.gminy,desc(glosy))%>%
  mutate(miejsce=row_number())%>%
  mutate(kandydat = gsub("Andrzej.Sebastian.Duda","Andrzej Duda", kandydat),
         kandydat = gsub("Bronisław.Maria.Komorowski","Bronisław Komorowski", kandydat),
         kandydat = gsub("Paweł.Piotr.Kukiz","Paweł Kukiz", kandydat),
         kandydat = gsub("Magdalena.Agnieszka.Ogórek","Magdalena Ogórek", kandydat),
         kandydat = gsub("Adam.Sebastian.Jarubas","Adam Jarubas", kandydat))

#wykres ilość glosów
ggplot(I.tura.long.gminy, aes(x=reorder(kandydat, -glosy), y=glosy))+
  geom_col()


# mapy gminy--------------------------------------------------------------------

## wgrywamy mapę Polski
shp1 <- readOGR("./mapa/gminy", layer = "Gminy")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "JPT_KOD_JE")
#usuwamy ostatnią cyfrę z terytu shp1f
shp1f$id <- str_sub(shp1f$id, 1, 6)

## kolory przypisujemy
kolor <- c("Andrzej Duda" = "blue", "Bronisław Komorowski" = "orange", "Paweł Kukiz" = "sienna4", "Adam Jarubas" = "darkgreen", "Magdalena Ogórek" = "red")

# I miejsce

temat <- theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
               panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), 
               legend.position = c(0.1, 0.1))

a <- filter(I.tura.long.gminy, miejsce==1)

ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=kandydat), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "I miejsce",
       fill="kandydat")+
  scale_fill_manual(values = kolor)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

a <- filter(I.tura.long.gminy, miejsce==2)

ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=kandydat), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "II miejsce",
       fill="kandydat")+
  scale_fill_manual(values = kolor)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

a <- filter(I.tura.long.gminy, miejsce==3)

ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=kandydat), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "III miejsce",
       fill="kandydat")+
  scale_fill_manual(values = kolor)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

# ilość głosów na dudę
a <- I.tura %>%
  mutate(duda=Andrzej.Sebastian.Duda/Liczba.głosów.ważnych)

ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=duda), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "procent głosów na Dudę",
       fill="poparcie")+
  scale_fill_gradient(low="white", high="blue", labels = percent)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

#ilość głosów na Komora
a <- I.tura %>%
  mutate(komorowski=Bronisław.Maria.Komorowski/Liczba.głosów.ważnych)

ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=komorowski), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "procent głosów na Komorowskiego",
       fill="poparcie")+
  scale_fill_gradient(low="white", high="orange", labels = percent)+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))

# gdzie Komor przegrał z Dudą
a <- I.tura %>%
  mutate(komor.duda=if_else(Bronisław.Maria.Komorowski<Andrzej.Sebastian.Duda, "Dudy", "Komorowskiego"))

png("./wykresy/2015.duda.komor.png", units="in", width=9, height=9, res=600)
ggplot() +
  geom_map(data=a, aes(map_id=TERYT.gminy, fill=komor.duda), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.25) + 
  coord_map(projection = "mercator") + 
  labs(title = "Porównanie wyników Komorowskiego i Dudy w I turze wyborów w 2015 r.",
       fill="przewaga",
       caption = "Dane: Państwowa Komisja Wyborcza")+
  scale_fill_manual(values=c("Dudy"="blue", "Komorowskiego"="orange"))+
  theme_bw()+
  temat+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
