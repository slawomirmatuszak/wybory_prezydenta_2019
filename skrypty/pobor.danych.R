library(tidyverse)
library(lubridate)
library(readxl)
library(scales)


# Pobór danych ------------------------------------------------------------

url <- "https://wybory.gov.pl/prezydent20200628/data/csv/obwody_glosowania_csv.zip"
download.file(url, file.path("./dane/obwody_komisje.zip"))
unzip(zipfile = "./dane/obwody_komisje.zip", exdir = "./dane")
file.remove("./dane/obwody_komisje.zip")

komisje <- read_csv2(file = "./dane/obwody_glosowania.csv")
names(komisje) <- gsub(" ", ".", names(komisje))

#filtr komisji w Warszawie
Warszawa <- komisje %>%
  filter(Powiat=="Warszawa")%>%
  select(TERYT.gminy)%>%
  unique()%>%
  pull()

#łączenie wyników z dzielnic w Wawie w jeden
Wawa <- komisje%>%
  filter(TERYT.gminy %in% Warszawa)%>%
  summarise(
    ilosc.punktow = n()
  )%>%
  mutate(TERYT.gminy = "146501")

ilosc.komisji <- komisje %>%
  group_by(TERYT.gminy)%>%
  summarise(
    ilosc.punktow = n()
  )%>%
  bind_rows(Wawa)


# kandydaci ---------------------------------------------------------------

kandydaci <- read_xlsx("./dane/kandydaci.xlsx")%>%
  mutate(nazwa = word(Imiona,1))%>%
  unite(col=nazwa, nazwa, Nazwisko, sep=" ")%>%
  mutate(nazwa = str_to_title(nazwa))

# Ludność gmin, powiatów i wojewodztw -------------------------------------------------

ludnosc <- read_xlsx("./dane/gminy_ludnosc.xlsx", sheet = 2) %>%
  slice(-c(1:6))%>%
  rename(teryt=1, wies=4, miasto=3)%>%
  mutate(miasto = as.numeric(miasto),
         wies = as.numeric(wies))

ludnosc.woj <- ludnosc %>%
  filter(str_detect(teryt, "..00000")==TRUE)%>%
  mutate(ludnosc.calosc = miasto+wies)%>%
  mutate(teryt = str_sub(teryt, 1,2))%>%
  rename(wojewodztwo=Nazwa)


ludnosc.pow <- ludnosc %>%
  filter(!str_detect(teryt, "..00000")==TRUE)%>%
  filter(str_detect(teryt, "....000")==TRUE)%>%
  mutate(ludnosc.calosc = miasto+wies)%>%
  mutate(teryt = str_sub(teryt, 1,4))%>%
  rename(powiat=Nazwa)

ludnosc.gmina <- ludnosc %>%
  filter(!str_detect(teryt, "..00000")==TRUE)%>%
  filter(!str_detect(teryt, "....000")==TRUE)%>%
  filter(Nazwa!="M.st.Warszawa od 2002 (1)")%>%
  mutate(cyfra = str_sub(teryt, 7,7))%>%
  mutate(teryt = str_sub(teryt, 1,6))%>%
  filter(!duplicated(teryt)==TRUE)%>%
  mutate(ludnosc.calosc = miasto+wies)%>%
  mutate(miasto.wies = case_when(cyfra=="2" ~ "wiejska",
                                 cyfra=="3" ~ "wiejsko-miejska",
                                 TRUE ~ "miejska"))%>%
  mutate(teryt.woj = str_sub(teryt, 1,2))%>%
  mutate(teryt.pow = str_sub(teryt, 1,4))%>%
  left_join(select(ludnosc.woj, c(wojewodztwo, teryt)), by=c("teryt.woj"="teryt"))%>%
  mutate(wojewodztwo = tolower(wojewodztwo)) %>%
  left_join(select(ludnosc.pow, c(powiat, teryt)), by=c("teryt.pow"="teryt"))


save(ludnosc.gmina, ludnosc.pow, ludnosc.woj, file = "./dane/ludnosc.Rda")
save(komisje, ilosc.komisji, file = "./dane/komisje.Rda")

# testowe wykresy ---------------------------------------------------------

# gminy wg wojewodztw
ggplot(ludnosc.gmina, aes(x=reorder(wojewodztwo, ludnosc.calosc), y=ludnosc.calosc))+
  geom_boxplot()+
  scale_y_continuous(trans='log10', labels = comma)
  #coord_cartesian(ylim = c(0,1e5))

#powiaty wg wojewodztw
a <- ludnosc.pow %>%
  mutate(teryt.woj = str_sub(teryt, 1,2))%>%
  left_join(select(ludnosc.woj, c(wojewodztwo, teryt)), by=c("teryt.woj"="teryt"))%>%
  mutate(wojewodztwo = tolower(wojewodztwo))%>%
  group_by(wojewodztwo)%>%
  mutate(ludnosc.woj = sum(ludnosc.calosc))

ggplot(a, aes(x=reorder(wojewodztwo, -ludnosc.woj), y=ludnosc.calosc))+
  geom_boxplot()+
  scale_y_continuous(labels = comma)+
  coord_cartesian(ylim = c(0,5e5))
