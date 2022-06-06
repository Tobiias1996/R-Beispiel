---
title: "Lauterjung - Einfluss internationaler und ausländischer Mobilität"
author: "Tobias Lauterjung"
date: "12 1 2022"
output: html_document
bibliography: "~/lfpma20-master/Covid19_Hausarbeit/bib/literatur.bib"
---

```{r Dateien einladen, include=FALSE}
library("rworldmap")
library("RColorBrewer")
library(devtools)
library(tidyverse)
library(ggplot2)
library(readxl)
library(grid)
library(gridExtra)
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(tidyr)
library(tinytex)
library(modelr)
tourismusdaten_laender_weltweit <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/tourismusdaten_laender_weltweit.rds")
polity_V_covid_merged <- readRDS("~/lfpma20/Datensatzaufbereitung_Covid19/polity_V_covid_merged.rds")
Polity_V_Variablen <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Polity V Variablen.xlsx")
Freedom_House_Variablen <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Freedom House Index Variablen.xlsx")
Freedom_House <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/Freedom_House.rds")
Freedom_House_2020 <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/Freedom_House_2020.rds") %>% unnest()
polity_V_Nicole <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/polity_V_Nicole.rds")
iso3_code <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/iso3_code.rds")
polity_V_covid_merged_Nicole <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/polity_V_covid_merged_Nicole.rds")

polity_V_covid_merged_freedom <- merge.data.frame(polity_V_covid_merged_Nicole, Freedom_House)

polity_V_test_2 <- polity_V_covid_merged_freedom %>% select(`data`, `country`, `Status_2020`) %>% unnest("data")

polity_V_test_2 <- polity_V_test_2 %>% mutate(date = str_remove_all(date, "-")) %>% select(`date`, `ConfPopProzent`, `confirmed`, `time_conf`, `ZuwachsPopProzent`,  `country`, `Status_2020`) %>% rename(`Date` = `date`) %>%  mutate_at("Date", as.numeric)   

polity_V_test_3 <- polity_V_covid_merged_freedom %>% unnest("Cambridge_Data") %>% mutate_at("SumMa", as.numeric) %>% mutate_at("SumMaZ", as.numeric) 
                                                                                      
polity_V_test_4 <- merge.data.frame(polity_V_test_2, polity_V_test_3)

MaCon <- polity_V_covid_merged_Nicole

MaCon_1 <- MaCon %>% select(`data`, `country`) %>% unnest("data")

MaCon_1 <- MaCon_1 %>% mutate(date = str_remove_all(date, "-")) %>% select(`date`, `confirmed`, `time_conf`, `ZuwachsPopProzent`, `country`) %>% rename(`Date` = `date`) %>%  mutate_at("Date", as.numeric)   

MaCon_2 <- MaCon %>% unnest("Cambridge_Data") %>% select(`SumMaZ`, `Date`, `country`)   
                                                                                      
MaCon <- merge.data.frame(MaCon_1, MaCon_2)
```

```{r}
polity_V_covid_merged <- readRDS("~/lfpma20/Datensatzaufbereitung_Covid19/polity_V_covid_merged.rds")
Staedtetourismus_weltweit <- readRDS("~/lfpma20/Datensatzaufbereitung_Covid19/Mobilität/zahlen_staedtetourismus_weltweit.rds")
Tourismusdaten_weltweit <- readRDS("~/lfpma20/Datensatzaufbereitung_Covid19/Mobilität/tourismusdaten_laender_weltweit.rds")
Tourismusausgaben <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Länder Tourismusausgaben bis 2018.xlsx")
Infrastruktur <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Länder-Infrastruktur 2018.xlsx")
Entwicklung_Passagierfluege_weltweit <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Entwicklung Passagierflugverkehrs weltweit bis 2038.xlsx") 
Beliebteste_Reiseziele <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Beliebteste Reiseziele nach Besucherzahlen 2019.xlsx")
Flugpassagiere_Deutschland <- read_excel("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Flupassagiere Deutschland bis 2019.xlsx")
Flugdaten <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/Flugdaten.rds")
zahlen_staedtetourismus_weltweit <- readRDS("~/lfpma20-master/Datensatzaufbereitung_Covid19/Mobilität/zahlen_staedtetourismus_weltweit.rds")
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

FlugdatenDiagramm<-Flugdaten%>% mutate(num=1:n())%>% filter(num<181)
FlugdatenDiagramm%>% ggplot(aes(x=date, y=`Number of flights`))+ geom_smooth(colour="black", se=FALSE)+ xlab("Datum") + ggtitle("Internationaler Flugverkehr 2020") + ylab("Anzahl der Flüge") 
```

TouristcontriesTop10%>% ggplot(aes(x=Date, y=`confirmed`))+ geom_smooth(colour="deeppink1", se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen in den USA 2020") + ylab("Anzahl der bestätigten Corona Infektionen") + scale_y_continuous(limits = c(0, 8000000), breaks = seq(0, 8000000, by = 1000000))

#### Internationale Reisekontrollen
Mit dem Status als Pandemie geht einher, dass das Coronavirus nicht lokal begrenzt ist, sondern sich global ausbreitet. 


```{r Internationale Reisekontrollen 2020}
polity_V_test_4%>% group_by (`C8_International travel controls`) %>%  ggplot(aes(x=Date, y=`confirmed`))+ geom_smooth(colour="black", se=FALSE)+  xlab("Datum") + ggtitle("Internationale Reisekontrollen 2020 I") + ylab("Infektionen") + facet_wrap(~ `C8_International travel controls`, nrow=4) 

polity_V_test_4%>% group_by (`C8_International travel controls`) %>%  ggplot(aes(x=Date, y=`C8_International travel controls`))+ geom_smooth(colour="black", se=TRUE)+  xlab("Datum") + ggtitle("Internationale Reisekontrollen 2020 II") + ylab("Infektionen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) 

```




```{r "Einreisen in die beliebsten Urlausbländer 2019}
Länder<-tourismusdaten_laender_weltweit%>% mutate(num=1:n())%>% filter(num<6)
Länder%>% ggplot(aes(x=num, y=`Anzahl der Ankünftein Tausend`))+ geom_point()+  xlab("Rang") + ggtitle("Einreisen in die beliebsten Urlausbländer 2019 I") + ylab("Anzahl der Ankünfte in Tausend") + facet_wrap(~ `Land`, nrow=4) 

Länder<-tourismusdaten_laender_weltweit%>% mutate(num=1:n())%>% filter(num>5, num<11)
Länder%>% ggplot(aes(x=num, y=`Anzahl der Ankünftein Tausend`))+ geom_point()+  xlab("Rang") + ggtitle("Einreisen in die beliebsten Urlausbländer 2019 II") + ylab("Anzahl der Ankünfte in Tausend")+ facet_wrap(~ `Land`, nrow=4)
```

```{r}

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("CHN", "THA", "USA","DEU","GBR","TUR","FRA", "ITA", "MEX", "ESP"))
TouristcontriesTop10%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+  xlab("Datum") + ggtitle("Coronamaßnahmen der beliebtesten Urlaubsländer 2020") + ylab("Maßnahmen")+ scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1))
```

```{r Verlauf Infektionszahlen beliebtester Urlaubsländer + Einzelgrafik für USA da Ausreißer}
TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("CHN", "THA", "USA","DEU","GBR","TUR","FRA", "ITA", "MEX", "ESP"))
TouristcontriesTop10%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen in den beliebtesten Urlaubsländer 2020") + ylab("Anzahl der bestätigten Corona Infektionen") + scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, 1500000, by = 150000)) 

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("USA"))
TouristcontriesTop10%>% ggplot(aes(x=Date, y=`confirmed`))+ geom_smooth(colour="deeppink1", se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen in den USA 2020") + ylab("Anzahl der bestätigten Corona Infektionen") + scale_y_continuous(limits = c(0, 8000000), breaks = seq(0, 8000000, by = 1000000))
```

```{r Population beliebtester Urlaubsländer}

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("CHN", "THA", "USA","DEU","GBR","TUR","FRA", "ITA", "MEX", "ESP"))
TouristcontriesTop10%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Bevölkerung der beliebtesten Urlaubsländer 2018") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4)

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("THA", "DEU","GBR","TUR","FRA", "ITA", "MEX", "ESP"))
TouristcontriesTop10%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Ohne China und USA") + ylab("Jahr") + facet_wrap(~ `country`, nrow=6)
```

```{r}
Städte<-zahlen_staedtetourismus_weltweit%>% mutate(num=1:n())%>% filter(num<11)
Städte%>% ggplot(aes(x=Stadt, y=`Besucher (2018)`))+ geom_point()+  xlab("Stadt") + ggtitle("Städte mit den höchsten Besucherzahlen 2019") + ylab("Anzahl der Besucher")
```

```{r Malaysia und Singapur}
StädteTopBesucherMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("SGP", "MYS"))
StädteTopBesucherMaßnahmen%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+  xlab("Datum") + ggtitle("Coronamaßnahmen von Malaysia und Singapur 2020") + ylab("Maßnahmen")+ scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) 

StädteTopBesucherInfektionen<-polity_V_test_4%>% filter(iso3c== c("SGP", "MYS"))
StädteTopBesucherInfektionen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen von Malaysia und Singapur 2020") + ylab("Anzahl der bestätigten Corona Infektionen")

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("MYS"))
TouristcontriesTop10%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Bevölkerung von Malaysia") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4) 

TouristcontriesTop10<-polity_V_test_4%>% filter(iso3c== c("SGP"))
TouristcontriesTop10%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Bevölkerung von Singapur") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4) 
```
```{r Coronamaßnahmen ausgewählter Asiatischer und Europäischer Länder 2020}
OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("JPN", "IDN", "KHM","TWN","VNM","MMR","MNG", "IND"))
OstasienMaßnahmen%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+  xlab("Datum") + ggtitle("Coronamaßnahmen ausgewählter Asiatischer Länder 2020") + ylab("Maßnahmen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1))

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "SWE", "PRT","NLD","RUS","BEL","CZE", "ROU"))
EuropaMaßnahmen%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Coronamaßnahmen ausgewählter Europäischer Länder 2020") + ylab("Anzahl der bestätigten Corona Infektionen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) 

```

```{r Infektionszahlen in ausgewählten Asiatischen u. Europäischen Ländern}
OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("JPN", "IDN", "KHM","TWN","VNM","MMR","MNG", "IND"))
OstasienMaßnahmen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen ausgewählter Asiatischer Länder 2020") + ylab("Anzahl der bestätigten Corona Infektionen")

OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("JPN", "IDN", "KHM","TWN","VNM","MMR","MNG"))
OstasienMaßnahmen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Ohne Indien") + ylab("Anzahl der bestätigten Corona Infektionen")

OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c ("MNG", "KHM", "TWN","VNM"))
OstasienMaßnahmen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Ohne Indien, Indonesien, Japan und Myanmar") + ylab("Anzahl der bestätigten Corona Infektionen")

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "SWE", "PRT","NLD","RUS","BEL","CZE", "ROU"))
EuropaMaßnahmen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Verlauf Infektionszahlen ausgewählter Europäischer Länder 2020") + ylab("Anzahl der bestätigten Corona Infektionen")

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "SWE", "PRT","NLD","BEL","CZE", "ROU"))
EuropaMaßnahmen%>% ggplot(aes(x=Date, y=`confirmed`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Ohne Russland") + ylab("Anzahl der bestätigten Corona Infektionen")
```
```{r Population Asiatischer und Europäischer Länder 2020}

OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("JPN", "IDN", "KHM","VNM","MMR","MNG", "IND"))
OstasienMaßnahmen%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Bevölkerung ausgewählter Asiatischer Länder 2018") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4)


OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("KHM","JPN","VNM","MMR", "MNG"))
OstasienMaßnahmen%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Ohne Indien und Indonesien") + ylab("Jahr") + facet_wrap(~ `country`, nrow=6) + scale_x_continuous(limits = c(0, 135000000), breaks = seq(0, 135000000, by = 20000000))

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "SWE", "PRT","NLD","RUS","BEL","CZE", "ROU"))
EuropaMaßnahmen%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Bevölkerung ausgewählter Europäischer Länder 2018") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4)

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "ROU","SWE", "PRT","NLD", "BEL","CZE"))
EuropaMaßnahmen%>% ggplot(aes(x=population, y=`year`))+ geom_point(se=FALSE)+  xlab("Einwohner") + ggtitle("Ohne Russland") + ylab("Jahr") + facet_wrap(~ `country`, nrow=4)
```

```{r Coronamaßnahmen ausgewählter Asiatischer und Europäischer Länder 2020}
OstasienMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("JPN", "IDN", "KHM","TWN","VNM","MMR","MNG", "IND"))
OstasienMaßnahmen%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+  xlab("Datum") + ggtitle("Coronamaßnahmen ausgewählter Asiatischer Länder 2020") + ylab("Maßnahmen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1))

EuropaMaßnahmen<-polity_V_test_4%>% filter(iso3c== c("POL", "SWE", "PRT","NLD","RUS","BEL","CZE", "ROU"))
EuropaMaßnahmen%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=country))+ geom_smooth(se=FALSE)+ xlab("Datum") + ggtitle("Coronamaßnahmen ausgewählter Europäischer Länder 2020") + ylab("Anzahl der bestätigten Corona Infektionen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) 
```

```{r Coronamaßnahmen Kontinente 2020}

Kontinente<-polity_V_test_4%>% filter(Continent_Name== c("Africa", "South America", "Europe", "Asia"))
Kontinente%>% ggplot(aes(x=Date, y=`C8_International travel controls`, colour=`Continent_Name`))+ geom_smooth( se=FALSE)+  xlab("Datum") + ggtitle("Coronamaßnahmen ausgewählter Kontinente 2020") + ylab("Maßnahmen") + scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1))

Kontinente<-polity_V_test_4%>% filter(Continent_Name== c("Africa", "South America", "Europe", "Asia"))
Kontinente%>% ggplot(aes(x=Date, y=`confirmed`, colour=`Continent_Name`))+ geom_smooth( se=FALSE)+  xlab("Datum") + ggtitle("Verlauf Infektionszahlen ausgewählter Kontinente 2020") + ylab("Maßnahmen")

```
