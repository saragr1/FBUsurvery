# laster inn kode jeg har bruk for under analysen og oppryddingen
library(tidyverse)
library(data.table)
library(RCurl)
library(rjson)
library(jsonlite)
library(lubridate)
library(readr)

df <- fromJSON("C:/Users/gse/Documents/R/FBU_SurveyData/questionnaireData.json", 
               flatten = TRUE)

# Variabelen [answers] inneholder baade variabelnavn og svar. I tillegg er alt
# kodet som strings og maa splittes og plasseres i nye variabler. 

# rydder litt opp for jeg splitter opp mellom variabelnavn og verdeiene.
df$answers <- gsub("c\\(", "", df$answers)
df$answers <- gsub("[\"]", "", df$answers)


# Spitter opp [anserws] variabelen
df1 <- df %>%
  separate_rows(answers, sep = ",\\s+") %>%
  separate(answers, into = c("key", "value"), sep=" ==\\s+") %>%
  mutate(key = na_if(key, "No Data")) %>%
  pivot_wider(names_from = key, values_from = value)
  #  select(-`NA`) %>% 
  # view()

# Yes, vakkert! :-) 

# Etter en kort inspeksjon er det enkelte variabler som har noen rare tegn og 
# noen er kodet feil? Litt usikker på det siste.


# Arbeid videre
## 1. fjerne rare symboler og evetuelle bokstaver i verdiene til variablene.

# test1,2,3




