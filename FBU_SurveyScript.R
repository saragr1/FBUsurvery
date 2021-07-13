
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


# c("hus1_4 == 4", "bol1_1 == 4", "bol2_1 == 120"

# rydder litt opp for jeg splitter opp mellom variabelnavn og verdeiene.
df$answers <- gsub("c\\(", "", df$answers)



do.call("<-",df$answers(parameter_name, parameter_value))


df %>%
  separate_rows(answers, sep = ",\\s+") %>%
  separate(answers, into = c("key", "value"), sep="=\\s+") %>%
  mutate(key = na_if(key, "No Data")) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  #  select(-`NA`) %>% 
  view()




data.frame(do.call("cbind", strsplit(as.character(df$answers), "==",
                                     fixed = TRUE
                                     ))) %>% 
  view()











































