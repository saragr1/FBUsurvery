#### Laster inn nødvendige pakker ####

## Lager en vektor med pakker som skal installeres og lastes ned
pakker <- c("tidyverse", "rjson", "jsonlite", "RCurl", "readr", "lubridate", 
            "data.table", "reshape", "stringr")

## Bruker en funksjon som installerer og laster ned pakker til vektoren av pakker
sapply(pakker, 
       FUN = function(x){
         # Sjekker om pakken er installert. Hvis ikke, installer den:
         if(x %in% rownames(installed.packages()) == FALSE){
           install.packages(x)
         }
         # Laster pakken
         library(x, character.only = TRUE)
       })


#### Laster inn datasettet fra working directory/prosjektmappa ####
df <- fromJSON("questionnaireData.json")

class(df) # data frame
str(df) # respondentId og ioNumber er character, answers er en list of 116
class(df$answers) # list
df$ioNumber <- as.numeric(df$ioNumber)
class(df$ioNumber) # numeric


#### String manipulation, fra én string-variabel til mange ####

## Rydder opp før variabelnavn og verdier i answers splittes.
df$answers <- gsub("c\\(", "", df$answers) # Fjerner c(
df$answers <- gsub("\"", "", df$answers) # Fjerner "" rundt hver character i stringen
df$answers <- gsub("\\)", "", df$answers) # Fjerner ) etter verdier

## Spitter opp [answers] variabelen
df1 <- df %>%
  separate_rows(answers, sep = ",\\s+") %>% # separerer når det er , 
  separate(answers, into = c("key", "value"), sep=" ==\\s") %>% # Splitter answers opp i key og value på " ==..."
  #mutate(key = na_if(key, "No Data")) %>%
  pivot_wider(names_from = key, values_from = value)

## Sjekker df1
names(df1)
sum(is.na(df1)) # 5 (rows 1777, 2675, 2676, 5478, 5479 (før pivot_wider))


#### Gjør NULL-verdier om til NA ####

class(df1)
df1 <- as.data.frame(df1)
df1[df1 == "NULL"] <- NA_character_
sum(is.na(df1)) # 42058, tyder på at det er blitt NAs

#### Fjerner () , "" i c("3", "4000")

df2 <- df1[,-1] %>%
  mutate_all(funs(gsub("[[:punct:]]", "", .)))

# Binder variabelen respondentId fra df sammen med df2
df2 <- as.data.frame(cbind(df$respondentId, df2))

# Endrer navn på respondentId variabelen
names(df2)[names(df2) == "df$respondentId"] <- "respondentId"

####  ####
gsub("[[:punct:]]", "", df2)
df3 <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(df2)))

class(df3$bol2_1)
#as.numeric(df3$bol2_1 <- )

#### Gjøremål ####
# 1. Gjøre IOnumber til numeric |JA
# 2. Fjerne NULL og erstatte med NA |JA
# 3. Fjerne ) etter verdier i noen variabler i datasettet |JA
# 4. Sjekke om det er noen verdier (ekstreme verdier), 2 standardavvik fra gjennomsnittet (matrise for å sjekke det) |





