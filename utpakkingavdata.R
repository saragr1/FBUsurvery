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


#### Fjerner c og to verdier i én kolonne i enkelte variabler ####

## Varigmøbler1_3 (fremgangsmåte)
# Separerer varigmøbler1_3 (c3 10000) inn i to nye variabler 
df2 <- df2 %>%
  separate(varigmøbler1_3, c("varigmøbler1_3_n", "varigmøbler2_3_n"))
# Slår sammen verdiene i varigmøbler2_3_n og 2_3 
df2$varigmøbler2_3_n <- coalesce(df2$varigmøbler2_3_n, df2$varigmøbler2_3)
# Fjerner varigmøbler2_3
df2 <- subset(df2, select = -varigmøbler2_3) ### OBS KAN GJØRES FOR ALLE?? ###
# Fjerner c i varigmøbler1_3_n
df2$varigmøbler1_3_n <- gsub("c", "", df2$varigmøbler1_3_n)

## Varigmøbler1_4
df2 <- df2 %>%
  separate(varigmøbler1_4, c("varigmøbler1_4_n", "varigmøbler2_4_n")) 
df2$varigmøbler2_4_n <- coalesce(df2$varigmøbler2_4_n, df2$varigmøbler2_4)
df2 <- subset(df2, select = -varigmøbler2_4)
df2$varigmøbler1_4_n <- gsub("c", "", df2$varigmøbler1_4_n)

## Varigmøbler1_6
df2 <- df2 %>%
  separate(varigmøbler1_6, c("varigmøbler1_6_n", "varigmøbler2_6_n"))
df2$varigmøbler2_6_n <- coalesce(df2$varigmøbler2_6_n, df2$varigmøbler2_6)
df2 <- subset(df2, select = -varigmøbler2_6)
df2$varigmøbler1_6_n <- gsub("c", "", df2$varigmøbler1_6_n)

## Varigmøbler1_2
df2 <- df2 %>%
  separate(varigmøbler1_2, c("varigmøbler1_2_n", "varigmøbler2_2_n"))
df2$varigmøbler2_2_n <- coalesce(df2$varigmøbler2_2_n, df2$varigmøbler2_2)
df2 <- subset(df2, select = -varigmøbler2_2)
df2$varigmøbler1_2_n <- gsub("c", "", df2$varigmøbler1_2_n)

## Varigmøbler1_7
df2 <- df2 %>%
  separate(varigmøbler1_7, c("varigmøbler1_7_n", "varigmøbler2_7_n"))
df2$varigmøbler2_7_n <- coalesce(df2$varigmøbler2_7_n, df2$varigmøbler2_7)
df2 <- subset(df2, select = -varigmøbler2_7)
df2$varigmøbler1_7_n <- gsub("c", "", df2$varigmøbler1_7_n)

## Ved3_3
df2 <- df2 %>%
  separate(ved3_3, c("ved3_3_n", "ved4_3_n"))
df2$ved3_3_n <- gsub("c", "", df2$ved3_3_n)

## Varigmøbler1_5
df2 <- df2 %>%
  separate(varigmøbler1_5, c("varigmøbler1_5_n", "varigmøbler2_5_n"))
df2$varigmøbler2_5_n <- coalesce(df2$varigmøbler2_5_n, df2$varigmøbler2_5)
df2 <- subset(df2, select = -varigmøbler2_5)
df2$varigmøbler1_5_n <- gsub("c", "", df2$varigmøbler1_5_n)

## Varigmøbler1_8
df2 <- df2 %>%
  separate(varigmøbler1_8, c("varigmøbler1_8_n", "varigmøbler2_8_n"))
df2$varigmøbler2_8_n <- coalesce(df2$varigmøbler2_8_n, df2$varigmøbler2_8)
df2 <- subset(df2, select = -varigmøbler2_8)
df2$varigmøbler1_8_n <- gsub("c", "", df2$varigmøbler1_8_n)

## Varigmøbler1_1
df2 <- df2 %>%
  separate(varigmøbler1_1, c("varigmøbler1_1_n", "varigmøbler2_1_n"))
df2$varigmøbler2_1_n <- coalesce(df2$varigmøbler2_1_n, df2$varigmøbler2_1)
df2 <- subset(df2, select = -varigmøbler2_1)
df2$varigmøbler1_1_n <- gsub("c", "", df2$varigmøbler1_1_n)

## Ved3_2
df2 <- df2 %>%
  separate(ved3_2, c("ved3_2_n", "ved4_2_n"))
df2$ved3_2_n <- gsub("c", "", df2$ved3_2_n)

## Leiefritid_1
df2 <- df2 %>%
  separate(leiefritid_1, c("leiefritid_1_n", "leiefritid_12_n"))
df2$leiefritid_1_n <- gsub("c", "", df2$leiefritid_1_n)
df2 <- subset(df2, select = -leiefritid_12_n)


#### Gjør tall-variabler til as.numeric ####

## Gjør variablene til numeric ved hjelp av indeksering (ikke den beste løsningen...)
df2[2:140] <- sapply(df2[2:140], as.numeric)
df2[142:148] <- sapply(df2[142:148], as.numeric)
df2[150:163] <- sapply(df2[150:163], as.numeric)
df2[165:166] <- sapply(df2[165:166], as.numeric)
df2[168:206] <- sapply(df2[168:206], as.numeric)
df2[208:220] <- sapply(df2[208:220], as.numeric)
df2[222:227] <- sapply(df2[222:227], as.numeric)
df2[229:244] <- sapply(df2[229:244], as.numeric)
df2[246:279] <- sapply(df2[246:279], as.numeric)
df2[281:292] <- sapply(df2[281:292], as.numeric)
df2[294:302] <- sapply(df2[294:302], as.numeric)
df2[304:351] <- sapply(df2[304:351], as.numeric)
df2[353:372] <- sapply(df2[353:372], as.numeric)
df2[374:405] <- sapply(df2[374:405], as.numeric)
df2[407:410] <- sapply(df2[407:410], as.numeric)
df2[412:423] <- sapply(df2[412:423], as.numeric)


#### Saving the new, tidy data frame ####
save(df2, file = "questionnaire_tidy_dataset.R")

## Sjekker at det funker å laste det inn
rm(df2)
load("questionnaire_tidy_dataset.R")


#### Gjøremål ####
# 1. Gjøre IOnumber til numeric |JA
# 2. Fjerne NULL og erstatte med NA |JA
# 3. Fjerne ) etter verdier i noen variabler i datasettet |JA
# 4. Fjerne c og at det er to verdier i én celle for enkelte variabler | JA
# 5. Gjøre alle tallvariabler om til numeric | JA



           