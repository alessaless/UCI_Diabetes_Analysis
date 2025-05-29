# Librerie
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# Carica il dataset
df <- read_csv("./dataset/diabetes_full_data.csv", 
               col_types = cols(
                 Date = col_character(),
                 Time = col_character(),
                 Code = col_integer(),
                 Value = col_double(),
                 PatientID = col_integer()
               ))

df <- df %>% drop_na()

df <- df %>%
  mutate(DateTime_raw = paste(Date, Time),
         DateTime = suppressWarnings(mdy_hm(DateTime_raw))) %>%
  filter(!is.na(DateTime))  # <--- questa riga rimuove i casi come 06-31 e 56:35

# Crea colonna DateTime con il formato corretto
df <- df %>%
  mutate(DateTime = mdy_hm(paste(Date, Time)))

# identifichiamo se i pazienti utilizzano l'insulina ultralente
ultralente_users <- df %>%
  filter(Code == 35) %>%
  distinct(PatientID)

# Aggiunge colonna booleana
df <- df %>%
  mutate(UsesUltraLente = PatientID %in% ultralente_users$PatientID)


# Rimuove le colonne non necessarie
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

glucose_df <- df %>%
  filter(Code %in% glucose_codes) %>%
  select(PatientID, DateTime, Code, Value, UsesUltraLente)


write_csv(df, "./dataset/diabetes_clean_data.csv")