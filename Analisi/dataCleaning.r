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

# Crea DateTime e rimuove righe non parseabili
df <- df %>%
  mutate(DateTime_raw = paste(Date, Time),
         DateTime = suppressWarnings(mdy_hm(DateTime_raw))) %>%
  filter(!is.na(DateTime))

# PRIMA: identifica chi ha usato UltraLente PRIMA di eliminare Code == 35
ultralente_users <- df %>%
  filter(Code == 35) %>%
  distinct(PatientID)

# Poi rimuovi i codici da eliminare
codici_da_escludere <- c(4 ,36, 65, 66, 67, 68, 69, 70, 71, 72)

df <- df %>%
  filter(!Code %in% codici_da_escludere)

# Aggiungi colonna booleana corretta
df <- df %>%
  mutate(UsesUltraLente = PatientID %in% ultralente_users$PatientID)

# Filtra solo i codici glicemici
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

glucose_df <- df %>%
  filter(Code %in% glucose_codes) %>%
  select(PatientID, DateTime, Code, Value, UsesUltraLente)

# Salva il CSV
write_csv(df, "./dataset/diabetes_clean_data3.csv")
