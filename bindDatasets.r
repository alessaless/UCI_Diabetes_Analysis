library(tidyverse)
library(readr)
library(stringr)
library(dplyr)

# Imposta il path dove hai salvato i file
data_folder <- "./Diabetes-Data"

# Lista dei file che iniziano con "data-"
file_list <- list.files(path = data_folder,
                        pattern = "^data-[0-9]+$",
                        full.names = TRUE)

# Funzione per leggere ciascun file e aggiungere PatientID
read_patient_file <- function(file_path) {
  patient_id <- str_extract(basename(file_path), "\\d+")
  df <- read_tsv(
    file_path,
    col_names = c("Date", "Time", "Code", "Value"),
    col_types = cols(
      Date = col_character(),
      Time = col_character(),
      Code = col_integer(),
      Value = col_double()
    )
  ) %>%
    mutate(PatientID = patient_id)

  df
}

# Applica la funzione a tutti i file
patient_data_list <- lapply(file_list, read_patient_file)
full_data <- bind_rows(patient_data_list)

# Riordina le colonne per mettere PatientID all'inizio
full_data <- full_data %>%
  select(PatientID, everything())

# Salva il dataframe completo in un file CSV nella cartella dataset
write_csv(full_data, "./dataset/diabetes_full_data.csv")