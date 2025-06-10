# Caricamento delle librerie necessarie
library(dplyr)
library(lubridate)
library(readr)

# Funzione per calcolare la GMI (Glucose Management Indicator)
# Formula: GMI = 3.31 + 0.02392 * mean_glucose_mg_dl
calculate_gmi <- function(glucose_values) {
  if (length(glucose_values) == 0 || all(is.na(glucose_values))) {
    return(NA)
  }
  mean_glucose <- mean(glucose_values, na.rm = TRUE)
  gmi <- 3.31 + 0.02392 * mean_glucose
  return(round(gmi, 2))
}

# Funzione principale per processare i dati
process_diabetes_data <- function(input_file, output_file) {
  
  # Lettura del dataset
  cat("Lettura del dataset...\n")
  diabetes_data <- read_csv(input_file)
  
  # Verifica della struttura del dataset
  cat("Struttura del dataset:\n")
  print(str(diabetes_data))
  print(head(diabetes_data, 3))
  
  # Conversione della data - usiamo la colonna Date
  diabetes_data$Date_parsed <- as.Date(diabetes_data$Date, format = "%m-%d-%Y")
  
  # Filtro per includere solo le misurazioni di glucosio nel sangue
  # Codici: 48, 57, 58, 59, 60, 61, 62, 63, 64
  glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)
  
  glucose_data <- diabetes_data %>%
    filter(Code %in% glucose_codes) %>%
    filter(!is.na(Value) & Value > 0) %>%  # Rimuove valori mancanti o non validi
    arrange(PatientID, Date_parsed)
  
  cat("Numero di misurazioni di glucosio trovate:", nrow(glucose_data), "\n")
  cat("Numero di pazienti unici:", length(unique(glucose_data$PatientID)), "\n")
  
  # Calcolo degli intervalli di due settimane per ogni paziente
  gmi_results <- glucose_data %>%
    group_by(PatientID) %>%
    mutate(
      # Calcola la data di inizio per ogni paziente
      start_date = min(Date_parsed, na.rm = TRUE),
      # Calcola il numero di settimane dalla data di inizio
      weeks_from_start = as.numeric(difftime(Date_parsed, start_date, units = "weeks")),
      # Raggruppa in intervalli di 2 settimane (0-1, 2-3, 4-5, etc.)
      week_interval = floor(weeks_from_start / 2) + 1
    ) %>%
    group_by(PatientID, week_interval) %>%
    summarise(
      Week = first(week_interval),
      GMI = calculate_gmi(Value),
      UsesUltraLente = first(UsesUltraLente),
      n_measurements = n(),
      date_range_start = min(Date_parsed, na.rm = TRUE),
      date_range_end = max(Date_parsed, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(GMI)) %>%  # Rimuove i casi dove GMI non può essere calcolato
    select(PatientID, Week, GMI, UsesUltraLente) %>%
    arrange(PatientID, Week)
  
  # Stampa statistiche riassuntive
  cat("\nStatistiche risultati:\n")
  cat("Numero totale di record GMI:", nrow(gmi_results), "\n")
  cat("Range settimane:", min(gmi_results$Week), "-", max(gmi_results$Week), "\n")
  cat("Range GMI:", round(min(gmi_results$GMI), 2), "-", round(max(gmi_results$GMI), 2), "\n")
  
  # Conteggio per paziente
  patient_summary <- gmi_results %>%
    group_by(PatientID) %>%
    summarise(
      n_intervals = n(),
      mean_gmi = round(mean(GMI), 2),
      .groups = 'drop'
    )
  
  cat("Media intervalli per paziente:", round(mean(patient_summary$n_intervals), 1), "\n")
  
  # Salvataggio del risultato
  write_csv(gmi_results, output_file)
  cat("File salvato come:", output_file, "\n")
  
  return(gmi_results)
}

# Esempio di utilizzo
# Sostituisci "diabetes_data.csv" con il nome del tuo file di input
# Sostituisci "gmi_results.csv" con il nome desiderato per il file di output
input_filename <- "./dataset/diabetes_clean_data3.csv"  # Modifica con il tuo nome file
output_filename <- "./dataset/gmi_results.csv"   # Nome del file di output

# Esecuzione del processamento
tryCatch({
  results <- process_diabetes_data(input_filename, output_filename)
  
  # Mostra i primi risultati
  cat("\nPrimi 10 risultati:\n")
  print(head(results, 10))
  
}, error = function(e) {
  cat("Errore durante il processamento:", e$message, "\n")
  cat("Verifica che il file di input esista e abbia il formato corretto.\n")
})
