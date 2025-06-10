# 📦 Pacchetti
library(dplyr)
library(broom)

# 📁 Caricamento dati
data <- read.csv("./dataset/diabetes_clean_data3.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(data) <- c("PatientID", "Date", "Time", "Code", "Value", "DateTime_raw", "DateTime", "UsesUltraLente")

# 🔄 Pulizia e conversioni sicure
data <- data %>%
  mutate(
    Code = as.numeric(Code),
    Value = as.numeric(gsub(",", ".", Value)),  # pulizia decimale se serve
    PatientID = as.character(PatientID)
  ) %>%
  filter(!is.na(Value), !is.na(Code))

# 🔍 Codici per i momenti della giornata
codici <- list(
  colazione = c(58, 59),
  pranzo = c(60, 61),
  cena = c(62, 63)
)

# 🧮 Calcolo GMI medio per ogni paziente
gmi_df <- data %>%
  filter(Code >= 48 & Code <= 64) %>%
  group_by(PatientID) %>%
  summarise(GMI = 3.31 + 0.02392 * mean(Value, na.rm = TRUE), .groups = "drop")

# 📊 Funzione per calcolare glicemia media per momento
glicemia_momenti <- lapply(names(codici), function(momento) {
  df <- data %>%
    filter(Code %in% codici[[momento]]) %>%
    group_by(PatientID) %>%
    summarise(glicemia_media = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    rename(!!momento := glicemia_media)
  return(df)
})

# 🔗 Unisci le tabelle (tutte con PatientID come character)
confronto_df <- Reduce(function(x, y) full_join(x, y, by = "PatientID"), c(list(gmi_df), glicemia_momenti))

# 🧮 Calcola modelli di regressione lineare per ciascun momento
risultati <- lapply(c("colazione", "pranzo", "cena"), function(var) {
  model <- lm(GMI ~ get(var), data = confronto_df)
  tidy_model <- tidy(model)
  glance_model <- glance(model)
  data.frame(
    Momento = var,
    Alpha = round(tidy_model$estimate[1], 4),
    Beta = round(tidy_model$estimate[2], 4),
    R2 = round(glance_model$r.squared, 4),
    p_value = round(tidy_model$p.value[2], 5)
  )
})

# 📋 Tabella finale
tabella_finale <- do.call(rbind, risultati)
print(tabella_finale)
