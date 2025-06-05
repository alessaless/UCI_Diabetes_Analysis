# === LIBRERIE ===
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# === 1. CARICAMENTO DATI ===
data <- read_csv("./dataset/diabetes_clean_data3.csv", show_col_types = FALSE)
data$DateTime <- ymd_hms(data$DateTime)

# === 2. FILTRA SOLO LE MISURAZIONI GLICEMICHE (Code 48–64) ===
glicemie <- data %>%
  filter(Code >= 48 & Code <= 64)

# === 3. CALCOLA LA MEDIA GLICEMICA PER PAZIENTE ===
medie_pazienti <- glicemie %>%
  group_by(PatientID) %>%
  summarise(MediaGlicemia = mean(Value, na.rm = TRUE)) %>%
  ungroup()

  print(medie_pazienti, n = Inf) 

# === 4. CREAZIONE GRAFICO ECDF ===
pdf("./grafici_plot/ecdf_glicemia_pazienti.pdf", width = 7, height = 5)

ggplot(medie_pazienti, aes(x = MediaGlicemia)) +
  stat_ecdf(geom = "step", color = "darkblue", size = 1) +
  labs(
    title = "Funzione di distribuzione empirica (ECDF)",
    x = "Media glicemia per paziente (mg/dL)",
    y = "F(x)"
  ) +
  theme_minimal()

dev.off()
