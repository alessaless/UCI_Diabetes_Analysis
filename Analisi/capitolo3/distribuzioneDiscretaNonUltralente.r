# === LIBRERIE ===
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# === 1. CARICAMENTO DATI ===
data <- read_csv("./dataset/diabetes_clean_data3.csv", show_col_types = FALSE)
data$DateTime <- ymd_hms(data$DateTime)
data$UsesUltraLente <- as.logical(data$UsesUltraLente)

# === 2. FILTRA PAZIENTI CHE USANO ULTRALENTE + GLICEMIE (Code 48–64) ===
glicemie_ultralente <- data %>%
  filter(UsesUltraLente == FALSE, Code >= 48 & Code <= 64)

# === 3. CALCOLA LA MEDIA GLICEMICA PER PAZIENTE ===
medie_ultralente <- glicemie_ultralente %>%
  group_by(PatientID) %>%
  summarise(MediaGlicemia = mean(Value, na.rm = TRUE)) %>%
  ungroup()

  print(medie_ultralente, n = Inf)  

# === 4. CREA E SALVA GRAFICO ECDF ===
pdf("./grafici_plot/ecdf_glicemia_pazienti_non_ultralente.pdf", width = 7, height = 5)

ggplot(medie_ultralente, aes(x = MediaGlicemia)) +
  stat_ecdf(geom = "step", color = "darkgreen", size = 1) +
  labs(
    title = "ECDF - Media glicemica per pazienti che non usano insulina ultralente",
    x = "Media glicemia (mg/dL)",
    y = "F(x)"
  ) +
  theme_minimal()

dev.off()
