# voglio verificare la correlazione tra valori di glicemia e iniezioni di insulina regolare e NPH
library(dplyr)
library(lubridate)
library(readr)
library(corrplot)

# === 1. CARICAMENTO DEL FILE ===
data <- read_csv("./dataset/diabetes_clean_data3.csv")

# === 2. CONVERTI DATETIME ===
data$DateTime <- ymd_hms(data$DateTime)

# === 3. FILTRA EVENTI DI INTERESSE ===
glicemie <- data %>%
  filter(Code >= 48 & Code <= 64)

iniezioni <- data %>%
  filter(Code == 33 | Code == 34)

# === 4. JOIN SU PATIENTID E FILTRA COPPIE TEMPORALI VALIDE ===
paired <- inner_join(
  glicemie,
  iniezioni,
  by = c("PatientID"),
  suffix = c("_glic", "_ins")
) %>%
  filter(
    as.Date(DateTime_glic) == as.Date(DateTime_ins),  # stessa giornata
    difftime(DateTime_ins, DateTime_glic, units = "mins") >= 0,  # iniezione DOPO
    difftime(DateTime_ins, DateTime_glic, units = "mins") <= 20  # max 20 minuti dopo
  )

# === 5. SALVA IL DATASET FINALE ===
write_csv(paired, "./dataset/glicemia_vs_insulina_reg_nph.csv")

# === 6. CREA MATRICE DI CORRELAZIONE (valore glicemia vs dose insulina) ===
mat <- paired %>%
  select(Value_glic, Value_ins) %>%
  na.omit()

cor_matrix <- cor(mat)

# === 7. CREA E SALVA IL CORRPLOT IN PDF ===
pdf("./grafici_plot/corrplot_glicemia_vs_insulina_reg_nph.pdf", width = 7, height = 5)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()
