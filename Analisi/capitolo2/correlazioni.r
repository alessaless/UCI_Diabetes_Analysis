# voglio verificare la correlazione tra valori di glicemia e iniezioni di insulina ultralente
library(dplyr)
library(lubridate)
library(readr)
library(corrplot)

# === 1. CARICAMENTO DEL FILE CSV ===
data <- read_csv("./dataset/diabetes_clean_data3.csv")

# === 2. FILTRA SOLO I PAZIENTI CHE USANO ULTRALENTE ===
data$UsesUltraLente <- as.logical(data$UsesUltraLente)
ultralente_data <- filter(data, UsesUltraLente == TRUE)

# === 3. CONVERTI DATETIME IN FORMATO POSIXct ===
ultralente_data$DateTime <- ymd_hms(ultralente_data$DateTime)

# === 4. FILTRA SOLO CODE 35 O 48-64 ===
filtered_data <- ultralente_data %>%
  filter(Code == 35 | (Code >= 48 & Code <= 64))

# === 5. SEPARA GLICEMIA E INIEZIONI ===
glucose <- filtered_data %>% filter(Code == 35)
injections <- filtered_data %>% filter(Code >= 48 & Code <= 64)

# === 6. ASSOCIA EVENTI ENTRO 20 MINUTI NELLA STESSA GIORNATA ===
paired <- inner_join(
  injections,
  glucose,
  by = c("PatientID"),
  suffix = c("_inj", "_glu")
) %>%
  filter(
    as.Date(DateTime_inj) == as.Date(DateTime_glu),
    abs(difftime(DateTime_inj, DateTime_glu, units = "mins")) <= 20
  )

# === 7. SALVA I RISULTATI SU FILE CSV ===
write_csv(paired, "./dataset/risultati_correlati.csv")

# === 8. CREAZIONE MATRICE DI CORRELAZIONE ===
mat <- paired %>%
  select(Value_glu, Value_inj) %>%
  na.omit()

cor_matrix <- cor(mat)

# === 9. SALVA IL CORRPLOT SU FILE PDF ===
pdf("./grafici_plot/grafico_corrplot.pdf", width = 7, height = 5)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()
