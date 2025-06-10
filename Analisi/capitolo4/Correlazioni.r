# ---- Pacchetti ----
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(ggplot2)
library(dplyr)

# ---- Caricamento dati ----
data <- read.csv("./dataset/diabetes_clean_data3.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(data) <- c("PatientID", "Date", "Time", "Code", "Value", "DateTime_raw", "DateTime", "UsesUltraLente")

# ---- Conversioni ----
data$Code <- as.numeric(data$Code)
data$Value <- as.numeric(data$Value)
data$PatientID <- as.factor(data$PatientID)

# ---- Calcolo GMI per ogni paziente ----
gmi_df <- data %>%
  filter(Code >= 48 & Code <= 64) %>%
  group_by(PatientID) %>%
  summarise(GMI = 3.31 + 0.02392 * mean(Value, na.rm = TRUE))

# ---- Momenti combinati ----
momenti_combinati <- list(
  colazione = c(58, 59),
  pranzo = c(60, 61),
  cena = c(62, 63)
)

# ---- Crea cartella se non esiste ----
if (!dir.exists("grafici_gmi")) {
  dir.create("grafici_gmi")
}

# ---- Funzione di analisi ----
analizza_momento_combinato <- function(codici, nome_momento) {
  df_misto <- data %>%
    filter(Code %in% codici) %>%
    group_by(PatientID) %>%
    summarise(media_glicemia = mean(Value, na.rm = TRUE))

  confronto <- merge(gmi_df, df_misto, by = "PatientID")

  # Correlazione
  cor_val <- cor(confronto$GMI, confronto$media_glicemia, use = "complete.obs", method = "pearson")

  # Grafico
  p <- ggplot(confronto, aes(x = media_glicemia, y = GMI)) +
    geom_point(color = "#1f77b4") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = paste("GMI vs glicemia media", nome_momento),
      subtitle = paste("Correlazione Pearson =", round(cor_val, 3)),
      x = paste("Glicemia media", nome_momento),
      y = "GMI"
    ) +
    theme_minimal(base_size = 13)

  # Salva grafico
  file_name <- paste0("grafici_gmi/gmi_vs_", nome_momento, ".pdf")
  ggsave(file_name, plot = p, width = 8, height = 6)
  cat("✅ Grafico salvato:", file_name, "\n")

  return(c(nome = nome_momento, correlazione = cor_val))
}

# ---- Esegui per ogni pasto combinato ----
risultati_correlazioni <- lapply(names(momenti_combinati), function(n) {
  analizza_momento_combinato(momenti_combinati[[n]], n)
})

# ---- Tabella risultati ----
cor_df <- do.call(rbind, risultati_correlazioni)
cor_df <- as.data.frame(cor_df)
print(cor_df)
