# 📦 Pacchetti
library(dplyr)
library(broom)
library(ggplot2)

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

# 📊 Calcolo glicemia media per ciascun momento
glicemia_momenti <- lapply(names(codici), function(momento) {
  df <- data %>%
    filter(Code %in% codici[[momento]]) %>%
    group_by(PatientID) %>%
    summarise(glicemia_media = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    rename(!!momento := glicemia_media)
  return(df)
})

# 🔗 Merge
confronto_df <- Reduce(function(x, y) full_join(x, y, by = "PatientID"), c(list(gmi_df), glicemia_momenti))

# 📁 Cartella per i grafici dei residui
if (!dir.exists("grafici_residui")) {
  dir.create("grafici_residui")
}

# 📊 Calcolo residui e salvataggio dei grafici
residui_risultati <- list()

analizza_residui <- function(xvar, nome_momento) {
  model <- lm(GMI ~ get(xvar), data = confronto_df)
  aug <- augment(model)
  media_residui <- mean(aug$.resid)
  var_residui <- var(aug$.resid)

  cat("📊", toupper(nome_momento), "\n")
  cat("Media residui:", round(media_residui, 5), "\n")
  cat("Varianza residui:", round(var_residui, 5), "\n\n")

  aug$momento <- nome_momento

  # 📉 Grafico dei residui
  p <- ggplot(aug, aes(x = .fitted, y = .resid)) +
    geom_point(color = "#1f77b4", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Grafico dei residui –", nome_momento),
      x = "Valori stimati (y^)",
      y = "Residui (e = y - y^)"
    ) +
    theme_minimal(base_size = 13)

  # Salvataggio PDF
  ggsave(
    filename = paste0("grafici_residui/residui_", nome_momento, ".pdf"),
    plot = p, width = 8, height = 5
  )

  return(aug)
}

residui_risultati$colazione <- analizza_residui("colazione", "colazione")
residui_risultati$pranzo    <- analizza_residui("pranzo", "pranzo")
residui_risultati$cena      <- analizza_residui("cena", "cena")

# 🔄 Per visualizzazioni multiple o analisi aggiuntive
residui_tutti <- bind_rows(residui_risultati)

# 📊 Modelli per tabella α, β, R², p
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

# 📋 Tabella finale dei modelli
tabella_finale <- do.call(rbind, risultati)
print(tabella_finale)
