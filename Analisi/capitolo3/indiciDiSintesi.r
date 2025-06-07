library(modeest)

library(ggplot2)
library(moments)

# Funzione per plottare skewness e salvare in PDF
plot_skewness <- function(dataframe, titolo_plot, nome_file_output, colore) {
  
  # Calcola skewness
  skew_val <- skewness(dataframe$Value)
  
  
  # Genera il grafico
  p <- ggplot(dataframe, aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = colore, color = "white", alpha = 0.7) +
    geom_density(color = "darkblue", size = 1) +
    labs(
      title = titolo_plot,
      subtitle = paste0("Skewness = ", round(skew_val, 3)),
      x = "Valori glicemici",
      y = "Densità"
    ) +
    theme_minimal(base_size = 14)
  
  # Salva il grafico come PDF
  ggsave(filename = paste0("./grafici_plot/", nome_file_output, ".pdf"),
         plot = p, width = 8, height = 6)
  
}


# Leggi il file CSV (standard con separatore ",")
data <- read.csv("./dataset/diabetes_clean_data3.csv", header = FALSE, stringsAsFactors = FALSE)


colnames(data) <- c("PatientID", "Date", "Time", "Code", "Value", "DateTime_raw", "DateTime", "UsesUltraLente")

# Converte Code e Value in numerico
data$Code <- as.numeric(data$Code)
data$Value <- as.numeric(data$Value)
data$Use <- as.logical(data$UsesUltraLente)

# Filtra i codici tra 48 e 64 (inclusi) di tutti i pazienti
glicemia_data <- subset(data, Code >= 48 & Code <= 64)

# Filtro i codici tra 48 e 64 per i pazienti che utilizzano insulina ultralente
glicemia_data_ultralente <- subset(data, Code >= 48 & Code <= 64 & Use == TRUE)

# Rimuove eventuali NA
glicemia_data <- glicemia_data[!is.na(glicemia_data$Value), ]
glicemia_data_ultralente <- glicemia_data_ultralente[!is.na(glicemia_data_ultralente$Value), ]

# Calcola media, mediana, moda
media_val <- round(mean(glicemia_data$Value))
mediana_val <- median(glicemia_data$Value)
moda_val <- mfv(glicemia_data$Value)

# Calcola media, mediana, moda
media_val_ultralente <- round(mean(glicemia_data_ultralente$Value))
mediana_val_ultralente <- median(glicemia_data_ultralente$Value)
moda_val_ultralente <- mfv(glicemia_data_ultralente$Value)




# Varianza e deviazione standard - Tutti i pazienti
varianza_val <- var(glicemia_data$Value)
dev_std_val <- sd(glicemia_data$Value)

# Varianza e deviazione standard - Pazienti con UltraLente
varianza_val_ultralente <- var(glicemia_data_ultralente$Value)
dev_std_val_ultralente <- sd(glicemia_data_ultralente$Value)
coeff_var_val <- dev_std_val / media_val
coeff_var_val_ultralente <- dev_std_val_ultralente / media_val_ultralente * 100


# Stampa risultati per tutti i pazienti
cat("📊 Statistiche glicemia (solo valori interi, codici 48–64):\n")
cat("Media:", media_val, "\n")
cat("Mediana:", mediana_val, "\n")
cat("Moda:", moda_val, "\n")
cat("Varianza:", varianza_val, "\n")
cat("Deviazione standard:", dev_std_val, "\n\n")
cat("Coefficiente di variazione:", coeff_var_val, "\n\n")

# Stampa risultati per pazienti che usano UltraLente
cat("📊 Statistiche glicemia (solo valori interi, codici 48–64 dei pazienti che usano l'insulina ultralenta):\n")
cat("Media:", media_val_ultralente, "\n")
cat("Mediana:", mediana_val_ultralente, "\n")
cat("Moda:", moda_val_ultralente, "\n")
cat("Varianza:", varianza_val_ultralente, "\n")
cat("Deviazione standard:", dev_std_val_ultralente, "\n")
cat("Coefficiente di variazione:", coeff_var_val_ultralente, "\n\n")


# Usa il dataset glicemia_data o glicemia_data_ultralente già filtrato
plot_skewness(glicemia_data, 
              titolo_plot = "Distribuzione glicemia - Tutti i pazienti", 
              nome_file_output = "glicemia_skewness_tutti", "#1f77b4")

plot_skewness(glicemia_data_ultralente, 
              titolo_plot = "Distribuzione glicemia - Pazienti con insulina UltraLente", 
              nome_file_output = "glicemia_skewness_ultralente", "#ec3131")


# Curtosi - Tutti i pazienti
kurtosis_all <- kurtosis(glicemia_data$Value)         # Curtosi grezza
excess_kurtosis_all <- kurtosis_all - 3               # Curtosi in eccesso

# Curtosi - Pazienti con UltraLente
kurtosis_ultralente <- kurtosis(glicemia_data_ultralente$Value)
excess_kurtosis_ultralente <- kurtosis_ultralente - 3

cat("📊 Curtosi glicemia (tutti i pazienti):\n")
cat("Curtosi in eccesso:", round(excess_kurtosis_all, 3), "\n")

cat("📊 Curtosi glicemia (pazienti con insulina UltraLente):\n")
cat("Curtosi in eccesso:", round(excess_kurtosis_ultralente, 3), "\n")


