library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(modeest)
library(moments)


# Funzione per plottare skewness e salvare in PDF
plot_skewness <- function(dataframe, titolo_plot, nome_file_output) {
  
  # Calcola skewness
  skew_val <- skewness(dataframe$Value)
  
  
  # Genera il grafico
  p <- ggplot(dataframe, aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#f47d0d", color = "white", alpha = 0.7) +
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

# STEP 1: Caricamento del dataset CSV
data <- read_csv("./dataset/diabetes_clean_data3.csv", 
                 col_types = cols(
                   PatientID = col_integer(),
                   Date = col_character(),
                   Time = col_character(),
                   Code = col_integer(),
                   Value = col_double(),
                   DateTime_raw = col_character(),
                   DateTime = col_datetime(),
                   UsesUltraLente = col_logical()
                 ))

# Filtra i codici tra 48 e 64 (inclusi) di tutti i pazienti
gliecemia_data_colazione <- subset(data, Code >= 58 & Code <= 59)

# Filtra i codici tra 48 e 64 (inclusi) di tutti i pazienti
gliecemia_data_pranzo <- subset(data, Code >= 60 & Code <= 61)

# Filtra i codici tra 48 e 64 (inclusi) di tutti i pazienti
gliecemia_data_cena <- subset(data, Code >= 62 & Code <= 63)

# Rimuove eventuali NA
gliecemia_data_colazione <- gliecemia_data_colazione[!is.na(gliecemia_data_colazione$Value), ]
gliecemia_data_pranzo <- gliecemia_data_pranzo[!is.na(gliecemia_data_pranzo$Value), ]
gliecemia_data_cena <- gliecemia_data_cena[!is.na(gliecemia_data_cena$Value), ]

# Calcola media, mediana, moda per colazione
media_colazione <- round(mean(gliecemia_data_colazione$Value))
mediana_colazione <- median(gliecemia_data_colazione$Value)
moda_colazione <- mfv(gliecemia_data_colazione$Value)

# Calcola media, mediana, moda per pranzo
media_pranzo <- round(mean(gliecemia_data_pranzo$Value))
mediana_pranzo <- median(gliecemia_data_pranzo$Value)
moda_pranzo <- mfv(gliecemia_data_pranzo$Value)

# Calcola media, mediana, moda per cena
media_cena <- round(mean(gliecemia_data_cena$Value))
mediana_cena <- median(gliecemia_data_cena$Value)
moda_cena <- mfv(gliecemia_data_cena$Value)

# Stampa risultati
cat("📊 Statistiche glicemia colazione:\n")
cat("Media:", media_colazione, "\n")
cat("Mediana:", mediana_colazione, "\n")
cat("Moda:", moda_colazione, "\n")

cat("📊 Statistiche glicemia pranzo:\n")
cat("Media:", media_pranzo, "\n")
cat("Mediana:", mediana_pranzo, "\n")
cat("Moda:", moda_pranzo, "\n")

cat("📊 Statistiche glicemia cena:\n")
cat("Media:", media_cena, "\n")
cat("Mediana:", mediana_cena, "\n")
cat("Moda:", moda_cena, "\n")

#VARIANZA E DEVIAZIONE STANDARD PER COLAZIONE
varianza_colazione <- var(gliecemia_data_colazione$Value)
dev_std_colazione <- sd(gliecemia_data_colazione$Value)

#VARIANZA E DEVIAZIONE STANDARD PER PRANZO
varianza_pranzo <- var(gliecemia_data_pranzo$Value)
dev_std_pranzo <- sd(gliecemia_data_pranzo$Value)

#VARIANZA E DEVIAZIONE STANDARD PER CENA
varianza_cena <- var(gliecemia_data_cena$Value)
dev_std_cena <- sd(gliecemia_data_cena$Value)

# Stampa varianza e deviazione standard
cat("📊 Varianza e Deviazione Standard Glicemia Colazione:\n")
cat("Varianza:", varianza_colazione, "\n")
cat("Deviazione Standard:", dev_std_colazione, "\n")

cat("📊 Varianza e Deviazione Standard Glicemia Pranzo:\n")
cat("Varianza:", varianza_pranzo, "\n")
cat("Deviazione Standard:", dev_std_pranzo, "\n")

cat("📊 Varianza e Deviazione Standard Glicemia Cena:\n")
cat("Varianza:", varianza_cena, "\n")
cat("Deviazione Standard:", dev_std_cena, "\n")

# Calcola coefficiente di variazione per colazione, pranzo e cena
cv_colazione <- (dev_std_colazione / media_colazione) * 100
cv_pranzo <- (dev_std_pranzo / media_pranzo) * 100
cv_cena <- (dev_std_cena / media_cena) * 100

# Stampa il coefficiente di variazione
cat("📊 Coefficiente di Variazione Glicemia Colazione:", cv_colazione, "%\n")
cat("📊 Coefficiente di Variazione Glicemia Pranzo:", cv_pranzo, "%\n")
cat("📊 Coefficiente di Variazione Glicemia Cena:", cv_cena, "%\n")


# Usa il dataset colazione
plot_skewness(gliecemia_data_colazione, 
              titolo_plot = "Distribuzione glicemia - Colazione", 
              nome_file_output = "glicemia_skewness_Colazione")

# Usa il dataset pranzo
plot_skewness(gliecemia_data_pranzo, 
              titolo_plot = "Distribuzione glicemia - Pranzo", 
              nome_file_output = "glicemia_skewness_Pranzo")

# Usa il dataset cena
plot_skewness(gliecemia_data_cena, 
              titolo_plot = "Distribuzione glicemia - Cena", 
              nome_file_output = "glicemia_skewness_Cena")

# Curtosi per colazione, pranzo e cena
kurtosis_colazione <- kurtosis(gliecemia_data_colazione$Value)
excess_kurtosis_colazione <- kurtosis_colazione - 3               # Curtosi in eccesso

kurtosis_pranzo <- kurtosis(gliecemia_data_pranzo$Value)
excess_kurtosis_pranzo <- kurtosis_pranzo - 3               # Curtosi in eccesso

kurtosis_cena <- kurtosis(gliecemia_data_cena$Value)
excess_kurtosis_cena <- kurtosis_cena - 3               # Curtosi in eccesso

cat("📊 Curtosi Glicemia Colazione:\n")
cat("Curtosi in eccesso:", round(excess_kurtosis_colazione, 3), "\n")

cat("📊 Curtosi Glicemia Pranzo:\n")
cat("Curtosi in eccesso:", round(excess_kurtosis_pranzo, 3), "\n")

cat("📊 Curtosi Glicemia Cena:\n")
cat("Curtosi in eccesso:", round(excess_kurtosis_cena, 3), "\n")