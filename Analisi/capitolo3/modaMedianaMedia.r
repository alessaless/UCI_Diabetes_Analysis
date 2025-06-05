library(modeest)

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

# Stampa risultati
cat("📊 Statistiche glicemia (solo valori interi, codici 48–64):\n")
cat("Media:", media_val, "\n")
cat("Mediana:", mediana_val, "\n")
cat("Moda:", moda_val, "\n")


cat("📊 Statistiche glicemia (solo valori interi, codici 48–64 dei pazienti che usano l'insulina ultralenta):\n")
cat("Media:", media_val_ultralente, "\n")
cat("Mediana:", mediana_val_ultralente, "\n")
cat("Moda:", moda_val_ultralente, "\n")

