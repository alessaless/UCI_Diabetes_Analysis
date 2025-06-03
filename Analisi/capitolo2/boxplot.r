# 📦 Librerie necessarie
library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(tidyr)

# 📂 Carica il dataset pulito contenente tutte le misurazioni e l'etichetta UltraLente
df <- read_csv("./dataset/diabetes_clean_data3.csv", 
               col_types = cols(
                 Date = col_character(),
                 Time = col_character(),
                 Code = col_integer(),
                 Value = col_double(),
                 PatientID = col_integer(),
                 UsesUltraLente = col_logical()
               ))


# 📌 Codici associati ai valori glicemici
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

# 📥 Filtro del dataset per includere solo i codici glicemici
glucose_df <- df %>%
  filter(Code %in% glucose_codes)


# Prepara i dati per il boxplot
glucose_df$UltraLenteGroup <- ifelse(glucose_df$UsesUltraLente, "Sì", "No")

# Crea il boxplot base (solo box con outlier)
pdf("grafici_plot/boxplot_glicemia_ultralente.pdf", width = 6, height = 4)

boxplot(Value ~ UltraLenteGroup, 
        data = glucose_df,
        col = c("orange", "darkgreen"),
        main = "Confronto valori glicemici (con/senza UltraLente)",
        xlab = "Usa UltraLente", 
        ylab = "Valore glicemia (mg/dL)",
        outline = TRUE)

dev.off()


####################################

media_paziente <- glucose_df %>%
  group_by(PatientID, UsesUltraLente) %>%
  summarise(ValoreMedio = mean(Value), .groups = "drop")

# Conversione in fattore per il boxplot
media_paziente$UltraLenteGroup <- ifelse(media_paziente$UsesUltraLente, "Sì", "No")

# Salvataggio del grafico come PDF
pdf("grafici_plot/boxplot_media_glicemica_per_paziente.pdf", width = 6, height = 4)

boxplot(ValoreMedio ~ UltraLenteGroup, 
        data = media_paziente,
        col = "#c75ff0",
        main = "Glicemia media per paziente (con/senza UltraLente)",
        xlab = "Usa UltraLente",
        ylab = "Valore glicemia medio",
        outline = TRUE)

dev.off()


##################################


# Ordina i PatientID in modo coerente
glucose_df$PatientID <- as.factor(glucose_df$PatientID)

# Salvataggio del grafico come PDF
pdf("grafici_plot/boxplot_glicemia_per_paziente.pdf", width = 10, height = 5)

boxplot(Value ~ PatientID, 
        data = glucose_df,
        col = "skyblue",
        main = "Distribuzione glicemica per paziente",
        xlab = "ID Paziente",
        ylab = "Glicemia (mg/dL)",
        outline = TRUE,
        las = 2,              # rotazione verticale delle etichette sull'asse X
        cex.axis = 0.6)       # riduce la dimensione del testo per evitare sovrapposizioni

dev.off()


#####################################


# Creazione colonna datetime + giorno della settimana in inglese
glucose_df <- glucose_df %>%
  mutate(
    DateTime = as.Date(Date, format = "%m-%d-%Y"),
    Weekday = weekdays(DateTime),
    Weekday = factor(Weekday, 
                     levels = c("Monday", "Tuesday", "Wednesday", 
                                "Thursday", "Friday", "Saturday", "Sunday"))
  )

# Salvataggio del grafico
pdf("grafici_plot/boxplot_glicemia_per_giorno.pdf", width = 8, height = 6)

boxplot(Value ~ Weekday,
        data = glucose_df,
        col = "#ff66b2",
        main = "Distribuzione glicemica per giorno della settimana",
        xlab = "",
        ylab = "Glicemia (mg/dL)",
        las = 2,         # Etichette verticali per i giorni
        cex.axis = 0.8)  # Font leggibile ma non ingombrante

dev.off()



##################################

