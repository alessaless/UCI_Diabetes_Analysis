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

# 🔢 Calcolo frequenza assoluta e relativa per ciascun tipo di evento (codice)
frequenze_code <- df %>%
  group_by(Code) %>%
  summarise(FreqAssoluta = n()) %>%
  mutate(FreqRelativa = round(FreqAssoluta / sum(FreqAssoluta), 4))

# 📋 Stampa la tabella dei codici evento
print(frequenze_code)

# 📊 Grafico: distribuzione degli eventi con colori viridis diversi per codice
plot_code <- ggplot(frequenze_code, aes(x = factor(Code), y = FreqAssoluta, fill = factor(Code))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Frequenza assoluta degli eventi (Code)",
    x = "Codice Evento",
    y = "Frequenza Assoluta",
    fill = "Codice"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "D")

# 💾 Salva il grafico
ggsave("grafici_plot/frequenza_code.pdf", plot = plot_code, width = 6, height = 4, dpi = 300)


######################################################


# 📊 Grafico: quanti pazienti usano UltraLente e quanti no
distrib_ultralente <- df %>%
  distinct(PatientID, UsesUltraLente) %>%
  count(UsesUltraLente, name = "FreqAssoluta") %>%
  mutate(FreqRelativa = round(FreqAssoluta / sum(FreqAssoluta), 4))
print(distrib_ultralente)

plot_ultralente <- ggplot(distrib_ultralente, aes(x = as.factor(UsesUltraLente), y = FreqAssoluta)) +
  geom_bar(stat = "identity", fill = "#e07939") +
  labs(
    title = "Distribuzione pazienti per uso UltraLente",
    x = "Usa UltraLente",
    y = "Frequenza Assoluta"
  ) +
  theme_minimal()
ggsave("grafici_plot/distribuzione_ultralente.pdf", plot = plot_ultralente, width = 6, height = 4, dpi = 300)



######################################################


# 📌 Codici associati ai valori glicemici
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

# 📥 Filtro del dataset per includere solo i codici glicemici
glucose_df <- df %>%
  filter(Code %in% glucose_codes)

# 📦 Suddividi i valori glicemici in classi (bin) di ampiezza 20 (come nel grafico)
glucose_freq <- glucose_df %>%
  mutate(ClasseGlicemia = cut(Value, breaks = seq(0, max(Value, na.rm = TRUE) + 20, by = 20), right = FALSE)) %>%
  group_by(ClasseGlicemia) %>%
  summarise(FreqAssoluta = n()) %>%
  mutate(FreqRelativa = round(FreqAssoluta / sum(FreqAssoluta), 4))

# 📋 Stampa tabella frequenze glicemia
print(glucose_freq, n = Inf)

# 📊 Istogramma della distribuzione dei valori glicemici
glicem <- ggplot(glucose_df, aes(x = Value)) +
  geom_histogram(aes(fill = ..count..), binwidth = 20, color = "white") +
  scale_fill_viridis_c(option = "D") +
  labs(
    title = "Distribuzione dei valori glicemici",
    x = "Glicemia (mg/dL)",
    y = "Frequenza",
    fill = "Frequenza"
  ) +
  theme_minimal()
ggsave("grafici_plot/distribuzione_valori_glicemici.pdf", plot = glicem, width = 6, height = 4, dpi = 300)


######################################################




# 📈 Calcola il valore medio di glicemia per ciascun paziente (usando solo codici glicemici)
media_paziente <- glucose_df %>%
  group_by(PatientID, UsesUltraLente) %>%
  summarise(ValoreMedio = mean(Value, na.rm = TRUE), .groups = "drop")

# 📊 Classifica ogni paziente in una fascia clinica in base alla glicemia media
media_paziente <- media_paziente %>%
  mutate(
    FasciaMedia = case_when(
      ValoreMedio < 70 ~ "Ipoglicemia",
      ValoreMedio <= 140 ~ "Normale",
      ValoreMedio <= 180 ~ "Iperglicemia moderata",
      TRUE ~ "Iperglicemia grave"
    )
  )

# 📦 Calcola la frequenza assoluta dei pazienti per ciascuna fascia e stato UltraLente
fasce_media <- media_paziente %>%
  group_by(UsesUltraLente, FasciaMedia) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(FreqRelativa = round(Count / sum(Count), 4))

# 📋 Stampa la tabella con frequenza assoluta e relativa
print(fasce_media, n = Inf)

# 📈 Grafico a barre della distribuzione delle fasce glicemiche medie
grafico_fasce_media <- ggplot(fasce_media, aes(x = FasciaMedia, y = Count, fill = as.factor(UsesUltraLente))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribuzione media glicemica per paziente (fasce cliniche)",
    x = "Fascia glicemica media",
    y = "Numero di pazienti",
    fill = "Usa UltraLente"
  ) +
  theme_minimal()
ggsave("grafici_plot/fasce_glicemia_media_per_paziente.pdf", plot = grafico_fasce_media, width = 7, height = 4.5, dpi = 300)


######################################################


# 📊 Mappa dei codici glicemici a momenti specifici della giornata
glucose_df <- glucose_df %>%
  mutate(Misurazione = case_when(
    Code == 58 ~ "Pre-colazione",
    Code == 59 ~ "Post-colazione",
    Code == 60 ~ "Pre-pranzo",
    Code == 61 ~ "Post-pranzo",
    Code == 62 ~ "Pre-cena",
    Code == 63 ~ "Post-cena",
    Code == 64 ~ "Pre-spuntino",
    TRUE ~ "Altro"
  ))

# 📦 Calcolo del conteggio per tipo di misurazione e uso UltraLente
tipi_misurazione <- glucose_df %>%
  group_by(UsesUltraLente, Misurazione) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(FreqRelativa = round(Count / sum(Count), 4))

# 📋 Stampa della tabella con frequenza assoluta e relativa
print(tipi_misurazione, n = Inf)

# 📈 Grafico leggibile della frequenza dei tipi di misurazione glicemica
grafico_misure <- ggplot(tipi_misurazione, aes(x = Misurazione, y = Count, fill = as.factor(UsesUltraLente))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frequenza dei tipi di misurazione glicemica",
    x = "Momento della giornata",
    y = "Conteggio",
    fill = "Usa UltraLente"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("grafici_plot/frequenza_misure_glicemiche.pdf", plot = grafico_misure, width = 9, height = 5, dpi = 300)
