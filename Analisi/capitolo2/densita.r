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


# 📊 Kernel Density Plot dei valori glicemici per gruppo UltraLente
density_plot <- ggplot(glucose_df, aes(x = Value, fill = as.factor(UsesUltraLente), color = as.factor(UsesUltraLente))) +
  geom_density(alpha = 0.4, size = 1) +
  scale_fill_viridis_d(option = "D", name = "Usa UltraLente") +
  scale_color_viridis_d(option = "D", name = "Usa UltraLente") +
  labs(
    title = "Distribuzione di densità dei valori glicemici",
    x = "Glicemia (mg/dL)",
    y = "Densità"
  ) +
  theme_minimal()

# 💾 Salvataggio
ggsave("grafici_plot/densita_valori_glicemici.pdf", plot = density_plot, width = 7, height = 4.5, dpi = 300)

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

density_misurazione <- ggplot(glucose_df, aes(x = Value, color = Misurazione, fill = Misurazione)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Distribuzione dei valori glicemici per momento della giornata",
    x = "Glicemia (mg/dL)",
    y = "Densità"
  ) +
  theme_minimal()

ggsave("grafici_plot/densita_glicemia_per_misurazione.pdf", plot = density_misurazione, width = 8, height = 5, dpi = 300)

media_giornaliera <- glucose_df %>%
  mutate(Data = as.Date(DateTime)) %>%
  group_by(PatientID, Data, UsesUltraLente) %>%
  summarise(Media = mean(Value), .groups = "drop")

density_giorno <- ggplot(media_giornaliera, aes(x = Media, fill = as.factor(UsesUltraLente), color = as.factor(UsesUltraLente))) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuzione della glicemia media giornaliera",
    x = "Glicemia media (mg/dL)",
    y = "Densità",
    fill = "Usa UltraLente",
    color = "Usa UltraLente"
  ) +
  theme_minimal()

ggsave("grafici_plot/densita_media_giornaliera.pdf", plot = density_giorno, width = 8, height = 5, dpi = 300)

glucose_df <- glucose_df %>%
  mutate(GiornoSettimana = weekdays(as.Date(DateTime)))

# Ordina i giorni della settimana in inglese
glucose_df$GiornoSettimana <- factor(glucose_df$GiornoSettimana,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

density_giorni <- ggplot(glucose_df, aes(x = Value, fill = GiornoSettimana, color = GiornoSettimana)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Distribuzione dei valori glicemici per giorno della settimana",
    x = "Glicemia (mg/dL)",
    y = "Densità"
  ) +
  theme_minimal()

ggsave("grafici_plot/densita_giorni_settimana.pdf", plot = density_giorni, width = 9, height = 5, dpi = 300)

