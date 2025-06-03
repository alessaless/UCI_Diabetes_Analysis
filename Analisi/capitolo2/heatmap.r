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



# 📊 Heatmap: valori glicemici medi per ora del giorno, divisi per uso UltraLente
# Questo grafico mostra a che ora del giorno i valori medi di glicemia sono più alti o più bassi,
# separando i pazienti in base all'uso dell'insulina UltraLente (TRUE/FALSE)

heatmap_ora <- glucose_df %>%
  mutate(Ora = format(as.POSIXct(DateTime), "%H")) %>%
  group_by(Ora, UsesUltraLente) %>%
  summarise(Media = mean(Value), .groups = "drop") %>%
  ggplot(aes(x = Ora, y = as.factor(UsesUltraLente), fill = Media)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Heatmap dei valori glicemici medi per ora",
    x = "Ora del giorno",
    y = "Usa UltraLente",
    fill = "Glicemia media"
  ) +
  theme_minimal()

# 💾 Salvataggio della heatmap nella cartella 'grafici'
ggsave("grafici_plot/heatmap_glicemia_media_per_ora.pdf", plot = heatmap_ora, width = 8, height = 4.5, dpi = 300)



######################################



# 🔢 Codici associati alle misurazioni glicemiche
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

# 📊 Creazione tabella con frequenze per tipo di evento per ciascun paziente
glicemia_per_insulina <- df %>%
  # Tieni solo i record di misurazione glicemica e somministrazione insulina
  filter(Code %in% c(glucose_codes, 33, 34, 35)) %>%
  
  # Crea una nuova colonna testuale che identifica il tipo di evento
  mutate(Evento = case_when(
    Code == 33 ~ "Insulina Regular",
    Code == 34 ~ "Insulina NPH",
    Code == 35 ~ "Insulina UltraLente",
    Code %in% glucose_codes ~ "Glicemia",
    TRUE ~ "Altro"
  )) %>%
  
  # Conta il numero di eventi per combinazione PatientID + Evento
  count(PatientID, Evento) %>%
  
  # Riorganizza la tabella: una colonna per ogni tipo di evento
  pivot_wider(names_from = Evento, values_from = n, values_fill = 0)

# 🔁 Converte in formato long per la heatmap (ogni riga = paziente × evento)
glicemia_long <- glicemia_per_insulina %>%
  pivot_longer(cols = -PatientID, names_to = "Evento", values_to = "Frequenza")

# 🎨 Crea una heatmap con ggplot
heatmap_plot <- ggplot(glicemia_long, aes(x = Evento, y = factor(PatientID), fill = Frequenza)) +
  geom_tile(color = "white") +  # Ogni cella è un rettangolo colorato
  scale_fill_viridis(name = "Frequenza", option = "D") +  # Palette viridis per la leggibilità
  labs(
    title = "Heatmap delle frequenze degli eventi per paziente",
    x = "Tipo di evento",
    y = "ID Paziente"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),        # Mostra le etichette dei pazienti (piccole)
    axis.ticks.y = element_line(size = 0.2)      # Aggiungi piccoli tick sull'asse Y
  )

# 💾 Salva il grafico in formato PDF nella cartella 'grafici_plot'
ggsave("grafici_plot/heatmap_eventi_pazienti.pdf", plot = heatmap_plot, width = 8, height = 6, dpi = 300)
