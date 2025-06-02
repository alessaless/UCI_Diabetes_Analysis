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
  scale_fill_viridis_d(option = "D")  # puoi anche provare: "A", "B", "C", "E", "F"

# 💾 Salva il grafico
ggsave("grafici_plot/frequenza_code.pdf", plot = plot_code, width = 6, height = 4, dpi = 300)



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

# 🩸 Filtra solo i codici relativi alle misurazioni glicemiche
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)
glucose_df <- df %>%
  filter(Code %in% glucose_codes)

# 📊 Istogramma con colori viridis in base alla frequenza
glicem <- ggplot(glucose_df, aes(x = Value)) +
  geom_histogram(aes(fill = ..count..), binwidth = 20, color = "white") +
  scale_fill_viridis_c(option = "D") +  # puoi cambiare palette qui (A, B, C, etc.)
  labs(
    title = "Distribuzione dei valori glicemici",
    x = "Glicemia (mg/dL)",
    y = "Frequenza",
    fill = "Frequenza"
  ) +
  theme_minimal()

# 💾 Salvataggio in PDF nella cartella grafici
ggsave("grafici_plot/distribuzione_valori_glicemici.pdf", plot = glicem, width = 6, height = 4, dpi = 300)


# 📊 Grafico: boxplot delle glicemie per tutti i pazienti, divisi per uso UltraLente
boxplot_glicemia <- ggplot(glucose_df, aes(x = as.factor(UsesUltraLente), y = Value, fill = as.factor(UsesUltraLente))) +
  geom_boxplot() +
  labs(
    title = "Confronto valori glicemici (con/senza UltraLente)",
    x = "Usa UltraLente",
    y = "Valore glicemia (mg/dL)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "darkgreen"), name = "Usa UltraLente")
ggsave("grafici_plot/boxplot_glicemia_ultralente.pdf", plot = boxplot_glicemia, width = 6, height = 4, dpi = 300)

# 📊 Calcolo media glicemica per ogni paziente
media_paziente <- glucose_df %>%
  group_by(PatientID, UsesUltraLente) %>%
  summarise(ValoreMedio = mean(Value), .groups = "drop")

# 📊 Grafico: boxplot della glicemia media per paziente, divisi per gruppo
boxplot_media <- ggplot(media_paziente, aes(x = as.factor(UsesUltraLente), y = ValoreMedio)) +
  geom_boxplot(fill = "#c75ff0") +
  labs(
    title = "Glicemia media per paziente (con/senza UltraLente)",
    x = "Usa UltraLente",
    y = "Valore glicemia medio"
  ) +
  theme_minimal()
ggsave("grafici_plot/boxplot_media_glicemica_per_paziente.pdf", plot = boxplot_media, width = 6, height = 4, dpi = 300)

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

# 📊 Grafico: distribuzione dei pazienti in fasce cliniche (basata sulla media)
fasce_media <- media_paziente %>%
  group_by(UsesUltraLente, FasciaMedia) %>%
  summarise(Count = n(), .groups = "drop")

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

# 📊 Mappa dei codici a momenti della giornata
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

# 📈 Conteggio per tipo di misurazione
tipi_misurazione <- glucose_df %>%
  group_by(UsesUltraLente, Misurazione) %>%
  summarise(Count = n(), .groups = "drop")

# 📊 Grafico leggibile della frequenza dei tipi di misurazione glicemica
grafico_misure <- ggplot(tipi_misurazione, aes(x = Misurazione, y = Count, fill = as.factor(UsesUltraLente))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frequenza dei tipi di misurazione glicemica",
    x = "Momento della giornata",
    y = "Conteggio",
    fill = "Usa UltraLente"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 👈 Etichette ruotate a 45°

# 💾 Salva in PDF con dimensione adeguata
ggsave("grafici_plot/frequenza_misure_glicemiche.pdf", plot = grafico_misure, width = 9, height = 5, dpi = 300)


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

# 📊 Boxplot: distribuzione glicemia per ogni paziente
boxplot_per_paziente <- ggplot(glucose_df, aes(x = as.factor(PatientID), y = Value)) +
  geom_boxplot(outlier.size = 0.5, fill = "skyblue") +
  labs(
    title = "Distribuzione glicemica per paziente",
    x = "ID Paziente",
    y = "Glicemia (mg/dL)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # nasconde le etichette pazienti per chiarezza

ggsave("grafici_plot/boxplot_glicemia_per_paziente.pdf", plot = boxplot_per_paziente, width = 10, height = 5, dpi = 300)

# 📊 Boxplot: glicemia per giorno della settimana ordinato correttamente in inglese
boxplot_per_giorno <- glucose_df %>%
  mutate(
    GiornoSettimana = weekdays(as.Date(DateTime)),
    GiornoSettimana = factor(GiornoSettimana,
                             levels = c("Monday", "Tuesday", "Wednesday", 
                                        "Thursday", "Friday", "Saturday", "Sunday"))
  ) %>%
  ggplot(aes(x = GiornoSettimana, y = Value)) +
  geom_boxplot(fill = "#ff66b2") +
  labs(
    title = "Distribuzione glicemica per giorno della settimana",
    x = "Giorno",
    y = "Glicemia (mg/dL)"
  ) +
  theme_minimal()

ggsave("grafici_plot/boxplot_glicemia_per_giorno.pdf", plot = boxplot_per_giorno, width = 8, height = 5, dpi = 300)

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



# 🔢 Codici associati alle misurazioni glicemiche
glucose_codes <- c(48, 57, 58, 59, 60, 61, 62, 63, 64)

# 📊 Creazione tabella con frequenze per tipo di evento per ciascun paziente
glicemia_per_insulina <- df2 %>%
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


