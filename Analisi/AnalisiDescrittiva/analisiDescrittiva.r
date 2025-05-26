library(tidyverse)

# Leggi il CSV
full_data <- read_csv("./dataset/diabetes_full_data.csv")

# Verifica struttura
glimpse(full_data)

# Numero di pazienti
num_pazienti <- n_distinct(full_data$PatientID)
cat("Numero totale di pazienti:", num_pazienti, "\n")

# Statistiche base
summary(full_data)

# Frequenza dei codici
code_counts <- full_data %>%
  count(Code) %>%
  arrange(desc(n))

# Visualizza i codici più frequenti
print(code_counts)


# Grafico a barre dei 15 codici più comuni
code_counts %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(as.factor(Code), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Eventi clinici più frequenti (codici)",
    x = "Codice evento",
    y = "Frequenza"
  ) +
  theme_minimal()


# Quanti eventi per paziente
eventi_paziente <- full_data %>%
  count(PatientID) %>%
  arrange(desc(n))

# Visualizza primi 10 pazienti per numero eventi
print(head(eventi_paziente, 10))

#per ogni paziente, quanti eventi di ciascun tipo ci sono stati
tabella_codici_pazienti <- full_data %>%
  count(PatientID, Code) %>%
  pivot_wider(names_from = Code, values_from = n, values_fill = 0)

# Visualizza una parte della tabella
print(head(tabella_codici_pazienti))


# Esempio per il codice glicemia (es. 58)
full_data %>%
  filter(Code == 58) %>%
  ggplot(aes(x = Value)) +
  geom_histogram(binwidth = 10, fill = "tomato", color = "white") +
  labs(title = "Distribuzione glicemia pre-colazione (Code 58)")


# Crea la colonna DateTime combinando Date e Time
full_data <- full_data %>%
  mutate(
    DateTime = mdy_hm(paste(Date, Time))
  )

full_data %>%
  filter(Code == 58 , PatientID=="01") %>%
  ggplot(aes(x = DateTime, y = Value, color = PatientID)) +
  geom_line(alpha = 0.5) +
  labs(title = "Andamento glicemia nel tempo (Code 58)")  

# crea un profiilo statistico per ciascun paziente
  full_data %>%
  group_by(PatientID, Code) %>%
  summarise(
    media = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  pivot_wider(names_from = Code, values_from = media, values_fill = NA)