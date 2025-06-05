# === LIBRERIE ===
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# === 1. CARICAMENTO DATI ===
data <- read_csv("./dataset/diabetes_clean_data3.csv", show_col_types = FALSE)
data$DateTime <- ymd_hms(data$DateTime)

# === 2. FILTRA EVENTI DI INTERESSE ===
glicemie <- data %>% filter(Code >= 48 & Code <= 64)
ultralente <- data %>% filter(Code == 35)

# === 3. PRIMA COPPIA: GLICEMIA → INSULINA ULTRALENTE entro 20 min ===
step1 <- inner_join(
  glicemie,
  ultralente,
  by = "PatientID",
  suffix = c("_glic", "_ins")
) %>%
  filter(
    as.Date(DateTime_glic) == as.Date(DateTime_ins),
    difftime(DateTime_ins, DateTime_glic, units = "mins") >= 0,
    difftime(DateTime_ins, DateTime_glic, units = "mins") <= 20
  )

# === 4. AGGIUNGI GLICEMIA 4h DOPO INIEZIONE (tra 4h e 6h) ===
glicemie_post <- glicemie %>%
  rename(DateTime_post = DateTime, Value_post = Value)

final <- inner_join(
  step1,
  glicemie_post,
  by = "PatientID"
) %>%
  filter(
    as.Date(DateTime_ins) == as.Date(DateTime_post),
    difftime(DateTime_post, DateTime_ins, units = "mins") >= 240,  # >= 4h
    difftime(DateTime_post, DateTime_ins, units = "mins") <= 360   # <= 6h
  ) %>%
  mutate(DifferenzaGlicemia = Value_post - Value_glic)

# === 5. SALVA IL FILE ===
write_csv(final, "./dataset/analisi_ultralente_4h.csv")

# === 6. CREA BOXPLOT DELLA DIFFERENZA ===
pdf("./grafici_plot/ultralente_glicemia_differenza_boxplot.pdf", width = 7, height = 5)

ggplot(final, aes(x = "", y = DifferenzaGlicemia)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +  # linee ai baffi
  geom_boxplot(
    fill = "skyblue", 
    outlier.colour = "black",
    outlier.shape = 16,     # cerchietto pieno
    outlier.size = 2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Variazione della glicemia 4h/6h dopo insulina ultralente",
       y = "Δ Glicemia (post - pre)",
       x = "") +
  theme_minimal()

dev.off()

