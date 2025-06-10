# 📦 Librerie
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(lubridate)

# 📁 Caricamento dati
data <- read.csv("./dataset/diabetes_clean_data3.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(data) <- c("PatientID", "Date", "Time", "Code", "Value", "DateTime_raw", "DateTime", "UsesUltraLente")

# 🔄 Conversioni
data$Code <- as.numeric(data$Code)
data$Value <- as.numeric(data$Value)
data$PatientID <- as.factor(data$PatientID)
data$Date <- as.Date(data$Date, format = "%m-%d-%Y")

# 🔍 Filtra solo codici glicemici (48–64)
glicemia <- data %>%
  filter(Code >= 48 & Code <= 64, !is.na(Date), !is.na(Value))

# 📆 Crea intervalli rigidi da 14 giorni
start_date <- min(glicemia$Date, na.rm = TRUE)
start_date <- start_date - as.integer(difftime(start_date, as.Date("1970-01-01"), units = "days")) %% 14
end_date <- max(glicemia$Date, na.rm = TRUE)
end_date <- end_date + (14 - as.integer(difftime(end_date, start_date, units = "days")) %% 14)
intervalli <- seq(start_date, end_date + 14, by = 14)

# ➕ Assegna ciascun record a un intervallo
glicemia <- glicemia %>%
  mutate(
    interval_start = cut(Date, breaks = intervalli, right = FALSE),
    interval_start = as.Date(as.character(interval_start))
  )

# 🧮 Calcola GMI e data media delle misurazioni
gmi_df <- glicemia %>%
  group_by(PatientID, interval_start) %>%
  summarise(
    GMI = 3.31 + 0.02392 * mean(Value, na.rm = TRUE),
    media_data = as.Date(mean(as.numeric(Date), na.rm = TRUE), origin = "1970-01-01"),
    .groups = "drop"
  )

# 📁 Crea cartella per output
if (!dir.exists("grafici_gmi_individuali")) {
  dir.create("grafici_gmi_individuali")
}

# 📊 Crea un grafico per ciascun paziente
for (pid in unique(gmi_df$PatientID)) {
  paziente_data <- gmi_df %>% filter(PatientID == pid)

  p <- ggplot(paziente_data, aes(x = interval_start, y = GMI, group = 1)) +
    geom_line(color = "#1f77b4") +
    geom_point(size = 2, color = "#1f77b4") +
    geom_text(aes(label = format(media_data, "%d-%b")), vjust = -1, size = 3) +
    scale_x_date(date_labels = "%d-%b", date_breaks = "2 weeks") +
    labs(
      title = paste("Andamento GMI – Paziente", pid),
      x = "Inizio intervallo (14 giorni)",
      y = "GMI"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = paste0("grafici_gmi_individuali/gmi_paziente_", pid, ".pdf"),
    plot = p, width = 8, height = 5
  )
}

cat("✅ Grafici GMI salvati in cartella: grafici_gmi_individuali/\n")
