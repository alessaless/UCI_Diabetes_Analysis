library(dplyr)
library(lubridate)

# Carica il dataset
dataset_path <- "./dataset/diabetes_full_data.csv"
diabetes_data <- read.csv(dataset_path, stringsAsFactors = FALSE)

# Parsing dell'orario
diabetes_data$Time <- as.character(diabetes_data$Time)
diabetes_data$ParsedTime <- parse_date_time(diabetes_data$Time, orders = c("%H:%M", "%H%M"), quiet = TRUE)

# Controllo errori di parsing
if (any(is.na(diabetes_data$ParsedTime))) {
  warning("Some time entries could not be parsed.")
}

# Filtro: solo Code == 48 e ora < 8:00
diabetes_filtered <- diabetes_data %>%
  filter(Code == 57, hour(ParsedTime) < 7)

# Mostra le prime righe
print(head(diabetes_filtered))

# Scrivi su file
write.csv(diabetes_filtered,
          "./dataset/diabetes_filtered_codes_before_8am.csv",
          row.names = FALSE)
