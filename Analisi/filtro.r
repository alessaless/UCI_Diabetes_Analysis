# Attempt to parse time, handling errors
library(dplyr)
library(lubridate)

# Carica il dataset
dataset_path <- "./dataset/diabetes_full_data.csv"
diabetes_data <- read.csv(dataset_path, stringsAsFactors = FALSE)

# Attempt to parse time, handling errors
diabetes_data$Time <- as.character(diabetes_data$Time)
time_parsed <- parse_date_time(diabetes_data$Time, orders = c("%H:%M", "%H%M"), quiet = TRUE)

# Check for parsing issues
if (any(is.na(time_parsed))) {
  warning("Some time entries could not be parsed.")
}

# Filtro: solo Code == 48 e ora < 8:00
diabetes_filtered <- diabetes_data %>%
  filter(Code == 48) %>%
  filter(hour(time_parsed) < 8)

# Mostra le prime righe
print(head(diabetes_filtered))

# Scrivi su file
write.csv(diabetes_filtered,
          "./dataset/diabetes_filtered_codes_before_8am.csv",
          row.names = FALSE)