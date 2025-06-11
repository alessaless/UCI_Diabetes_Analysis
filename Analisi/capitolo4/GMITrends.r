# ──────────────────────────────────────────────────────────────────────────────
# Trend GMI settimanale per pazienti con/senza insulina ultralenta
#   – 2 PDF per i pazienti CON ultralenta
#   – 5 PDF per i pazienti SENZA ultralenta
#   – legenda con PatientID e colore
#   – riepilogo finale: quanti pazienti migliorano (GMI finale > GMI iniziale)
# ──────────────────────────────────────────────────────────────────────────────

# ── Librerie ─────────────────────────────────────────────────────────────────
library(readr)     # read_csv()
library(dplyr)     # filter(), mutate(), summarise()
library(ggplot2)   # grafici

# ── Percorsi ─────────────────────────────────────────────────────────────────
data_path  <- "dataset/gmi_results.csv"   # CSV di origine
output_dir <- "gmi_trends"                # cartella per i PDF
if (!dir.exists(output_dir)) dir.create(output_dir)

# ── Import dati ──────────────────────────────────────────────────────────────
df <- read_csv(data_path, show_col_types = FALSE) |>
        mutate(
          Week           = as.integer(Week),
          UsesUltraLente = as.logical(UsesUltraLente),
          PatientID      = as.factor(PatientID)
        )

# ── Funzione: suddivide i pazienti in gruppi e genera un PDF per gruppo ──────
plot_by_groups <- function(data, uses_ultra, n_groups, prefix) {
  ids <- sort(unique(data$PatientID))                # ordina gli ID paziente
  group_index <- cut(seq_along(ids),
                     breaks = n_groups,
                     labels = FALSE, include.lowest = TRUE)
  groups <- split(ids, group_index)

  for (i in seq_along(groups)) {
    subset_ids <- groups[[i]]
    if (length(subset_ids) == 0) next

    titolo <- sprintf(
      "GMI – pazienti %s (gruppo %d/%d)",
      if (uses_ultra) "CON insulina ultralenta" else "SENZA insulina ultralenta",
      i, n_groups
    )

    p <- ggplot(
           filter(data, PatientID %in% subset_ids),
           aes(x = Week, y = GMI, group = PatientID, colour = PatientID)
         ) +
           geom_line(linewidth = 0.9, alpha = 0.8) +
           geom_point(size = 1.5) +
           labs(title = titolo,
                x = "Settimana",
                y = "GMI",
                colour = "PatientID") +
           theme_minimal() +
           guides(colour = guide_legend(ncol = 3))   # legenda a 3 colonne

    file_name <- sprintf("%s_group%02d.pdf", prefix, i)
    ggsave(file.path(output_dir, file_name), p, width = 8, height = 5)
  }
}

# ── Creazione grafici ────────────────────────────────────────────────────────
plot_by_groups(
  data       = filter(df, UsesUltraLente),
  uses_ultra = TRUE,
  n_groups   = 2,
  prefix     = "gmi_ultralente"
)

plot_by_groups(
  data       = filter(df, !UsesUltraLente),
  uses_ultra = FALSE,
  n_groups   = 5,
  prefix     = "gmi_no_ultralente"
)

# ── Riepilogo finale: pazienti con GMI finale > GMI iniziale ─────────────────
gmi_change <- df |>
  arrange(PatientID, Week) |>
  group_by(PatientID, UsesUltraLente) |>
  summarise(first_GMI = first(GMI),
            last_GMI  = last(GMI),
            .groups   = "drop") |>
  mutate(increase = last_GMI > first_GMI)

count_ultra     <- sum(gmi_change$increase & gmi_change$UsesUltraLente)
count_no_ultra  <- sum(gmi_change$increase & !gmi_change$UsesUltraLente)

cat(sprintf("\nPazienti che usano ultralenta con GMI finale maggiore dell'iniziale: %d\n",
            count_ultra))
cat(sprintf("Pazienti che non usano ultralenta con GMI finale maggiore dell'iniziale: %d\n",
            count_no_ultra))
