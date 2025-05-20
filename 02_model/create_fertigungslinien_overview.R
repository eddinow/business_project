# # Initialisierung ----------------------------------------------------------------
# 
# rm(list = ls())
# set.seed(1)

library(dplyr)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Transform------------------
# Für die Übersichtsseite der Linien erstellen wir ein Tableau mit allen Linien.
# Für jedes Werk wird die Avg LT berechnet (Median aller LT). Avg Delay ist Median
# aller Abweichung, no of orders die Anzahl der Aufträge. Servicelevel = Anzahl
#Aufträge mit Abweichung größer gleich 0 / Anzahl aller Aufträge

fertigungslinien_overview <- all_data_finalized %>%
    group_by(fertigungslinie) %>%
    summarise(
        `Avg LT` = median(lead_time_ist, na.rm = TRUE),
        `Avg Delay` = round(min(median(abweichung, na.rm = TRUE), 0), 0),
        `# orders` = n(),
        Servicelevel = mean(abweichung >= 0, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(Servicelevel = paste0(round(Servicelevel * 100, 0), "%")) %>%
    rename(Fertigungslinie = fertigungslinie)
