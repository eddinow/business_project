# Wir plotten für jede LT die min und max Sollmenge um die Losgrößen zu verstehen
# bzw rauszufinden ab welche Mengensprüngen sich die LT erhöht

# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(dplyr)

# Daten laden -------------------------------------------------------------------
source("00_tidy/create_all_data_finalized.R") 

lt_vs_sollmenge <- all_data_finalized %>%
    group_by(lead_time_soll) %>%
    summarise(
        Sollmenge_min = min(sollmenge, na.rm = TRUE),
        Sollmenge_max = max(sollmenge, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(lead_time_soll)

view(lt_vs_sollmenge)