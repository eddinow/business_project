library(dplyr)
source("00_tidy/create_all_data_finalized.R")  

# 1. KPIs berechnen (inkl. Durchlaufzeit direkt in der Pipe berechnet, nicht gespeichert)
kpi_linie_vorgangsfolge <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    mutate(
        Durchlaufzeit = as.numeric(endtermin_ist - starttermin_ist)
    ) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        Ø_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Termintreue = round(mean(endtermin_ist <= endtermin_soll, na.rm = TRUE), 2),
        Ø_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    group_by(fertigungslinie) %>%
    mutate(Anteil = round(Anzahl / sum(Anzahl), 3)) %>%
    ungroup() %>%
    mutate(
        Anteil = ifelse(Anteil < 0.001, "<0.001", format(round(Anteil, 3), nsmall = 3))
    )

