library(dplyr)

source("00_tidy/create_all_data_finalized.R")  

# Sicherstellen, dass LT korrekt ist
all_data_finalized <- all_data_finalized %>%
    mutate(
        Durchlaufzeit = as.numeric(`endtermin_ist` - `starttermin_ist`)
    )

# 1. Häufigkeit der Vorgangsfolgen pro Fertigungslinie
vorgangsanteile <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    group_by(fertigungslinie) %>%
    mutate(
        Anteil = round(Anzahl / sum(Anzahl), 3)
    ) %>%
    arrange(fertigungslinie, desc(Anteil))

# 2. Durchschnittliche LT und Losgröße pro Linie + Vorgangsfolge
lt_pro_vorgangslinie <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        .groups = "drop"
    )

# 3. Kombiniere Anteil + LT
kpi_linie_vorgangsfolge <- vorgangsanteile %>%
    left_join(lt_pro_vorgangslinie, by = c("fertigungslinie", "vorgangsfolge")) %>%
    arrange(fertigungslinie, desc(Anteil))