if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

linien_overview <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(lead_time_ist, na.rm = TRUE), 1),
        Median_LT = round(median(lead_time_ist, na.rm = TRUE), 1),
        Abweichung = round(mean(lead_time_ist - lead_time_soll, na.rm = TRUE), 1),
        Termintreue_prozent = round(mean(lead_time_ist <= lead_time_soll, na.rm = TRUE) * 100, 1),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        Liefertreue_prozent = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE) * 100, 1),
        .groups = "drop"
    ) %>%
    group_by(fertigungslinie) %>%
    mutate(
        Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)
    ) %>%
    ungroup()
