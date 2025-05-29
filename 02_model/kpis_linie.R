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
        Termintreue = round(mean(lead_time_ist <= lead_time_soll, na.rm = TRUE), 2),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    group_by(fertigungslinie) %>%
    mutate(Anteil = round(Anzahl / sum(Anzahl), 3)) %>%
    ungroup() %>%
    mutate(
        Anteil = ifelse(Anteil < 0.001, "<0.001", format(round(as.numeric(Anteil), 3), nsmall = 3))
    )
