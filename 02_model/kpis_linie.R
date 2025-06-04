if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

linien_overview <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    mutate(
        ist_verspaetet = lead_time_ist > lead_time_soll,
        delay = ifelse(ist_verspaetet, lead_time_ist - lead_time_soll, NA),
        termintreu     = lead_time_ist <= lead_time_soll,
        liefertreu     = gelieferte_menge >= sollmenge,
        servicelevel   = termintreu & liefertreu
    ) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(lead_time_ist, na.rm = TRUE), 1),
        Median_LT = round(median(lead_time_ist, na.rm = TRUE), 1),
        Abweichung = round(mean(lead_time_ist - lead_time_soll, na.rm = TRUE), 1),
        Average_Delay = round(mean(delay, na.rm = TRUE), 1),  # ← jetzt korrekt!
        Termintreue_prozent = round(mean(termintreu, na.rm = TRUE) * 100, 1),
        Liefertreue_prozent = round(mean(liefertreu, na.rm = TRUE) * 100, 1),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        Servicelevel_prozent = round(mean(servicelevel, na.rm = TRUE) * 100, 1),
        .groups = "drop"
    ) %>%
    group_by(fertigungslinie) %>%
    mutate(
        Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)
    ) %>%
    ungroup()

