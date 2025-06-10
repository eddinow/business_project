if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

library(stringr)

werke_overview <- auftraege_lt_unit %>%
    filter(!is.na(werk), !is.na(vorgangsfolge)) %>%
    mutate(
        ist_verspaetet = lt_ist_order > lt_soll_order,
        termintreu = lt_ist_order <= lt_soll_order,
        liefertreu = gelieferte_menge >= sollmenge,
        servicelevel = sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(auftraege_lt_unit$abweichung_unit))
    ) %>%
    group_by(werk, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Median_LT = round(median(lead_time_ist, na.rm = TRUE), 1),
        Durchschnitt_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Average_Delay = round(mean(abweichung[abweichung > 0], na.rm = TRUE), 1),
        Termintreue_prozent = round(mean(termintreu, na.rm = TRUE) * 100, 1),
        Liefertreue_prozent = round(mean(liefertreu, na.rm = TRUE) * 100, 1),
        Servicelevel_prozent = servicelevel,
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        .groups = "drop"
    ) %>%
    group_by(werk) %>%
    mutate(
        Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)
    ) %>%
    ungroup()
