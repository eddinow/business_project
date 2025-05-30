if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

library(stringr)

werke_overview <- all_data_finalized %>%
    filter(!is.na(werk), !is.na(vorgangsfolge)) %>%
    mutate(
        Durchlaufzeit = as.numeric(lead_time_ist),
        Komplexitaet = str_count(vorgangsfolge, "→") + 1,
        Startverzoegerung = ifelse(
            starttermin_ist > starttermin_soll,
            as.numeric(difftime(starttermin_ist, starttermin_soll, units = "days")),
            0
        )
    ) %>%
    group_by(werk, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        Durchschnitt_Komplexitaet = round(mean(Komplexitaet, na.rm = TRUE), 1),
        Durchschnitt_Startverzoegerung = round(mean(Startverzoegerung, na.rm = TRUE), 1),
        Anteil_verspaetet_prozent = round(mean(abweichung > 0, na.rm = TRUE) * 100, 1),
        Durchschnitt_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 0),
        Liefertreue_prozent = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE) * 100, 1),
        Termintreue_prozent = round(mean(abweichung <= 0, na.rm = TRUE) * 100, 1),
        .groups = "drop"
    )  %>%
    group_by(werk) %>%
    mutate(Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%
    ungroup()
