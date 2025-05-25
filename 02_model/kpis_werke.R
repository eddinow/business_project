if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

library(stringr)

werke_overview <- all_data_finalized %>%
    filter(!is.na(werk), !is.na(vorgangsfolge)) %>%
    mutate(
        Durchlaufzeit = as.numeric(lead_time_ist),
        Komplexität = str_count(vorgangsfolge, "→") + 1,
        Startverzögerung = as.numeric(starttermin_ist - starttermin_soll)
    ) %>%
    group_by(werk, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        SD_LT = round(sd(Durchlaufzeit, na.rm = TRUE), 1),
        Ø_Komplexität = round(mean(Komplexität, na.rm = TRUE), 1),
        Ø_Startverzögerung = round(mean(Startverzögerung, na.rm = TRUE), 1),
        Anteil_verspätet = round(mean(abweichung > 0, na.rm = TRUE), 2),
        Ø_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Ø_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 0),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        Termintreue = round(mean(abweichung <= 0, na.rm = TRUE), 2),
        .groups = "drop"
    )
