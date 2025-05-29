if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}

library(stringr)

werke_overview <- all_data_finalized %>%
    filter(!is.na(werk), !is.na(vorgangsfolge)) %>%
    mutate(
        Durchlaufzeit = as.numeric(lead_time_ist),
        Komplexität = str_count(vorgangsfolge, "→") + 1,
        Startverzoegerung = as.numeric(starttermin_ist - starttermin_soll)
    ) %>%
    group_by(werk, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        Durchschnitt_Komplexitaet = round(mean(Komplexität, na.rm = TRUE), 1),
        Durchschnitt_Startverzoegerung = round(mean(Startverzoegerung, na.rm = TRUE), 1),
        Anteil_verspaetet = round(mean(abweichung > 0, na.rm = TRUE), 2),
        Durchschnitt_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 0),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        Termintreue = round(mean(abweichung <= 0, na.rm = TRUE), 2),
        .groups = "drop"
    )
