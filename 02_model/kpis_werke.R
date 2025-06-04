if (!exists("all_data_finalized")) {
    source("00_tidy/create_all_data_finalized.R", local = TRUE)
}
library(stringr)

werke_overview <- all_data_finalized %>%
    filter(!is.na(werk), !is.na(vorgangsfolge)) %>%
    mutate(
        Durchlaufzeit = as.numeric(lead_time_ist),
        Startverzoegerung = ifelse(
            starttermin_ist > starttermin_soll,
            as.numeric(difftime(starttermin_ist, starttermin_soll, units = "days")),
            0
        ),
        ist_verspaetet = lead_time_ist > lead_time_soll,
        termintreu     = lead_time_ist <= lead_time_soll,
        liefertreu     = gelieferte_menge >= sollmenge,
        servicelevel   = termintreu & liefertreu
    ) %>%
    group_by(werk, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        Durchschnitt_Abweichung = round(mean(lead_time_ist - lead_time_soll, na.rm = TRUE), 1),
        Average_Delay = round(mean((lead_time_ist - lead_time_soll)[lead_time_ist > lead_time_soll], na.rm = TRUE), 1),
        Anteil_verspaetet_prozent = round(mean(ist_verspaetet, na.rm = TRUE) * 100, 1),
        Termintreue_prozent = round(mean(termintreu, na.rm = TRUE) * 100, 1),
        Liefertreue_prozent = round(mean(liefertreu, na.rm = TRUE) * 100, 1),
        Servicelevel_prozent = round(mean(servicelevel, na.rm = TRUE) * 100, 1),
        Durchschnitt_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 0),
        .groups = "drop"
    ) %>%
    group_by(werk) %>%
    mutate(Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%
    ungroup()
