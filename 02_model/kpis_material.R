library(dplyr)
library(stringr)
library(readxl)

# Lade deine Unit-Daten (z.B. erzeugt mit create_lt_unit.R)
source("01_transform/create_lt_unit.R", local = TRUE)

# Hauptabfolge (ohne Ausreißer): Häufigste Vorgangsfolge pro Materialnummer
hauptfolge <- all_data_finalized %>%
    group_by(materialnummer, vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    group_by(materialnummer) %>%
    top_n(1, Anzahl) %>%
    slice(1) %>%
    ungroup() %>%
    select(materialnummer, hauptabfolge = vorgangsfolge)

# Materialnummern-Übersicht
materialnummer_overview <- auftraege_lt_unit %>%
    group_by(materialnummer) %>%
    summarise(
        Anzahl = n(),
        Gesamtmenge = sum(gelieferte_menge, na.rm = TRUE),
        Sollmenge = sum(sollmenge, na.rm = TRUE),
        Ø_Abweichung = round(mean(abweichung_unit, na.rm = TRUE), 2),
        Ø_LT_pro_Unit = round(mean(lt_ist_order, na.rm = TRUE), 2),
        Ø_SOLL_LT_pro_Unit = round(mean(lt_soll_order, na.rm = TRUE), 2)
    ) %>%
    left_join(hauptfolge, by = "materialnummer")

# ABC-Analyse (auf Gesamtmenge)
materialnummer_overview <- materialnummer_overview %>%
    arrange(desc(Gesamtmenge)) %>%
    mutate(
        kum_anteil = cumsum(Gesamtmenge) / sum(Gesamtmenge),
        ABC_Klasse = case_when(
            kum_anteil <= 0.8 ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE ~ "C"
        ),
        Prozesstiefe = str_count(hauptabfolge, "→") + 1
    )

# Für die ABC-Summary-Tabelle (SOLL_LT/Unit nur hier)
abc_summary <- materialnummer_overview %>%
    group_by(ABC_Klasse) %>%
    summarise(
        Anzahl_Materialien = n(),
        Gesamt_Anzahl = sum(Anzahl, na.rm = TRUE),
        Gesamtmenge = sum(Gesamtmenge, na.rm = TRUE),
        Gesamtsollmenge = sum(Sollmenge, na.rm = TRUE),
        Ø_Abweichung = round(mean(Ø_Abweichung, na.rm = TRUE), 2),
        Ø_LT_pro_Unit = round(mean(Ø_LT_pro_Unit, na.rm = TRUE), 2),
        Ø_SOLL_LT_pro_Unit = round(mean(Ø_SOLL_LT_pro_Unit, na.rm = TRUE), 2),
        Anteil_pünktlich = round(mean((Ø_Abweichung <= 0), na.rm = TRUE), 2),
        Prozesstiefe = round(mean(Prozesstiefe, na.rm = TRUE), 2)
    )
