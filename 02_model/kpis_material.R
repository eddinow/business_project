library(dplyr)
library(readxl)

# Daten einlesen
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
source("01_transform/create_lt_unit.R", local = TRUE)

# Hauptabfolge (= häufigste Vorgangsfolge je Materialnummer)
material_abfolge <- all_data_finalized %>%
    group_by(materialnummer, vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    group_by(materialnummer) %>%
    slice_max(order_by = Anzahl, n = 1, with_ties = FALSE) %>%
    dplyr::select(materialnummer, Hauptabfolge = vorgangsfolge)

# Materialnummer-Übersicht auf Unit-Basis
materialnummer_overview <- auftraege_lt_unit %>%
    group_by(materialnummer) %>%
    summarise(
        Anzahl = n(),
        Gesamtmenge = sum(gelieferte_menge, na.rm = TRUE),
        Sollmenge = sum(sollmenge, na.rm = TRUE),
        Ø_Abweichung = round(mean(abweichung_unit, na.rm = TRUE), 2),
        Ø_LT_pro_Unit = round(mean(lt_ist_order, na.rm = TRUE), 2),
        Anteil_pünktlich = mean(abweichung_unit <= 0, na.rm = TRUE) # KEIN round, Anteil für Prozent später
    ) %>%
    left_join(material_abfolge, by = "materialnummer") %>%
    ungroup()

# ABC-Analyse auf Gesamtmenge
materialnummer_overview <- materialnummer_overview %>%
    arrange(desc(Gesamtmenge)) %>%
    mutate(
        kum_anteil = cumsum(Gesamtmenge) / sum(Gesamtmenge),
        ABC_Klasse = case_when(
            kum_anteil <= 0.8 ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE ~ "C"
        )
    )

# Übersichtstabelle je Klasse (ABC)
abc_summary <- materialnummer_overview %>%
    group_by(ABC_Klasse) %>%
    summarise(
        Anzahl_Materialien = n(),
        Gesamt_Anzahl = sum(Anzahl, na.rm = TRUE),
        Gesamtmenge = sum(Gesamtmenge, na.rm = TRUE),
        Gesamtsollmenge = sum(Sollmenge, na.rm = TRUE),
        Ø_Abweichung = round(mean(Ø_Abweichung, na.rm = TRUE), 2),
        Ø_LT_pro_Unit = round(mean(Ø_LT_pro_Unit, na.rm = TRUE), 2),
        Anteil_pünktlich = mean(Anteil_pünktlich, na.rm = TRUE) # KEIN round!
    ) %>%
    ungroup()
