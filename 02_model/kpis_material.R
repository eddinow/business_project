library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Daten laden
source("01_transform/create_lt_unit.R", local = TRUE)
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Schritt 1: Prozessschritte ("Hauptabfolge") je Materialnummer bestimmen
material_abfolge <- all_data_finalized %>%
    group_by(materialnummer) %>%
    summarise(
        prozessschritte = names(sort(table(vorgangsfolge), decreasing = TRUE))[1], .groups = "drop"
    )

# Schritt 2: Materialnummer-Übersicht OHNE Soll-LT
materialnummer_overview <- auftraege_lt_unit %>%
    group_by(materialnummer) %>%
    summarise(
        Anzahl = n(),
        Gesamtmenge = sum(gelieferte_menge, na.rm = TRUE),
        Sollmenge = sum(sollmenge, na.rm = TRUE),
        Ø_Abweichung_h = round(mean(abweichung_unit, na.rm = TRUE) / 3600, 2),
        Ø_LT_pro_Unit_h = round(mean(lt_ist_order, na.rm = TRUE) / 3600, 2),
        Anteil_pünktlich = round(mean(abweichung_unit <= 0, na.rm = TRUE), 2)
    ) %>%
    arrange(desc(Gesamtmenge))

# Schritt 3: Prozessschritte dazujoinen
materialnummer_overview <- materialnummer_overview %>%
    left_join(material_abfolge, by = "materialnummer") %>%
    mutate(Prozesstiefe = str_count(prozessschritte, "→") + 1)

# Schritt 4: ABC-Klassen berechnen
materialnummer_overview <- materialnummer_overview %>%
    mutate(
        kum_anteil = cumsum(Gesamtmenge) / sum(Gesamtmenge),
        ABC_Klasse = case_when(
            kum_anteil <= 0.8 ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE ~ "C"
        )
    )

# Schritt 5: ABC-Summary (hier NUR Soll-LT mit aufnehmen!)
abc_summary <- materialnummer_overview %>%
    left_join(
        auftraege_lt_unit %>%
            group_by(materialnummer) %>%
            summarise(Ø_Soll_LT_pro_Unit_h = round(mean(lt_soll_order, na.rm = TRUE) / 3600, 2)),
        by = "materialnummer"
    ) %>%
    group_by(ABC_Klasse) %>%
    summarise(
        Anzahl_Materialien = n(),
        Gesamtmenge = sum(Gesamtmenge, na.rm = TRUE),
        Ø_Abweichung_h = round(mean(Ø_Abweichung_h, na.rm = TRUE), 2),
        Ø_LT_pro_Unit_h = round(mean(Ø_LT_pro_Unit_h, na.rm = TRUE), 2),
        Ø_Soll_LT_pro_Unit_h = round(mean(Ø_Soll_LT_pro_Unit_h, na.rm = TRUE), 2),
        Anteil_pünktlich = round(mean(Anteil_pünktlich, na.rm = TRUE), 2),
        Prozesstiefe = round(mean(Prozesstiefe, na.rm = TRUE), 2)
    )
