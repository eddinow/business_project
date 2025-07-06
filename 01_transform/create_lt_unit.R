library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")
#source("02_model/kpis_material.R")

# Tidy -------------------------------------------------------------------------

# Weil bislang nur das df der Auftragsköpfe bereinigt ist, übertragen wir diese
# Logik auch auf die SAP-Vorgangsdaten. Das brauchen wir, um die Aufträge wieder
# auf Vorgangsebene (Unterteilung von Aufträgen nach durchlaufenden Vorgangsfolgen)
# analysieren zu können.

vorgaenge_raw <- vorgaenge_raw %>%
    filter(Auftragsnummer %in% all_data_finalized$auftragsnummer)

# Vorgänge cleaned enthält pro Vorgangsnummer noch eine Zeile pro Auftrag, so dass 
# ein Auftrag mehrere Zeilen hat. Das ist notwendig um zwischen Prozesschritten
# differenzieren zu können
vorgaenge_cleaned <- vorgaenge_raw %>%
    left_join(
        all_data_finalized %>%
            dplyr::select(
                auftragsnummer,
                materialnummer,
                werk,
                starttermin_soll,
                endtermin_soll,
                fertigungslinie,
                planer,
                vorgangsfolge,
                arbeitsplatzfolge,
                sollmenge
            ),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    dplyr::select(-`ME`, -`Ausschuss Vorgang`)

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        `Iststart Vorgang` = as.Date(`Iststart Vorgang`),
        `Istende Vorgang`  = as.Date(`Istende Vorgang`)
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        solldauer = as.numeric(difftime(endtermin_soll, starttermin_soll, units = "days")),
        istdauer = as.numeric(difftime(`Istende Vorgang`, `Iststart Vorgang`, units = "days")),
        abweichung = istdauer - solldauer
    )


vorgaenge_cleaned <- vorgaenge_cleaned %>%
    arrange(vorgangsfolge, Vorgangsnummer)

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(gesamtdauer = as.numeric(`Istende Vorgang` - `Iststart Vorgang`))

vorgaenge_sorted <- vorgaenge_cleaned %>%
    arrange(Auftragsnummer, `Iststart Vorgang`)

vorgaenge_sorted$starttermin_soll <- as.Date(vorgaenge_sorted$starttermin_soll)
vorgaenge_sorted$`Istende Vorgang` <- as.Date(vorgaenge_sorted$`Istende Vorgang`)

vorgaenge_sorted <- vorgaenge_sorted %>%
    mutate(
        lt_soll_order = ifelse(sollmenge > 0, round((solldauer / sollmenge) * 86400, 2), NA),
        lt_ist_order = ifelse(`Gutmenge Vorgang` > 0, round((istdauer / `Gutmenge Vorgang`) * 86400, 2), NA),
        abweichung_unit = round(istdauer - solldauer, 2)
    )

# Klassifikation nach Sollmenge in vorgaenge_sorted
material_klassifikation_vorgang <- vorgaenge_sorted %>%
    group_by(materialnummer) %>%
    summarise(
        total_sollmenge = sum(sollmenge, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(total_sollmenge)) %>%
    mutate(
        kum_anteil = cumsum(total_sollmenge) / sum(total_sollmenge, na.rm = TRUE),
        klassifikation = case_when(
            kum_anteil <= 0.8  ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE               ~ "C"
        )
    ) %>%
    dplyr::select(materialnummer, klassifikation)
vorgaenge_sorted <- vorgaenge_sorted %>%
    left_join(material_klassifikation_vorgang, by = "materialnummer")


# Transform --------------------------------------------------------------------

# Umrechnen der existierenden Lead Times pro Auftrag auf Ebene von einer 
# gefertigten Mengeneinheit. Für Soll-LT wird auf Sollmenge umgelegt, für Ist-LT
# wird auf Gutmenge umgelegt. Diese LT-Konvertierung nehmen wir sowohl für das DF
# der Auftragsdaten, als auch auf das der Vorgänge vor.

# Vorgangsebene-----

material_klassifikation_vorgang <- all_data_finalized %>%
    group_by(materialnummer) %>%
    summarise(
        total_menge = sum(gelieferte_menge, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(total_menge)) %>%
    mutate(
        kum_anteil = cumsum(total_menge) / sum(total_menge, na.rm = TRUE),
        klassifikation = case_when(
            kum_anteil <= 0.8  ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE               ~ "C"
        )
    ) %>%
    dplyr::select(materialnummer, klassifikation)


vorgaenge_lt_unit <- all_data_finalized %>%
    mutate(
        lt_soll_order    = (lead_time_soll / sollmenge) * 86400,
        lt_ist_order     = (lead_time_ist / gelieferte_menge) * 86400,
        abweichung_unit  = round(lt_ist_order - lt_soll_order, 2),
        lt_soll_order    = round(lt_soll_order, 2),
        lt_ist_order     = round(lt_ist_order, 2)
    ) %>%
    left_join(material_klassifikation_vorgang, by = "materialnummer")


# Auftragsebene

material_klassifikation_auftrag <- all_data_finalized %>%
    group_by(materialnummer) %>%
    summarise(
        total_menge = sum(gelieferte_menge, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(total_menge)) %>%
    mutate(
        kum_anteil = cumsum(total_menge) / sum(total_menge, na.rm = TRUE),
        klassifikation = case_when(
            kum_anteil <= 0.8  ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE               ~ "C"
        )
    ) %>%
    dplyr::select(materialnummer, klassifikation)


auftraege_lt_unit <- all_data_finalized %>%
    mutate(
        lt_soll_order    = (lead_time_soll / sollmenge) * 86400,
        lt_ist_order     = (lead_time_ist / gelieferte_menge) * 86400,
        abweichung_unit  = round(lt_ist_order - lt_soll_order, 2),
        lt_soll_order    = round(lt_soll_order, 2),
        lt_ist_order     = round(lt_ist_order, 2)
    ) %>%
    left_join(material_klassifikation_auftrag, by = "materialnummer")



# Model -----------

# Visualize -------

# Communicate ------
