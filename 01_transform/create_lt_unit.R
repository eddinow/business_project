library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")

# Tidy -------------------------------------------------------------------------

# Weil bislang nur das df der Auftragsköpfe bereinigt ist, übertragen wir diese
# Logik auch auf die SAP-Vorgangsdaten. Das brauchen wir, um die Aufträge wieder
# auf Vorgangsebene (Unterteilung von Aufträgen nach durchlaufenden Vorgangsfolgen)
# analysieren zu können.

vorgaenge_raw <- vorgaenge_raw %>%
    filter(Auftragsnummer %in% all_data_finalized$auftragsnummer)

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
                arbeitsplatzfolge
            ),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    dplyr::select(-`ME`, -`Gutmenge Vorgang`, -`Ausschuss Vorgang`)

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


# Transform --------------------------------------------------------------------

# Umrechnen der existierenden Lead Times pro Auftrag auf Ebene von einer 
# gefertigten Mengeneinheit. Für Soll-LT wird auf Sollmenge umgelegt, für Ist-LT
# wird auf Gutmenge umgelegt. Diese LT-Konvertierung nehmen wir sowohl für das DF
# der Auftragsdaten, als auch auf das der Vorgänge vor.

# Vorgangsebene-----

vorgaenge_lt_unit <- vorgaenge_sorted |> 
    # Hole Sollmenge je Auftrag (1 Zeile je Auftrag)
    dplyr::left_join(
        all_data_finalized |> dplyr::select(auftragsnummer, sollmenge),
        by = c("Auftragsnummer" = "auftragsnummer")
    ) |>
    # Hole Gutmenge je Auftrag + Vorgang
    dplyr::left_join(
        vorgaenge_raw |> dplyr::select(Auftragsnummer, Vorgangsnummer, `Gutmenge Vorgang`),
        by = c("Auftragsnummer", "Vorgangsnummer")
    ) |>
    # Berechne Lead Time pro Einheit (in Sekunden, LT*24*60*60)
    dplyr::mutate(
        lt_soll_order = (solldauer / sollmenge) * 86400,
        lt_ist_order  = (istdauer / `Gutmenge Vorgang`) * 86400,
        abweichung_unit = lt_ist_order - lt_soll_order
    ) |>
    
    dplyr::mutate(
        abweichung_unit = round(abweichung_unit, 2),
        lt_soll_order = round(lt_soll_order, 2),
        lt_ist_order = round(lt_ist_order, 2)
    )


# Auftragsebene

auftraege_lt_unit <- all_data_finalized %>%
   
    # Berechne Lead Times je Auftrag (in Sekunden)
    dplyr::mutate(
        lt_soll_order = (lead_time_soll / sollmenge) * 86400,
        lt_ist_order  = (lead_time_ist / gelieferte_menge) * 86400,
        abweichung_unit = lt_ist_order - lt_soll_order
    ) %>%

    dplyr::mutate(
        abweichung_unit = round(abweichung_unit, 2),
        lt_soll_order = round(lt_soll_order, 2),
        lt_ist_order = round(lt_ist_order, 2)
    )

# Model -----------

# Visualize -------

# Communicate ------
