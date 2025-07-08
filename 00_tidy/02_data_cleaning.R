# Initialize -------------------------------------------------------------------

# Hinweis: Löschen der Umgebung und Setzen des Zufallsseeds ggf. manuell aktivieren
# rm(list = ls())
# set.seed(1)

# Pakete laden
library(tidyverse)    # Datenmanipulation und -visualisierung
library(readxl)       # Excel-Dateien einlesen
library(lubridate)    # Datumshandling
library(janitor)      # Spaltennamen bereinigen
library(dplyr)        # Datenoperationen

# Import -----------------------------------------------------------------------

# SAP-Rohdaten einlesen
auftragskoepfe_sap_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_sap_raw       <- read_excel("vorgaenge_sap_raw.xlsx")

# Tidy -------------------------------------------------------------------------

# Spaltennamen in snake_case konvertieren
auftragskoepfe_sap_raw <- clean_names(auftragskoepfe_sap_raw)
vorgaenge_sap_raw      <- clean_names(vorgaenge_sap_raw)

# Datentypen vereinheitlichen – Auftragsköpfe
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw |> 
    mutate(
        sollmenge        = as.numeric(sollmenge),
        gelieferte_menge = as.numeric(gelieferte_menge),
        starttermin_soll = as_date(starttermin_soll),
        endtermin_soll   = as_date(endtermin_soll),
        starttermin_ist  = as_date(starttermin_ist),
        endtermin_ist    = as_date(endtermin_ist),
        erfassungsdatum  = as_date(erfassungsdatum),
        materialnummer   = as.character(materialnummer)  # wichtig für spätere Joins
    )

# Datentypen vereinheitlichen – Vorgänge
vorgaenge_sap_raw <- vorgaenge_sap_raw |> 
    mutate(
        gutmenge_vorgang  = as.numeric(gutmenge_vorgang),
        ausschuss_vorgang = as.numeric(ausschuss_vorgang),
        iststart_vorgang  = as_date(iststart_vorgang),
        istende_vorgang   = as_date(istende_vorgang)
    )


# Fehlende Werte analysieren – nur Spalten mit NA anzeigen

# Auftragsköpfe
auftragskoepfe_sap_raw |>
    summarise(across(everything(), list(
        n_missing   = ~ sum(is.na(.)),
        pct_missing = ~ mean(is.na(.)) * 100
    ), .names = "{.col}_{.fn}")) |>
    pivot_longer(everything(), names_to = c("column", "metric"),
                 names_pattern = "(.*)_(n_missing|pct_missing)$") |>
    pivot_wider(names_from = metric, values_from = value) |>
    filter(n_missing > 0)

# Vorgänge
vorgaenge_sap_raw |>
    summarise(across(everything(), list(
        n_missing   = ~ sum(is.na(.)),
        pct_missing = ~ mean(is.na(.)) * 100
    ), .names = "{.col}_{.fn}")) |>
    pivot_longer(everything(), names_to = c("column", "metric"),
                 names_pattern = "(.*)_(n_missing|pct_missing)$") |>
    pivot_wider(names_from = metric, values_from = value) |>
    filter(n_missing > 0)

# Konsistenzprüfung der Auftragsnummern
orders_ids <- unique(auftragskoepfe_sap_raw$auftragsnummer)
ops_ids    <- unique(vorgaenge_sap_raw$auftragsnummer)

# Hinweis bei Unstimmigkeiten (Ausgabe optional)
# if (!setequal(orders_ids, ops_ids)) { ... }


# Transform --------------------------------------------------------------------


# Unvollständige Vorgänge (ohne Iststart) entfernen
vorgaenge_sap_ohne_na <- vorgaenge_sap_raw |> 
    filter(!is.na(iststart_vorgang))

# Aufträge auf Basis dieser gültigen Vorgänge einschränken
auftragskoepfe_sap_ohne_na <- auftragskoepfe_sap_raw |> 
    semi_join(vorgaenge_sap_ohne_na, by = "auftragsnummer")

# Sicherstellen, dass beide Datensätze identische Auftragsnummern enthalten
stopifnot(setequal(auftragskoepfe_sap_ohne_na$auftragsnummer,
                   vorgaenge_sap_ohne_na$auftragsnummer))

# Ist-Daten aus Vorgängen übernehmen 
op_dates <- vorgaenge_sap_ohne_na |>
    group_by(auftragsnummer) |>
    summarise(
        min_iststart = min(iststart_vorgang, na.rm = TRUE),
        max_istende  = max(istende_vorgang, na.rm = TRUE),
        .groups = "drop"
    )

auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge <- 
    auftragskoepfe_sap_ohne_na |>
    left_join(op_dates, by = "auftragsnummer") |>
    mutate(
        replaced_start = is.na(starttermin_ist) | starttermin_ist != min_iststart,
        replaced_end   = is.na(endtermin_ist)  | endtermin_ist   != max_istende,
        starttermin_ist = min_iststart,
        endtermin_ist   = max_istende
    )

# Anzahl ersetzter Datumswerte (für Bericht)
replacement_summary <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge |>
    summarise(
        starttermine_ersetzt = sum(replaced_start, na.rm = TRUE),
        endtermine_ersetzt   = sum(replaced_end,   na.rm = TRUE)
    )

# Hilfsspalten entfernen
auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge <- 
    auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge |> 
    select(-min_iststart, -max_istende, -replaced_start, -replaced_end)

# Aufträge mit gelieferter Menge = 0 (Test-/Simulationsdaten) entfernen
auftraege_mit_null <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge |>
    filter(gelieferte_menge == 0) |>
    pull(auftragsnummer)

auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge_ohne0 <- 
    auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge |>
    filter(!auftragsnummer %in% auftraege_mit_null)

vorgaenge_sap_ohne_na_ohne0 <- 
    vorgaenge_sap_ohne_na |>
    filter(!auftragsnummer %in% auftraege_mit_null)


# Model (Kennzahlen berechnen) -------------------------------------------------


# Lead Times und Abweichungen berechnen
fast_fertiger_datensatz_auftragskoepfe <- 
    auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge_ohne0 |> 
    mutate(
        lead_time_ist  = as.numeric(difftime(endtermin_ist, starttermin_ist, units = "days")) + 1,
        lead_time_soll = as.numeric(difftime(endtermin_soll, starttermin_soll, units = "days")) + 1,
        abweichung     = lead_time_ist - lead_time_soll
    )


# Visualize --------------------------------------------------------------------


# Boxplot: Verteilung der Abweichungen
ggplot(fast_fertiger_datensatz_auftragskoepfe, aes(y = abweichung)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Verteilung der Abweichungen", y = "Tage") +
    theme_minimal()

# Histogramm: Abweichungshäufigkeit
ggplot(fast_fertiger_datensatz_auftragskoepfe, aes(x = abweichung)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "yellow") +
    labs(title = "Histogramm der Abweichungen", x = "Lead Time (Tage)", y = "Anzahl") +
    theme_minimal()

# Balkendiagramm: Unter-, Über- und Exaktlieferungen
fast_fertiger_datensatz_auftragskoepfe |>
    mutate(
        lieferquote = gelieferte_menge / sollmenge,
        status = case_when(
            lieferquote < 1  ~ "Unterlieferung",
            lieferquote == 1 ~ "Exakt",
            lieferquote > 1  ~ "Überlieferung"
        ),
        status = factor(status, levels = c("Unterlieferung", "Exakt", "Überlieferung"))
    ) |>
    count(status) |>
    ggplot(aes(x = status, y = n)) +
    geom_col(fill = "grey70", width = 0.6) +
    geom_text(aes(label = n), vjust = -0.3, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
        title = "Übersicht: Unter-, Über- und exakte Lieferungen",
        x = NULL, y = "Anzahl Aufträge"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        axis.text.x = element_text(face = "bold"),
        plot.title  = element_text(face = "bold", hjust = 0.5)
    )


# Communicate ------------------------------------------------------------------


# Mittlere Mengenabweichung (Rohdaten) – Information für Qualitätsbericht
mean_diff <- auftragskoepfe_sap_raw |>
    mutate(diff = gelieferte_menge - sollmenge) |>
    summarise(mean_diff = mean(diff, na.rm = TRUE)) |>
    pull(mean_diff)

if (mean_diff > 0) {
    cat("📈 Im Schnitt gibt es eine Überlieferung um", round(mean_diff, 1), "Einheiten.\n")
} else if (mean_diff < 0) {
    cat("📉 Im Schnitt gibt es eine Unterlieferung um", abs(round(mean_diff, 1)), "Einheiten.\n")
} else {
    cat("✅ Im Schnitt wird exakt geliefert (0 Einheiten Abweichung).\n")
}

# Größte Ausreißer bei der Liefermenge analysieren (rel. Abweichung > 5 %)
threshold <- 0.05
top_outliers <- fast_fertiger_datensatz_auftragskoepfe |>
    mutate(
        diff     = gelieferte_menge - sollmenge,
        rel_diff = diff / sollmenge
    ) |>
    filter(abs(rel_diff) > threshold) |>
    slice_max(order_by = abs(diff), n = 10) |>
    select(
        auftragsnummer, sollmenge, gelieferte_menge, diff, rel_diff
    )

# print(top_outliers)  # → Ausgabe optional für Bericht
