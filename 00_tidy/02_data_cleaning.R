# Initialize -------------------------------------------------------------------

# # Clear all objects from the environment
# rm(list = ls())
# 
# # Set seed for reproducibility
# set.seed(1)

# Load required packages
library(tidyverse)    # data manipulation and visualization
library(readxl)       # reading Excel files
library(lubridate)    # working with dates
library(janitor)
library(dplyr)

# Import -----------------------------------------------------------------------

auftragskoepfe_sap_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_sap_raw    <- read_excel("vorgaenge_sap_raw.xlsx")

# Tidy -------------------------------------------------------------------------

# 1) Clean column names to snake_case
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw |>
    clean_names()

vorgaenge_sap_raw <- vorgaenge_sap_raw |>
    clean_names()

# 2) Ensure correct column formats

# str(auftragskoepfe_sap_raw)
# str(vorgaenge_sap_raw)

# a) Auftragskoepfe: numeric vs. date vs. leave-as-is
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw |>
    mutate(
        # Numeric columns
        sollmenge        = as.numeric(sollmenge),
        gelieferte_menge = as.numeric(gelieferte_menge),
        
        # Date columns (using lubridate::as_date)
        starttermin_soll = as_date(starttermin_soll),
        endtermin_soll   = as_date(endtermin_soll),
        endtermin_ist    = as_date(endtermin_ist),
        starttermin_ist  = as_date(starttermin_ist),
        erfassungsdatum  = as_date(erfassungsdatum)
        
        # all other columns keep their existing types
    )

# make sure materialnummer is a character, not numeric
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw |>
    mutate(materialnummer = as.character(materialnummer))

# b) Vorgaenge: numeric vs. date vs. leave-as-is
vorgaenge_sap_raw <- vorgaenge_sap_raw |>
    mutate(
        # Numeric columns
        gutmenge_vorgang  = as.numeric(gutmenge_vorgang),
        ausschuss_vorgang = as.numeric(ausschuss_vorgang),
        
        # Date columns
        iststart_vorgang  = as_date(iststart_vorgang),
        istende_vorgang   = as_date(istende_vorgang)
        
        # all other columns keep their existing types
    )
# str(auftragskoepfe_sap_raw)
# str(vorgaenge_sap_raw)



# Insights for Data Quality from raw data

# Completeness checks: only show columns with missing values (console output)
# 
# Auftragskoepfe
auftragskoepfe_sap_raw |>
    summarise(
        across(
            everything(),
            list(
                n_missing   = ~ sum(is.na(.)),
                pct_missing = ~ mean(is.na(.)) * 100
            ),
            .names = "{.col}_{.fn}"
        )
    ) |>
    pivot_longer(
        cols        = everything(),
        names_to    = c("column", "metric"),
        names_pattern = "(.*)_(n_missing|pct_missing)$"
    ) |>
    pivot_wider(
        names_from  = metric,
        values_from = value
    ) |>
    filter(n_missing > 0)

# Vorgaenge
vorgaenge_sap_raw |>
    summarise(
        across(
            everything(),
            list(
                n_missing   = ~ sum(is.na(.)),
                pct_missing = ~ mean(is.na(.)) * 100
            ),
            .names = "{.col}_{.fn}"
        )
    ) |>
    pivot_longer(
        cols        = everything(),
        names_to    = c("column", "metric"),
        names_pattern = "(.*)_(n_missing|pct_missing)$"
    ) |>
    pivot_wider(
        names_from  = metric,
        values_from = value
    ) |>
    filter(n_missing > 0)


# Order number consistency check

cat("### Auftragsnummern Übereinstimmung\n")

# 1) Einzigartige Auftragsnummern aus beiden Datensätzen extrahieren
orders_ids <- auftragskoepfe_sap_raw |>
    pull(auftragsnummer) |>
    unique()

ops_ids <- vorgaenge_sap_raw |>
    pull(auftragsnummer) |>
    unique()

# 2) Prüfen, ob beide Sets identisch sind
if (setequal(orders_ids, ops_ids)) {
    cat("✔️ Beide Dateien besitzen exakt dieselben Auftragsnummern.\n")
} else {
    cat("❌ Die Dateien unterscheiden sich in den Auftragsnummern.\n")
    only_in_orders <- setdiff(orders_ids, ops_ids)
    only_in_ops    <- setdiff(ops_ids, orders_ids)
    cat("- Nur in 'Auftragsköpfe':", length(only_in_orders), "Aufträge\n")
    if (length(only_in_orders) > 0) {
        cat("  Beispiel-Aufträge (Auftragsköpfe):", head(only_in_orders, 10), "\n")
    }
    cat("- Nur in 'Vorgänge':", length(only_in_ops), "Aufträge\n")
    if (length(only_in_ops) > 0) {
        cat("  Beispiel-Aufträge (Vorgänge):", head(only_in_ops, 10), "\n")
    }
}


# nun will ich die Zeilen mit den leeren Datumsangaben löschen.
# Dafür schaue ich mir erstmal die vorgaenge an. Wenn dort ein Datum bei einem 
# Auftrag fehlt, dann fehlt es immer bei beiden, also Iststart und Istende.
# Schaut man sich die Auftragsnummern bei auftragskoepfe an, dann sind die Ist-Datumsangaben 
# auch unvollständig, weshalb der ganze Auftrag aussortiert werden kann.

# Zellen mit leeren Datumseintragungen in vorgaenge löschen
vorgaenge_sap_ohne_na <- vorgaenge_sap_raw |>
    filter(!is.na(iststart_vorgang))

# die auftragsnummern aus vorgaenge nehmen und in auftragskoepfe danach filtern und neues objekt schaffen
auftragskoepfe_sap_ohne_na <- 
    auftragskoepfe_sap_raw |>
    dplyr::semi_join(
        vorgaenge_sap_ohne_na,
        by = "auftragsnummer"
    )


# das ist nur ein Check um zu schauen, ob die beiden neuen objekte "ohne_na" die identischen Auftragsnummern haben
# 1) Extract the two order-number vectors
hdr_ids <- auftragskoepfe_sap_ohne_na$auftragsnummer
op_ids  <- vorgaenge_sap_ohne_na$auftragsnummer

# 2) Check if they’re exactly the same set
same_ids <- setequal(hdr_ids, op_ids)
print(same_ids)
# → TRUE means they match perfectly; FALSE means there are mismatches

# 3) If FALSE, find which IDs are exclusive to each
only_in_headers <- setdiff(hdr_ids, op_ids)
only_in_ops     <- setdiff(op_ids,  hdr_ids)

# 4) Inspect the mismatches (if any)
print("In headers but not in operations:")
print(only_in_headers)

print("In operations but not in headers:")
print(only_in_ops)


# Einträge aus vorgaenge wurden nun in auftragskoepfe übertragen, da diese laut prof von besserer Qualität sein sollen.

# 1) Summarise operations: min start, max end per order
op_dates <- vorgaenge_sap_ohne_na %>%
    group_by(auftragsnummer) %>%
    summarise(
        min_iststart = min(iststart_vorgang, na.rm = TRUE),
        max_istende  = max(istende_vorgang,  na.rm = TRUE),
        .groups = "drop"
    )

# 2) Join onto headers and update dates
auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge <-
    auftragskoepfe_sap_ohne_na %>%
    left_join(op_dates, by = "auftragsnummer") %>%
    
    # 3) Flag rows where we will replace start or end
    mutate(
        replaced_start = case_when(
            is.na(starttermin_ist) & !is.na(min_iststart)       ~ TRUE,
            !is.na(starttermin_ist) & starttermin_ist != min_iststart ~ TRUE,
            TRUE                                                 ~ FALSE
        ),
        replaced_end = case_when(
            is.na(endtermin_ist)  & !is.na(max_istende)         ~ TRUE,
            !is.na(endtermin_ist) & endtermin_ist != max_istende ~ TRUE,
            TRUE                                                 ~ FALSE
        ),
        
        # 4) Actually overwrite the two columns
        starttermin_ist = min_iststart,
        endtermin_ist   = max_istende
    )

# 5) Summarise how many replacements happened FÜR QUALITÄTSBERICHT
replacement_summary <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge %>%
    summarise(
        start_dates_replaced = sum(replaced_start, na.rm = TRUE),
        end_dates_replaced   = sum(replaced_end,   na.rm = TRUE)
    )

print(replacement_summary)

# 6) Remove helper columns
auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge <-
    auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge %>%
    dplyr::select(
        -min_iststart,
        -max_istende,
        -replaced_start,
        -replaced_end
    )


# test ob es geklappt hat. Hab auf die Schnelle Aufrag gefunden, bei der sich die Daten unterschieden haben
# 1) Show the header row in your enriched headers object
auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(auftragsnummer == "1068473") %>%
    print()

# 2) Show all matching operation rows in the filtered operations object
vorgaenge_sap_ohne_na %>%
    filter(auftragsnummer == "1068473") %>%
    print()

#1.2 fertig                                             

# Aufträge mit gelieferte Menge = 0 entfernen, da diese nur Versuchsaufträge waren

# 1. Auftragsnummern mit 0 in gelieferter Menge finden
auftraege_mit_null <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(gelieferte_menge == 0) %>%
    pull(auftragsnummer)

# 2. Diese Aufträge aus df_lieferungen entfernen
auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge_ohne0 <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(!auftragsnummer %in% auftraege_mit_null)

# 3. Auch die Vorgänge dieser Aufträge entfernen
vorgaenge_sap_ohne_na_ohne0 <- vorgaenge_sap_ohne_na %>%
    filter(!auftragsnummer %in% auftraege_mit_null)


#Gibt es noch fehlende Werte?
sum(is.na(auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge_ohne0))
sum(is.na(vorgaenge_sap_ohne_na_ohne0))

# Ist Leadtimes berechnen um statistische Ausreißer zu ermitteln. 

# Lead Times berechen
fast_fertiger_datensatz_auftragskoepfe <- auftragskoepfe_sap_ohne_na_datumseinträge_von_vorgaenge_ohne0 %>%
    mutate(
        lead_time_ist = as.numeric(difftime(endtermin_ist, starttermin_ist, units = "days")),
        lead_time_soll = as.numeric(difftime(endtermin_soll, starttermin_soll, units = "days"))
    )
#von Datum auf Produktionstage umstellen
fast_fertiger_datensatz_auftragskoepfe <- fast_fertiger_datensatz_auftragskoepfe %>%
    mutate(
        lead_time_ist = lead_time_ist + 1,
        lead_time_soll = lead_time_soll + 1
    )

# Abweichungen berechnen
fast_fertiger_datensatz_auftragskoepfe <- fast_fertiger_datensatz_auftragskoepfe %>%
    mutate(
        abweichung = lead_time_ist - lead_time_soll)

mean(fast_fertiger_datensatz_auftragskoepfe$abweichung)

# Abweichungen visualisieren
ggplot(fast_fertiger_datensatz_auftragskoepfe, aes(y = abweichung)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Verteilung der Abweichungen", y = "Tage") +
    theme_minimal()

ggplot(fast_fertiger_datensatz_auftragskoepfe, aes(x = abweichung)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "yellow") +
    labs(title = "Histogramm der Abweichungen", x = "Lead Time (Tage)", y = "Anzahl") +
    theme_minimal()



#für data quality report:
# Visualisierung der Soll- vs. Ist-Mengen --------------------------------------

# 1) Balkendiagramm: Anzahl Unter-, Über- und exakte Lieferungen
fast_fertiger_datensatz_auftragskoepfe |>
    mutate(
        lieferquote = gelieferte_menge / sollmenge,
        status = case_when(
            lieferquote < 1  ~ "Unterlieferung",
            lieferquote == 1 ~ "Exakt",
            lieferquote > 1  ~ "Überlieferung"
        ),
        # Faktor mit sinnvoller Reihenfolge
        status = factor(status, levels = c("Unterlieferung", "Exakt", "Überlieferung"))
    ) |>
    count(status) |>
    ggplot(aes(x = status, y = n)) +
    geom_col(fill = "grey70", width = 0.6) +
    geom_text(aes(label = n), 
              vjust = -0.3, 
              size  = 4) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
        title = "Übersicht: Unter-, Über- und exakte Lieferungen",
        x     = NULL,
        y     = "Anzahl Aufträge"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        axis.text.x = element_text(face = "bold"),
        plot.title  = element_text(face = "bold", hjust = 0.5)
    )



# 1) Mittlere Differenz (Ist – Soll) der gelieferten Menge vs. Sollmenge 
mean_diff <- auftragskoepfe_sap_raw |>
    mutate(diff = gelieferte_menge - sollmenge) |>
    summarise(mean_diff = mean(diff, na.rm = TRUE)) |>
    pull(mean_diff)

# 2) Auswertung und Ausgabe
if (mean_diff > 0) {
    cat("📈 Im Schnitt gibt es eine Überlieferung um", 
        round(mean_diff, 1), "Einheiten.\n")
} else if (mean_diff < 0) {
    cat("📉 Im Schnitt gibt es eine Unterlieferung um", 
        abs(round(mean_diff, 1)), "Einheiten.\n")
} else {
    cat("✅ Im Schnitt wird exakt geliefert (0 Einheiten Abweichung).\n")
}




# Im letzten Schritt ergänzen wir die fehlenden Informationen über die Workflows
# und Arbeitsplätze für jeden Auftrag zu dem bereinigten data frame. Die source
# datei darf erst hier unten stehen, weil auftragskoepfe_sap_raw dadrin ver-
# ändert wird und der code oben sonst nicht mehr funktioniert!


# Ausreißer in der Liefermenge identifizieren und anzeigen

# Top-Ausreißer mit diff so, dass positive Werte Überlieferung bedeuten

# Schwellenwert für relative Abweichung (z.B. 5 %)
threshold <- 0.05

# Ermitteln und Sortieren der Ausreißer
top_outliers <- fast_fertiger_datensatz_auftragskoepfe |>
    mutate(
        # diff positiv, wenn gelieferte Menge > Sollmenge
        diff     = gelieferte_menge - sollmenge,
        # relative Abweichung: diff geteilt durch Sollmenge
        rel_diff = diff / sollmenge
    ) |>
    filter(abs(rel_diff) > threshold) |>
    # nach absoluter Differenz absteigend sortieren und nur die Top 10 nehmen
    slice_max(order_by = abs(diff), n = 10) |>
    dplyr::select(
        auftragsnummer,
        sollmenge,
        gelieferte_menge,
        diff,       # jetzt positiv = Überlieferung, negativ = Unterlieferung
        rel_diff    # rel_diff > 0 → Überlieferung, rel_diff < 0 → Unterlieferung
    )

# Ausgabe der Top 10
print(top_outliers)
