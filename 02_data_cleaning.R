# Initialize -------------------------------------------------------------------

# Clear all objects from the environment
rm(list = ls())

# Set seed for reproducibility
set.seed(1)

# Load required packages
library(tidyverse)    # data manipulation and visualization
library(readxl)       # reading Excel files
library(lubridate)    # working with dates
library(janitor)
library(dplyr)

# Import -----------------------------------------------------------------------

auftragskoepfe_sap_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_sap_raw    <- read_excel("vorgaenge_sap_raw.xlsx")

View(auftragskoepfe_sap_raw)
View(vorgaenge_sap_raw) 
# Tidy -------------------------------------------------------------------------

# 1) Clean column names to snake_case
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw |>
    clean_names()

vorgaenge_sap_raw <- vorgaenge_sap_raw |>
    clean_names()

# 2) Ensure correct column formats

str(auftragskoepfe_sap_raw)
str(vorgaenge_sap_raw)

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
str(auftragskoepfe_sap_raw)
str(vorgaenge_sap_raw)


# Erster Check für die Annahme, dass Fertigungslinien mit 0 vorne selten erscheinen und somit zugeordnet werden können.
# 1) Count absolute frequencies
fertigungslinie_counts <- auftragskoepfe_sap_raw |>
    count(fertigungslinie, name = "count") |>
    arrange(desc(count))

print(fertigungslinie_counts)

# 2) (Optional) Also compute relative frequencies as percentages
fertigungslinie_counts <- fertigungslinie_counts |>
    mutate(pct = count / sum(count) * 100)

print(fertigungslinie_counts)



# Zweiter Check: Geschaut, ob die Fertigungslinien mit den 0en komplett andere Werje und Planer bedienen --> Nein

# 1) Create a little helper that strips leading zeros
strip_zeros <- function(x) str_remove(x, "^0+")

# 2) Build a mapping table: for each raw & stripped line, list unique Werks and Planers
line_planer_werk_map <- auftragskoepfe_sap_raw %>%
    # keep both the original and the stripped‐zero version
    mutate(trimmed_line = strip_zeros(fertigungslinie)) %>%
    # group by raw variant and its normalized form
    group_by(fertigungslinie, trimmed_line) %>%
    # summarise which Werks and Planers ever occur
    summarise(
        werke  = paste(sort(unique(werk)), collapse = ", "),
        planers = paste(sort(unique(planer)), collapse = ", "),
        n_orders = n(),
        .groups = "drop"
    )

print(line_planer_werk_map)


# remove any number of zeros at the very start of the string in fertigungslinie (ANNAHME getroffen)
auftragskoepfe_sap_angepasste_fertigungslinie <- auftragskoepfe_sap_raw |>
    mutate(
        fertigungslinie = str_remove(fertigungslinie, "^0+")
    )
#testen, ob es geklappt hat die 0 zu löschen
unique(auftragskoepfe_sap_angepasste_fertigungslinie$fertigungslinie)


# nun will ich die Zeilen mit den leeren Datumsangaben löschen.
# Dafür schaue ich mir erstmal die vorgaenge an. Wenn dort ein Datum bei einem 
# Auftrag fehlt, dann fehlt es immer bei beiden, also Iststart und Istende.
# Schaut man sich die Auftragsnummern bei auftragskoepfe an, dann sind die Ist-Datumsangaben 
# auch unvollständig, weshalb der ganze Auftrag aussortiert werden kann.

# Zellen mit leeren Datumseintragungen in vorgaenge löschen
vorgaenge_sap_ohne_na <- vorgaenge_sap_raw |>
    filter(!is.na(iststart_vorgang))

# die auftragsnummern aus vorgaenge nehmen und in auftragskoepfe danach filtern und neues objekt schaffen
auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na <- 
    auftragskoepfe_sap_angepasste_fertigungslinie |>
    dplyr::semi_join(
        vorgaenge_sap_ohne_na,
        by = "auftragsnummer"
    )


# das ist nur ein Check um zu schauen, ob die beiden neuen objekte "ohne_na" die identischen Auftragsnummern haben
# 1) Extract the two order-number vectors
hdr_ids <- auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na$auftragsnummer
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
auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge <-
    auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na %>%
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
replacement_summary <- auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge %>%
    summarise(
        start_dates_replaced = sum(replaced_start, na.rm = TRUE),
        end_dates_replaced   = sum(replaced_end,   na.rm = TRUE)
    )

print(replacement_summary)

# 6) Remove helper columns
auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge <-
    auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge %>%
    select(
        -min_iststart,
        -max_istende,
        -replaced_start,
        -replaced_end
    )


# test ob es geklappt hat. Hab auf die Schnelle Aufrag gefunden, bei der sich die Daten unterschieden haben
# 1) Show the header row in your enriched headers object
auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(auftragsnummer == "1068473") %>%
    print()

# 2) Show all matching operation rows in the filtered operations object
vorgaenge_sap_ohne_na %>%
    filter(auftragsnummer == "1068473") %>%
    print()

#1.2 fertig                                             

# Aufträge mit gelieferte Menge = 0 entfernen, da diese nur Versuchsaufträge waren

# 1. Auftragsnummern mit 0 in gelieferter Menge finden
auftraege_mit_null <- auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(gelieferte_menge == 0) %>%
    pull(auftragsnummer)

# 2. Diese Aufträge aus df_lieferungen entfernen
auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge_ohne0 <- auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge %>%
    filter(!auftragsnummer %in% auftraege_mit_null)

# 3. Auch die Vorgänge dieser Aufträge entfernen
vorgaenge_sap_ohne_na_ohne0 <- vorgaenge_sap_ohne_na %>%
    filter(!auftragsnummer %in% auftraege_mit_null)


#Gibt es noch fehlende Werte?
sum(is.na(auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge_ohne0))
sum(is.na(vorgaenge_sap_ohne_na_ohne0))

# Ist Leadtimes berechnen um statistische Ausreißer zu ermitteln. 

# Lead Times berechen
fast_fertiger_datensatz_auftragskoepfe <- auftragskoepfe_sap_angepasste_fertigungslinie_ohne_na_datumseinträge_von_vorgaenge_ohne0 %>%
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