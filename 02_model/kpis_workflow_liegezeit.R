# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)
library(stringr)
library(plotly)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")


# BALKENDIAGRAMM ZEITEN FÜR WORKFLOWS
#Um wieder die Lead Times auf Vorgangs- und Arbeitsplatzebene sehen zu können, 
# müssen die sap_vorgaenge cleanen und dann pro Vorgang u Arbeitsplatz wieder die LT
# ermitteln. Wir ermitteln außerdem die Liegezeiten als Differenz zwischen Enddatum
# Vorgang 1 und Startdatum Folgevorgang. Wir ermitteln LT/Unit!

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

library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# Dummy-Init für das Ergebnis
create_lt_delay_workflows <- data.frame()

# Alle eindeutigen Auftragsnummern durchgehen
all_steps <- list()

# Für jede Auftragsnummer einzeln durchlaufen
for (auftrag in unique(vorgaenge_sorted$Auftragsnummer)) {
    
    vorgaenge <- vorgaenge_sorted %>%
        filter(Auftragsnummer == auftrag) %>%
        arrange(starttermin_soll)
    
    if (nrow(vorgaenge) == 0) next
    
    workflow_str <- paste(vorgaenge$vorgangsfolge, collapse = "-")
    
    used_indices <- c()
    max_steps <- 10
    step <- 1
    
    # Starte mit dem ersten Vorgang
    current_idx <- which.min(vorgaenge$starttermin_soll)
    used_indices <- c(used_indices, current_idx)
    
    all_steps[[length(all_steps) + 1]] <- data.frame(
        Auftragsnummer = auftrag,
        workflow = workflow_str,
        Schritt = step,
        Dauer = vorgaenge$istdauer[current_idx],
        Typ = "Prozess"
    )
    
    current_end <- vorgaenge$`Istende Vorgang`[current_idx]
    
    while (step < max_steps && length(used_indices) < nrow(vorgaenge)) {
        
        remaining_indices <- setdiff(1:nrow(vorgaenge), used_indices)
        if (length(remaining_indices) == 0 || is.na(current_end)) break
        
        remaining <- vorgaenge[remaining_indices, ]
        
        # Berechne Differenzen – nur mit gültigen Datumswerten
        diffs <- suppressWarnings(abs(as.numeric(remaining$starttermin_soll - current_end)))
        
        if (all(is.na(diffs))) break
        
        next_idx_local <- which.min(diffs)
        next_idx <- remaining_indices[next_idx_local]
        
        next_start <- vorgaenge$starttermin_soll[next_idx]
        if (is.na(next_start)) break
        
        delay_days <- as.numeric(next_start - current_end)
        
        if (!is.na(delay_days) && delay_days > 0) {
            step <- step + 1
            if (step > max_steps) break
            
            all_steps[[length(all_steps) + 1]] <- data.frame(
                Auftragsnummer = auftrag,
                workflow = workflow_str,
                Schritt = step,
                Dauer = delay_days,
                Typ = "Delay"
            )
        }
        
        step <- step + 1
        if (step > max_steps) break
        
        all_steps[[length(all_steps) + 1]] <- data.frame(
            Auftragsnummer = auftrag,
            workflow = workflow_str,
            Schritt = step,
            Dauer = vorgaenge$istdauer[next_idx],
            Typ = "Prozess"
        )
        
        current_end <- vorgaenge$`Istende Vorgang`[next_idx]
        if (is.na(current_end)) break
        
        used_indices <- c(used_indices, next_idx)
    }
    
}

# Binde alles zu einem DataFrame
create_lt_delay_workflows <- bind_rows(all_steps)


