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
library(lubridate)

# Daten laden -------------------------------------------------------------------
source("01_transform/create_lt_unit.R")

# all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
# vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")

# 
# # BALKENDIAGRAMM ZEITEN FÜR WORKFLOWS
# #Um wieder die Lead Times auf Vorgangs- und Arbeitsplatzebene sehen zu können, 
# # müssen die sap_vorgaenge cleanen und dann pro Vorgang u Arbeitsplatz wieder die LT
# # ermitteln. Wir ermitteln außerdem die Liegezeiten als Differenz zwischen Enddatum
# # Vorgang 1 und Startdatum Folgevorgang. Wir ermitteln LT/Unit!
# 
# 
# # Dummy-Init für das Ergebnis
# create_lt_delay_workflows <- data.frame()
# 
# # Alle eindeutigen Auftragsnummern durchgehen
# all_steps <- list()
# 
# # Für jede Auftragsnummer einzeln durchlaufen
# for (auftrag in unique(vorgaenge_sorted$Auftragsnummer)) {
#     
#     vorgaenge <- vorgaenge_sorted %>%
#         filter(Auftragsnummer == auftrag) %>%
#         arrange(starttermin_soll)
#     
#     if (nrow(vorgaenge) == 0) next
#     
#     workflow_str <- paste(vorgaenge$vorgangsfolge, collapse = "-")
#     
#     used_indices <- c()
#     max_steps <- 10
#     step <- 1
#     
#     # Starte mit dem ersten Vorgang
#     current_idx <- which.min(vorgaenge$starttermin_soll)
#     used_indices <- c(used_indices, current_idx)
#     
#     all_steps[[length(all_steps) + 1]] <- data.frame(
#         Auftragsnummer = auftrag,
#         workflow = workflow_str,
#         Schritt = step,
#         Dauer = vorgaenge$istdauer[current_idx],
#         Typ = "Prozess"
#     )
#     
#     current_end <- vorgaenge$`Istende Vorgang`[current_idx]
#     
#     while (step < max_steps && length(used_indices) < nrow(vorgaenge)) {
#         
#         remaining_indices <- setdiff(1:nrow(vorgaenge), used_indices)
#         if (length(remaining_indices) == 0 || is.na(current_end)) break
#         
#         remaining <- vorgaenge[remaining_indices, ]
#         
#         # Berechne Differenzen – nur mit gültigen Datumswerten
#         diffs <- suppressWarnings(abs(as.numeric(remaining$starttermin_soll - current_end)))
#         
#         if (all(is.na(diffs))) break
#         
#         next_idx_local <- which.min(diffs)
#         next_idx <- remaining_indices[next_idx_local]
#         
#         next_start <- vorgaenge$starttermin_soll[next_idx]
#         if (is.na(next_start)) break
#         
#         delay_days <- as.numeric(next_start - current_end)
#         
#         if (!is.na(delay_days) && delay_days > 0) {
#             step <- step + 1
#             if (step > max_steps) break
#             
#             all_steps[[length(all_steps) + 1]] <- data.frame(
#                 Auftragsnummer = auftrag,
#                 workflow = workflow_str,
#                 Schritt = step,
#                 Dauer = delay_days,
#                 Typ = "Delay"
#             )
#         }
#         
#         step <- step + 1
#         if (step > max_steps) break
#         
#         all_steps[[length(all_steps) + 1]] <- data.frame(
#             Auftragsnummer = auftrag,
#             workflow = workflow_str,
#             Schritt = step,
#             Dauer = vorgaenge$istdauer[next_idx],
#             Typ = "Prozess"
#         )
#         
#         current_end <- vorgaenge$`Istende Vorgang`[next_idx]
#         if (is.na(current_end)) break
#         
#         used_indices <- c(used_indices, next_idx)
#     }
#     
# }
# 
# # Binde alles zu einem DataFrame
# create_lt_delay_workflows <- bind_rows(all_steps)


# neu-----------------------------

vorgaenge_chrono <- vorgaenge_lt_unit %>%
    mutate(
        `Iststart Vorgang` = ymd(`Iststart Vorgang`),
        `Istende Vorgang` = ymd(`Istende Vorgang`)
    ) %>%
    arrange(Auftragsnummer, `Iststart Vorgang`)

# Schritt 2: Liegezeiten berechnen
vorgaenge_mit_liegezeit <- vorgaenge_chrono %>%
    group_by(Auftragsnummer) %>%
    mutate(
        next_start = lead(`Iststart Vorgang`),
        liegezeit = as.numeric(next_start - `Istende Vorgang`),
        vorgaenger_vorgangsnummer = Vorgangsnummer
    ) %>%
    ungroup()

# Schritt 3: Neue Liegezeit-Zeilen erzeugen
liegezeiten_df <- vorgaenge_mit_liegezeit %>%
    filter(!is.na(liegezeit) & liegezeit > 0) %>%
    transmute(
        Auftragsnummer = Auftragsnummer,
        Vorgangsnummer = paste0("Liegezeit nach ", vorgaenger_vorgangsnummer),
        Arbeitsplatz = NA,
        `Iststart Vorgang` = NA,
        `Istende Vorgang` = NA,
        istdauer = liegezeit,
        liegezeit = liegezeit,
        vorgangsfolge = vorgangsfolge
    )

# Schritt 4: Original und Liegezeit-Zeilen kombinieren
ausgabe_df <- bind_rows(vorgaenge_chrono, liegezeiten_df) %>%
    arrange(Auftragsnummer, `Iststart Vorgang`, Vorgangsnummer)