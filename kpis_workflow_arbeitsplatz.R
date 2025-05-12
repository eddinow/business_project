# Initialisierung ----------------------------------------------------------------

rm(list = ls())
set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)

# Daten laden -------------------------------------------------------------------
source("create_all_data_finalized.R")  # erzeugt 'all_data_finalized'
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")


# Wir wollen ein Ampelsystem für alle Arbeitsplätze hinsichtlich ihrer Performance
# schaffen. Negative Abweichungen über 60% = rot, 20-60% = orange, 0-10%=grün).
# Dazu müssen wir zuerst die Spalte Abweichung aus all_data_finalized abhängig 
# von der 'Auftragsnummer' dem df vorgaenge_raw hinzufügen

vorgaenge_raw <- vorgaenge_raw %>%
    left_join(
        dplyr::select(all_data_finalized, auftragsnummer, abweichung),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

# Ampelsystem-Spalte hinzufügen
vorgaenge_raw <- vorgaenge_raw %>%
    mutate(ampelfarbe = case_when(
        abweichung / lead_time_soll > 0.6 ~ "rot",
        abweichung / lead_time_soll > 0.2 ~ "orange",
        abweichung / lead_time_soll >= 0 ~ "grün",
        TRUE ~ NA_character_
    ))

names(all_data_finalized)
names(vorgaenge_raw)