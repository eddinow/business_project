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

# Import -----------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

#Material nach häufigkeit sortiert
tabelle <- all_data_finalized %>%
    group_by(materialnummer, vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    arrange(desc(Anzahl))

dopplungen <- tabelle %>%
    group_by(materialnummer) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    filter(Anzahl > 1)

materialnummer_overview <- all_data_finalized %>%
    filter(!is.na(materialnummer)) %>%
    mutate(
        Durchlaufzeit = as.numeric(lead_time_ist),
        Komplexität = str_count(vorgangsfolge, "→") + 1,
        Startverzögerung = as.numeric(starttermin_ist - starttermin_soll)
    ) %>%
    group_by(materialnummer) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        SD_LT = round(sd(Durchlaufzeit, na.rm = TRUE), 1),
        Ø_Komplexität = round(mean(Komplexität, na.rm = TRUE), 1),
        Ø_Startverzögerung = round(mean(Startverzögerung, na.rm = TRUE), 1),
        Anteil_verspätet = round(mean(abweichung > 0, na.rm = TRUE), 2),
        Ø_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Ø_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 0),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        Termintreue = round(mean(abweichung <= 0, na.rm = TRUE), 2),
        .groups = "drop"
    )