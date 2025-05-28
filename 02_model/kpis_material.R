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

materialnummer_overview <- all_data_finalized %>%
    group_by(materialnummer) %>%
    summarise(
        Anzahl = n(),
        Gesamtmenge = sum(gelieferte_menge, na.rm = TRUE),
        Sollmenge = sum(sollmenge, na.rm = TRUE),
        Ø_Abweichung = mean(abweichung, na.rm = TRUE),
        Ø_LT = mean(lead_time_ist, na.rm = TRUE),
        Anteil_pünktlich = mean(abweichung <= 0, na.rm = TRUE),
        Ø_Komplexität = mean(str_count(vorgangsfolge, "→") + 1, na.rm = TRUE)
    ) %>%
    arrange(desc(Gesamtmenge)) %>%
    mutate(
        LT_pro_Schritt = Ø_LT / Ø_Komplexität  # <<--- NEU
    )

# ABC-Analyse auf Gesamtmenge
materialnummer_overview <- materialnummer_overview %>%
    mutate(
        kum_anteil = cumsum(Gesamtmenge) / sum(Gesamtmenge),
        ABC_Klasse = case_when(
            kum_anteil <= 0.8 ~ "A",
            kum_anteil <= 0.95 ~ "B",
            TRUE ~ "C"
        )
    )

#Überblick abc
abc_summary <- materialnummer_overview %>%
    group_by(ABC_Klasse) %>%
    summarise(
        Anzahl_Materialien = n(),
        Gesamt_Anzahl = sum(Anzahl, na.rm = TRUE),
        Gesamtmenge = sum(Gesamtmenge, na.rm = TRUE),
        Gesamtsollmenge = sum(Sollmenge, na.rm = TRUE),
        Ø_Abweichung = round(mean(Ø_Abweichung, na.rm = TRUE), 2),
        Ø_LT = round(mean(Ø_LT, na.rm = TRUE), 2),
        Anteil_pünktlich = round(mean(Anteil_pünktlich, na.rm = TRUE), 2),
        Ø_Komplexität = round(mean(Ø_Komplexität, na.rm = TRUE), 2),
        LT_pro_Schritt = round(mean(LT_pro_Schritt, na.rm = TRUE), 2)
    )

#plots

ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Gesamtmenge je ABC-Klasse",
        x = "ABC-Klasse",
        y = "Gesamt gelieferte Menge"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")

abc_long3 <- abc_summary %>%
    select(ABC_Klasse, Anzahl_Materialien, LT_pro_Schritt, Ø_Abweichung, Anteil_pünktlich) %>%
    pivot_longer(
        cols = -ABC_Klasse, 
        names_to = "Kennzahl", 
        values_to = "Wert"
    )

ggplot(abc_long3, aes(x = ABC_Klasse, y = Wert, fill = ABC_Klasse)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Kennzahl, scales = "free_y") +
    labs(
        title = "ABC-Klassen: Materialanzahl, LT/Schritt, Ø-Abweichung, Anteil pünktlich",
        x = "ABC-Klasse",
        y = "Wert"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")