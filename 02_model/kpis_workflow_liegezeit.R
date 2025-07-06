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

# Sortieren nach Iststart
vorgaenge_chrono <- vorgaenge_sorted %>%
    mutate(
        `Iststart Vorgang` = ymd(`Iststart Vorgang`),
        `Istende Vorgang`  = ymd(`Istende Vorgang`)
    ) %>%
    arrange(Auftragsnummer, `Iststart Vorgang`)

# Liegezeiten berechnen
vorgaenge_mit_liegezeit <- vorgaenge_chrono %>%
    group_by(Auftragsnummer) %>%
    mutate(
        next_start = lead(`Iststart Vorgang`),
        liegezeit = as.numeric(next_start - `Istende Vorgang`),
        vorgaenger_vorgangsnummer = Vorgangsnummer
    ) %>%
    ungroup()

# Neue Liegezeit-Zeilen erzeugen
liegezeiten_df <- vorgaenge_mit_liegezeit %>%
    filter(!is.na(liegezeit) & liegezeit > 0) %>%
    transmute(
        Auftragsnummer = Auftragsnummer,
        Vorgangsnummer = paste0("LZ nach ", vorgaenger_vorgangsnummer),
        Arbeitsplatz = NA,
        `Iststart Vorgang` = NA,
        `Istende Vorgang` = NA,
        istdauer = liegezeit,
        liegezeit = liegezeit,
        vorgangsfolge = vorgangsfolge
    )

# Original und Liegezeit-Zeilen kombinieren
df_lz_bz <- bind_rows(vorgaenge_chrono, liegezeiten_df) %>%
    arrange(Auftragsnummer, `Iststart Vorgang`, Vorgangsnummer)
