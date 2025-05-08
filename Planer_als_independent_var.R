#Initialize ------

rm(list = ls())
set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)

# Import -----------------------------------------------------------------------

auftraege_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")
source("Arbeitsschritte_gebuendelt_als independent_var.R")

# Tidy -------------------------------------------------------------------------

auftraege_nach_material <- auftraege_raw %>%
    group_split(Materialnummer)

names(auftraege_nach_material) <- auftraege_raw %>% pull(Materialnummer) %>% unique()

# Transform --------------------------------------------------------------------


# Model -----------

#ABC Material und Planer
planer_linien_klassifiziert <- auftraege_raw %>%
    count(Planer, Fertigungslinie, name = "anzahl") %>%
    group_by(Planer) %>%
    arrange(desc(anzahl)) %>%
    mutate(
        gesamt = sum(anzahl),
        kum_anteil = cumsum(anzahl) / gesamt,
        klasse = case_when(
            kum_anteil <= 0.70 ~ "Top70",
            kum_anteil <= 0.90 ~ "Top20",
            TRUE ~ "Top10"
        )
    ) %>%
    ungroup()

planer_linien_abgestuft <- planer_linien_klassifiziert %>%
    group_by(Planer, klasse) %>%
    summarise(
        linien = paste(Fertigungslinie, collapse = ", "),
        anzahl_linien = n(),
        auftraege = sum(anzahl),
        gesamt_auftraege = first(gesamt),
        anteil = round(auftraege / gesamt_auftraege, 3),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = klasse,
        values_from = c(linien, anzahl_linien, auftraege, anteil),
        names_glue = "{klasse}_{.value}",
        names_expand = TRUE,
        values_fill = list(
            linien = "-", 
            anzahl_linien = 0,
            auftraege = 0,
            anteil = 0
        )
    )

View(planer_linien_abgestuft)


#Planer und Linien (welche Linien von welchen Planern?
linie_pro_planer <- auftraege_raw %>%
    count(Planer, Fertigungslinie) %>%
    pivot_wider(
        names_from = Fertigungslinie,
        values_from = n,
        values_fill = 0
    )


#Planer und Werk 
werk_pro_planer <- auftraege_raw %>%
    count(Planer, Werk) %>%
    pivot_wider(
        names_from = Werk,
        values_from = n,
        values_fill = 0
    )

#Planer und Workflows 
workflow_pro_planer <- auftraege_inkl_vorgangsfolgen %>%
    count(Planer, Vorgangsfolge) %>%
    pivot_wider(
        names_from = Vorgangsfolge,
        values_from = n,
        values_fill = 0
    )

#Planer und arbeitsplatz 
arbeitsplatz_pro_planer <- auftraege_inkl_vorgangsfolgen %>%
    count(Planer, Vorgangsfolge) %>%
    pivot_wider(
        names_from = Vorgangsfolge,
        values_from = n,
        values_fill = 0
    )

#Planer und material 
material_pro_planer <- auftraege_inkl_vorgangsfolgen %>%
    count(Planer, Materialnummer) %>%
    pivot_wider(
        names_from = Materialnummer,
        values_from = n,
        values_fill = 0
    )