# -------------------------
# INIT
# -------------------------
rm(list = ls())
set.seed(1)

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)

# -------------------------
# IMPORT
# -------------------------
auftraege_raw <- read_excel("2025-04-08_Auftragsköpfe SAP.xlsx")

# -------------------------
# KREUZTABELLE (Planer vs Fertigungslinie)
# -------------------------
table(auftraege_raw$Planer, auftraege_raw$Fertigungslinie)

# -------------------------
# ANZAHL AUFTRÄGE PRO PLANER UND FERTIGUNGSLINIE
# -------------------------
planer_linien_counts <- auftraege_raw %>%
    count(Planer, Fertigungslinie, name = "anzahl")

# -------------------------
# ABC-ANALYSE je Fertigungslinie
# -------------------------
abc_planer_linien <- planer_linien_counts %>%
    group_by(Fertigungslinie) %>%
    arrange(desc(anzahl)) %>%
    mutate(
        gesamt = sum(anzahl),
        kum_anteil = cumsum(anzahl) / gesamt,
        klasse = case_when(
            kum_anteil <= 0.10 ~ "C",
            kum_anteil <= 0.30 ~ "B",
            TRUE ~ "A"
        )
    ) %>%
    ungroup()

# -------------------------
# ZUSAMMENFASSUNG je Linie und ABC-Klasse
# -------------------------
abc_planer_summary_linien <- abc_planer_linien %>%
    group_by(Fertigungslinie, klasse) %>%
    summarise(
        planer = paste(Planer, collapse = ", "),
        anzahl = n(),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = klasse,
        values_from = c(planer, anzahl),
        names_glue = "{klasse}_{.value}"
    )

# -------------------------
# ANZEIGEN
# -------------------------
View(abc_planer_summary_linien)