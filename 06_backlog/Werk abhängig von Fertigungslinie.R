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
# ANZAHL AUFTRÄGE PRO WERK UND LINIE
# -------------------------
werk_linien_counts <- auftraege_raw %>%
    count(Fertigungslinie, Werk, name = "anzahl")

# -------------------------
# ABC-ANALYSE je Fertigungslinie
# -------------------------
abc_werk_linien <- werk_linien_counts %>%
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
# ZUSAMMENFASSUNG je Linie und Klasse
# -------------------------
abc_werk_summary_linien <- abc_werk_linien %>%
    group_by(Fertigungslinie, klasse) %>%
    summarise(
        werke = paste(Werk, collapse = ", "),
        anzahl = n(),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = klasse,
        values_from = c(werke, anzahl),
        names_glue = "{klasse}_{.value}"
    )

# -------------------------
# ANZEIGEN
# -------------------------
View(abc_werk_summary_linien)