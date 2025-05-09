#Initialize ------
setwd("C:\\Users\\julia\\OneDrive\\Dokumente\\04 Supply Chain Management Master\\13 Business Project\\Github Verbindung\\business_project")

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

# Tidy -------------------------------------------------------------------------

auftraege_nach_material <- auftraege_raw %>%
    group_split(Materialnummer)

names(auftraege_nach_material) <- auftraege_raw %>% pull(Materialnummer) %>% unique()

# Transform --------------------------------------------------------------------


# Model -----------

#Kreuztabelle mit Material und Linie um zu checken welche Werke welche Linien haben
table(auftraege_raw$Materialnummer, auftraege_raw$Fertigungslinie)

# Wir führen eine ABC-Analyse durch (welche Linien sind für welches Material zuständig)
material_linien <- auftraege_raw %>%
    count(Materialnummer, Fertigungslinie) %>%
    pivot_wider(names_from = Fertigungslinie, values_from = n, values_fill = 0)

abc_material_linien_klassifiziert <- auftraege_raw %>%
    count(Fertigungslinie, Materialnummer, name = "anzahl") %>%
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

abc_material_linien <- abc_material_linien_klassifiziert %>%
    group_by(Fertigungslinie, klasse) %>%
    summarise(
        materialien = paste(Materialnummer, collapse = ", "),
        anzahl = n(),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = klasse,
        values_from = c(materialien, anzahl),
        names_glue = "{klasse}_{.value}"
    )

View(abc_material_linien)



# Visualize -------




# Communicate ------

