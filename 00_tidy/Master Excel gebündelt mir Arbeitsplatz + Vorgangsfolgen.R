# Initialize -------------------------------------------------------------------
#RM müssen wir hier aussschalten weil für create all data finalized die datei 
# aus zwei skripten erstellt wird und wir sonst dieses data frame hier wieder
#"sperren"

#rm(list = ls())
#set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)

# Import -----------------------------------------------------------------------

auftraege_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")

# Vorgangsfolgen erzeugen -----------------------------------------------------

vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " → "), .groups = "drop")

# Arbeitsplatzfolgen erzeugen --------------------------------------------------

arbeitsplatzfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Arbeitsplatzfolge = paste(Arbeitsplatz, collapse = " → "), .groups = "drop")

# Aufträge mit Vorgangs- und Arbeitsplatzfolgen verknüpfen ---------------------

auftraege_inkl_vorgangsfolgen <- auftraege_raw %>%
    left_join(vorgangsfolgen, by = "Auftragsnummer") %>%
    left_join(arbeitsplatzfolgen, by = "Auftragsnummer")

# Vorgangsfolgen & Fertigungslinien --------------------------------------------

Vorgangsfolgen_und_Fertigungslinien <- auftraege_inkl_vorgangsfolgen %>%
    filter(!is.na(Fertigungslinie)) %>%
    count(Vorgangsfolge, Fertigungslinie) %>%
    pivot_wider(names_from = Fertigungslinie, values_from = n, values_fill = 0) %>%
    mutate(Gesamt = rowSums(.[, -1]))

# Materialnummern zu Vorgangsfolgen --------------------------------------------

Vorgangsfolgen_und_Materialnummern <- auftraege_inkl_vorgangsfolgen %>%
    count(Vorgangsfolge, Materialnummer) %>%
    group_by(Vorgangsfolge) %>%
    summarise(
        Materialnummern = paste(Materialnummer, collapse = ", "),
        Anzahl_Materialnummern = n(),
        Top_Material = Materialnummer[which.max(n)],
        .groups = "drop"
    )

anzahl_auftraege <- auftraege_inkl_vorgangsfolgen %>%
    count(Vorgangsfolge, name = "Anzahl_Auftraege")

Vorgangsfolgen_und_Materialnummern <- Vorgangsfolgen_und_Materialnummern %>%
    left_join(anzahl_auftraege, by = "Vorgangsfolge")

#view(auftraege_inkl_vorgangsfolgen)
