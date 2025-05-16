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

# Transform --------------------------------------------------------------------

vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " → "), .groups = "drop")

einzigartige_folgen_inkl_anzahl <- vorgangsfolgen %>%
    count(Vorgangsfolge, sort = TRUE)

#View(einzigartige_folgen_inkl_anzahl)

#Vorgänge und Linien

vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " → "), .groups = "drop")

auftraege_inkl_vorgangsfolgen <- auftraege_raw %>%
    left_join(vorgangsfolgen, by = "Auftragsnummer")

#View(auftraege_inkl_vorgangsfolgen)

Vorgangsfolgen_und_Fertigungslinien <- auftraege_inkl_vorgangsfolgen %>%
    filter(!is.na(Fertigungslinie)) %>%
    count(Vorgangsfolge, Fertigungslinie) %>%
    pivot_wider(names_from = Fertigungslinie,
                values_from = n,
                values_fill = 0)
Vorgangsfolgen_und_Fertigungslinien <- Vorgangsfolgen_und_Fertigungslinien %>%
    mutate(Gesamt = rowSums(.[, -1]))

#View(einzigartige_folgen_mit_gesamt)

View(Vorgangsfolgen_und_Fertigungslinien)

#Material und Vorgänge

Vorgangsfolgen_und_Materialnummern <- auftraege_inkl_vorgangsfolgen %>%
    count(Vorgangsfolge, Materialnummer) %>%
    group_by(Vorgangsfolge) %>%
    summarise(
        Alle_Materialnummern = paste(Materialnummer, collapse = ", "),
        Anzahl_Materialien = n(),
        Top_Material = Materialnummer[which.max(n)],
        .groups = "drop"
    )

Vorgangsfolgen_und_Materialnummern <- Vorgangsfolgen_und_Materialnummern %>%
    rename(
        Materialnummern = Alle_Materialnummern,
        Anzahl_Materialnummern = Anzahl_Materialien
    )


anzahl_auftraege <- auftraege_inkl_vorgangsfolgen %>%
    count(Vorgangsfolge, name = "Anzahl_Auftraege")

Vorgangsfolgen_und_Materialnummern <- Vorgangsfolgen_und_Materialnummern %>%
    left_join(anzahl_auftraege, by = "Vorgangsfolge")

view(auftraege_inkl_vorgangsfolgen)
