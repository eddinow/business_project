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


# Tidy -------------------------------------------------------------------------

# Transform --------------------------------------------------------------------

vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " → "), .groups = "drop")

einzigartige_folgen <- vorgangsfolgen %>%
    count(Vorgangsfolge, sort = TRUE)

View(einzigartige_folgen)

#Arbeitsschritte und Linien

vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " → "), .groups = "drop")

# Schritt 2: Diese Information an die Auftragsköpfe anhängen
auftraege_inkl_vorgangsfolgen <- auftraege_raw %>%
    left_join(vorgangsfolgen, by = "Auftragsnummer")

# Ergebnis prüfen
View(auftraege_inkl_vorgangsfolgen)