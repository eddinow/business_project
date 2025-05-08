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
source("Arbeitsschritte_gebuendelt_als independent_var.R")


# Tidy -------------------------------------------------------------------------

# Transform --------------------------------------------------------------------

# Arbeitsplatzfolge aus vorgaenge_raw erzeugen
arbeitsplatzfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Arbeitsplatzfolge = paste(Arbeitsplatz, collapse = " → "), .groups = "drop")

# An den bestehenden DataFrame anhängen
auftraege_inkl_vorgangsfolgen <- auftraege_inkl_vorgangsfolgen %>%
    left_join(arbeitsplatzfolgen, by = "Auftragsnummer")

view(auftraege_inkl_vorgangsfolgen)
