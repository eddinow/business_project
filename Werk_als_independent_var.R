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

# Transform --------------------------------------------------------------------

#Werke und Linien (welche Linien gibt es in welchen Werken?)
linie_pro_werk <- auftraege_raw %>%
    count(Werk, Fertigungslinie) %>%
    pivot_wider(
        names_from = Fertigungslinie,
        values_from = n,
        values_fill = 0
    )


#Werke und Planer (welche Planer gibt es für welche Werke?)
planer_pro_werk <- auftraege_raw %>%
    count(Werk, Planer) %>%
    pivot_wider(
        names_from = Planer,
        values_from = n,
        values_fill = 0
    )

#Werke und Workflows 
workflow_pro_werk <- auftraege_inkl_vorgangsfolgen %>%
    count(Werk, Vorgangsfolge) %>%
    pivot_wider(
        names_from = Vorgangsfolge,
        values_from = n,
        values_fill = 0
    )

#Werke und arbeitsplatz 
arbeitsplatz_pro_werk <- auftraege_inkl_vorgangsfolgen %>%
    count(Werk, Vorgangsfolge) %>%
    pivot_wider(
        names_from = Vorgangsfolge,
        values_from = n,
        values_fill = 0
    )

#Werke und material 
material_pro_werk <- auftraege_inkl_vorgangsfolgen %>%
    count(Werk, Materialnummer) %>%
    pivot_wider(
        names_from = Materialnummer,
        values_from = n,
        values_fill = 0
    )

view(arbeitsplatz_pro_werk)
