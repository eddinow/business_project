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

auftraege_raw <- read_excel("2025-04-08_Auftragsköpfe SAP.xlsx")
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")


# Tidy -------------------------------------------------------------------------

# Transform --------------------------------------------------------------------

#Werke und Linien (welche Linien gibt es in welchen Werken?)
werk_linie_kreuztabelle <- auftraege_raw %>%
    count(Werk, Fertigungslinie) %>%
    pivot_wider(
        names_from = Fertigungslinie,
        values_from = n,
        values_fill = 0
    )

View(werk_linie_kreuztabelle)

#Werke und Planer (welche Planer gibt es für welche Werke?)
werk_planer_kreuztabelle <- auftraege_raw %>%
    count(Werk, Planer) %>%
    pivot_wider(
        names_from = Planer,
        values_from = n,
        values_fill = 0
    )

View(werk_planer_kreuztabelle)