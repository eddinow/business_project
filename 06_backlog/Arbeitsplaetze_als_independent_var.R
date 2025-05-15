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



#Code fürs Mappen in der App

#Arbeitsschrittfolgen und Linien (welche Linien für welche AS-Folgen?)
#linie_pro_AS <- auftraege_raw %>%
 #   count(Vorgangsfolge, Fertigungslinie) %>%
  #  pivot_wider(
   #     names_from = Fertigungslinie,
    #    values_from = n,
     #   values_fill = 0
    #)


#Werke und Planer (welche Planer gibt es für welche Werke?)
planer_pro_AS <- auftraege_raw %>%
    count(Vorgangsfolge, Planer) %>%
    pivot_wider(
        names_from = Planer,
        values_from = n,
        values_fill = 0
    )

#Werke und Workflows 
workflow_pro_AS <- auftraege_inkl_vorgangsfolgen %>%
    count(Vorgangsfolge, ) %>%
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
