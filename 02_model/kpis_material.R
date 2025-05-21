# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)
library(stringr)
library(plotly)

# Import -----------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

#Material nach häufigkeit sortiert
tabelle <- all_data_finalized %>%
    group_by(materialnummer, arbeitsplatzfolge, vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    arrange(desc(Anzahl))
