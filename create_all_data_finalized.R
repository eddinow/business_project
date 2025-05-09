# Initialize -------------------------------------------------------------------

# Clear all objects from the environment
rm(list = ls())

# Set seed for reproducibility
set.seed(1)

# Load required packages
library(tidyverse)    # data manipulation and visualization
library(readxl)       # reading Excel files
library(lubridate)    # working with dates
library(janitor)
library(dplyr)

# Import -----------------------------------------------------------------------

source("Master Excel gebündelt mir Arbeitsplatz + Vorgangsfolgen.R")
ls()
source("02_data_cleaning.R")

all_data_finalized <- vorgaenge_sap_ohne_na_ohne0 %>%
    left_join(
        auftraege_inkl_vorgangsfolgen %>%
            dplyr::select(
                Auftragsnummer,
                vorgangsfolge = Vorgangsfolge,
                arbeitsplatzfolge = Arbeitsplatzfolge
            ),
        by = c("auftragsnummer" = "Auftragsnummer")
    )

view(all_data_finalized)
glimpse(all_data_finalized)
