# Initialize -------------------------------------------------------------------

# rm(list = ls())
# 
# set.seed(1)

library(tidyverse)    
library(readxl)      
library(lubridate)    
library(janitor)
library(dplyr)

# Import -----------------------------------------------------------------------

source("00_tidy/02_data_cleaning.R")
source("00_tidy/Master Excel gebündelt mir Arbeitsplatz + Vorgangsfolgen.R")


all_data_finalized <- fast_fertiger_datensatz_auftragskoepfe %>%
    left_join(
        auftraege_inkl_vorgangsfolgen %>%
            dplyr::select(
                Auftragsnummer,
                vorgangsfolge = Vorgangsfolge,
                arbeitsplatzfolge = Arbeitsplatzfolge
            ),
        by = c("auftragsnummer" = "Auftragsnummer")
    )



