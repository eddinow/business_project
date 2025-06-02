# Initialize -------------------------------------------------------------------

# rm(list = ls())
# 
# set.seed(1)

library(tidyverse)    
library(readxl)      
library(lubridate)    
library(janitor)
library(dplyr)
library(writexl)

# Import -----------------------------------------------------------------------

source("00_tidy/02_data_cleaning.R")
source("00_tidy/Master Excel gebündelt mir Arbeitsplatz + Vorgangsfolgen.R")


# Tidy --------

# Transform -------

# Model -----------

# Den bereinigten Auftragsdaten werden die Vorgangsfolgen und Arbeitsplatz-
# folgen hinzugefügt, damit wir die Lead Times auf Workflowebene abgebildet 
# werden können.


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

# Visualize -------

# Communicate ------

# Erzeugen einer Exceldatei, um Ladedauer zu reduzieren

write_xlsx(all_data_finalized, path = "00_tidy/all_data_finalized.xlsx")
