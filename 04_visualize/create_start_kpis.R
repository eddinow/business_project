# Für die Startseite berechnen wir hier den median der abweichung aller aufträge 
# und den servicelevel als anteil aufträge mit abweichung >=0 von allen aufträgen

# Initialisierung ----------------------------------------------------------------

rm(list = ls())
set.seed(1)

library(dplyr)

# Daten laden -------------------------------------------------------------------
source("00_tidy/create_all_data_finalized.R")  

# Transform------------------

start_kpis <- data.frame(
    `Avg LT` = median(all_data_finalized$lead_time_ist, na.rm = TRUE),
    `Avg Servicelevel` = round(mean(all_data_finalized$abweichung >= 0, na.rm = TRUE) * 100, 0)
)

view(start_kpis)