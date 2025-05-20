# Für die Startseite berechnen wir hier den median der abweichung aller aufträge 
# und den servicelevel als anteil aufträge mit abweichung >=0 von allen aufträgen

# Initialisierung ----------------------------------------------------------------

#rm(list = ls())
#set.seed(1)

library(dplyr)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Transform------------------
avg_lt <- median(all_data_finalized$lead_time_ist, na.rm = TRUE)
avg_sl <- round(mean(all_data_finalized$abweichung >= 0, na.rm = TRUE) * 100, 0)

