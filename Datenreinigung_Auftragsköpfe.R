                                                                                                                                              
# Initialize--------------------------------------------------------------------
auftragskoepfe_sap_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
View(auftragskoepfe_sap_raw) 

# Import -----------------------------------------------------------------------
library(readxl)
library(tidyverse)
install.packages("janitor")
library(janitor)

# Tidy -------------------------------------------------------------------------
# Spaltennamen bereinigen
auftragskoepfe_sap_raw <- clean_names(auftragskoepfe_sap_raw)
names(auftragskoepfe_sap_raw)

#POSIXct zu Date
auftragskoepfe_sap_raw <- auftragskoepfe_sap_raw %>%
         mutate(across(where(~ inherits(., "POSIXct")), as.Date))
View(auftragskoepfe_sap_raw)
class(auftragskoepfe_sap_raw$)

# Transform---------------------------------------------------------------------
 #NAs rauslöschen
auftragsköpfe_ohne_nas <- auftragskoepfe_sap_raw %>% filter(!is.na(`endtermin_ist`))

 #0 und 1 bei gelieferten Mengen rausfiltern
auftragsköpfe_ohne_nas_ohne_nullundeins <- auftragsköpfe_ohne_nas %>% filter(!(gelieferte_menge %in% c(0, 1)))

# Model-------------------------------------------------------------------------


# Visualize---------------------------------------------------------------------


# communicate-------------------------------------------------------------------
