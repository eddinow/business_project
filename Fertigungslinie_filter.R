# load packages

library(readxl)

library(dplyr)

# Import -----------------------------------------------------------------------

df <- read_excel("2025-04-08_Auftragsköpfe SAP.xlsx", sheet = 1)

# Tidy -------------------------------------------------------------------------

df <- df %>%
    
    mutate(Fertigungslinie_clean = as.character(Fertigungslinie),
           Fertigungslinie_clean = gsub("^0+", "", Fertigungslinie_clean)) 

# Transform --------------------------------------------------------------------

linien_list <- split(df, df$Fertigungslinie_clean)