# load packages

library(readxl)

library(dplyr)

# Import -----------------------------------------------------------------------

file_path <- "C:\\Users\\Uzun\\Downloads\\business_project\\2025-04-08_Auftragsköpfe SAP.xlsx"

excel_sheets(file_path)

df <- read_excel(file_path, sheet = 1)
# Tidy -------------------------------------------------------------------------

df <- df %>%
    
    mutate(Fertigungslinie_clean = as.character(Fertigungslinie),
           Fertigungslinie_clean = gsub("^0+", "", Fertigungslinie_clean)) 

# Transform --------------------------------------------------------------------

linien_list <- split(df, df$Fertigungslinie_clean)