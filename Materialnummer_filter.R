# load packages

library(readxl)

library(dplyr)

# Import -----------------------------------------------------------------------

file_path <- "C:\\Users\\Uzun\\Downloads\\business_project\\2025-04-08_Auftragsköpfe SAP.xlsx"

df <- read_excel(file_path)

# Tidy -------------------------------------------------------------------------

df$Materialnummer <- as.character(df$Materialnummer)

Materialnummer_list <- split(df, df$Materialnummer)
# Transform --------------------------------------------------------------------

Materialnummer_counts <- table(df$Materialnummer)
