# load packages

library(readxl)

library(dplyr)

# Import -----------------------------------------------------------------------

file_path <- "C:\\Users\\Uzun\\Downloads\\business_project\\2025-04-08_Auftragsköpfe SAP.xlsx"

df <- read_excel(file_path)

# Tidy -------------------------------------------------------------------------

df$Planer <- as.character(df$Planer)

planer_list <- split(df, df$Planer)
# Transform --------------------------------------------------------------------

planer_counts <- table(df$Planer)
sum(planer_counts)
