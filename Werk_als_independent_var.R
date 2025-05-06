# load packages

library(readxl)

library(dplyr)

# Import -----------------------------------------------------------------------

file_path <- "C:\\Users\\Uzun\\Downloads\\business_project\\2025-04-08_Auftragsköpfe SAP.xlsx"

df <- read_excel(file_path)

# Tidy -------------------------------------------------------------------------

df$Werk<- as.character(df$Werk)

Werk_list <- split(df, df$Werk)
# Transform --------------------------------------------------------------------

Werk_counts <- table(df$Werk)
