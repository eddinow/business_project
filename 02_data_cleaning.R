# Initialize -------------------------------------------------------------------

# Clear all objects from the environment
rm(list = ls())

# Set seed for reproducibility
set.seed(1)

# Load required packages
library(tidyverse)    # data manipulation and visualization
library(readxl)       # reading Excel files
library(lubridate)    # working with dates

# Import -----------------------------------------------------------------------

# Read raw SAP data files from the 'data' directory
order_headers <- read_excel("auftragskoepfe_sap_raw.xlsx")
operations    <- read_excel("vorgaenge_sap_raw.xlsx")

# Tidy -------------------------------------------------------------------------

# 1) Standardize column names to snake_case: lowercase + underscores
clean_names_simple <- function(df) {
    names(df) <- names(df) %>%
        tolower() %>%
        str_replace_all("\\s+", "_")
    df
}

order_headers <- clean_names_simple(order_headers)
operations    <- clean_names_simple(operations)

# 2) Basic cleaning on both tables:
#    - Trim whitespace in all character columns
#    - Remove exact duplicates
#    - Drop rows without an order number
order_headers <- order_headers %>%
    mutate(across(where(is.character), str_trim)) %>%
    distinct() %>%
    filter(!is.na(auftragsnummer) & auftragsnummer != "")

operations <- operations %>%
    mutate(across(where(is.character), str_trim)) %>%
    distinct() %>%
    filter(!is.na(auftragsnummer) & auftragsnummer != "")

# 3) Print missing-value summary for each column
print(order_headers %>% summarise(across(everything(), ~ sum(is.na(.)))))
print(operations    %>% summarise(across(everything(), ~ sum(is.na(.)))))