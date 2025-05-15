# Libraries
library(tidyverse)
library(readxl)

# Daten laden
auftraege_raw <- read_excel("2025-04-08_Auftragsköpfe SAP.xlsx")
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")

# Vorgangsfolgen erstellen
vorgangsfolgen <- vorgaenge_raw %>%
    arrange(Auftragsnummer, Vorgangsnummer) %>%
    group_by(Auftragsnummer) %>%
    summarise(Vorgangsfolge = paste(Vorgangsnummer, collapse = " -> "), .groups = "drop")

# Join: Aufträge + Vorgangsfolge
auftraege_inkl_vorgangsfolgen <- auftraege_raw %>%
    left_join(vorgangsfolgen, by = "Auftragsnummer")

# 🔎 Prozessketten nach Fertigungslinie analysieren
linien_prozesse <- auftraege_inkl_vorgangsfolgen %>%
    group_by(Fertigungslinie, Vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    arrange(Fertigungslinie, desc(Anzahl))

# 🔎 Prozessketten nach Material analysieren
material_prozesse <- auftraege_inkl_vorgangsfolgen %>%
    group_by(Materialnummer, Vorgangsfolge) %>%
    summarise(Anzahl = n(), .groups = "drop") %>%
    arrange(Materialnummer, desc(Anzahl))

# Optional: Exportieren für Shiny-App später
write.csv(linien_prozesse, "linien_prozessketten.csv", row.names = FALSE)
write.csv(material_prozesse, "material_prozessketten.csv", row.names = FALSE)

# Anzeigen in RStudio
View(linien_prozesse)
View(material_prozesse)