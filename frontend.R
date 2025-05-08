#Initialize ------
setwd("C:\\Users\\julia\\OneDrive\\Dokumente\\04 Supply Chain Management Master\\13 Business Project\\Github Verbindung\\business_project")

rm(list = ls())
set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)

# Import -----------------------------------------------------------------------

auftraege_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")
source("Arbeitsschritte_gebuendelt_als independent_var.R")
source("Arbeitsplaetze_als_independent_var.R")



# Wir mappen die Daten: Erstellen einer Matrix mit Werk, Linie, Planer, 
#Vorgangsfolge, Arbeitsplatz & Material pro Werk als Zeilen und Spalten. Zeilen
# bilden die Oberkategorie und Spalten die Unterkategorie, also bspw Zeile Werk
# Spalte Linie = Linien pro Werk. So haben wir gute Möglichkeit data frames 
# gezielt einzubetten und UI aufzubauen


mapping <- list()

# Werk als Quelle
mapping$`Werk pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk) %>% group_by(Werk) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk, Fertigungslinie) %>% group_by(Werk) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk, Planer) %>% group_by(Werk) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk, Vorgangsfolge) %>% group_by(Werk) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk, Arbeitsplatzfolge) %>% group_by(Werk) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Werk` <- auftraege_inkl_vorgangsfolgen %>% distinct(Werk, Materialnummer) %>% group_by(Werk) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Fertigungslinie als Quelle
mapping$`Werk pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie, Werk) %>% group_by(Fertigungslinie) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie) %>% group_by(Fertigungslinie) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie, Planer) %>% group_by(Fertigungslinie) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie, Vorgangsfolge) %>% group_by(Fertigungslinie) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie, Arbeitsplatzfolge) %>% group_by(Fertigungslinie) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Linie` <- auftraege_inkl_vorgangsfolgen %>% distinct(Fertigungslinie, Materialnummer) %>% group_by(Fertigungslinie) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Planer als Quelle
mapping$`Werk pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer, Werk) %>% group_by(Planer) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer, Fertigungslinie) %>% group_by(Planer) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer) %>% group_by(Planer) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer, Vorgangsfolge) %>% group_by(Planer) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer, Arbeitsplatzfolge) %>% group_by(Planer) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Planer` <- auftraege_inkl_vorgangsfolgen %>% distinct(Planer, Materialnummer) %>% group_by(Planer) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Vorgangsfolge als Quelle
mapping$`Werk pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge, Werk) %>% group_by(Vorgangsfolge) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge, Fertigungslinie) %>% group_by(Vorgangsfolge) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge, Planer) %>% group_by(Vorgangsfolge) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge) %>% group_by(Vorgangsfolge) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge, Arbeitsplatzfolge) %>% group_by(Vorgangsfolge) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Vorgangsfolge` <- auftraege_inkl_vorgangsfolgen %>% distinct(Vorgangsfolge, Materialnummer) %>% group_by(Vorgangsfolge) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Arbeitsplatz als Quelle
mapping$`Werk pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge, Werk) %>% group_by(Arbeitsplatzfolge) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge, Fertigungslinie) %>% group_by(Arbeitsplatzfolge) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge, Planer) %>% group_by(Arbeitsplatzfolge) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge, Vorgangsfolge) %>% group_by(Arbeitsplatzfolge) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge) %>% group_by(Arbeitsplatzfolge) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Arbeitsplatz` <- auftraege_inkl_vorgangsfolgen %>% distinct(Arbeitsplatzfolge, Materialnummer) %>% group_by(Arbeitsplatzfolge) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Materialnummer als Quelle
mapping$`Werk pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer, Werk) %>% group_by(Materialnummer) %>% summarise(Werk = list(unique(Werk)), .groups = "drop")
mapping$`Linie pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer, Fertigungslinie) %>% group_by(Materialnummer) %>% summarise(Linie = list(unique(Fertigungslinie)), .groups = "drop")
mapping$`Planer pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer, Planer) %>% group_by(Materialnummer) %>% summarise(Planer = list(unique(Planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer, Vorgangsfolge) %>% group_by(Materialnummer) %>% summarise(Vorgangsfolge = list(unique(Vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer, Arbeitsplatzfolge) %>% group_by(Materialnummer) %>% summarise(Arbeitsplatz = list(unique(Arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Material` <- auftraege_inkl_vorgangsfolgen %>% distinct(Materialnummer) %>% group_by(Materialnummer) %>% summarise(Material = list(unique(Materialnummer)), .groups = "drop")

# Visualize-----------------------------------------

ui <- fluidPage(
    titlePanel("Mapping-Explorer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("mapping_auswahl",
                        "Wähle ein Mapping:",
                        choices = names(mapping),
                        selected = names(mapping)[1])
        ),
        mainPanel(
            DTOutput("mapping_tabelle")
        )
    )
)

server <- function(input, output, session) {
    output$mapping_tabelle <- renderDT({
        req(input$mapping_auswahl)
        mapping[[input$mapping_auswahl]]
    })
}

shinyApp(ui, server)

