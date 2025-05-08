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

# Visualize---------------

ui <- fluidPage(
    titlePanel("Lead Time Calculator"),
    uiOutput("main_ui")
)

# Server ------------------------------------------------------------------------
server <- function(input, output, session) {
    
    # Reactive values to track navigation state
    nav <- reactiveValues(
        seite = "start",
        quelle = NULL,
        auspraegung = NULL,
        mappingziel = NULL
    )
    
    # Dynamisches UI je nach Zustand
    output$main_ui <- renderUI({
        if (nav$seite == "start") {
            fluidRow(
                column(12,
                       h2("Kategorie auswählen, um zu beginnen:"),
                       br(),
                       fluidRow(
                           column(2, actionButton("btn_werk", "Werk", class = "btn btn-primary", style = "width:100px; height:100px;")),
                           column(2, actionButton("btn_linien", "Fertigungslinie", class = "btn btn-primary", style = "width:100px; height:100px;")),
                           column(2, actionButton("btn_planer", "Planer", class = "btn btn-primary", style = "width:100px; height:100px;")),
                           column(2, actionButton("btn_workflows", "Workflows", class = "btn btn-primary", style = "width:100px; height:100px;")),
                           column(2, actionButton("btn_ap", "Arbeitsplätze", class = "btn btn-primary", style = "width:100px; height:100px;")),
                           column(2, actionButton("btn_material", "Material", class = "btn btn-primary", style = "width:100px; height:100px;"))
                       )
                )
            )
        } else if (nav$seite == "uebersicht") {
            fluidPage(
                h3(paste("Übersicht", nav$quelle)),
                DTOutput("auspraegungstabelle"),
                br(),
                selectInput("ausgewaehlte_auspraegung", paste("Wähle eine", nav$quelle, "aus:"), choices = NULL),
                actionButton("bestaetige_auswahl", "Auswahl bestätigen"),
                br(), br(),
                actionButton("back_to_start", "Zurück zur Startseite")
            )
        } else if (nav$seite == "detail") {
            fluidPage(
                h3(paste("Performance Dashboard Ausprägung für", nav$auspraegung)),
                tags$div(
                    tags$strong("Work in Progress"), ": Anzahl aller Aufträge in dieser Ausprägung", tags$br(),
                    tags$strong("Average LT"), ": ", tags$em("Text: Formel muss noch formuliert werden"), tags$br(),
                    tags$strong("Performance"), ": ", tags$em("Text: Formel muss noch formuliert werden"), tags$br(),
                    tags$strong("Platzhalter"), ": Platzhalter", tags$br(), tags$br(),
                    tags$h4("Siehe nähere Details:"),
                    tableOutput("auspraegung_detail"),
                    br()
                ),
                actionButton("back_to_uebersicht", "Zurück zur Übersicht"),
                br(), br(),
                actionButton("back_to_start", "Zurück zur Startseite")
            )
        }
    })
    
    # Navigation durch Buttons
    observeEvent(input$btn_werk, {
        nav$quelle <- "Werk"
        nav$seite <- "uebersicht"
    })
    observeEvent(input$btn_linien, {
        nav$quelle <- "Fertigungslinie"
        nav$seite <- "uebersicht"
    })
    observeEvent(input$btn_planer, {
        nav$quelle <- "Planer"
        nav$seite <- "uebersicht"
    })
    observeEvent(input$btn_workflows, {
        nav$quelle <- "Vorgangsfolge"
        nav$seite <- "uebersicht"
    })
    observeEvent(input$btn_ap, {
        nav$quelle <- "Arbeitsplatzfolge"
        nav$seite <- "uebersicht"
    })
    observeEvent(input$btn_material, {
        nav$quelle <- "Materialnummer"
        nav$seite <- "uebersicht"
    })
    
    # Dynamische Auswahl aktualisieren
    observe({
        if (!is.null(nav$quelle)) {
            auspraegungen <- sort(unique(auftraege_inkl_vorgangsfolgen[[nav$quelle]]))
            updateSelectInput(session, "ausgewaehlte_auspraegung", choices = auspraegungen, selected = nav$auspraegung)
        }
    })
    
    # Tabelle mit allen Ausprägungen
    output$auspraegungstabelle <- renderDT({
        req(nav$quelle)
        
        df <- auftraege_inkl_vorgangsfolgen %>%
            group_by_at(nav$quelle) %>%
            summarise(`Anzahl Aufträge` = n(),
                      `Current LT` = "Platzhalter LT",
                      `Performance` = "Platzhalter Perf",
                      .groups = "drop") %>%
            rename(Auspraegung = all_of(nav$quelle))
        
        datatable(df, selection = "single")
    })
    
    # Auswahl über Klick auf Tabelle
    observeEvent(input$auspraegungstabelle_rows_selected, {
        req(input$auspraegungstabelle_rows_selected)
        df <- auftraege_inkl_vorgangsfolgen %>%
            group_by_at(nav$quelle) %>%
            summarise(`Anzahl Aufträge` = n(), .groups = "drop") %>%
            rename(Auspraegung = all_of(nav$quelle))
        
        auswahl <- df$Auspraegung[input$auspraegungstabelle_rows_selected]
        updateSelectInput(session, "ausgewaehlte_auspraegung", selected = auswahl)
    })
    
    # Auswahl bestätigen und zur Detailansicht
    observeEvent(input$bestaetige_auswahl, {
        req(input$ausgewaehlte_auspraegung)
        nav$auspraegung <- input$ausgewaehlte_auspraegung
        nav$seite <- "detail"
    })
    
    # Zurück zur Übersicht
    observeEvent(input$back_to_uebersicht, {
        nav$seite <- "uebersicht"
    })
    
    # Zurück zur Startseite
    observeEvent(input$back_to_start, {
        nav$seite <- "start"
        nav$auspraegung <- NULL
        nav$mappingziel <- NULL
        nav$quelle <- NULL
    })
    
    # Detailansicht - Platzhalter Tabelle
    output$auspraegung_detail <- renderTable({
        data.frame(
            Kategorie = c("Linien", "Planer", "Workflows", "Arbeitsplätze", "Material"),
            Insgesamt = rep("Platzhalter", 5),
            QuickView = paste("Liste Namen der ersten", 15:19)
        )
    })
}

# Run App -----------------------------------------------------------------------
shinyApp(ui, server)