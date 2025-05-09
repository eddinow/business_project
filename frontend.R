# Initialisierung ----------------------------------------------------------------

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

# Daten laden -------------------------------------------------------------------
source("create_all_data_finalized.R")  # erzeugt 'all_data_finalized'


#style_red_max_lt <- function(data) {
 #   if (!"durchschnitt_lt" %in% names(data)) return(NULL)
  #  max_row <- which.max(data$durchschnitt_lt)
   # JS(
    #    sprintf(
     #       "function(row, data, index) {
      #   if(index === %d) {
       #    $('td', row).css('background-color', '#fdd');
        #   $('td:eq(0)', row).css('color', 'red');
         #}
    #   }",
     #       max_row - 1
      #  )
#    )
#}

einstieg_kategorie <- function(df, kategorie_spalte) {
    df %>%
        group_by(.data[[kategorie_spalte]]) %>%
        summarise(
            durchschnitt_lt = round(mean(lead_time_ist, na.rm = TRUE), 1),
            work_in_progress = n(),
            durchschnitt_performance = round(mean(abweichung, na.rm = TRUE), 1),
            platzhalter = "",
            .groups = "drop"
        ) %>%
        rename(Kategorie = !!kategorie_spalte) %>%
        arrange(desc(durchschnitt_lt))
}

uebersichtswerte <- list(
    werk              = einstieg_kategorie(all_data_finalized, "werk"),
    fertigungslinie   = einstieg_kategorie(all_data_finalized, "fertigungslinie"),
    planer            = einstieg_kategorie(all_data_finalized, "planer"),
    vorgangsfolge     = einstieg_kategorie(all_data_finalized, "vorgangsfolge"),
    arbeitsplatzfolge = einstieg_kategorie(all_data_finalized, "arbeitsplatzfolge"),
    materialnummer    = einstieg_kategorie(all_data_finalized, "materialnummer")
)




# Mapping vorbereiten ----------------------------------------------------------
mapping <- list()

# Werk als Quelle
mapping$`Werk pro Werk` <- all_data_finalized %>% distinct(werk) %>% group_by(werk) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Werk` <- all_data_finalized %>% distinct(werk, fertigungslinie) %>% group_by(werk) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Werk` <- all_data_finalized %>% distinct(werk, planer) %>% group_by(werk) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Werk` <- all_data_finalized %>% distinct(werk, vorgangsfolge) %>% group_by(werk) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Werk` <- all_data_finalized %>% distinct(werk, arbeitsplatzfolge) %>% group_by(werk) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Werk` <- all_data_finalized %>% distinct(werk, materialnummer) %>% group_by(werk) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

# Fertigungslinie als Quelle
mapping$`Werk pro Linie` <- all_data_finalized %>% distinct(fertigungslinie, werk) %>% group_by(fertigungslinie) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Linie` <- all_data_finalized %>% distinct(fertigungslinie) %>% group_by(fertigungslinie) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Linie` <- all_data_finalized %>% distinct(fertigungslinie, planer) %>% group_by(fertigungslinie) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Linie` <- all_data_finalized %>% distinct(fertigungslinie, vorgangsfolge) %>% group_by(fertigungslinie) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Linie` <- all_data_finalized %>% distinct(fertigungslinie, arbeitsplatzfolge) %>% group_by(fertigungslinie) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Linie` <- all_data_finalized %>% distinct(fertigungslinie, materialnummer) %>% group_by(fertigungslinie) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

# Planer als Quelle
mapping$`Werk pro Planer` <- all_data_finalized %>% distinct(planer, werk) %>% group_by(planer) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Planer` <- all_data_finalized %>% distinct(planer, fertigungslinie) %>% group_by(planer) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Planer` <- all_data_finalized %>% distinct(planer) %>% group_by(planer) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Planer` <- all_data_finalized %>% distinct(planer, vorgangsfolge) %>% group_by(planer) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Planer` <- all_data_finalized %>% distinct(planer, arbeitsplatzfolge) %>% group_by(planer) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Planer` <- all_data_finalized %>% distinct(planer, materialnummer) %>% group_by(planer) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

# Vorgangsfolge als Quelle
mapping$`Werk pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge, werk) %>% group_by(vorgangsfolge) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge, fertigungslinie) %>% group_by(vorgangsfolge) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge, planer) %>% group_by(vorgangsfolge) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge) %>% group_by(vorgangsfolge) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge, arbeitsplatzfolge) %>% group_by(vorgangsfolge) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Vorgangsfolge` <- all_data_finalized %>% distinct(vorgangsfolge, materialnummer) %>% group_by(vorgangsfolge) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

# Arbeitsplatz als Quelle
mapping$`Werk pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge, werk) %>% group_by(arbeitsplatzfolge) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge, fertigungslinie) %>% group_by(arbeitsplatzfolge) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge, planer) %>% group_by(arbeitsplatzfolge) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge, vorgangsfolge) %>% group_by(arbeitsplatzfolge) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge) %>% group_by(arbeitsplatzfolge) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Arbeitsplatz` <- all_data_finalized %>% distinct(arbeitsplatzfolge, materialnummer) %>% group_by(arbeitsplatzfolge) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

# Materialnummer als Quelle
mapping$`Werk pro Material` <- all_data_finalized %>% distinct(materialnummer, werk) %>% group_by(materialnummer) %>% summarise(werk = list(unique(werk)), .groups = "drop")
mapping$`Linie pro Material` <- all_data_finalized %>% distinct(materialnummer, fertigungslinie) %>% group_by(materialnummer) %>% summarise(linie = list(unique(fertigungslinie)), .groups = "drop")
mapping$`Planer pro Material` <- all_data_finalized %>% distinct(materialnummer, planer) %>% group_by(materialnummer) %>% summarise(planer = list(unique(planer)), .groups = "drop")
mapping$`Vorgangsfolge pro Material` <- all_data_finalized %>% distinct(materialnummer, vorgangsfolge) %>% group_by(materialnummer) %>% summarise(vorgangsfolge = list(unique(vorgangsfolge)), .groups = "drop")
mapping$`Arbeitsplatz pro Material` <- all_data_finalized %>% distinct(materialnummer, arbeitsplatzfolge) %>% group_by(materialnummer) %>% summarise(arbeitsplatz = list(unique(arbeitsplatzfolge)), .groups = "drop")
mapping$`Material pro Material` <- all_data_finalized %>% distinct(materialnummer) %>% group_by(materialnummer) %>% summarise(material = list(unique(materialnummer)), .groups = "drop")

uebersichtswerte <- list(
    werk = einstieg_kategorie(all_data_finalized, "werk"),
    fertigungslinie = einstieg_kategorie(all_data_finalized, "fertigungslinie"),
    planer = einstieg_kategorie(all_data_finalized, "planer"),
    vorgangsfolge = einstieg_kategorie(all_data_finalized, "vorgangsfolge"),
    arbeitsplatzfolge = einstieg_kategorie(all_data_finalized, "arbeitsplatzfolge"),
    materialnummer = einstieg_kategorie(all_data_finalized, "materialnummer")
)


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Lead Time Calculator"),
    uiOutput("main_ui")
)

# Server ------------------------------------------------------------------------

server <- function(input, output, session) {
    
    nav <- reactiveValues(
        seite = "start",
        quelle = NULL,
        auspraegung = NULL,
        mappingziel = NULL
    )
    
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
    
    observeEvent(input$btn_werk,            { nav$quelle <- "werk"; nav$seite <- "uebersicht" })
    observeEvent(input$btn_linien,          { nav$quelle <- "fertigungslinie"; nav$seite <- "uebersicht" })
    observeEvent(input$btn_planer,          { nav$quelle <- "planer"; nav$seite <- "uebersicht" })
    observeEvent(input$btn_workflows,       { nav$quelle <- "vorgangsfolge"; nav$seite <- "uebersicht" })
    observeEvent(input$btn_ap,              { nav$quelle <- "arbeitsplatzfolge"; nav$seite <- "uebersicht" })
    observeEvent(input$btn_material,        { nav$quelle <- "materialnummer"; nav$seite <- "uebersicht" })
    
    observe({
        if (!is.null(nav$quelle)) {
            auspraegungen <- sort(unique(all_data_finalized[[nav$quelle]]))
            updateSelectInput(session, "ausgewaehlte_auspraegung", choices = auspraegungen, selected = nav$auspraegung)
        }
    })
    
    output$auspraegungstabelle <- renderDT({
        req(nav$quelle)
        
        df <- einstieg_kategorie(all_data_finalized, nav$quelle)
        max_val <- max(df$durchschnitt_lt, na.rm = TRUE)
        
        df %>%
            datatable(
                selection = "single",
                options = list(pageLength = 10)
            ) %>%
            formatStyle(
                columns = names(df),
                target = 'row',
                backgroundColor = styleEqual(
                    levels = df$durchschnitt_lt,
                    values = ifelse(df$durchschnitt_lt == max_val, "#fdd", NA)
                ),
                color = styleEqual(
                    levels = df$durchschnitt_lt,
                    values = ifelse(df$durchschnitt_lt == max_val, "red", "black")
                )
            )
    })
    
    observeEvent(input$auspraegungstabelle_rows_selected, {
        req(input$auspraegungstabelle_rows_selected)
        df <- all_data_finalized %>%
            group_by_at(nav$quelle) %>%
            summarise(`Anzahl Aufträge` = n(), .groups = "drop") %>%
            rename(Auspraegung = all_of(nav$quelle))
        
        auswahl <- df$Auspraegung[input$auspraegungstabelle_rows_selected]
        updateSelectInput(session, "ausgewaehlte_auspraegung", selected = auswahl)
    })
    
    observeEvent(input$bestaetige_auswahl, {
        req(input$ausgewaehlte_auspraegung)
        nav$auspraegung <- input$ausgewaehlte_auspraegung
        nav$seite <- "detail"
    })
    
    observeEvent(input$back_to_uebersicht, {
        nav$seite <- "uebersicht"
    })
    
    observeEvent(input$back_to_start, {
        nav$seite <- "start"
        nav$auspraegung <- NULL
        nav$mappingziel <- NULL
        nav$quelle <- NULL
    })
    
    output$auspraegung_detail <- renderTable({
        data.frame(
            Kategorie = c("Linien", "Planer", "Workflows", "Arbeitsplätze", "Material"),
            Insgesamt = rep("Platzhalter", 5),
            QuickView = paste("Liste Namen der ersten", 15:19)
        )
    })
}

# App starten -------------------------------------------------------------------

shinyApp(ui, server)
