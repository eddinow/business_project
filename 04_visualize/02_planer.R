library(shiny)
library(DT)

source("02_model/create_planer_overview.R", local = TRUE)

# UI-Modul-Funktion
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Planer", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Planer."),
                DTOutput(ns("planer_table"))
            )
        )
    )
}

# Server-Modul-Funktion
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (exists("planer_overview")) {
            output$planer_table <- renderDT({
                datatable(planer_overview,
                          options = list(
                              pageLength = 10,
                              autoWidth = TRUE,
                              dom = 'tip',
                              scrollX = TRUE
                          ),
                          rownames = FALSE,
                          class = "stripe hover cell-border")
            })
        } else {
            output$planer_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
    })
}