# Initialize ------
rm(list = ls())
set.seed(1)
library(shiny)
library(argonDash)
library(DT)

source("02_model/create_fertigungslinien_overview.R")


# UI-Modul-Funktion
linien_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Fertigungslinien", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Fertigungslinie."),
                DTOutput(ns("linien_table"))
            )
        )
    )
}

# Server-Modul-Funktion
linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (exists("linien_overview")) {
            output$linien_table <- renderDT({
                datatable(linien_overview,
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