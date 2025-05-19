library(shiny)
library(argonDash)
library(DT)

source("02_model/create_werke_overview.R")


# In werke_ui kommt der ui-teil des shinydashboards. 
#Den namen werke_ui nicht ändern!


# UI-Modul-Funktion
werke_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Werke", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Werk."),
                DTOutput(ns("werke_table"))
            )
        )
    )
}

# Server-Modul-Funktion
werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (exists("werke_overview")) {
            output$werke_table <- renderDT({
                datatable(werke_overview,
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