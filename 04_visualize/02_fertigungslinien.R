library(shiny)
library(DT)

source("02_model/kpis_linie.R", local = TRUE)

linien_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title       = "Übersicht aller Fertigungslinien",
                width       = 12,
                solidHeader = TRUE,
                status      = "primary",
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Fertigungslinie."),
                DTOutput(ns("linien_table"))
            )
        )
    )
}

linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (exists("linien_overview")) {
            output$linien_table <- renderDT({
                datatable(
                    linien_overview,
                    options = list(
                        pageLength = 10,
                        scrollX = TRUE
                    ),
                    rownames = FALSE,
                    class = "stripe hover cell-border"
                )
            })
        } else {
            output$linien_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
    })
}
