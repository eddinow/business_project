library(shiny)
library(argonDash)
library(DT)

#@KASPAR: Hier in create_material_overview muss noch eine Übersichtstabelle für jedes einzelne Material ge-
#macht werden, ähnlich wie für die anderen Kategorien. war mir nur nicht sicher
#wie man das anbetracht der vielen mat-nr. machen soll. vllt abc?

#source("02_model/create_material_overview.R")


# In material_ui kommt der ui-teil des shinydashboards. 
#Den namen materia_ui nicht ändern!

# UI-Modul-Funktion
material_ui <- function(id) {
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
material_server <- function(id) {
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