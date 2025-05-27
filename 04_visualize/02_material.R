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
            box(
                title = "Materialnummer auswählen",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                if (exists("materialnummer_overview")) {
                    selectInput(
                        ns("matnr_select"),
                        "Materialnummer:",
                        choices = sort(unique(materialnummer_overview$materialnummer)),
                        selected = sort(unique(materialnummer_overview$materialnummer))[1]
                    )
                } else {
                    p("⚠️ Keine Daten vorhanden.")
                }
            )
        ),
        fluidRow(
            box(
                title = "Kennzahlen zur Materialnummer",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput(ns("material_table")),
                br(),
                downloadButton(ns("download_csv"), "CSV exportieren")
            )
        ),
        fluidRow(
            box(
                title = "Automatische Interpretation",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("material_insights"))
            )
        )
    )
}

# Server-Modul-Funktion
material_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (!exists("materialnummer_overview")) return()
        
        daten_gefiltert <- reactive({
            req(input$matnr_select)
            materialnummer_overview %>%
                filter(materialnummer == input$matnr_select)
        })
        
        output$material_table <- renderDT({
            req(nrow(daten_gefiltert()) > 0)
            datatable(
                daten_gefiltert(),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("kpi_materialnummer_", input$matnr_select, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(daten_gefiltert(), file, row.names = FALSE)
            }
        )
        
        output$material_insights <- renderUI({
            df <- daten_gefiltert()
            req(nrow(df) > 0)
            
            HTML(paste0(
                "<p><strong>Anzahl Einträge:</strong> ", df$Anzahl, "</p>",
                "<p><strong>Ø Durchlaufzeit:</strong> ", df$Durchschnitt_LT, " Tage</p>",
                "<p><strong>Termintreue:</strong> ", round(df$Termintreue * 100), "%</p>",
                "<p><strong>Liefertreue:</strong> ", round(df$Liefertreue * 100), "%</p>"
            ))
        })
    })
}