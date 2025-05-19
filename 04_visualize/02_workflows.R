library(shiny)
library(DT)
library(ggplot2)
library(plotly)

source("02_model/create_workflows_overview.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)

# UI-Modul-Funktion
workflows_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Workflows", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Workflow."),
                DTOutput(ns("workflows_table"))
            )
        ),
        fluidRow(
            box(title = "Lead Time je Workflow (Soll vs. Ist)", width = 12, status = "primary", solidHeader = TRUE,
                selectInput(ns("selected_workflow"), "Workflow auswählen",
                            choices = NULL),  # wird serverseitig gefüllt
                plotlyOutput(ns("workflow_plot"))
            )
        )
    )
}

# Server

workflows_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Tabelle
        if (exists("workflows_overview")) {
            output$workflows_table <- renderDT({
                datatable(workflows_overview,
                          options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip', scrollX = TRUE),
                          rownames = FALSE,
                          class = "stripe hover cell-border")
            })
        } else {
            output$workflows_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
        
        # Dropdown füllen
        observe({
            updateSelectInput(session, "selected_workflow",
                              choices = unique(all_data_finalized$vorgangsfolge))
        })
        
        # Kombinierter Plot (Soll + Ist)
        est_plot_obj <- reactive({
            req(input$selected_workflow)
            create_est_lt_combined(all_data_finalized, input$selected_workflow)
        })
        
        output$workflow_plot <- plotly::renderPlotly({
            result <- est_plot_obj()
            req(result)
            plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
        })
    })
}