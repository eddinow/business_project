library(shiny)
library(DT)
library(ggplot2)

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
            box(title = "Soll-LT pro Workflow", width = 12, status = "primary", solidHeader = TRUE,
                selectInput(ns("selected_workflow"), "Workflow auswählen",
                            choices = NULL),  # wird serverseitig gefüllt
                plotOutput(ns("workflow_plot"))
            )
        ),
        
        fluidRow(
            box(title = "Ist-LT pro Workflow", width = 12, status = "primary", solidHeader = TRUE,
                selectInput(ns("selected_workflow"), "Workflow auswählen",
                            choices = NULL),  # wird serverseitig gefüllt
                plotOutput(ns("workflow_plot"))
            )
        )
    )
}

# Server

workflows_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Tabelle rendern (wie gehabt)
        if (exists("workflows_overview")) {
            output$workflows_table <- renderDT({
                datatable(workflows_overview,
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
            output$workflows_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
        
        observe({
            updateSelectInput(session, "selected_workflow",
                              choices = unique(all_data_finalized$vorgangsfolge))
        })
        
        # Reaktives Objekt für den gewählten Workflow
        est_plot_obj <- reactive({
            req(input$selected_workflow)
            result <- create_est_lt_ist(all_data_finalized, input$selected_workflow)  # <- hier angepasst!
            return(result)
        })
        
        # Plot ausgeben
        output$workflow_plot <- renderPlot({
            result <- est_plot_obj()
            req(result)
            print(result$plot)  # <- ganz wichtig!
        })
    })  # ← Diese schließende Klammer war gefehlt
}