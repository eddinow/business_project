library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)

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
        ),
        
        sliderInput("selected_sollmenge", "Sollmenge wählen:",
                    min = 0, max = 1000000, value = 10000, step = 1000),
        
        verbatimTextOutput("sollmenge_info")
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
            create_est_lt_combined(all_data_finalized, input$selected_workflow, session = session)
        })
        
        output$workflow_plot <- plotly::renderPlotly({
            result <- est_plot_obj()
            req(result)
            plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
        })
        
        output$sollmenge_info <- renderPrint({
            req(input$selected_sollmenge)
            df <- est_plot_obj()$table
            
            bin_data <- df %>%
                filter(bin_start <= input$selected_sollmenge, bin_end > input$selected_sollmenge)
            
            if (nrow(bin_data) == 0) return("Keine Daten für diese Sollmenge.")
            
            ist_val <- bin_data %>% filter(variante == "Ist") %>% pull(lt_median)
            soll_val <- bin_data %>% filter(variante == "Soll") %>% pull(lt_median)
            
            cat("Gewählte Sollmenge:", input$selected_sollmenge, "\n")
            cat("Avg. LT IST:", ifelse(length(ist_val) > 0, round(ist_val, 2), "—"), "\n")
            cat("Avg. LT SOLL:", ifelse(length(soll_val) > 0, round(soll_val, 2), "—"), "\n")
        })
        
        
        output$workflow_plot <- plotly::renderPlotly({
            result <- est_plot_obj()
            req(result)
            plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
        })
        
        observe({
            req(input$selected_workflow)
            
            # Nur für Slider-Update – Plot wird hier NICHT benutzt
            create_est_lt_combined(all_data_finalized, input$selected_workflow, session = session)
        })
    })
}
