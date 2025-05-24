library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)

source("02_model/create_workflows_overview.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)
source("02_model/kpis_workflow_arbeitsplatz.R", local = TRUE)

# UI-Modul-Funktion
# UI-Modul-Funktion
workflows_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Workflows", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Workflow."),
                selectInput(ns("selected_workflow"), "Workflow auswählen:", choices = NULL),
                DTOutput(ns("workflows_table"))
            )
        ),
        fluidRow(
            box(title = "Mengenabhängige Lead Time", width = 12, status = "primary", solidHeader = TRUE,
                plotlyOutput(ns("workflow_plot"))
            )
        ),
        fluidRow(
            column(
                width = 8,
                box(title = "Detailansicht Vorgänge", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput(ns("stacked_bar_plot"))
                )
            ),
            column(
                width = 4,
                box(title = textOutput(ns("table_title")), width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput(ns("workflow_table_detail"))
                )
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
            workflows <- unique(all_data_finalized$vorgangsfolge)
            updateSelectInput(session, "selected_workflow", choices = workflows)
        })
        
        # Plot: Soll vs. Ist
        est_plot_obj <- reactive({
            req(input$selected_workflow)
            create_est_lt_combined(all_data_finalized, input$selected_workflow, session = session)
        })
        
        output$workflow_plot <- plotly::renderPlotly({
            result <- est_plot_obj()
            req(result)
            plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
        })
        
        # Balkendiagramm Zeitanteile
        output$stacked_bar_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow)
            
            plot_workflow_structure(df)
        })
        
        # Detailtabelle Zeitanteile
        output$workflow_table_detail <- renderDT({
            req(input$selected_workflow)
            
            df <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                pivot_longer(
                    cols = ends_with("_median"),
                    names_to = "Vorgang",
                    values_to = "Time"
                ) %>%
                filter(!is.na(Time)) %>%
                mutate(
                    Vorgang = str_replace(Vorgang, "_median", ""),
                    Vorgang = ifelse(Vorgang == "Liegedauer_gesamt", "Avg. Delay", Vorgang)
                ) %>%
                arrange(desc(Time)) %>%
                mutate(
                    `Avg. LT (d)` = round(Time, 1),
                    `Avg. LT (%)` = round(Time / sum(Time) * 100, 1)
                ) %>%
                dplyr::select(Vorgang, `Avg. LT (d)`, `Avg. LT (%)`)
            
            # Summenzeile hinzufügen
            total_row <- df %>%
                summarise(
                    Vorgang = "Gesamt",
                    `Avg. LT (d)` = sum(`Avg. LT (d)`, na.rm = TRUE),
                    `Avg. LT (%)` = sum(`Avg. LT (%)`, na.rm = TRUE)
                )
            
            df <- bind_rows(df, total_row)
            
            datatable(
                df,
                escape = FALSE,
                rownames = FALSE,
                options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE
                ),
                class = "stripe cell-border compact"
            ) %>%
                formatStyle(
                    columns = names(df),
                    target = "row",
                    fontWeight = styleEqual("Gesamt", "bold"),
                    backgroundColor = styleEqual("Gesamt", "#f0f0f0"),
                    color = styleEqual("Gesamt", "black")
                ) %>%
                formatStyle(
                    columns = names(df),
                    textAlign = 'center'
                )
        })
        
        output$table_title <- renderText({
            req(input$selected_workflow)
            input$selected_workflow
        })
    })  # schließt moduleServer
}
