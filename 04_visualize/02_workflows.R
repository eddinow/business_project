library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)

source("02_model/create_workflows_overview.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)
source("02_model/kpis_workflow_arbeitsplatz.R", local = TRUE)

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
                selectInput(ns("selected_workflow"), "Workflow auswählen:",
                            choices = NULL),
                plotlyOutput(ns("workflow_plot"))
            )
        ),
        fluidRow(
            box(title = "Zeitaufteilung pro Workflow (Stacked Bar)", width = 12, status = "primary", solidHeader = TRUE,
                selectInput(ns("selected_workflow_bar"), "Workflow auswählen:", choices = NULL),
                plotlyOutput(ns("stacked_bar_plot"))
            )
        ),
        sliderInput(ns("selected_sollmenge"), "Sollmenge wählen:",
                    min = 0, max = 1000000, value = 10000, step = 1000),
        verbatimTextOutput(ns("sollmenge_info"))
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
            updateSelectInput(session, "selected_workflow_bar", choices = workflows)
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
        
        # Zeitbalken: stacked bar je Workflow (horizontal)
        output$stacked_bar_plot <- renderPlotly({
            req(input$selected_workflow_bar)
            
            daten <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow_bar) %>%
                pivot_longer(
                    cols = -vorgangsfolge,
                    names_to = "Vorgang",
                    values_to = "Dauer"
                ) %>%
                filter(Dauer > 0) %>%
                mutate(
                    Vorgang = gsub("_median", "", Vorgang),
                    Kategorie = ifelse(Vorgang == "Liegedauer_gesamt", "Liegezeit", Vorgang)
                ) %>%
                mutate(Kategorie = factor(Kategorie, levels = c("0005", "0010", "0020", "0030", "0032", "0040", "0050", "0060", "0070", "Liegezeit")[c("0005", "0010", "0020", "0030", "0032", "0040", "0050", "0060", "0070", "Liegezeit") %in% Kategorie])) %>%
                mutate(
                    Anteil = round(100 * Dauer / sum(Dauer), 1),
                    text = paste0(
                        "Kategorie: ", Kategorie, "<br>",
                        "Dauer: ", Dauer, " Tage<br>",
                        "Anteil: ", Anteil, " %"
                    )
                )
            
            farben_fest <- c(
                "0005" = "#A6CEE3",
                "0010" = "#1F78B4",
                "0020" = "#B2DF8A",
                "0030" = "#33A02C",
                "0032" = "#FB9A99",
                "0040" = "#E31A1C",
                "0050" = "#FDBF6F",
                "0060" = "#FF7F00",
                "0070" = "#CAB2D6",
                "Liegezeit" = "#8B0000"
            )
            
            p <- ggplot(daten, aes(y = vorgangsfolge, x = Dauer, fill = Kategorie, text = text)) +
                geom_bar(stat = "identity", width = 0.4) +
                scale_fill_manual(values = farben_fest) +
                labs(
                    title = paste("Zeitaufteilung für:", input$selected_workflow_bar),
                    y = NULL,
                    x = "Dauer (Median in Tagen)",
                    fill = "Vorgang"
                ) +
                theme_minimal(base_size = 14)
            
            ggplotly(p, tooltip = "text")
        })
        
        # Slider dynamisch setzen
        observe({
            req(input$selected_workflow)
            create_est_lt_combined(all_data_finalized, input$selected_workflow, session = session)
        })
    })
}
