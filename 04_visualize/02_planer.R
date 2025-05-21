library(shiny)
library(DT)

source("02_model/create_planer_overview.R", local = TRUE)
source("02_model/kpis_planer.R")

# UI-Modul-Funktion
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(title = "Übersicht aller Planer", width = 12, status = "primary", solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Planer."),
                DTOutput(ns("planer_table"))
            ),
            titlePanel("Planner Detail View (Interactive)"),
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "planer_select",
                        label   = "Select Planner:",
                        choices = sort(unique(all_data_finalized$planer))
                    )
                ),
                mainPanel(
                    plotlyOutput("plot_werke"),
                    plotlyOutput("plot_fertigungslinie"),
                    plotlyOutput("plot_material")
                )
            )            
        )
    )
}

# Server-Modul-Funktion
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # ————— Bestehende Tabelle (unverändert) —————
        if (exists("planer_overview")) {
            output$planer_table <- renderDT({
                datatable(
                    planer_overview,
                    options = list(
                        pageLength = 10,
                        autoWidth  = TRUE,
                        dom        = 'tip',
                        scrollX    = TRUE
                    ),
                    rownames = FALSE,
                    class = "stripe hover cell-border"
                )
            })
        } else {
            output$planer_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
        
        # ————— Neue interaktive Plots —————
        
        # 1) Reactive Subset für den ausgewählten Planer
        df_planer <- reactive({
            req(input$planer_select)
            all_data_finalized %>%
                filter(planer == input$planer_select)
        })
        
        # 2) Werke nach Anzahl der Aufträge
        output$plot_werke <- renderPlotly({
            df <- df_planer() %>%
                count(werk, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(werk = factor(werk, levels = werk))
            
            p <- ggplot(df, aes(
                x = werk,
                y = orders,
                text = paste0("Anzahl der Aufträge: ", orders)
            )) +
                geom_col() +
                labs(
                    title = "Werke nach Anzahl der Aufträge",
                    x     = "Werk",
                    y     = "Anzahl der Aufträge"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # 3) Fertigungslinien nach Anzahl der Aufträge
        output$plot_fertigungslinie <- renderPlotly({
            df <- df_planer() %>%
                count(fertigungslinie, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
            
            p <- ggplot(df, aes(
                x = fertigungslinie,
                y = orders,
                text = paste0("Anzahl der Aufträge: ", orders)
            )) +
                geom_col() +
                labs(
                    title = "Fertigungslinien nach Anzahl der Aufträge",
                    x     = "Fertigungslinie",
                    y     = "Anzahl der Aufträge"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # 4) Top 15 Materialien nach Anzahl der Aufträge
        output$plot_material <- renderPlotly({
            df <- df_planer() %>%
                count(materialnummer, name = "orders") %>%
                slice_max(order_by = orders, n = 15) %>%
                arrange(desc(orders)) %>%
                mutate(materialnummer = factor(materialnummer, levels = materialnummer))
            
            p <- ggplot(df, aes(
                x = materialnummer,
                y = orders,
                text = paste0("Anzahl der Aufträge: ", orders)
            )) +
                geom_col() +
                labs(
                    title = "Top 15 Materialien nach Anzahl der Aufträge",
                    x     = "Materialnummer",
                    y     = "Anzahl der Aufträge"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
    })
}