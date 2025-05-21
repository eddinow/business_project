# Packages --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

source("02_model/create_planer_overview.R", local = TRUE)
source("02_model/kpis_planer.R")

# UI-Modul --------------------------------------------------------------------
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Übersichtstabelle
        fluidRow(
            box(
                title       = "Übersicht aller Planer",
                width       = 12,
                status      = "primary",
                solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Planer."),
                DTOutput(ns("planer_table"))
            )
        ),
        # Planner‐Selector
        fluidRow(
            box(
                title = "Select Planner",
                width = 12,
                selectInput(
                    inputId = ns("planer_select"),
                    label   = NULL,
                    choices = sort(unique(all_data_finalized$planer)),
                    selected = NULL
                )
            )
        ),
        # Drei interaktive Plots
        fluidRow(
            box(
                title        = "Werke nach Anzahl der Aufträge",
                width        = 4,
                plotlyOutput(ns("plot_werke"))
            ),
            box(
                title        = "Fertigungslinien nach Anzahl der Aufträge",
                width        = 4,
                plotlyOutput(ns("plot_fertigungslinie"))
            ),
            box(
                title        = "Top 15 Materialien nach Anzahl der Aufträge",
                width        = 4,
                plotlyOutput(ns("plot_material"))
            )
        )
    )
}

# Server-Modul ----------------------------------------------------------------
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # ——— Tabelle (unverändert) ———
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
                    class    = "stripe hover cell-border"
                )
            })
        } else {
            output$planer_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
        
        # ——— Reactive Subset für den ausgewählten Planer ———
        df_planer <- reactive({
            req(input$planer_select)
            all_data_finalized %>%
                filter(planer == input$planer_select)
        })
        
        # ——— Plot 1: Werke nach Anzahl der Aufträge ———
        output$plot_werke <- renderPlotly({
            df <- df_planer() %>%
                count(werk, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(werk = factor(werk, levels = werk))
            
            p <- ggplot(df, aes(x = werk, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Werk", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # ——— Plot 2: Fertigungslinien nach Anzahl der Aufträge ———
        output$plot_fertigungslinie <- renderPlotly({
            df <- df_planer() %>%
                count(fertigungslinie, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
            
            p <- ggplot(df, aes(x = fertigungslinie, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Fertigungslinie", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # ——— Plot 3: Top 15 Materialien nach Anzahl der Aufträge ———
        output$plot_material <- renderPlotly({
            df <- df_planer() %>%
                count(materialnummer, name = "orders") %>%
                slice_max(order_by = orders, n = 15) %>%
                arrange(desc(orders)) %>%
                mutate(materialnummer = factor(materialnummer, levels = materialnummer))
            
            p <- ggplot(df, aes(x = materialnummer, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Materialnummer", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
    })
}

# Haupt‐UI und Server ----------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "TrueTime"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Start",        tabName = "start", icon = icon("home")),
            menuItem("Workflows",    tabName = "workflows", icon = icon("tasks")),
            menuItem("Material",     tabName = "material", icon = icon("boxes")),
            menuItem("Fertigungs­linien", tabName = "linien", icon = icon("industry")),
            menuItem("Werke",        tabName = "werke", icon = icon("building")),
            menuItem("Planer",       tabName = "planer", icon = icon("user"))
        )
    ),
    dashboardBody(
        tabItems(
            # … andere Tabs …
            tabItem(
                tabName = "planer",
                planer_ui("planer")
            )
        )
    )
)

server <- function(input, output, session) {
    planer_server("planer")
}

# App starten -----------------------------------------------------------------
shinyApp(ui, server)