# Initialize -------------------------------------------------------------------
# rm(list = ls())
# set.seed(1)


# load packages 
library(shiny)
library(tidyverse)
library(readxl)
library(plotly)

# Import -----------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# UI --------------------------------------------------------------------------
ui <- fluidPage(
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

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
    
    # reactive subset for the selected planner
    df_planer <- reactive({
        req(input$planer_select)
        all_data_finalized %>%
            filter(planer == input$planer_select)
    })
    
    # 1. Werke nach Anzahl der Aufträge (interactive)
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
    
    # 2. Fertigungslinien nach Anzahl der Aufträge (interactive)
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
    
    # 3. Top 15 Materialien nach Anzahl der Aufträge (interactive)
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
}

# Communicate ------------------------------------------------------------------
shinyApp(ui = ui, server = server)