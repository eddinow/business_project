# Initialize -------------------------------------------------------------------
# rm(list = ls())
# set.seed(1)


# load packages 
library(shiny)
library(tidyverse)
library(readxl)

# Import -----------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Tidy -------------------------------------------------------------------------
# (all_data_finalized is assumed already in “tidy” format, with columns:
#   planer, werk, fertigungslinie, materialnummer, …)

# Transform --------------------------------------------------------------------
# (we’ll filter by planner reactively in the server)

# Model ------------------------------------------------------------------------
# (no statistical model needed for these plots)

# Visualize --------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Planner Detail View"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "planer_select",
                label   = "Select Planner:",
                choices = sort(unique(all_data_finalized$planer))
            )
        ),
        mainPanel(
            plotOutput("plot_werke"),
            plotOutput("plot_fertigungslinie"),
            plotOutput("plot_material")
        )
    )
)

server <- function(input, output, session) {
    
    # reactive subset for the selected planner
    df_planer <- reactive({
        req(input$planer_select)
        all_data_finalized %>%
            filter(planer == input$planer_select)
    })
    
    # 1. Werke nach Anzahl der Aufträge
    output$plot_werke <- renderPlot({
        df <- df_planer() %>%
            count(werk, name = "orders") %>%
            arrange(desc(orders)) %>%
            mutate(werk = factor(werk, levels = werk))
        
        ggplot(df, aes(x = werk, y = orders)) +
            geom_col() +
            labs(
                title = "Werke nach Anzahl der Aufträge",
                x     = "Werk",
                y     = "Anzahl der Aufträge"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
    
    # 2. Fertigungslinien nach Anzahl der Aufträge
    output$plot_fertigungslinie <- renderPlot({
        df <- df_planer() %>%
            count(fertigungslinie, name = "orders") %>%
            arrange(desc(orders)) %>%
            mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
        
        ggplot(df, aes(x = fertigungslinie, y = orders)) +
            geom_col() +
            labs(
                title = "Fertigungslinien nach Anzahl der Aufträge",
                x     = "Fertigungslinie",
                y     = "Anzahl der Aufträge"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
    
    # 3. Top 15 Materialien nach Anzahl der Aufträge
    output$plot_material <- renderPlot({
        df <- df_planer() %>%
            count(materialnummer, name = "orders") %>%
            slice_max(order_by = orders, n = 15) %>%
            arrange(desc(orders)) %>%
            mutate(materialnummer = factor(materialnummer, levels = materialnummer))
        
        ggplot(df, aes(x = materialnummer, y = orders)) +
            geom_col() +
            labs(
                title = "Top 15 Materialien nach Anzahl der Aufträge",
                x     = "Materialnummer",
                y     = "Anzahl der Aufträge"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1)
            )
    })
}

# Communicate ------------------------------------------------------------------
# Launch the Shiny app
shinyApp(ui = ui, server = server)