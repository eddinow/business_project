library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

source("02_model/create_workflows_overview.R", local = TRUE)

start_ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      body {
        background-color: #f5f7fa;
        margin: 0;
        padding: 0;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .navbar {
        background-color: white;
        border-bottom: 1px solid #ddd;
        padding: 1rem;
        font-weight: bold;
        font-size: 18px;
      }
      .white-box {
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
        padding: 20px;
        margin-bottom: 20px;
        width: 100%;
      }
    "))
    ),
    
    # Menüleiste oben
    div(class = "navbar", "Meine App"),
    
    # Suchleiste mit Dropdown
    fluidRow(
        column(
            width = 12,
            div(
                class = "white-box",
                fluidRow(
                    column(
                        width = 4,
                        selectInput("region", "Region auswählen:", 
                                    choices = c("Deutschland", "Europa", "USA"), 
                                    selected = "Deutschland")
                    ),
                    column(
                        width = 8,
                        textInput("search", "Suchbegriff eingeben", placeholder = "z. B. 'Erdbeben Türkei Rhodos'")
                    )
                )
            )
        )
    ),
    
    # Vier weiße Boxen untereinander
    fluidRow(
        column(width = 12, div(class = "white-box", DTOutput("workflow_table"))),
        column(width = 12, div(class = "white-box", "Lead Times nach Material")),
        column(width = 12, div(class = "white-box", "Lead Times nach Linien")),
        column(width = 12, div(class = "white-box", "Lead Times nach Werken"))
    )
)

start_server <- function(input, output, session) {
    #Workflows
    output$workflow_table <- renderDT({
        datatable(workflows_overview, 
                  options = list(pageLength = 10, autoWidth = TRUE), 
                  rownames = FALSE)
    })
}

shinyApp(start_ui, start_server)

getwd()
