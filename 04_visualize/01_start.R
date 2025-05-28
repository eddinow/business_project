library(shiny)
library(shinydashboard)
library(DT)


source("02_model/create_start_kpis.R", local = TRUE)
source("04_visualize/02_planer.R", local = TRUE)
source("04_visualize/02_workflows.R", local = TRUE)  
source("04_visualize/02_fertigungslinien.R", local = TRUE)  
source("04_visualize/02_material.R", local = TRUE) 
source("04_visualize/02_werke.R", local = TRUE)  

# UI -------------------------------------------------------------------------
start_ui <- dashboardPage(
    dashboardHeader(title = "TrueTime"),
    dashboardSidebar(
        sidebarMenu(id = "main_tabs",
                    menuItem("Start", tabName = "start", icon = icon("home")),
                    menuItem("Workflows", tabName = "workflows", icon = icon("gears")),
                    menuItem("Material", tabName = "material", icon = icon("boxes")),
                    menuItem("Fertigungslinien", tabName = "linien", icon = icon("industry")),
                    menuItem("Werke", tabName = "werke", icon = icon("building")),
                    menuItem("Planer", tabName = "planer", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tags$head(
            
            tags$style(HTML(
                ".fancy-box {
          background-color: white;
          color: #002366;
          border: none;
          border-radius: 16px;
          box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.2);
          font-weight: bold;
          font-size: 2vw;
          padding: 20px;
          height: 15vw;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          cursor: pointer;
        }
        .fancy-box .value {
          font-size: 4vw;
        }
        .fancy-box .subtitle {
          font-size: 2.4vw;
        }
        .spacer {
          margin-bottom: 2vw;
        }
        .h3-fancy {
          font-size: 2.4vw;
          font-weight: bold;
          margin-top: 3vw;
          margin-bottom: 1.5vw;
        }
        .button_grid {
          display: flex;
          flex-wrap: wrap;
          gap: 2vw;
        }
        .button_grid .fancy-box {
          flex: 1 1 calc(33.333% - 2vw);
          max-width: calc(33.333% - 2vw);
        }
        @media (max-width: 1000px) {
          .button_grid .fancy-box {
            flex: 1 1 calc(50% - 2vw);
            max-width: calc(50% - 2vw);
          }
        }
        @media (max-width: 600px) {
          .button_grid .fancy-box {
            flex: 1 1 calc(100% - 2vw);
            max-width: calc(100% - 2vw);
          }
        }"
            ))
        ),
        tabItems(
            tabItem(tabName = "start",
                    h2("Behalte Deine Lead Times mit TrueTime im Auge.",
                       style = "font-size: 3vw; font-weight: bold;"),
                    div(class = "spacer"),
                    fluidRow(
                        column(width = 6,
                               div(class = "fancy-box",
                                   div(class = "value", avg_lt),
                                   div(class = "subtitle", "Current est. LT")
                               )
                        ),
                        column(width = 6,
                               div(class = "fancy-box",
                                   div(class = "value", paste0(avg_sl, "%")),
                                   div(class = "subtitle", "Servicelevel")
                               )
                        )
                    ),
                    h3("Kategorie wählen", class = "h3-fancy"),
                    div(class = "button_grid",
                        div(class = "fancy-box", onclick = "Shiny.setInputValue('go_tab', 'workflows')", "Workflows"),
                        div(class = "fancy-box", onclick = "Shiny.setInputValue('go_tab', 'material')", "Material"),
                        div(class = "fancy-box", onclick = "Shiny.setInputValue('go_tab', 'linien')", "Fertigungslinien"),
                        div(class = "fancy-box", onclick = "Shiny.setInputValue('go_tab', 'werke')", "Werke"),
                        div(class = "fancy-box", onclick = "Shiny.setInputValue('go_tab', 'planer')", "Planer")
                    )
            ),
            tabItem(tabName = "workflows", workflows_ui("workflows")),
            tabItem(tabName = "material", material_ui("material")),
            tabItem(tabName = "linien", linien_ui("linien")),
            tabItem(tabName = "werke", werke_ui("werke")),
            tabItem(tabName = "planer", planer_ui("planer"))
        )
    )
)

# Server-----------------------------------------------------------------

start_server <- function(input, output, session) {
    
    workflows_server("workflows")
    material_server("material")
    linien_server("linien")
    werke_server("werke")
    planer_server("planer")
    

    observeEvent(input$go_tab, {
        updateTabItems(session, "main_tabs", input$go_tab)
    })
}

shinyApp(start_ui, start_server)