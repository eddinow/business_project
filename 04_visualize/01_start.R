# Dummy-Dashboard mit shinydashboard (Start & Planer) ----------------------------------

library(shiny)
library(shinydashboard)
library(DT)


source("02_model/create_start_kpis.R", local = TRUE)
source("04_visualize/02_planer.R", local = TRUE)
print(avg_lt)

# UI -------------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "TrueTime"),
    dashboardSidebar(
        sidebarMenu(id = "main_tabs",
                    menuItem("Start", tabName = "start", icon = icon("home")),
                    menuItem("Planer", tabName = "planer", icon = icon("chart-bar"))
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
          align-items: flex-start;
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
                    actionButton("go_planer", "Planer",
                                 style = "
                              background-color: white;
                              color: #002366;
                              border: none;
                              border-radius: 16px;
                              box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.2);
                              font-weight: bold;
                              width: 20vw;
                              height: 20vw;
                              font-size: 2.4vw;
                              display: flex;
                              align-items: flex-end;
                              justify-content: flex-start;
                              padding: 0 0 10px 10px;
                            "
                    )
            ),
            tabItem(tabName = "planer",
                    planer_ui("planer")
            )
        )
    )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
    observeEvent(input$go_planer, {
        updateTabItems(session, inputId = "main_tabs", selected = "planer")
    })
    
    planer_server("planer")
}

# Start App ------------------------------------------------------------------

shinyApp(ui, server)
