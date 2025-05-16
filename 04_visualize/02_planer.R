library(shiny)
library(argonDash)
library(DT)

source("02_model/create_planer_overview.R")


planer_ui <- argonDashPage(
    title = "Planer Übersicht",
    sidebar = argonDashSidebar(
        id = "planer_sidebar",
        vertical = TRUE,
        skin = "light",
        background = "white",
        size = "md",
        div(h4("Planer Dashboard", class = "text-primary text-center mt-3"))
    ),
    header = argonDashHeader(
        gradient = TRUE,
        color = "primary",
        separator = TRUE,
        bottom_border = TRUE
    ),
    body = argonDashBody(
        argonTabItems(
            argonTabItem(
                tabName = "planer",
                h1("Übersicht aller Planer", class = "mb-4"),
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Planer."),
                fluidRow(
                    column(12,
                           div(class = "card shadow-sm p-4 bg-white rounded",
                               h4("Planer KPIs", class = "mb-3"),
                               DTOutput("planer_table")
                           )
                    )
                )
            )
        )
    ),
    footer = argonDashFooter(
        copyright = "Made by Julia"
    )
)

planer_server <- function(input, output, session) {
    if (exists("planer_overview")) {
        output$planer_table <- renderDT({
            datatable(planer_overview,
                      options = list(
                          pageLength = 10,
                          autoWidth = TRUE,
                          dom = 'tip',
                          scrollX = TRUE
                      ),
                      rownames = FALSE,
                      class = "stripe hover cell-border")
        })
    } else {
        output$planer_table <- renderDT({
            datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
        })
    }
}

shinyApp(planer_ui, planer_server)
