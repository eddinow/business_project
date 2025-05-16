library(shiny)
library(argonDash)
library(DT)

source("02_model/create_workflows_overview.R")


ui <- argonDashPage(
    title = "Übersicht Workflows",
    sidebar = argonDashSidebar(
        id = "workflows_sidebar",
        vertical = TRUE,
        skin = "light",
        background = "white",
        size = "md",
        div(h4("Workflows Dashboard", class = "text-primary text-center mt-3"))
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
                tabName = "vorgangsfolge",
                h1("Übersicht aller Workflows", class = "mb-4"),
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Werk."),
                fluidRow(
                    column(12,
                           div(class = "card shadow-sm p-4 bg-white rounded",
                               h4("Workflows KPIs", class = "mb-3"),
                               DTOutput("workflows_table")
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

server <- function(input, output, session) {
    if (exists("workflows_overview")) {
        output$workflows_table <- renderDT({
            datatable(workflows_overview,
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
        output$workflows_table <- renderDT({
            datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
        })
    }
}

shinyApp(ui, server)
