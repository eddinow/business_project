# Das ist die Benutzeroberfläche für die Einstiegsseite. Der User kann mit einem
# schnellen Blick die avg abweichung insgesamt und den servicelevel als anteil aller
# aufträge mit abweichung >= 0 (heißt entweder zu früh oder jit fertig)

# Initialize ------
rm(list = ls())
set.seed(1)

library(shiny)
library(argonDash)

ui <- argonDashPage(
    title = "Dein Dashboard",
    sidebar = argonDashSidebar(
        id = "sidebar",
        vertical = TRUE,
        skin = "light",
        background = "white",
        size = "md",
        
        # Branding oben (als Text ohne argonRow)
        div(h3("Julia's App", class = "text-primary text-center mt-3")),
        
        argonSidebarMenu(
            argonSidebarItem(tabName = "start", icon = icon("home"), "Start"),
            argonSidebarItem(tabName = "planer", icon = icon("chart-bar"), "Planer"),
            argonSidebarItem(tabName = "werke", icon = icon("industry"), "Werke"),
            argonSidebarItem(tabName = "fertigung", icon = icon("cogs"), "Fertigungslinien"),
            argonSidebarItem(tabName = "workflows", icon = icon("project-diagram"), "Workflows"),
            argonSidebarItem(tabName = "material", icon = icon("boxes"), "Material")
        )
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
                tabName = "start",
                h1("Willkommen im Dashboard"),
                fluidRow(
                    argonInfoCard(title = "Lead Time aktuell", value = "3", icon = icon("clock")),
                    argonInfoCard(title = "Servicelevel", value = "87%", icon = icon("check-circle"))
                ),
                h3("Kategorie wählen"),
                fluidRow(
                    column(3, actionButton("go_planer", "Planer", class = "btn btn-primary btn-block")),
                    column(3, actionButton("go_werke", "Werke", class = "btn btn-primary btn-block")),
                    column(3, actionButton("go_fertigung", "Fertigungslinien", class = "btn btn-primary btn-block")),
                    column(3, actionButton("go_workflows", "Workflows", class = "btn btn-primary btn-block")),
                    column(3, actionButton("go_material", "Material", class = "btn btn-primary btn-block"))
                )
            ),
            
            argonTabItem(
                tabName = "planer",
                h2("Planer-Detailseite"),
                verbatimTextOutput("text_planer")
            ),
            
            argonTabItem(
                tabName = "werke",
                h2("Werke-Detailseite"),
                verbatimTextOutput("text_werke")
            ),
            
            argonTabItem(
                tabName = "fertigung",
                h2("Fertigungslinien-Detailseite"),
                verbatimTextOutput("text_fertigung")
            ),
            
            argonTabItem(
                tabName = "workflows",
                h2("Workflows-Detailseite"),
                verbatimTextOutput("text_workflows")
            ),
            
            argonTabItem(
                tabName = "material",
                h2("Material-Detailseite"),
                verbatimTextOutput("text_material")
            )
        )
    ),
    
    footer = argonDashFooter(copyright = "Made by Julia")
)

server <- function(input, output, session) {
    output$text_planer <- renderText({ "Hier könnten deine Planer-Daten stehen." })
    output$text_werke <- renderText({ "Hier könnten deine Werke-Daten stehen." })
    output$text_fertigung <- renderText({ "Hier könnten deine Fertigungslinien-Daten stehen." })
    output$text_workflows <- renderText({ "Hier könnten deine Workflows-Daten stehen." })
    output$text_material <- renderText({ "Hier könnten deine Material-Daten stehen." })
}

shinyApp(ui, server)