# Das ist die Benutzeroberfläche für die Einstiegsseite. Der User kann mit einem
# schnellen Blick die median abweichung insgesamt und den servicelevel als anteil aller
# aufträge mit abweichung >= 0 (heißt entweder zu früh oder jit fertig)

# Initialize ------
rm(list = ls())
set.seed(1)

library(shiny)
library(argonDash)

source("02_model/create_start_kpis.R")

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
                    column(6, div(
                        class = "card shadow-sm p-3 mb-3 bg-white rounded",
                        h5("Lead Time aktuell"),
                        icon("clock", class = "fa-2x mb-2"),
                        div(style = "font-size: 32px; font-weight: bold; color: #1b1e23;", shiny::textOutput("avg_lt"))
                    )),
                    column(6, div(
                        class = "card shadow-sm p-3 mb-3 bg-white rounded",
                        h5("Servicelevel"),
                        icon("check-circle", class = "fa-2x mb-2"),
                        div(style = "font-size: 32px; font-weight: bold; color: #1b1e23;", shiny::textOutput("avg_servicelevel"))
                    ))
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
    
    # KPI-Daten berechnen (Dummy für Demonstration)
    if (exists("all_data_finalized")) {
        start_kpis <- data.frame(
            `Avg LT` = median(all_data_finalized$lead_time_ist, na.rm = TRUE),
            `Avg Servicelevel` = round(mean(all_data_finalized$abweichung >= 0, na.rm = TRUE) * 100, 0)
        )
    } else {
        start_kpis <- data.frame(`Avg LT` = NA, `Avg Servicelevel` = NA)
    }
    
    output$avg_lt <- renderText({ as.character(start_kpis$`Avg LT`) })
    output$avg_servicelevel <- renderText({ paste0(start_kpis$`Avg Servicelevel`, "%") })
    
    output$text_planer <- renderText({ "Hier könnten deine Planer-Daten stehen." })
    output$text_werke <- renderText({ "Hier könnten deine Werke-Daten stehen." })
    output$text_fertigung <- renderText({ "Hier könnten deine Fertigungslinien-Daten stehen." })
    output$text_workflows <- renderText({ "Hier könnten deine Workflows-Daten stehen." })
    output$text_material <- renderText({ "Hier könnten deine Material-Daten stehen." })
}

shinyApp(ui, server)