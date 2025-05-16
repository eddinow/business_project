# Das ist die Benutzeroberfläche für die Einstiegsseite. Der User kann mit einem
# schnellen Blick die median abweichung insgesamt und den servicelevel als anteil aller
# aufträge mit abweichung >= 0 (heißt entweder zu früh oder jit fertig)

# Initialize ------
rm(list = ls())
set.seed(1)

library(shiny)
library(argonDash)

source("02_model/create_start_kpis.R")
source("04_visualize/02_fertigungslinien.R")
source("04_visualize/02_planer.R")
source("04_visualize/02_werke.R")
source("04_visualize/02_workflows.R")

planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h2("Planer-Detailseite"),
        verbatimTextOutput(ns("text"))
    )
}

planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        source("04_visualize/02_planer.R", local = TRUE)
    })
}

werke_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h2("Werke-Detailseite"),
        verbatimTextOutput(ns("text"))
    )
}

werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        source("04_visualize/02_werke.R", local = TRUE)
    })
}

fertigung_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h2("Fertigungslinien-Detailseite"),
        verbatimTextOutput(ns("text"))
    )
}

fertigung_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        source("04_visualize/02_fertigungslinien.R", local = TRUE)
    })
}

workflows_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h2("Workflows-Detailseite"),
        verbatimTextOutput(ns("text"))
    )
}

workflows_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        source("04_visualize/02_workflows.R", local = TRUE)
    })
}

ui <- argonDashPage(
    title = "Dein Dashboard",
    sidebar = argonDashSidebar(
        id = "sidebar",
        vertical = TRUE,
        skin = "light",
        background = "white",
        size = "md",
        
        # Branding oben
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
                planer_ui("planer")
            ),
            
            argonTabItem(
                tabName = "werke",
                werke_ui("werke")
            ),
            
            argonTabItem(
                tabName = "fertigung",
                fertigung_ui("fertigung")
            ),
            
            argonTabItem(
                tabName = "workflows",
                workflows_ui("workflows")
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
    # Navigation über Buttons aktivieren
    observeEvent(input$go_planer, {
        updateTabItems(session, "sidebar", "planer")
    })
    observeEvent(input$go_werke, {
        updateTabItems(session, "sidebar", "werke")
    })
    observeEvent(input$go_fertigung, {
        updateTabItems(session, "sidebar", "fertigung")
    })
    observeEvent(input$go_workflows, {
        updateTabItems(session, "sidebar", "workflows")
    })
    observeEvent(input$go_material, {
        updateTabItems(session, "sidebar", "material")
    })
    # KPI-Daten berechnen
    if (exists("all_data_finalized")) {
        start_kpis <- data.frame(
            Avg LT = median(all_data_finalized$lead_time_ist, na.rm = TRUE),
            Avg Servicelevel = round(mean(all_data_finalized$abweichung >= 0, na.rm = TRUE) * 100, 0)
        )
    } else {
        start_kpis <- data.frame(Avg LT = NA, Avg Servicelevel = NA)
    }
    
    output$avg_lt <- renderText({ as.character(start_kpis$Avg LT) })
    output$avg_servicelevel <- renderText({ paste0(start_kpis$Avg Servicelevel, "%") })
    
    # Module Server-Funktionen aufrufen
    planer_server("planer")
    werke_server("werke")
    fertigung_server("fertigung")
    workflows_server("workflows")
    
    output$text_material <- renderText({ "Hier könnten deine Material-Daten stehen." })
}