# Das ist die Benutzeroberfläche für die Einstiegsseite. Der User kann mit einem
# schnellen Blick die median abweichung insgesamt und den servicelevel als anteil aller
# aufträge mit abweichung >= 0 (heißt entweder zu früh oder jit fertig)

# Initialize ------
# rm(list = ls())
# set.seed(1)

library(shiny)
library(argonDash)

source("02_model/create_start_kpis.R", echo=FALSE)
source("04_visualize/02_fertigungslinien.R", echo=FALSE)
source("04_visualize/02_planer.R", echo=FALSE)
source("04_visualize/02_werke.R", echo=FALSE)
source("04_visualize/02_workflows.R", echo=FALSE)

dummy_ui <- function(id, title) {
    ns <- NS(id)
    tagList(
        h2(title),
        verbatimTextOutput(ns("text"))
    )
}

# Dummy-Server-Modul
dummy_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$text <- renderText({ paste("Dummy-Inhalt für", id) })
    })
}

# UI -------------------------------------------------------------------------

ui <- argonDashPage(
    title = "Dummy-Dashboard",
    sidebar = argonDashSidebar(
        id = "sidebar",
        vertical = TRUE,
        skin = "light",
        background = "white",
        size = "md",
        
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
                        div(style = "font-size: 32px; font-weight: bold; color: #1b1e23;",
                            shiny::textOutput("avg_lt"))
                    )),
                    column(6, div(
                        class = "card shadow-sm p-3 mb-3 bg-white rounded",
                        h5("Servicelevel"),
                        icon("check-circle", class = "fa-2x mb-2"),
                        div(style = "font-size: 32px; font-weight: bold; color: #1b1e23;",
                            shiny::textOutput("avg_servicelevel"))
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
            
            argonTabItem(tabName = "planer", dummy_ui("planer", "Planer-Detailseite")),
            argonTabItem(tabName = "werke", dummy_ui("werke", "Werke-Detailseite")),
            argonTabItem(tabName = "fertigung", dummy_ui("fertigung", "Fertigungslinien-Detailseite")),
            argonTabItem(tabName = "workflows", dummy_ui("workflows", "Workflows-Detailseite")),
            argonTabItem(
                tabName = "material",
                h2("Material-Detailseite"),
                verbatimTextOutput("text_material")
            )
        )
    ),
    
    footer = argonDashFooter(
        copyright = "Made by Julia"
    )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
    tryCatch({
        source("02_model/create_start_kpis.R", local = TRUE)
    }, error = function(e) {
        avg_lt <<- NA
        avg_sl <<- NA
        message("Fehler beim Laden von KPIs: ", e$message)
    })
    
    output$avg_lt <- renderText({ as.character(avg_lt) })
    output$avg_servicelevel <- renderText({ paste0(avg_sl, "%") })
    
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
    
    dummy_server("planer")
    dummy_server("werke")
    dummy_server("fertigung")
    dummy_server("workflows")
    
    output$text_material <- renderText({ "Dummy-Inhalt für Material" })
}

# Start App ------------------------------------------------------------------

shinyApp(ui, server)
