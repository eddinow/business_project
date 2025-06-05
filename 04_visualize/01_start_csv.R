# Start UI-Datei -------------------------------------------------------------
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
        tags$head(tags$style(HTML("...dein CSS bleibt unverändert..."))),
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
                    ),
                    br(), br(),
                    box(title = "Datenexport (CSV)", width = 12, status = "primary", solidHeader = TRUE,
                        selectInput("export_kategorie", "Kategorie wählen:", choices = c("Werke", "Planer", "Fertigungslinien", "Workflows")),
                        uiOutput("export_filter_1"),
                        uiOutput("export_filter_2"),
                        downloadButton("export_download", "Gefilterte Daten exportieren")
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

# Server -----------------------------------------------------------------------
start_server <- function(input, output, session) {
    
    workflows_server("workflows")
    material_server("material")
    linien_server("linien")
    werke_server("werke")
    planer_server("planer")
    
    observeEvent(input$go_tab, {
        updateTabItems(session, "main_tabs", input$go_tab)
    })
    
    # Dynamische Filter
    output$export_filter_1 <- renderUI({
        req(input$export_kategorie)
        switch(input$export_kategorie,
               "Werke" = selectInput("export_wert1", "Werk:", choices = unique(werke_overview$werk)),
               "Planer" = selectInput("export_wert1", "Planer:", choices = unique(planer_overview$planer)),
               "Fertigungslinien" = selectInput("export_wert1", "Linie:", choices = unique(linien_overview$fertigungslinie)),
               "Workflows" = selectInput("export_wert1", "Workflow:", choices = unique(workflows_overview$vorgangsfolge))
        )
    })
    
    output$export_filter_2 <- renderUI({
        req(input$export_kategorie)
        if (input$export_kategorie == "Fertigungslinien") {
            req(input$export_wert1)
            vorgaenge <- linien_overview %>%
                filter(fertigungslinie == input$export_wert1) %>%
                pull(vorgangsfolge) %>%
                unique()
            selectInput("export_wert2", "Vorgangsfolge:", choices = vorgaenge)
        } else {
            return(NULL)
        }
    })
    
    export_data <- reactive({
        req(input$export_kategorie, input$export_wert1)
        
        switch(input$export_kategorie,
               "Werke" = werke_overview %>% filter(werk == input$export_wert1),
               "Planer" = planer_overview %>% filter(planer == input$export_wert1),
               "Fertigungslinien" = {
                   df <- linien_overview %>% filter(fertigungslinie == input$export_wert1)
                   if (!is.null(input$export_wert2)) df <- df %>% filter(vorgangsfolge == input$export_wert2)
                   df
               },
               "Workflows" = workflows_overview %>% filter(vorgangsfolge == input$export_wert1)
        )
    })
    
    output$export_download <- downloadHandler(
        filename = function() {
            paste0("TrueTime_Export_", input$export_kategorie, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(export_data(), file, row.names = FALSE)
        }
    )
}

shinyApp(start_ui, start_server)
