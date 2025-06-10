library(shiny)
library(shinydashboard)
library(DT)
library(writexl)
library(dplyr)

# --- Daten laden ---
source("02_model/create_start_kpis.R", local = TRUE)
source("02_model/create_planer_overview.R", local = TRUE)
source("02_model/create_workflows_overview.R", local = TRUE)
source("02_model/create_material_overview.R", local = TRUE)
source("02_model/create_werke_overview.R", local = TRUE)
source("02_model/create_fertigungslinien_overview.R", local = TRUE)

source("04_visualize/02_planer.R", local = TRUE)
source("04_visualize/02_workflows.R", local = TRUE)
source("04_visualize/02_fertigungslinien.R", local = TRUE)
source("04_visualize/02_material.R", local = TRUE)
source("04_visualize/02_werke.R", local = TRUE)

# --- UI ---
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
        tabItems(
            tabItem(tabName = "start",
                    h2("Behalte Deine Lead Times mit TrueTime im Auge.",
                       style = "font-size: 3vw; font-weight: bold;"),
                    br(), br(),
                    box(title = "Datenexport (CSV)", width = 12, status = "primary", solidHeader = TRUE,
                        selectInput("export_kategorie", "Kategorie wählen:",
                                    choices = c("Workflows", "Planer", "Fertigungslinien", "Werke", "Material")),
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

# --- SERVER ---
start_server <- function(input, output, session) {
    
    workflows_server("workflows")
    material_server("material")
    linien_server("linien")
    werke_server("werke")
    planer_server("planer")
    
    observeEvent(input$go_tab, {
        updateTabItems(session, "main_tabs", input$go_tab)
    })
    
    # 1. Hauptfilter
    output$export_filter_1 <- renderUI({
        req(input$export_kategorie)
        
        auswahl <- switch(input$export_kategorie,
                          "Workflows"         = unique(workflows_overview$Workflow),
                          "Planer"            = unique(planer_overview$Planer),
                          "Fertigungslinien"  = unique(linien_overview$fertigungslinie),
                          "Werke"             = unique(werke_overview$werk),
                          "Material"          = unique(materialnummer_overview$ABC_Klasse),  # ← zuerst ABC
                          character(0)
        )
        
        selectInput("export_wert1",
                    paste("Wert aus", input$export_kategorie),
                    choices = sort(auswahl))
    })
    
    # 2. Zweiter Filter (Material umgedreht!)
    output$export_filter_2 <- renderUI({
        req(input$export_kategorie, input$export_wert1)
        
        if (input$export_kategorie == "Fertigungslinien") {
            vals <- linien_overview %>%
                filter(fertigungslinie == input$export_wert1) %>%
                pull(vorgangsfolge) %>% unique()
            if (length(vals) > 1) {
                selectInput("export_wert2", "Vorgangsfolge:", choices = sort(vals))
            }
            
        } else if (input$export_kategorie == "Werke") {
            vals <- werke_overview %>%
                filter(werk == input$export_wert1) %>%
                pull(vorgangsfolge) %>% unique()
            if (length(vals) > 1) {
                selectInput("export_wert2", "Vorgangsfolge:", choices = sort(vals))
            }
            
        } else if (input$export_kategorie == "Material") {
            vals <- materialnummer_overview %>%
                filter(ABC_Klasse == input$export_wert1) %>%
                pull(materialnummer) %>% unique()  # ← jetzt nach ABC filtern → Materialnummer
            if (length(vals) > 1) {
                selectInput("export_wert2", "Materialnummer:", choices = sort(vals))
            }
            
        } else {
            return(NULL)
        }
    })
    
    # 3. Exportdaten
    export_data <- reactive({
        req(input$export_kategorie, input$export_wert1)
        
        df <- switch(input$export_kategorie,
                     "Workflows"         = workflows_overview,
                     "Planer"            = planer_overview,
                     "Fertigungslinien"  = linien_overview,
                     "Werke"             = werke_overview,
                     "Material"          = materialnummer_overview,
                     data.frame()
        )
        
        # neue Reihenfolge für Material!
        col_map <- list(
            "Workflows"         = "Workflow",
            "Planer"            = "Planer",
            "Fertigungslinien"  = "fertigungslinie",
            "Werke"             = "werk",
            "Material"          = "ABC_Klasse"
        )
        
        spalte <- col_map[[input$export_kategorie]]
        df <- df[df[[spalte]] == input$export_wert1, , drop = FALSE]
        
        # Zweiter Filter
        if (input$export_kategorie %in% c("Fertigungslinien", "Werke", "Material") &&
            !is.null(input$export_wert2)) {
            
            df <- switch(input$export_kategorie,
                         "Fertigungslinien"  = df[df$vorgangsfolge == input$export_wert2, ],
                         "Werke"             = df[df$vorgangsfolge == input$export_wert2, ],
                         "Material"          = df[df$materialnummer == input$export_wert2, ],
                         df
            )
        }
        
        df
    })
    
    # 4. DownloadHandler
    output$export_download <- downloadHandler(
        filename = function() {
            paste0("TrueTime_Export_", input$export_kategorie, "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(export_data(), path = file)
        }
    )
}

# START
shinyApp(start_ui, start_server)
