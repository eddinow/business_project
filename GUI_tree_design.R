#Initialize ------

rm(list = ls())
set.seed(1)

library(shiny)
library(shinyTree)
library(dplyr)
library(bslib)

# Import -----------------------------------------------------------------------

source("Arbeitsschritte_gebuendelt_als independent_var.R")
auftraege_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")


# Tidy -------------------------------------------------------------------------

# Transform --------------------------------------------------------------------

werke <- unique(auftraege_raw$Werk)
linien <- unique(auftraege_raw$Fertigungslinie)
planer <- unique(auftraege_raw$Planer)
material <- unique(auftraege_raw$Materialnummer)
workflows <- unique(auftraege_inkl_vorgangsfolgen$Vorgangsfolge)

# Model -----------

ui <- fluidPage(
    titlePanel("Einstiegsseite"),
    
    sidebarLayout(
        sidebarPanel(
            shinyTree("tree", search = TRUE)
        ),
        mainPanel(
            h4("Ausgewählter Knoten:"),
            verbatimTextOutput("selected_node")
        )
    )
)

server <- function(input, output, session) {
    
    output$tree <- renderTree({
        
        # Dynamische Werte aus auftraege_raw
        werke <- auftraege_raw%>% pull(Werk) %>% unique() %>% sort()
        linien <- auftraege_raw %>% pull(Fertigungslinie) %>% unique() %>% sort()
        planer <- auftraege_raw %>% pull(Planer) %>% unique() %>% sort()
        material <- auftraege_raw %>% pull(Materialnummer) %>% unique() %>% sort()
        workflows <- auftraege_inkl_vorgangsfolgen %>% pull(Vorgangsfolge) %>% unique() %>% sort()
        
        
        # Rückgabe als verschachtelte Liste
        list(
            "Werke" = as.list(setNames(rep("", length(werke)), werke)),
            "Linien" = as.list(setNames(rep("", length(linien)), linien)),
            "Planer" = as.list(setNames(rep("", length(planer)), planer)),
            "Material" = as.list(setNames(rep("", length(material)), material)),
            "Workflows" = as.list(setNames(rep("", length(workflows)), workflows))
            
        )
    })
    
    # Auswahl anzeigen
    output$selected_node <- renderPrint({
        get_selected(input$tree, format = "names")
    })
}


ui <- fluidPage(
    tags$style(HTML("
    .tile-button {
      width: 180px;
      height: 120px;
      font-size: 20px;
      margin: 10px;
      background-color: #f2f2f2;
      border: 2px solid #007cc0;
      border-radius: 10px;
      box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
      display: inline-block;
      vertical-align: top;
    }
    .tile-button:hover {
      background-color: #d9eefa;
    }
    .subtile {
      margin-left: 30px;
      font-size: 16px;
    }
  ")),
    
    titlePanel("Einstiegsseite"),
    
    fluidRow(
        actionButton("btn_werke", "Werke", class = "tile-button"),
        actionButton("btn_linien", "Linien", class = "tile-button"),
        actionButton("btn_planer", "Planer", class = "tile-button"),
        actionButton("btn_material", "Material", class = "tile-button"),
        actionButton("btn_workflows", "Workflows", class = "tile-button")
    ),
    
    uiOutput("subtiles"),
    verbatimTextOutput("selected_tile")
)

server <- function(input, output, session) {
    state <- reactiveValues(
        show_werke = FALSE,
        show_linien = FALSE,
        show_planer = FALSE,
        show_material = FALSE,
        show_workflows = FALSE,
        selected = NULL
    )
    
    observeEvent(input$btn_werke, { state$show_werke <- !state$show_werke })
    observeEvent(input$btn_linien, { state$show_linien <- !state$show_linien })
    observeEvent(input$btn_planer, { state$show_planer <- !state$show_planer })
    observeEvent(input$btn_material, { state$show_material <- !state$show_material })
    observeEvent(input$btn_workflows, { state$show_workflows <- !state$show_workflows })
    
    output$subtiles <- renderUI({
        subtile_ui <- tagList()
        
        if (state$show_werke) {
            subtile_ui <- tagAppendChildren(subtile_ui, lapply(werke, function(w) {
                actionLink(paste0("sel_", w), w, class = "subtile")
            }))
        }
        if (state$show_linien) {
            subtile_ui <- tagAppendChildren(subtile_ui, lapply(linien, function(w) {
                actionLink(paste0("sel_", w), w, class = "subtile")
            }))
        }
        if (state$show_planer) {
            subtile_ui <- tagAppendChildren(subtile_ui, lapply(planer, function(w) {
                actionLink(paste0("sel_", w), w, class = "subtile")
            }))
        }
        if (state$show_material) {
            subtile_ui <- tagAppendChildren(subtile_ui, lapply(material, function(w) {
                actionLink(paste0("sel_", w), w, class = "subtile")
            }))
        }
        if (state$show_workflows) {
            subtile_ui <- tagAppendChildren(subtile_ui, lapply(workflows, function(w) {
                actionLink(paste0("sel_", w), w, class = "subtile")
            }))
        }
        
        subtile_ui
    })
    
    observe({
        all_items <- c(werke, linien, planer, material, workflows)
        for (item in all_items) {
            local({
                item_local <- item
                observeEvent(input[[paste0("sel_", item_local)]], {
                    state$selected <- item_local
                })
            })
        }
    })
    
    output$selected_tile <- renderPrint({
        if (!is.null(state$selected)) {
            paste("Ausgewählt:", state$selected)
        } else {
            "Noch nichts ausgewählt."
        }
    })
}

shinyApp(ui, server)
