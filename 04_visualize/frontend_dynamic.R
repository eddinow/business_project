# Initialize ------
rm(list = ls())
set.seed(1)

library(shiny)
library(shinyjs)
library(bslib)
library(shiny.fluent)

# UI --------
ui <- fluidPage(
    theme = bs_theme(   
        version = 5,
        primary = "#7bd88f",              
        bg = "#ffffff",
        fg = "#212529",
        base_font = font_google("Poppins")
    ),
    
    useShinyjs(),
    uiOutput("dynamic_ui"),
    
    tags$head(  # dein Custom-CSS bleibt weiterhin aktiv!
        tags$style(HTML("
      .kachel-button {
        display: inline-block;
        width: 150px;
        height: 120px;
        margin: 15px;
        background-color: #f9f9f9;
        border-radius: 20px;
        box-shadow: 0px 4px 10px rgba(0,0,0,0.1);
        text-align: center;
        vertical-align: middle;
        line-height: 120px;
        font-weight: bold;
        font-size: 16px;
        color: #444;
        cursor: pointer;
        transition: 0.2s;
      }

      .kachel-button:hover {
        background-color: #e6f0ff;
        color: #1a73e8;
        box-shadow: 0px 6px 12px rgba(0,0,0,0.15);
      }

      .kpi-box {
        display: inline-block;
        width: 160px;
        height: 100px;
        margin: 10px;
        padding: 10px;
        color: white;
        font-weight: bold;
        box-shadow: 0px 4px 8px rgba(0,0,0,0.1);
      }

      .kpi-red {
        background-color: #f75f5f;
      }

      .kpi-green {
        background-color: #7bd88f;
      }

      .kpi-value {
        font-size: 28px;
      }

      .kpi-label {
        font-size: 14px;
      }
    "))
    )
)

# Server --------
server <- function(input, output, session) {
    
    # 🛠 bslib Theme-Editor aktivieren (nur lokal sichtbar)
    bs_themer()  # ← Klickbares ⚙️-Symbol erscheint
    
    aktive_kategorie <- reactiveVal(NULL)
    
    uebersicht_ui <- navbarPage("Übersicht",
                                tabPanel("Planer", h3("Planer-Übersicht")),
                                tabPanel("Werke", h3("Werke-Übersicht")),
                                tabPanel("Fertigungslinien", h3("Fertigungslinien-Übersicht")),
                                tabPanel("Workflows", h3("Workflows-Übersicht")),
                                tabPanel("Material", h3("Material-Übersicht"))
    )
    
    output$dynamic_ui <- renderUI({
        if (is.null(aktive_kategorie())) {
            tagList(   # ✅ korrekt, bleibt innerhalb des äußeren fluidPage()
                h1("Menü"),
                fluidRow(
                    column(6,
                           div(class = "kpi-box kpi-red",
                               div(class = "kpi-value", "3"),
                               div(class = "kpi-label", "Lead Time aktuell")
                           )
                    ),
                    column(6,
                           div(class = "kpi-box kpi-green",
                               div(class = "kpi-value", "87%"),
                               div(class = "kpi-label", "Servicelevel")
                           )
                    )
                ),
                h3("Kategorie wählen"),
                fluidRow(
                    column(12,
                           div(class = "kachel-button", id = "go_planer", "Planer"),
                           div(class = "kachel-button", id = "go_werke", "Werke"),
                           div(class = "kachel-button", id = "go_fertigung", "Fertigungslinien"),
                           div(class = "kachel-button", id = "go_workflows", "Workflows"),
                           div(class = "kachel-button", id = "go_material", "Material")
                    )
                )
            )
        } else {
            uebersicht_ui
        }
    })
    
    observe({
        onclick("go_planer", function() { aktive_kategorie("planer") })
        onclick("go_werke", function() { aktive_kategorie("werke") })
        onclick("go_fertigung", function() { aktive_kategorie("fertigung") })
        onclick("go_workflows", function() { aktive_kategorie("workflows") })
        onclick("go_material", function() { aktive_kategorie("material") })
    })
}

# App starten
shinyApp(ui, server)