#Initialize ------

rm(list = ls())
set.seed(1)

library(shiny)
library(shinyTree)
library(dplyr)

# Import -----------------------------------------------------------------------

auftraege_raw <- read_excel("2025-04-08_Auftragsköpfe SAP.xlsx")
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")

# Tidy -------------------------------------------------------------------------

# Transform --------------------------------------------------------------------

werke <- unique(auftraege_raw$Werk)
linien <- unique(auftraege_raw$Fertigungslinie)
planer <- unique(auftraege_raw$Planer)

# Model -----------

ui <- fluidPage(
    titlePanel("Navigation – Werke, Linien, Planer"),
    
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
        werke <- auftraege_raw %>% pull(Werk) %>% unique() %>% sort()
        linien <- auftraege_raw %>% pull(Fertigungslinie) %>% unique() %>% sort()
        planer <- auftraege_raw %>% pull(Planer) %>% unique() %>% sort()
        
        # Rückgabe als verschachtelte Liste
        list(
            "Werke" = as.list(setNames(rep("", length(werke)), werke)),
            "Linien" = as.list(setNames(rep("", length(linien)), linien)),
            "Planer" = as.list(setNames(rep("", length(planer)), planer)),
            "Material Flow" = list()
        )
    })
    
    # Auswahl anzeigen
    output$selected_node <- renderPrint({
        get_selected(input$tree, format = "names")
    })
}

shinyApp(ui, server)