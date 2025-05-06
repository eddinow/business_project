#Initialize ------

rm(list = ls())
set.seed(1)

library(shiny)
library(shinyTree)
library(dplyr)

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

shinyApp(ui, server)