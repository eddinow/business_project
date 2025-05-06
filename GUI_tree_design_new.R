# Initialize ------
rm(list = ls())
set.seed(1)

library(shiny)
library(shinyTree)
library(dplyr)
library(readxl)

# Import -----------------------------------------------------------------------
# Achtung: Stelle sicher, dass abc_material_linien vorher erstellt oder gesourced wurde!
source("Fertigungslinie_als independent_var.R")

auftraege_raw <- read_excel("auftragskoepfe_sap_raw.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Einstiegsseite"),
    
    sidebarLayout(
        sidebarPanel(
            shinyTree("tree", search = TRUE)
        ),
        mainPanel(
            h4("Ausgewählter Knoten:"),
            verbatimTextOutput("selected_node"),
            h4("A-Materialien für die Linie:"),
            textOutput("materialien_info")
        )
    )
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
    
    output$tree <- renderTree({
        werke <- auftraege_raw %>% pull(Werk) %>% unique() %>% sort()
        linien <- auftraege_raw %>% pull(Fertigungslinie) %>% unique() %>% sort()
        planer <- auftraege_raw %>% pull(Planer) %>% unique() %>% sort()
        material <- auftraege_raw %>% pull(Materialnummer) %>% unique() %>% sort()
        
        list(
            "Werke" = as.list(setNames(rep("", length(werke)), werke)),
            "Linien" = as.list(setNames(rep("", length(linien)), linien)),
            "Planer" = as.list(setNames(rep("", length(planer)), planer)),
            "Material" = as.list(setNames(rep("", length(material)), material))
        )
    })
    
    output$selected_node <- renderPrint({
        get_selected(input$tree, format = "names")
    })
    
    output$materialien_info <- renderText({
        selected <- get_selected(input$tree, format = "names")
        
        if (length(selected) == 1) {
            linie <- selected[[1]]
            if (exists("abc_material_linien") && linie %in% abc_material_linien$Fertigungslinie) {
                mat <- abc_material_linien %>%
                    filter(Fertigungslinie == linie) %>%
                    pull(A_materialien)
                
                if (length(mat) > 0 && !is.na(mat)) {
                    return(paste("A-Materialien für Linie", linie, ":\n", mat))
                } else {
                    return(paste("Keine A-Materialien für Linie", linie))
                }
            }
        }
        return("Bitte eine Fertigungslinie auswählen.")
    })
}

# Run App ----------------------------------------------------------------------
shinyApp(ui, server)