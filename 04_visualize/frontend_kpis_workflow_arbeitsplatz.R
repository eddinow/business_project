library(shiny)
library(shinydashboard)
library(plotly)

source("kpis_workflow_arbeitsplatz.R")


# Wir erstellen ein Panel mit dem Workflow der die höchste Liegezeit hat. Klickt
# man drauf kommt man zu Säulendiagramm für einzelne Workflows

ui <- dashboardPage(
    dashboardHeader(title = "Produktions-Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Übersicht", tabName = "overview", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                        valueBoxOutput("gesamtDauerBox"),
                        valueBoxOutput("durchschnittLiegezeitBox"),
                        valueBoxOutput("anzahlAuftraegeBox")
                    ),
                    fluidRow(
                        box(title = "Zeitaufteilung", status = "primary", solidHeader = TRUE,
                            plotlyOutput("stackedPlot"), width = 12)
                    )
            )
        )
    )
)

server <- function(input, output) {
    
    output$gesamtDauerBox <- renderValueBox({
        anteil_0 <- arbeitsplatz_abweichung %>%
            filter(kategorie == "Median = 0") %>%
            pull(anteil_prozent)
        
        valueBox(
            value = paste0(anteil_0, " %"),
            subtitle = "Arbeitsplätze mit Median = 0",
            icon = icon("percentage"),
            color = "blue"
        )
    })
    
    output$durchschnittLiegezeitBox <- renderValueBox({
        max_row <- vorgaenge_lz_bz %>%
            filter(!is.na(Liegedauer_gesamt_median)) %>%
            arrange(desc(Liegedauer_gesamt_median)) %>%
            slice(1)
        
        valueBox(
            value = paste0(max_row$Liegedauer_gesamt_median, " Tage"),
            subtitle = paste("Längste Liegezeit:", max_row$vorgangsfolge),
            icon = icon("pause"),
            color = "red"
        )
    })
    
    output$anzahlAuftraegeBox <- renderValueBox({
        valueBox(
            value = nrow(vorgaenge_lz_bz),
            subtitle = "Anzahl der Aufträge",
            icon = icon("tasks"),
            color = "green"
        )
    })
    
    # Dein bestehender renderPlotly-Code für das gestapelte Balkendiagramm
    output$stackedPlot <- renderPlotly({
        # ... dein bestehender Code ...
    })
}

shinyApp(ui, server)