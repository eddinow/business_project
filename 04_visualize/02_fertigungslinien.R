library(shiny)
library(DT)
library(ggplot2)
library(plotly)

# Lade KPI-Tabelle
source("02_model/kpis_linie.R", local = TRUE)

linien_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Fertigungslinie auswählen",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                    ns("linie_select"),
                    "Fertigungslinie:",
                    choices = sort(unique(linien_overview$fertigungslinie)),
                    selected = sort(unique(linien_overview$fertigungslinie))[1]
                )
            )
        ),
        fluidRow(
            box(
                title = "KPIs je Vorgangsfolge",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput(ns("linien_table"))
            )
        ),
        fluidRow(
            box(
                title = "Durchschnittliche Lead Time je Vorgangsfolge",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("lt_plot"))
            )
        )
    )
}

linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$linie_select)
            filter(linien_overview, fertigungslinie == input$linie_select)
        })
        
        output$linien_table <- renderDT({
            datatable(
                daten_gefiltert(),
                options = list(
                    pageLength = 10,
                    scrollX = TRUE
                ),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        output$lt_plot <- renderPlotly({
            df <- daten_gefiltert()
            
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, Durchschnitt_LT),
                y = Durchschnitt_LT,
                text = paste("Median LT:", Median_LT, "<br>Anzahl:", Anzahl)
            )) +
                geom_col(fill = "#2C3E50") +
                coord_flip() +
                labs(
                    x = "Vorgangsfolge",
                    y = "Ø Lead Time (Tage)"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = "text")
        })
    })
}
