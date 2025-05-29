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
        ),
        fluidRow(
            box(
                title = "Anteil der Vorgangsfolgen (Donut Chart)",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("anteil_donut"))
            )
        ),
        fluidRow(
            box(
                title = "Verspätung je Vorgangsfolge (Ø Abweichung)",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("plot_abweichung"))
            )
        ),
        fluidRow(
            box(
                title = "Termintreue vs. Liefertreue",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("plot_treue"))
            )
        ),
        fluidRow(
            box(
                title = "Vorgangsfolge mit höchster Ø LT-Abweichung",
                width = 12,
                status = "warning",
                solidHeader = TRUE,
                plotlyOutput(ns("plot_top_abweichung"))
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
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Ø LT Balkendiagramm
        output$lt_plot <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, Durchschnitt_LT),
                y = Durchschnitt_LT,
                text = paste("Median LT:", Median_LT, "<br>Anzahl:", Anzahl)
            )) +
                geom_col(fill = "#2C3E50") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Ø Lead Time (Tage)") +
                theme_minimal()
            ggplotly(p, tooltip = "text")
        })
        
        # Donut für Anteil der Vorgangsfolgen
        output$anteil_donut <- renderPlotly({
            df <- daten_gefiltert()
            plot_ly(
                data = df,
                labels = ~vorgangsfolge,
                values = ~as.numeric(Anteil),
                type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                hole = 0.5
            ) %>%
                layout(
                    showlegend = TRUE,
                    margin = list(t = 10, b = 10, l = 10, r = 10)
                )
        })
        
        # Ø Abweichung (Verspätung)
        output$plot_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            ggplotly(
                ggplot(df, aes(
                    x = reorder(vorgangsfolge, Ø_Abweichung),
                    y = Ø_Abweichung
                )) +
                    geom_col(fill = "#E74C3C") +
                    coord_flip() +
                    labs(x = "Vorgangsfolge", y = "Ø Abweichung (Tage)") +
                    theme_minimal()
            )
        })
        
        # Termintreue vs. Liefertreue
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert()
            ggplotly(
                ggplot(df, aes(
                    x = Termintreue,
                    y = Liefertreue,
                    label = vorgangsfolge
                )) +
                    geom_point(aes(size = Anzahl), color = "#2980B9", alpha = 0.7) +
                    labs(x = "Termintreue", y = "Liefertreue") +
                    theme_minimal()
            )
        })
        
        # Top-Abweichung
        output$plot_top_abweichung <- renderPlotly({
            df <- daten_gefiltert() %>%
                filter(`Ø_Abweichung` == max(`Ø_Abweichung`, na.rm = TRUE)) %>%
                select(vorgangsfolge, abweichung = `Ø_Abweichung`)
            
            plot_ly(
                data = df,
                x = ~vorgangsfolge,
                y = ~abweichung,
                type = 'bar',
                marker = list(color = "#F39C12")
            ) %>%
                layout(
                    yaxis = list(title = "Höchste Ø Abweichung (Tage)"),
                    xaxis = list(title = "Vorgangsfolge")
                )
        })
        
        