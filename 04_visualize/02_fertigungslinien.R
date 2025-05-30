library(shiny)
library(DT)
library(ggplot2)
library(plotly)

# Lade bereinigte KPI-Tabelle
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
            box(title = "KPIs je Vorgangsfolge", width = 12, DTOutput(ns("linien_table")))
        ),
        fluidRow(
            box(title = "Median Lead Time je Vorgangsfolge", width = 12, plotlyOutput(ns("lt_plot")))
        ),
        fluidRow(
            box(title = "Anteil der Vorgangsfolgen (Donut Chart)", width = 12, plotlyOutput(ns("anteil_donut")))
        ),
        fluidRow(
            box(title = "Ø Abweichung je Vorgangsfolge", width = 12, plotlyOutput(ns("plot_abweichung")))
        ),
        fluidRow(
            box(title = "Termintreue vs. Liefertreue (%)", width = 12, plotlyOutput(ns("plot_treue")))
        ),
        fluidRow(
            box(title = "Vorgangsfolge mit höchster Ø Abweichung", width = 12, status = "warning", plotlyOutput(ns("plot_top_abweichung")))
        )
    )
}

linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$linie_select)
            linien_overview %>%
                filter(fertigungslinie == input$linie_select) %>%
                rename(
                    abweichung_durchschnitt = Abweichung,
                    median_lt = Median_LT,
                    anteil_prozent = Anteil_prozent
                )
        })
        
        output$linien_table <- renderDT({
            datatable(
                daten_gefiltert(),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        output$lt_plot <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(x = reorder(vorgangsfolge, median_lt), y = median_lt)) +
                geom_col(fill = "#2C3E50") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Median LT (Tage)") +
                theme_minimal()
            ggplotly(p)
        })
        
        output$anteil_donut <- renderPlotly({
            df <- daten_gefiltert()
            plot_ly(
                data = df,
                labels = ~paste0(vorgangsfolge, " (", anteil_prozent, "%)"),
                values = ~as.numeric(anteil_prozent),
                type = 'pie',
                hole = 0.5,
                textinfo = "label+percent"
            ) %>%
                layout(title = paste("Anteil der Vorgangsfolgen in Linie", input$linie_select))
        })
        
        output$plot_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(x = reorder(vorgangsfolge, abweichung_durchschnitt), y = abweichung_durchschnitt)) +
                geom_col(fill = "#E74C3C") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Ø Abweichung (Tage)") +
                theme_minimal()
            ggplotly(p)
        })
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert() %>%
                dplyr::select(vorgangsfolge, Termintreue_prozent, Liefertreue_prozent) %>%
                tidyr::pivot_longer(cols = c(Termintreue_prozent, Liefertreue_prozent),
                                    names_to = "Treueart", values_to = "Wert")
            
            p <- ggplot(df, aes(x = reorder(vorgangsfolge, -Wert), y = Wert, fill = Treueart)) +
                geom_col(position = "dodge") +
                labs(x = "Vorgangsfolge", y = "Treue (%)", fill = "Treueart") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p)
        })
        
        output$plot_top_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            df_max <- df %>% filter(abweichung_durchschnitt == max(abweichung_durchschnitt, na.rm = TRUE))
            
            plot_ly(
                data = df_max,
                x = ~vorgangsfolge,
                y = ~abweichung_durchschnitt,
                type = 'bar',
                marker = list(color = "#F39C12")
            ) %>%
                layout(
                    yaxis = list(title = "Höchste Ø Abweichung"),
                    xaxis = list(title = "Vorgangsfolge")
                )
        })
    })
}
