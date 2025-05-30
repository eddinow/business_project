library(shiny)
library(DT)
library(ggplot2)
library(plotly)

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
                title = "Median Lead Time je Vorgangsfolge",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("lt_plot"))
            )
        ),
        fluidRow(
            box(
                title = "Anteil der Vorgangsfolgen (Donut Chart)",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("anteil_donut"))
            )
        ),
        fluidRow(
            box(
                title = "Verspätung je Vorgangsfolge",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_abweichung"))
            )
        ),
        fluidRow(
            box(
                title = "Termintreue vs. Liefertreue (%)",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_treue"))
            )
        ),
        fluidRow(
            box(
                title = "🚨 Bottleneck: Höchste Ø Abweichung",
                width = 12,
                status = "danger",
                solidHeader = TRUE,
                htmlOutput(ns("bottleneck_text"))
            )
        )
    )
}

linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$linie_select)
            linien_overview %>%
                filter(fertigungslinie == input$linie_select)
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
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, Median_LT),
                y = Median_LT
            )) +
                geom_col(fill = "#2C3E50") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Median Lead Time (Tage)") +
                theme_minimal()
            ggplotly(p)
        })
        
        output$anteil_donut <- renderPlotly({
            df <- daten_gefiltert()
            plot_ly(
                data = df,
                labels = ~paste0(vorgangsfolge, " (", Anteil_prozent, "%)"),
                values = ~Anzahl,
                type = "pie",
                textposition = "inside",
                textinfo = "label+percent",
                hole = 0.5
            ) %>%
                layout(title = "Verteilung der Vorgangsfolgen")
        })
        
        output$plot_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, Abweichung),
                y = Abweichung
            )) +
                geom_col(fill = "#E74C3C") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Ø Abweichung (Tage)") +
                theme_minimal()
            ggplotly(p)
        })
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert() %>%
                select(vorgangsfolge, Termintreue_prozent, Liefertreue_prozent) %>%
                tidyr::pivot_longer(
                    cols = c(Termintreue_prozent, Liefertreue_prozent),
                    names_to = "Treueart",
                    values_to = "Wert"
                )
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, -Wert),
                y = Wert,
                fill = Treueart
            )) +
                geom_bar(stat = "identity", position = "dodge") +
                labs(x = "Vorgangsfolge", y = "Treue (%)", fill = "Treueart") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p, tooltip = c("x", "y", "fill"))
        })
        
        output$bottleneck_text <- renderUI({
            df <- daten_gefiltert()
            req(nrow(df) > 0)
            bottleneck <- df %>% filter(Abweichung == max(Abweichung, na.rm = TRUE)) %>% slice(1)
            HTML(paste0(
                "<strong>🚨 Engpass erkannt:</strong><br>",
                "Vorgangsfolge <strong>", bottleneck$vorgangsfolge, "</strong> hat die höchste durchschnittliche Abweichung mit <strong>",
                bottleneck$Abweichung, " Tagen</strong>."
            ))
        })
        
    })
}
