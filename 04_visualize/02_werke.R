library(shiny)
library(DT)
library(ggplot2)
library(plotly)

source("02_model/kpis_werke.R", local = TRUE)

werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$werk_select)
            werke_overview %>%
                filter(werk == input$werk_select) %>%
                arrange(desc(Anzahl)) %>%
                mutate(
                    Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)
                ) %>%
                slice_max(order_by = Anzahl, n = 10)
        })
        
        output$werke_table <- renderDT({
            datatable(
                daten_gefiltert(),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("kpi_werk_", input$werk_select, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(daten_gefiltert(), file, row.names = FALSE)
            }
        )
        
        output$werk_insights <- renderUI({
            df <- daten_gefiltert()
            req(nrow(df) > 0)
            
            top_row <- df[which.max(df$Anzahl), ]
            
            HTML(paste0(
                "<p><strong>Häufigste Vorgangsfolge:</strong> ", top_row$vorgangsfolge, 
                " (", top_row$Anteil_prozent, "% Anteil)</p>",
                "<p><strong>Kürzeste Median LT:</strong> ",
                df$vorgangsfolge[which.min(df$Median_LT)], 
                " (", min(df$Median_LT, na.rm = TRUE), " Tage)</p>",
                "<p><strong>Niedrigste Termintreue:</strong> ",
                df$vorgangsfolge[which.min(df$Termintreue_prozent)], 
                " (", min(df$Termintreue_prozent, na.rm = TRUE), "%)</p>"
            ))
        })
        
        output$plot_start_delay <- renderPlotly({
            df <- daten_gefiltert()
            ggplotly(
                ggplot(df, aes(
                    x = reorder(vorgangsfolge, Durchschnitt_Startverzoegerung),
                    y = Durchschnitt_Startverzoegerung
                )) +
                    geom_col(fill = "#E67E22") +
                    coord_flip() +
                    labs(x = "Vorgangsfolge", y = "Ø Startverzögerung (Tage)") +
                    theme_minimal()
            )
        })
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert() %>%
                select(vorgangsfolge, Termintreue_prozent, Liefertreue_prozent) %>%
                pivot_longer(
                    cols = c(Termintreue_prozent, Liefertreue_prozent),
                    names_to = "Treueart",
                    values_to = "Wert"
                )
            
            ggplotly(
                ggplot(df, aes(
                    x = reorder(vorgangsfolge, -Wert),
                    y = Wert,
                    fill = Treueart
                )) +
                    geom_bar(stat = "identity", position = "dodge") +
                    labs(x = "Vorgangsfolge", y = "Treue (%)", fill = "Treueart") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            )
        })
        
        output$donut_top_vorgaenge <- renderPlotly({
            df <- daten_gefiltert()
            plot_ly(
                data = df,
                labels = ~paste0(vorgangsfolge, " (", Anteil_prozent, "%)"),
                values = ~Anteil_prozent,
                type = 'pie',
                hole = 0.5,
                textinfo = "label+percent"
            ) %>%
                layout(title = paste("Top Vorgangsfolgen in Werk", input$werk_select))
        })
    })
}

werke_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Werk auswählen",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                    ns("werk_select"),
                    "Werk:",
                    choices = sort(unique(werke_overview$werk)),
                    selected = sort(unique(werke_overview$werk))[1]
                )
            )
        ),
        fluidRow(
            box(
                title = "Top-Vorgangsfolgen im Werk",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput(ns("werke_table")),
                br(),
                downloadButton(ns("download_csv"), "CSV exportieren")
            )
        ),
        fluidRow(
            box(
                title = "Ø Startverzögerung je Vorgangsfolge",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_start_delay"))
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
                title = "Top-Vorgangsfolgen im Werk (Donut)",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("donut_top_vorgaenge"))
            )
        ),
        fluidRow(
            box(
                title = "Automatische Interpretation",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("werk_insights"))
            )
        )
    )
}
