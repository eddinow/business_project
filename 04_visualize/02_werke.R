library(shiny)
library(DT)
library(dplyr)

source("02_model/kpis_werke.R", local = TRUE)

werke_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Werk auswählen",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                if (exists("werke_overview")) {
                    selectInput(
                        ns("werk_select"),
                        "Werk:",
                        choices = sort(unique(werke_overview$werk)),
                        selected = sort(unique(werke_overview$werk))[1]
                    )
                } else {
                    p("⚠️ Keine Daten vorhanden.")
                }
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
                title = "Automatische Interpretation",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("werk_insights"))
            )
        )
    )
}

werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        if (!exists("werke_overview")) return()
        
        daten_gefiltert <- reactive({
            req(input$werk_select)
            werke_overview %>%
                filter(werk == input$werk_select) %>%
                arrange(desc(Anzahl)) %>%
                slice_max(order_by = Anzahl, n = 10)
        })
        
        output$werke_table <- renderDT({
            req(nrow(daten_gefiltert()) > 0)
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
            
            top_vorgang <- df$vorgangsfolge[which.max(df$Anzahl)]
            best_lt <- df %>% filter(Durchschnitt_LT == min(Durchschnitt_LT, na.rm = TRUE)) %>% slice(1)
            worst_tt <- df %>% filter(Termintreue == min(Termintreue, na.rm = TRUE)) %>% slice(1)
            
            HTML(paste0(
                "<p><strong>Häufigste Vorgangsfolge:</strong> ", top_vorgang, "</p>",
                "<p><strong>Kürzeste durchschnittliche Lead Time:</strong> ",
                best_lt$vorgangsfolge, " (", best_lt$Durchschnitt_LT, " Tage)</p>",
                "<p><strong>Niedrigste Termintreue:</strong> ",
                worst_tt$vorgangsfolge, " (", round(worst_tt$Termintreue * 100), "%)</p>"
            ))
        })
        output$plot_start_delay <- renderPlotly({
            df <- werke_overview %>%
                group_by(werk) %>%
                summarise(Startverzoegerung = mean(Durchschnitt_Startverzoegerung, na.rm = TRUE))
            
            p <- ggplot(df, aes(
                x = reorder(werk, -Startverzoegerung),
                y = Startverzoegerung
            )) +
                geom_col(fill = "#E67E22") +
                labs(x = "Werk", y = "Ø Startverzögerung (Tage)") +
                theme_minimal()
            
            ggplotly(p)
        })
        output$plot_treue_werke <- renderPlotly({
            df <- werke_overview %>%
                group_by(werk) %>%
                summarise(
                    Termintreue = mean(Termintreue, na.rm = TRUE),
                    Liefertreue = mean(Liefertreue, na.rm = TRUE),
                    Anzahl = sum(Anzahl, na.rm = TRUE)
                )
            
            p <- ggplot(df, aes(
                x = Termintreue,
                y = Liefertreue,
                size = Anzahl,
                label = werk
            )) +
                geom_point(color = "#2980B9", alpha = 0.7) +
                labs(x = "Termintreue", y = "Liefertreue") +
                theme_minimal()
            
            ggplotly(p)
        })
        output$donut_top_vorgaenge <- renderPlotly({
            req(input$werk_select)
            df <- werke_overview %>%
                filter(werk == input$werk_select) %>%
                slice_max(order_by = Anzahl, n = 5)
            
            plot_ly(
                data = df,
                labels = ~vorgangsfolge,
                values = ~Anzahl,
                type = 'pie',
                hole = 0.5,
                textinfo = "label+percent"
            ) %>%
                layout(title = paste("Top Vorgangsfolgen in Werk", input$werk_select))
        })
        fluidRow(
            box(title = "Ø Startverzögerung je Werk", width = 12, plotlyOutput(ns("plot_start_delay")))
        )
        fluidRow(
            box(title = "Termintreue vs. Liefertreue", width = 12, plotlyOutput(ns("plot_treue_werke")))
        )
        fluidRow(
            box(title = "Top 5 Vorgangsfolgen im Werk (Donut)", width = 12, plotlyOutput(ns("donut_top_vorgaenge")))
        )
    })
}
