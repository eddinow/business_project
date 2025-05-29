library(shiny)
library(DT)
library(ggplot2)
library(plotly)

# Lade bereinigte KPI-Tabelle
source("02_model/kpis_linie.R", local = TRUE)

werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$werk_select)
            werke_overview %>%
                filter(werk == input$werk_select) %>%
                arrange(desc(Anzahl)) %>%
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
            
            top_vorgang <- df$vorgangsfolge[which.max(df$Anzahl)]
            best_lt <- df %>% filter(Durchschnitt_LT == min(Durchschnitt_LT, na.rm = TRUE)) %>% slice(1)
            worst_tt <- df %>% filter(Termintreue == min(Termintreue, na.rm = TRUE)) %>% slice(1)
            
            HTML(paste0(
                "<p><strong>Häufigste Vorgangsfolge:</strong> ", top_vorgang, "</p>",
                "<p><strong>Kürzeste Median LT:</strong> ",
                best_lt$vorgangsfolge, " (", best_lt$Median_LT, " Tage)</p>",
                "<p><strong>Niedrigste Termintreue:</strong> ",
                worst_tt$vorgangsfolge, " (", round(worst_tt$Termintreue * 100), "%)</p>"
            ))
        })
        
        output$plot_start_delay <- renderPlotly({
            df <- daten_gefiltert()
            
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, Durchschnitt_Startverzoegerung),
                y = Durchschnitt_Startverzoegerung
            )) +
                geom_col(fill = "#E67E22") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Ø Startverzögerung (Tage)") +
                theme_minimal()
            
            ggplotly(p)
        })
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert()
            
            p <- ggplot(df, aes(
                x = Termintreue,
                y = Liefertreue,
                size = Anzahl,
                label = vorgangsfolge
            )) +
                geom_point(color = "#2980B9", alpha = 0.7) +
                labs(x = "Termintreue", y = "Liefertreue") +
                theme_minimal()
            
            ggplotly(p)
        })
        
        output$donut_top_vorgaenge <- renderPlotly({
            df <- daten_gefiltert()
            
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
        
    })
}
