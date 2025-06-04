library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# KPI-Daten laden
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
                title = "Automatische Interpretation",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("werk_insights"))
            )
        ),
        fluidRow(
            box(
                title = "KPIs je Vorgangsfolge",
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
                title = "Anteil der Vorgangsfolgen (Donut Chart)",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("donut_top_vorgaenge"))
            )
        ),
        fluidRow(
            box(
                title = "Termintreue, Liefertreue & Servicelevel (%)",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_treue"))
            )
        ),
        fluidRow(
            box(
                title = "Durchschnittliche Liefermenge je Vorgangsfolge",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_liefermenge"))
            )
        )
    )
}

werke_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$werk_select)
            werke_overview %>%
                filter(werk == input$werk_select) %>%
                arrange(desc(Anzahl)) %>%
                mutate(Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%
                slice_max(order_by = Anzahl, n = 10)
        })
        
        output$werke_table <- renderDT({
            df <- daten_gefiltert() %>%
                select(
                    vorgangsfolge, Anzahl,
                    Median_LT,
                    Durchschnitt_Abweichung,
                    Average_Delay,
                    Servicelevel_prozent
                )
            
            datatable(
                df,
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
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert() %>%
                select(vorgangsfolge, Termintreue_prozent, Liefertreue_prozent, Servicelevel_prozent) %>%
                pivot_longer(
                    cols = c(Termintreue_prozent, Liefertreue_prozent, Servicelevel_prozent),
                    names_to = "Treueart",
                    values_to = "Wert"
                )
            
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, -Wert),
                y = Wert,
                fill = Treueart
            )) +
                geom_bar(stat = "identity", position = "dodge") +
                labs(x = "Vorgangsfolge", y = "Quote (%)", fill = "Kennzahl") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = c("x", "y", "fill"))
        })
        
        output$plot_liefermenge <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, -Durchschnitt_Liefermenge),
                y = Durchschnitt_Liefermenge
            )) +
                geom_col(fill = "#3498DB") +
                labs(x = "Vorgangsfolge", y = "Ø Liefermenge") +
                coord_flip() +
                theme_minimal()
            
            ggplotly(p)
        })
        
    })
}
