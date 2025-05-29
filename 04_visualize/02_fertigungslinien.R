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
                title = "Termintreue vs. Liefertreue",
                width = 12,
                solidHeader = TRUE,
                plotlyOutput(ns("plot_treue"))
            )
        ),
        fluidRow(
            box(
                title = "Vorgangsfolge mit höchster Ø Abweichung",
                width = 12,
                solidHeader = TRUE,
                status = "warning",
                plotlyOutput(ns("plot_top_abweichung"))
            )
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
                    durchschnitt_lt = Durchschnitt_LT,
                    median_lt = Median_LT
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
        
        # Plot 1: Median LT
        output$lt_plot <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, median_lt),
                y = median_lt,
                text = paste("Median LT:", median_lt, "<br>Anzahl:", Anzahl)
            )) +
                geom_col(fill = "#2C3E50") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Median Lead Time (Tage)") +
                theme_minimal()
            
            ggplotly(p, tooltip = "text")
        })
        
        # Plot 2: Donut
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
                layout(showlegend = TRUE)
        })
        
        # Plot 3: Ø Abweichung
        output$plot_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = reorder(vorgangsfolge, abweichung_durchschnitt),
                y = abweichung_durchschnitt
            )) +
                geom_col(fill = "#E74C3C") +
                coord_flip() +
                labs(x = "Vorgangsfolge", y = "Ø Abweichung (Tage)") +
                theme_minimal()
            
            ggplotly(p)
        })
        
        # Plot 4: Termintreue vs Liefertreue
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert()
            p <- ggplot(df, aes(
                x = Termintreue,
                y = Liefertreue,
                label = vorgangsfolge
            )) +
                geom_point(aes(size = Anzahl), color = "#2980B9", alpha = 0.7) +
                labs(x = "Termintreue", y = "Liefertreue") +
                theme_minimal()
            
            ggplotly(p)
        })
        
        # Plot 5: Höchste Abweichung
        output$plot_top_abweichung <- renderPlotly({
            df <- daten_gefiltert()
            df_max <- df %>%
                filter(abweichung_durchschnitt == max(abweichung_durchschnitt, na.rm = TRUE))
            
            plot_ly(
                data = df_max,
                x = ~vorgangsfolge,
                y = ~abweichung_durchschnitt,
                type = 'bar',
                marker = list(color = "#F39C12")
            ) %>%
                layout(
                    yaxis = list(title = "Höchste Abweichung Durchschnitt"),
                    xaxis = list(title = "Vorgangsfolge")
                )
        })
        
        # Delay-Ampeltabellen
        delay_summary <- all_data_finalized %>%
            filter(!is.na(abweichung)) %>%
            mutate(delay = ifelse(abweichung > 0, abweichung, 0)) %>%
            group_by(werk, fertigungslinie, planer) %>%
            summarise(avg_delay = round(mean(delay, na.rm = TRUE), 1), .groups = "drop")
        
        color_column <- function(values) {
            sapply(values, function(v) {
                if (is.na(v)) return("⚪")
                else if (v > 3) return("🔴")
                else if (v > 1) return("🟠")
                else return("🟢")
            })
        }
        
        output$table_werke <- renderDT({
            df <- delay_summary %>%
                select(Werk = werk, avg_delay) %>%
                mutate(Status = color_column(avg_delay))
            
            datatable(df, colnames = c("Werk", "Avg. Delay [d]", " "), escape = FALSE,
                      rownames = FALSE, options = list(dom = 'tip'))
        })
        
        output$table_linien <- renderDT({
            df <- delay_summary %>%
                select(Linie = fertigungslinie, avg_delay) %>%
                mutate(Status = color_column(avg_delay))
            
            datatable(df, colnames = c("Linie", "Avg. Delay [d]", " "), escape = FALSE,
                      rownames = FALSE, options = list(dom = 'tip'))
        })
        
        output$table_planer <- renderDT({
            df <- delay_summary %>%
                select(Planer = planer, avg_delay) %>%
                mutate(Status = color_column(avg_delay))
            
            datatable(df, colnames = c("Planer", "Avg. Delay [d]", " "), escape = FALSE,
                      rownames = FALSE, options = list(dom = 'tip'))
        })
        
    })  # Ende moduleServer
}      # Ende linien_server
