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
                title = "Fertigungslinie & Zeitraum auswählen",
                width = 12, solidHeader = TRUE, status = "primary",
                selectInput(ns("linie_select"), "Fertigungslinie:",
                            choices = sort(unique(linien_overview$fertigungslinie)),
                            selected = sort(unique(linien_overview$fertigungslinie))[1]),
                dateRangeInput(ns("date_range"), "Zeitraum wählen:",
                               start = min(all_data_finalized$starttermin_ist, na.rm = TRUE),
                               end   = max(all_data_finalized$starttermin_ist, na.rm = TRUE))
            )
        ),
        fluidRow(
            box(
                title = "KPIs je Vorgangsfolge",
                width = 12, solidHeader = TRUE, status = "primary",
                DTOutput(ns("linien_table")),
                br(),
                downloadButton(ns("download_csv"), "Tabelle als CSV speichern")
            )
        ),
        fluidRow(
            box(
                title = "Interpretation",
                width = 12, solidHeader = TRUE, status = "info",
                htmlOutput(ns("linie_insights"))
            )
        )
    )
}


linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$linie_select, input$date_range)
            
            # Join starttermin_ist anhand vorgangsfolge
            df_dates <- all_data_finalized %>%
                select(vorgangsfolge, starttermin_ist) %>%
                filter(!is.na(starttermin_ist)) %>%
                distinct()
            
            linien_overview %>%
                filter(fertigungslinie == input$linie_select) %>%
                left_join(df_dates, by = "vorgangsfolge") %>%
                filter(starttermin_ist >= input$date_range[1],
                       starttermin_ist <= input$date_range[2])
        })
        
        
        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("kpi_", input$linie_select, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(daten_gefiltert(), file, row.names = FALSE)
            }
        )
        
        output$linie_insights <- renderUI({
            df <- daten_gefiltert()
            req(nrow(df) > 0)
            
            top_vorgang <- df$vorgangsfolge[which.max(df$Anzahl)]
            best_lt <- df %>% filter(Durchschnitt_LT == min(Durchschnitt_LT, na.rm = TRUE)) %>% slice(1)
            worst_termintreue <- df %>% filter(Termintreue == min(Termintreue, na.rm = TRUE)) %>% slice(1)
            
            HTML(paste0(
                "<p><strong>Meistgenutzte Vorgangsfolge:</strong> ", top_vorgang, "</p>",
                "<p><strong>Kürzeste durchschnittliche Lead Time:</strong> ", best_lt$vorgangsfolge, 
                " (", best_lt$Durchschnitt_LT, " Tage)</p>",
                "<p><strong>Niedrigste Termintreue:</strong> ", worst_termintreue$vorgangsfolge,
                " (", round(worst_termintreue$Termintreue * 100), "%)</p>"
            ))
        })
    })
}
