library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

source("02_model/kpis_linie.R", local = TRUE)

linien_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Fertigungslinie auswählen",
                width = 12, status = "primary", solidHeader = TRUE,
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
                width = 12, status = "primary", solidHeader = TRUE,
                DTOutput(ns("linien_table"))
            )
        ),
        fluidRow(
            box(
                title = "Anteil der Vorgangsfolgen (Donut Chart)",
                width = 12, solidHeader = TRUE,
                plotlyOutput(ns("anteil_donut"))
            )
        ),
        fluidRow(
            box(
                title = "Termintreue, Liefertreue & Servicelevel (%)",
                width = 12, solidHeader = TRUE,
                plotlyOutput(ns("plot_treue"))
            )
        ),
        fluidRow(
            box(
                title = "Durchschnittliche Liefermenge je Vorgangsfolge",
                width = 12, solidHeader = TRUE,
                plotlyOutput(ns("plot_liefermenge"))
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
            df <- daten_gefiltert() %>%
                dplyr::select(
                    vorgangsfolge, fertigungslinie, Anzahl,
                    Median_LT, Abweichung, Average_Delay
                )
            datatable(
                df,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        output$anteil_donut <- renderPlotly({
            df <- daten_gefiltert()
            plot_ly(
                data = df,
                labels = ~paste0(vorgangsfolge, " (", Anteil_prozent, "%)"),
                values = ~Anteil_prozent,
                type = "pie",
                textposition = "inside",
                textinfo = "label+percent",
                hole = 0.5
            ) %>%
                layout(title = "Anteil der Vorgangsfolgen")
        })
        
        output$plot_treue <- renderPlotly({
            df <- daten_gefiltert() %>%
                dplyr::select(vorgangsfolge, Termintreue_prozent, Liefertreue_prozent, Servicelevel_prozent) %>%
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
                labs(x = "Vorgangsfolge", y = "Quote (%)", fill = "Treueart") +
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
