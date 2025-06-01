library(shiny)
library(argonDash)
library(DT)
library(shinydashboard)
library(shinyBS)

#@KASPAR: Hier in create_material_overview muss noch eine Übersichtstabelle für jedes einzelne Material ge-
#macht werden, ähnlich wie für die anderen Kategorien. war mir nur nicht sicher
#wie man das anbetracht der vielen mat-nr. machen soll. vllt abc?

#source("02_model/create_material_overview.R")
source("02_model/kpis_material.R")


# In material_ui kommt der ui-teil des shinydashboards. 
#Den namen materia_ui nicht ändern!

# UI-Modul-Funktion für Materialnummern
material_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h1("Materialnummern-Analyse", style = "font-weight: bold; margin-bottom: 40px;"),
        
        fluidRow(
            infoBoxOutput(ns("material_servicelevel")),
            infoBoxOutput(ns("material_avg_delay")),
            infoBoxOutput(ns("material_avg_lt"))
        ),
        
        fluidRow(
            box(title = "ABC-Klassen Übersicht", width = 12, status = "primary", solidHeader = TRUE,
                DTOutput(ns("abc_table")),
                br(),
                selectInput(ns("abc_select"), "ABC-Klasse auswählen:", choices = c("A", "B", "C"), selected = "A"),
                DTOutput(ns("abc_class_table"))
            )
        ),
        
        fluidRow(
            box(title = "Gesamtmenge je ABC-Klasse", width = 12, status = "success", solidHeader = TRUE,
                plotOutput(ns("abc_barplot"))
            )
        ),
        
        fluidRow(
            box(title = "Kennzahlenvergleich zwischen ABC-Klassen", width = 12, status = "info", solidHeader = TRUE,
                plotOutput(ns("abc_kpi_plot"))
            )
        )
    )
}

# Server-Modul-Funktion für Materialnummern
material_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # Infobox: Ø Servicelevel
        output$material_servicelevel <- renderInfoBox({
            sl <- mean(materialnummer_overview$Anteil_pünktlich, na.rm = TRUE)
            sl_percent <- round(sl * 100)
            color <- if (sl_percent < 70) {
                "red"
            } else if (sl_percent < 95) {
                "orange"
            } else {
                "green"
            }
            infoBox(
                title = "Ø Servicelevel",
                value = paste0(sl_percent, "%"),
                icon = icon("percent"),
                color = color,
                fill = TRUE
            )
        })
        
        # Infobox: Ø Abweichung
        output$material_avg_delay <- renderInfoBox({
            avg_delay <- round(mean(materialnummer_overview$Ø_Abweichung, na.rm = TRUE), 2)
            infoBox(
                title = "Ø Abweichung [d]",
                value = avg_delay,
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # Infobox: Ø Lead Time
        output$material_avg_lt <- renderInfoBox({
            avg_lt <- round(mean(materialnummer_overview$Ø_LT, na.rm = TRUE), 2)
            infoBox(
                title = "Ø Lead Time [d]",
                value = avg_lt,
                icon = icon("clock"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # Übersichtstabelle der ABC-Klassen
        output$abc_table <- renderDT({
            datatable(
                abc_summary,
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Reaktive Tabelle für gewählte ABC-Klasse
        filtered_materials <- reactive({
            req(input$abc_select)
            materialnummer_overview %>%
                filter(ABC_Klasse == input$abc_select)
        })
        
        output$abc_class_table <- renderDT({
            datatable(
                filtered_materials(),
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Barplot: Gesamtmenge je ABC-Klasse
        output$abc_barplot <- renderPlot({
            ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse)) +
                geom_bar(stat = "identity") +
                labs(
                    title = "Gesamtmenge je ABC-Klasse",
                    x = "ABC-Klasse",
                    y = "Gesamt gelieferte Menge"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
        
        # KPI-Plot: Materialanzahl, LT/Schritt, Ø-Abweichung, Anteil pünktlich
        output$abc_kpi_plot <- renderPlot({
            abc_long3 <- abc_summary %>%
                select(ABC_Klasse, Anzahl_Materialien, LT_pro_Schritt, Ø_Abweichung, Anteil_pünktlich) %>%
                pivot_longer(
                    cols = -ABC_Klasse, 
                    names_to = "Kennzahl", 
                    values_to = "Wert"
                )
            ggplot(abc_long3, aes(x = ABC_Klasse, y = Wert, fill = ABC_Klasse)) +
                geom_bar(stat = "identity", position = "dodge") +
                facet_wrap(~Kennzahl, scales = "free_y") +
                labs(
                    title = "ABC-Klassen: Materialanzahl, LT/Schritt, Ø-Abweichung, Anteil pünktlich",
                    x = "ABC-Klasse",
                    y = "Wert"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
    })
}
