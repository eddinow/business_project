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
        # Infoboxen
        output$material_servicelevel <- renderInfoBox({
            sl <- mean(materialnummer_overview$Anteil_pünktlich, na.rm = TRUE)
            sl_percent <- round(sl * 100, 2)
            color <- if (sl_percent < 70) {
                "red"
            } else if (sl_percent < 95) {
                "orange"
            } else {
                "green"
            }
            infoBox(
                title = "Overall Servicelevel",
                value = paste0(sl_percent, "%"),
                icon = icon("percent"),
                color = color,
                fill = TRUE
            )
        })
        
        output$material_avg_delay <- renderInfoBox({
            avg_delay <- round(mean(materialnummer_overview$Ø_Abweichung_h, na.rm = TRUE), 2)
            infoBox(
                title = "Avg. Delay/Unit [h]",
                value = avg_delay,
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        output$material_avg_lt <- renderInfoBox({
            avg_lt <- round(mean(materialnummer_overview$Ø_LT_pro_Unit_h, na.rm = TRUE), 2)
            infoBox(
                title = "Avg LT/Unit [h]",
                value = avg_lt,
                icon = icon("clock"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # ABC-Summary Tabelle (mit Soll LT pro Unit NUR hier!)
        output$abc_table <- renderDT({
            datatable(
                abc_summary,
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Tabelle für gewählte ABC-Klasse
        filtered_materials <- reactive({
            req(input$abc_select)
            materialnummer_overview %>%
                filter(ABC_Klasse == input$abc_select) %>%
                select(
                    -kum_anteil, -ABC_Klasse
                ) %>%
                relocate(prozessschritte, .after = materialnummer) %>%
                rename("Prozesstiefe" = Prozesstiefe)
        })
        
        output$abc_class_table <- renderDT({
            datatable(
                filtered_materials(),
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Barplot: Gesamtmenge je ABC-Klasse mit Wert-Labels
        output$abc_barplot <- renderPlot({
            ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse, label = Gesamtmenge)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = Gesamtmenge), vjust = -0.2, size = 4) +
                labs(
                    title = "Gesamtmenge je ABC-Klasse",
                    x = "ABC-Klasse",
                    y = "Gesamt gelieferte Menge"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
        
        # KPI-Plot: alle KPIs (mit Wert-Labels)
        output$abc_kpi_plot <- renderPlot({
            abc_long <- abc_summary %>%
                select(ABC_Klasse, Anzahl_Materialien, Ø_Abweichung_h, Ø_LT_pro_Unit_h, Ø_Soll_LT_pro_Unit_h, Anteil_pünktlich, Prozesstiefe) %>%
                pivot_longer(
                    cols = -ABC_Klasse,
                    names_to = "Kennzahl",
                    values_to = "Wert"
                )
            ggplot(abc_long, aes(x = ABC_Klasse, y = Wert, fill = ABC_Klasse, label = round(Wert,2))) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(position = position_dodge(width = 1), vjust = -0.3, size = 3) +
                facet_wrap(~Kennzahl, scales = "free_y") +
                labs(
                    title = "ABC-Klassen: KPIs-Vergleich",
                    x = "ABC-Klasse",
                    y = "Wert"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
    })
}
