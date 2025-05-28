library(shiny)
library(argonDash)
library(DT)

#@KASPAR: Hier in create_material_overview muss noch eine Übersichtstabelle für jedes einzelne Material ge-
#macht werden, ähnlich wie für die anderen Kategorien. war mir nur nicht sicher
#wie man das anbetracht der vielen mat-nr. machen soll. vllt abc?

#source("02_model/create_material_overview.R")


# In material_ui kommt der ui-teil des shinydashboards. 
#Den namen materia_ui nicht ändern!

# UI-Modul-Funktion
material_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "ABC-Klassen Übersicht",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput(ns("abc_table"))
            )
        ),
        fluidRow(
            box(
                title = "Gesamtmenge je ABC-Klasse",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                plotOutput(ns("abc_barplot"))
            )
        ),
        fluidRow(
            box(
                title = "Kennzahlenvergleich zwischen ABC-Klassen",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                plotOutput(ns("abc_kpi_plot"))
            )
        )
    )
}

# Server-Modul-Funktion
material_server <- function(id, abc_summary) {
    moduleServer(id, function(input, output, session) {
        
        # DataTable: Übersicht
        output$abc_table <- renderDT({
            datatable(
                abc_summary,
                options = list(pageLength = 5, scrollX = TRUE),
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