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
                DTOutput(ns("abc_table"))
            )
        ),
        fluidRow(
            box(title = "Materialnummern je ABC-Klasse", width = 12, status = "primary", solidHeader = TRUE,
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
        
        # --- Service Level Infobox ---
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
                title = "Overall Servicelevel",
                value = paste0(sl_percent, "%"),
                icon = icon("percent"),
                color = color,
                fill = TRUE
            )
        })
        
        # --- AVG DELAY Infobox ---
        output$material_avg_delay <- renderInfoBox({
            avg_delay <- round(mean(materialnummer_overview$Ø_Abweichung, na.rm = TRUE), 2)
            infoBox(
                title = "Avg. Delay/Unit [s]",
                value = format(avg_delay, big.mark = ".", decimal.mark = ","),
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # --- AVG LT Infobox ---
        output$material_avg_lt <- renderInfoBox({
            avg_lt <- round(mean(materialnummer_overview$Ø_LT_pro_Unit, na.rm = TRUE), 2)
            infoBox(
                title = "Avg. LT/Unit [s]",
                value = format(avg_lt, big.mark = ".", decimal.mark = ","),
                icon = icon("clock"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # --- ABC-Übersichtstabelle ---
        output$abc_table <- renderDT({
            datatable(
                abc_summary %>%
                    select(
                        ABC_Klasse,
                        Anzahl_Materialien,
                        Gesamt_Anzahl,
                        Gesamtmenge,
                        Gesamtsollmenge,
                        `Ø_Abweichung [s]` = Ø_Abweichung,
                        `Ø_LT_pro_Unit [s]` = Ø_LT_pro_Unit,
                        Anteil_pünktlich,
                        Prozesstiefe,
                        `SOLL_LT_pro_Unit [s]`
                    ) %>%
                    mutate(across(where(is.numeric), ~format(.x, big.mark = ".", decimal.mark = ","))),
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # --- Tabelle Materialnummern einer ABC-Klasse ---
        filtered_materials <- reactive({
            req(input$abc_select)
            materialnummer_overview %>%
                filter(ABC_Klasse == input$abc_select) %>%
                select(
                    materialnummer,
                    Anzahl,
                    Gesamtmenge,
                    Sollmenge,
                    `Ø_Abweichung [s]` = Ø_Abweichung,
                    `Ø_LT_pro_Unit [s]` = Ø_LT_pro_Unit,
                    Anteil_pünktlich,
                    Prozesstiefe,
                    hauptabfolge
                ) %>%
                mutate(across(where(is.numeric), ~format(.x, big.mark = ".", decimal.mark = ",")))
        })
        
        output$abc_class_table <- renderDT({
            datatable(
                filtered_materials() %>%
                    rename(
                        "Materialnummer" = materialnummer,
                        "Aufträge" = Anzahl,
                        "Gelieferte Menge" = Gesamtmenge,
                        "Sollmenge" = Sollmenge,
                        "Ø Delay/Unit [s]" = `Ø_Abweichung [s]`,
                        "Ø LT/Unit [s]" = `Ø_LT_pro_Unit [s]`,
                        "Servicelevel" = Anteil_pünktlich,
                        "Prozesstiefe" = Prozesstiefe,
                        "Prozessschritte" = hauptabfolge
                    ),
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # --- Barplot Gesamtmenge je ABC-Klasse MIT Wert IN Balken ---
        output$abc_barplot <- renderPlot({
            ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse)) +
                geom_bar(stat = "identity") +
                geom_text(
                    aes(label = format(round(Gesamtmenge, 2), big.mark = ".", decimal.mark = ",")),
                    vjust = 1.1, color = "white", fontface = "bold", size = 5
                ) +
                labs(
                    title = "Gesamtmenge je ABC-Klasse",
                    x = "ABC-Klasse",
                    y = "Gesamt gelieferte Menge"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
        
        # --- KPI-Plot mit Wert IN Balken ---
        output$abc_kpi_plot <- renderPlot({
            abc_long3 <- abc_summary %>%
                select(
                    ABC_Klasse, Anzahl_Materialien, `Ø_LT_pro_Unit [s]`, `Ø_Abweichung [s]` = Ø_Abweichung, Anteil_pünktlich
                ) %>%
                pivot_longer(
                    cols = -ABC_Klasse,
                    names_to = "Kennzahl",
                    values_to = "Wert"
                )
            ggplot(abc_long3, aes(x = ABC_Klasse, y = as.numeric(Wert), fill = ABC_Klasse)) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(
                    aes(label = format(round(Wert, 2), big.mark = ".", decimal.mark = ",")),
                    position = position_dodge(width = 0.9), vjust = 1.1, color = "white", fontface = "bold", size = 5
                ) +
                facet_wrap(~Kennzahl, scales = "free_y") +
                labs(
                    title = "ABC-Klassen: Materialanzahl, LT/Unit, Ø-Abweichung, Anteil pünktlich",
                    x = "ABC-Klasse",
                    y = "Wert"
                ) +
                theme_minimal() +
                scale_fill_brewer(palette = "Set2")
        })
    })
}
