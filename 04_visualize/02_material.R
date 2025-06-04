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
            box(
                title = "ABC-Klassen Übersicht", width = 12, status = "primary", solidHeader = TRUE,
                DTOutput(ns("abc_table")),
                br(),
                selectInput(ns("abc_select"), "ABC-Klasse auswählen:", choices = c("A", "B", "C"), selected = "A"),
                DTOutput(ns("abc_class_table"))
            )
        ),
        fluidRow(
            box(
                title = "Gesamtmenge je ABC-Klasse", width = 12, status = "success", solidHeader = TRUE,
                plotOutput(ns("abc_barplot"))
            )
        ),
        fluidRow(
            box(
                title = "Kennzahlenvergleich zwischen ABC-Klassen", width = 12, status = "info", solidHeader = TRUE,
                plotOutput(ns("abc_kpi_plot"))
            )
        )
    )
}


# Server-Modul-Funktion für Materialnummern
material_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # Infobox: Overall Servicelevel
        output$material_servicelevel <- renderInfoBox({
            sl <- mean(materialnummer_overview$Anteil_pünktlich, na.rm = TRUE)
            sl_percent <- sl * 100
            color <- if (sl_percent < 70) "red" else if (sl_percent < 95) "orange" else "green"
            infoBox(
                title = "Overall Servicelevel",
                value = paste0(formatC(sl_percent, format="f", digits=2, decimal.mark = ","), "%"),
                icon = icon("percent"),
                color = color,
                fill = TRUE
            )
        })
        
        # Infobox: Avg. Delay/Unit [s]
        output$material_avg_delay <- renderInfoBox({
            avg_delay <- mean(materialnummer_overview$Ø_Abweichung, na.rm = TRUE)
            infoBox(
                title = "Avg. Delay/Unit [s]",
                value = formatC(avg_delay, format="f", big.mark=".", decimal.mark = ",", digits=2),
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # Infobox: Avg LT/Unit [s]
        output$material_avg_lt <- renderInfoBox({
            avg_lt <- mean(materialnummer_overview$Ø_LT_pro_Unit, na.rm = TRUE)
            infoBox(
                title = "Avg LT/Unit [s]",
                value = formatC(avg_lt, format="f", big.mark=".", decimal.mark = ",", digits=2),
                icon = icon("clock"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        # ABC-Summary Tabelle **MIT** ABC-Klasse-Spalte!
        output$abc_table <- renderDT({
            abc_summary_display <- abc_summary %>%
                mutate(
                    Servicelevel = Anteil_pünktlich * 100,
                    Servicelevel = paste0(formatC(Servicelevel, format="f", digits=2, decimal.mark = ","), "%")
                ) %>%
                select(
                    "ABC-Klasse" = ABC_Klasse,
                    "Materialanzahl" = Anzahl_Materialien,
                    "Gesamtanzahl Aufträge" = Gesamt_Anzahl,
                    "Gesamtmenge" = Gesamtmenge,
                    "Gesamtsollmenge" = Gesamtsollmenge,
                    "Ø Abweichung [s]" = Ø_Abweichung,
                    "Ø LT/Unit [s]" = Ø_LT_pro_Unit,
                    "Servicelevel"
                ) %>%
                mutate(
                    across(
                        c("Materialanzahl", "Gesamtanzahl Aufträge", "Gesamtmenge", "Gesamtsollmenge",
                          "Ø Abweichung [s]", "Ø LT/Unit [s]"),
                        ~formatC(.x, format="f", big.mark=".", decimal.mark = ",", digits=2)
                    )
                )
            
            datatable(
                abc_summary_display,
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Reaktive Tabelle für gewählte ABC-Klasse
        filtered_materials <- reactive({
            req(input$abc_select)
            materialnummer_overview %>%
                filter(ABC_Klasse == input$abc_select) %>%
                select(
                    materialnummer, Anzahl, Gesamtmenge, Sollmenge,
                    Ø_Abweichung, Ø_LT_pro_Unit, Anteil_pünktlich, Hauptabfolge
                ) %>%
                rename(
                    "Materialnummer" = materialnummer,
                    "# Aufträge" = Anzahl,
                    "Gelieferte Menge" = Gesamtmenge,
                    "Sollmenge" = Sollmenge,
                    "Ø Abweichung [s]" = Ø_Abweichung,
                    "Ø LT/Unit [s]" = Ø_LT_pro_Unit,
                    "Servicelevel" = Anteil_pünktlich,
                    "Prozessschritte" = Hauptabfolge
                ) %>%
                mutate(
                    "# Aufträge" = formatC(`# Aufträge`, format="f", big.mark=".", digits=0),
                    `Gelieferte Menge` = formatC(`Gelieferte Menge`, format="f", big.mark=".", digits=0),
                    Sollmenge = formatC(Sollmenge, format="f", big.mark=".", digits=0),
                    `Ø Abweichung [s]` = formatC(`Ø Abweichung [s]`, format="f", big.mark=".", decimal.mark = ",", digits=2),
                    `Ø LT/Unit [s]` = formatC(`Ø LT/Unit [s]`, format="f", big.mark=".", decimal.mark = ",", digits=2),
                    Servicelevel = paste0(formatC(as.numeric(Servicelevel) * 100, format="f", digits=2, decimal.mark = ","), "%")
                )
        })
        
        output$abc_class_table <- renderDT({
            datatable(
                filtered_materials(),
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
        })
        
        # Barplot: Gesamtmenge je ABC-Klasse mit Werten auf den Balken
        output$abc_barplot <- renderPlot({
            ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = formatC(Gesamtmenge, format="f", big.mark=".", digits=0)), vjust = -0.2, size = 5) +
                labs(
                    title = "Gesamtmenge je ABC-Klasse",
                    x = "ABC-Klasse",
                    y = "Gesamt gelieferte Menge"
                ) +
                theme_minimal(base_size = 15) +
                scale_fill_brewer(palette = "Set2")
        })
        
        # KPI-Plot: Materialanzahl, LT/Unit, Ø-Abweichung, Servicelevel mit Beschriftung
        output$abc_kpi_plot <- renderPlot({
            abc_long <- abc_summary %>%
                select(ABC_Klasse, Anzahl_Materialien, Ø_LT_pro_Unit, Ø_Abweichung, Anteil_pünktlich) %>%
                tidyr::pivot_longer(
                    cols = -ABC_Klasse,
                    names_to = "Kennzahl",
                    values_to = "Wert"
                ) %>%
                mutate(
                    Wert_anzeige = case_when(
                        Kennzahl == "Anteil_pünktlich" ~ paste0(formatC(Wert * 100, format="f", digits=2, decimal.mark = ","), "%"),
                        TRUE ~ formatC(Wert, format="f", big.mark=".", decimal.mark = ",", digits=2)
                    ),
                    Kennzahl = dplyr::recode(Kennzahl,
                                             "Anzahl_Materialien" = "Materialanzahl",
                                             "Ø_LT_pro_Unit" = "Ø LT/Unit [s]",
                                             "Ø_Abweichung" = "Ø Abweichung [s]",
                                             "Anteil_pünktlich" = "Servicelevel"
                    )
                )
            
            ggplot(abc_long, aes(x = ABC_Klasse, y = Wert, fill = ABC_Klasse)) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(aes(label = Wert_anzeige), vjust = -0.2, size = 4) +
                facet_wrap(~Kennzahl, scales = "free_y") +
                labs(
                    title = "ABC-Klassen: Materialanzahl, LT/Unit, Ø-Abweichung, Servicelevel",
                    x = "ABC-Klasse",
                    y = "Wert"
                ) +
                theme_minimal(base_size = 13) +
                scale_fill_brewer(palette = "Set2")
        })
        
    })
}
