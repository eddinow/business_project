# 02_fertigungslinien.R
library(shiny)
library(DT)
library(dplyr)

# Daten laden (wird von globalem App-Code vorausgesetzt)
source("00_tidy/create_all_data_finalized.R", local = TRUE)

# KPI vorbereiten (dein Code 1:1 übernommen)
kpi_linie_vorgangsfolge <- all_data_finalized %>%
    filter(!is.na(fertigungslinie), !is.na(vorgangsfolge)) %>%
    mutate(Durchlaufzeit = as.numeric(endtermin_ist - starttermin_ist)) %>%
    group_by(fertigungslinie, vorgangsfolge) %>%
    summarise(
        Anzahl = n(),
        Durchschnitt_LT = round(mean(Durchlaufzeit, na.rm = TRUE), 1),
        Median_LT = round(median(Durchlaufzeit, na.rm = TRUE), 1),
        Ø_Abweichung = round(mean(abweichung, na.rm = TRUE), 1),
        Termintreue = round(mean(endtermin_ist <= endtermin_soll, na.rm = TRUE), 2),
        Ø_Liefermenge = round(mean(gelieferte_menge, na.rm = TRUE), 1),
        Liefertreue = round(mean(gelieferte_menge >= sollmenge, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    group_by(fertigungslinie) %>%
    mutate(Anteil = round(Anzahl / sum(Anzahl), 3)) %>%
    ungroup() %>%
    mutate(
        Anteil = ifelse(Anteil < 0.001, "<0.001", format(round(as.numeric(Anteil), 3), nsmall = 3))
    )

# UI-Funktion für Modul "Fertigungslinien"
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
                    choices = sort(unique(kpi_linie_vorgangsfolge$fertigungslinie)),
                    selected = sort(unique(kpi_linie_vorgangsfolge$fertigungslinie))[1]
                )
            )
        ),
        fluidRow(
            box(
                title = "Detail-KPIs pro Vorgangsfolge",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput(ns("kpi_tabelle"))
            )
        )
    )
}

# Server-Funktion für Modul "Fertigungslinien"
linien_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        daten_gefiltert <- reactive({
            req(input$linie_select)
            filter(kpi_linie_vorgangsfolge, fertigungslinie == input$linie_select)
        })
        
        output$kpi_tabelle <- renderDT({
            datatable(
                daten_gefiltert(),
                rownames = FALSE,
                options = list(
                    pageLength = 10,
                    scrollX = TRUE
                )
            )
        })
    })
}
