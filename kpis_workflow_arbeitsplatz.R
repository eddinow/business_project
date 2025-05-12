# Initialisierung ----------------------------------------------------------------

rm(list = ls())
set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)

# Daten laden -------------------------------------------------------------------
source("create_all_data_finalized.R")  # erzeugt 'all_data_finalized'
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")


# Wir wollen ein Ampelsystem für alle Arbeitsplätze hinsichtlich ihrer Performance
# schaffen. Negative Abweichungen über 60% = rot, 20-60% = orange, 0-10%=grün).
# Dazu müssen wir zuerst die Spalte Abweichung aus all_data_finalized abhängig 
# von der 'Auftragsnummer' dem df vorgaenge_raw hinzufügen

vorgaenge_raw <- vorgaenge_raw %>%
    left_join(
        dplyr::select(all_data_finalized, auftragsnummer, abweichung, lead_time_soll),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

# Ampelsystem-Spalte hinzufügen
vorgaenge_raw <- vorgaenge_raw %>%
    mutate(ampelfarbe = case_when(
        abweichung / lead_time_soll > 0.6 ~ "rot",
        abweichung / lead_time_soll > 0.2 ~ "orange",
        abweichung / lead_time_soll >= 0 ~ "grün",
        TRUE ~ NA_character_
    ))

ampel_übersicht <- vorgaenge_raw %>%
  filter(!is.na(ampelfarbe)) %>%
  group_by(ampelfarbe) %>%
  summarise(
    anzahl = n()
  ) %>%
  mutate(
    prozent = round(anzahl / sum(anzahl) * 100, 1)
  )

ui <- fluidPage(
    titlePanel("Arbeitsplatz-Performance (Ampelsystem)"),
    fluidRow(
        column(
            width = 12,
            DTOutput("ampel_tabelle")
        )
    )
)

ui <- fluidPage(
    titlePanel("Arbeitsplatz-Performance (Ampelsystem)"),
    
    fluidRow(
        column(
            width = 12,
            h4("Übersicht der Ampelfarben (Anteil in %)"),
            tableOutput("ampel_summary")
        )
    ),
    
    fluidRow(
        column(
            width = 12,
            DTOutput("ampel_tabelle")
        )
    )
)

server <- function(input, output, session) {
    
    output$ampel_tabelle <- renderDT({
        datatable(
            vorgaenge_raw,
            options = list(pageLength = 10),
            rownames = FALSE
        ) %>%
            formatStyle(
                "ampelfarbe",
                target = "row",
                backgroundColor = styleEqual(
                    c("rot", "orange", "grün"),
                    c("#f44336", "#ff9800", "#4caf50")
                ),
                color = "white"
            )
    })
}

shinyApp(ui, server)