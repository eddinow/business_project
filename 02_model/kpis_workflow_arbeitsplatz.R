# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)
library(stringr)
library(plotly)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")


# BALKENDIAGRAMM ZEITEN FÜR WORKFLOWS
#Um wieder die Lead Times auf Vorgangs- und Arbeitsplatzebene sehen zu können, 
# müssen die sap_vorgaenge cleanen und dann pro Vorgang u Arbeitsplatz wieder die LT
# ermitteln. Wir ermitteln außerdem die Liegezeiten als Differenz zwischen Enddatum
# Vorgang 1 und Startdatum Folgevorgang. Wir ermitteln LT/Unit!

vorgaenge_raw <- vorgaenge_raw %>%
    filter(Auftragsnummer %in% all_data_finalized$auftragsnummer)

vorgaenge_cleaned <- vorgaenge_raw %>%
    left_join(
        all_data_finalized %>%
            dplyr::select(
                auftragsnummer,
                materialnummer,
                werk,
                starttermin_soll,
                endtermin_soll,
                fertigungslinie,
                planer,
                vorgangsfolge,
                arbeitsplatzfolge
            ),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    dplyr::select(-`ME`, -`Gutmenge Vorgang`, -`Ausschuss Vorgang`)

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        `Iststart Vorgang` = as.Date(`Iststart Vorgang`),
        `Istende Vorgang`  = as.Date(`Istende Vorgang`)
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        solldauer = as.numeric(difftime(endtermin_soll, starttermin_soll, units = "days")),
        istdauer = as.numeric(difftime(`Istende Vorgang`, `Iststart Vorgang`, units = "days")),
        abweichung = istdauer - solldauer
    )
    

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    arrange(vorgangsfolge, Vorgangsnummer)
    
vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(gesamtdauer = as.numeric(`Istende Vorgang` - `Iststart Vorgang`))

vorgaenge_sorted <- vorgaenge_cleaned %>%
    arrange(Auftragsnummer, `Iststart Vorgang`)

liegezeiten_plot <- vorgaenge_sorted %>%
    group_by(Auftragsnummer) %>%
    arrange(`Iststart Vorgang`) %>%
    mutate(
        vorheriges_ende = lag(`Istende Vorgang`),
        aktuelles_start = `Iststart Vorgang`
    ) %>%
    filter(!is.na(vorheriges_ende) & aktuelles_start > vorheriges_ende) %>%
    mutate(
        Typ = "Liegezeit",
        Start = vorheriges_ende,
        Ende = aktuelles_start
    ) %>%
    dplyr::select(Auftragsnummer, Typ, Start, Ende)


vorgaenge_plot <- vorgaenge_sorted %>%
    dplyr::select(
        Auftragsnummer,
        Vorgangsnummer,
        Typ = Vorgangsnummer,
        Start = `Iststart Vorgang`,
        Ende = `Istende Vorgang`
    )

workflow_plot_data <- bind_rows(vorgaenge_plot, liegezeiten_plot) %>%
    arrange(Auftragsnummer, Start) %>%
    group_by(Auftragsnummer) %>%
    mutate(Schritt = row_number()) %>%
    ungroup()

ggplot(workflow_plot_data, aes(x = Start, xend = Ende, y = Schritt, yend = Schritt, color = Typ)) +
    geom_segment(size = 6) +
    facet_wrap(~ Auftragsnummer, scales = "free_y") +
    labs(title = "Workflow-Zeitleiste inkl. Liegezeiten",
         x = "Datum", y = "Reihenfolge im Workflow") +
    theme_minimal()



liegezeiten_df <- vorgaenge_sorted %>%
    group_by(Auftragsnummer) %>%
    mutate(
        vorheriges_ende = lag(`Istende Vorgang`),
        aktuelles_start = `Iststart Vorgang`,
        liegedauer = as.numeric(aktuelles_start - vorheriges_ende)
    ) %>%
    filter(!is.na(liegedauer) & liegedauer > 0) %>%  # Nur echte Liegezeiten
    summarise(
        Liegezeit_start = min(vorheriges_ende, na.rm = TRUE),
        Liegezeit_ende   = max(aktuelles_start, na.rm = TRUE),
        Liegedauer_gesamt = sum(liegedauer, na.rm = TRUE),
        .groups = "drop"
    )

liegezeit_u_auftragszeit <- vorgaenge_sorted %>%
    dplyr::select(Auftragsnummer, Vorgangsnummer, istdauer) %>%
    pivot_wider(
        names_from = Vorgangsnummer,
        values_from = istdauer
    )

liegezeit_u_auftragszeit <- liegezeiten_df %>%
    left_join(liegezeit_u_auftragszeit, by = "Auftragsnummer")


# Zusammenfassen der Aufträgen nach Vorgangsnummern um die Daten den einzelnen
# Workflows zuordnen zu können. Hier nutzen wir Median Werte für Liegezeit und
# Bearbeitungszeit

liegezeiten_df_erweitert <- liegezeit_u_auftragszeit %>%
    left_join(
        vorgaenge_cleaned %>% dplyr::select(Auftragsnummer, vorgangsfolge) %>% distinct(),
        by = "Auftragsnummer"
    )

vorgaenge_lz_bz <- liegezeiten_df_erweitert %>%
    group_by(vorgangsfolge) %>%
    summarise(across(
        .cols = c(Liegedauer_gesamt, `0010`, `0020`, `0030`, `0040`, `0050`, `0060`, `0070`, `0005`, `0032`),
        .fns = ~ median(.x, na.rm = TRUE),
        .names = "{.col}_median"
    )) 




# BESTER ARBEITSPLATZ - Wir wollen jetzt schauen welcher Arbeitsplatz die niedrigste
# Abweichung von den Soll Leadtimes hat

arbeitsplatz_median_df <- vorgaenge_cleaned %>%
    filter(!is.na(Arbeitsplatz), !is.na(abweichung), !is.na(solldauer)) %>%
    group_by(Arbeitsplatz) %>%
    summarise(
        median_abweichung = median(abweichung),
        median_solldauer  = median(solldauer),
        .groups = "drop"
    )

# Schritt 2: Prüfungen
gesamt <- nrow(arbeitsplatz_median_df)

anzahl_median_0 <- sum(arbeitsplatz_median_df$median_abweichung == 0)
anzahl_median_gt2x <- sum(arbeitsplatz_median_df$median_abweichung > 2 * arbeitsplatz_median_df$median_solldauer)

# Schritt 3: Ergebnis-DataFrame
arbeitsplatz_abweichung <- tibble(
    kategorie = c("Median = 0", "Median > 2 * Solldauer"),
    anteil_prozent = round(c(
        anzahl_median_0 / gesamt * 100,
        anzahl_median_gt2x / gesamt * 100
    ), 1)
)


# visualize-----------------------------------

ui <- fluidPage(
    titlePanel("Gestapeltes Diagramm je Vorgangsfolge"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "vorgangsfolge",
                label = "Wähle eine Vorgangsfolge:",
                choices = unique(vorgaenge_lz_bz$vorgangsfolge)
            )
        ),

  mainPanel(
    plotlyOutput("stackedPlot"),
    plotOutput("workflow_zeitplot")  
  )
)
        
    )


server <- function(input, output) {
    
    output$stackedPlot <- renderPlotly({
        
        # 1. Daten vorbereiten
        daten <- vorgaenge_lz_bz %>%
            filter(vorgangsfolge == input$vorgangsfolge) %>%
            pivot_longer(
                cols = -vorgangsfolge,
                names_to = "Vorgang",
                values_to = "Dauer"
            ) %>%
            filter(Dauer > 0) %>%
            mutate(
                Vorgang = gsub("_median", "", Vorgang),
                Kategorie = ifelse(Vorgang == "Liegedauer_gesamt", "Liegezeit", Vorgang),
                Kategorie = factor(Kategorie, levels = c(setdiff(Kategorie, "Liegezeit"), "Liegezeit"))
            ) %>%
            mutate(
                Anteil = round(100 * Dauer / sum(Dauer), 1),
                text = paste0(
                    "Kategorie: ", Kategorie, "<br>",
                    "Dauer: ", Dauer, " Tage<br>",
                    "Anteil: ", Anteil, " %"
                )
            )
        
        # 2. Farbpalette wie gehabt
        farben <- c(
            "Liegezeit" = "#8B0000",
            setNames(RColorBrewer::brewer.pal(n = 9, name = "Blues")[3 + seq_along(setdiff(unique(daten$Kategorie), "Liegezeit")) - 1],
                     setdiff(unique(daten$Kategorie), "Liegezeit"))
        )
        
        # 3. Interaktives Plotly-Diagramm
        p <- ggplot(daten, aes(x = vorgangsfolge, y = Dauer, fill = Kategorie, text = text)) +
            geom_bar(stat = "identity", width = 0.4) +
            scale_fill_manual(values = farben) +
            labs(
                title = paste("Zeitaufteilung für:", input$vorgangsfolge),
                x = NULL,
                y = "Dauer (Median in Tagen)",
                fill = "Vorgang"
            ) +
            theme_minimal(base_size = 14)
        
        ggplotly(p, tooltip = "text")
    })
}
    
# 
#     output$stackedPlot <- renderPlot({
# 
#         # Filtere nach gewählter vorgangsfolge
#         daten <- vorgaenge_lz_bz %>%
#             filter(vorgangsfolge == input$vorgangsfolge) %>%
#             pivot_longer(
#                 cols = -vorgangsfolge,
#                 names_to = "Vorgang",
#                 values_to = "Dauer"
#             ) %>%
#             filter(Dauer > 0)  # Optional: nur positive Werte
# 
#         ggplot(daten, aes(x = vorgangsfolge, y = Dauer, fill = Vorgang)) +
#             geom_bar(stat = "identity") +
#             labs(
#                 title = paste("Zeitaufteilung für:", input$vorgangsfolge),
#                 x = NULL, y = "Dauer (Median in Tagen)"
#             ) +
#             theme_minimal()
#     })

shinyApp(ui = ui, server = server)

