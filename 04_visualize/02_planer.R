# Packages --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)      # für str_count()

# Quellen laden (erstellt u. a. 'planer_overview' und 'all_data_finalized')
source("02_model/create_planer_overview.R", local = TRUE)
source("02_model/kpis_planer.R")

# UI-Modul --------------------------------------------------------------------
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # — Übersichtstabelle —
        fluidRow(
            box(
                title       = "Übersicht aller Planer",
                width       = 12,
                status      = "primary",
                solidHeader = TRUE,
                p("Hier findest du eine interaktive Tabelle mit den wichtigsten Kennzahlen pro Planer."),
                DTOutput(ns("planer_table"))
            )
        ),
        # — Planner-Selector —
        fluidRow(
            box(
                title = "Select Planner",
                width = 12,
                selectInput(
                    inputId  = ns("planer_select"),
                    label    = NULL,
                    choices  = sort(unique(all_data_finalized$planer)),
                    selected = NULL
                )
            )
        ),
        # — Drei interaktive Plots —
        fluidRow(
            box(
                title = "Werke nach Anzahl der Aufträge",
                width = 4,
                plotlyOutput(ns("plot_werke"))
            ),
            box(
                title = "Fertigungslinien nach Anzahl der Aufträge",
                width = 4,
                plotlyOutput(ns("plot_fertigungslinie"))
            ),
            box(
                title = "Top 15 Materialien nach Anzahl der Aufträge",
                width = 4,
                plotlyOutput(ns("plot_material"))
            )
        ),
        # — Planer Performance Check Tabelle —
        fluidRow(
            box(
                title = "Planer Performance Check",
                width = 12,
                DTOutput(ns("perf_check_table"))
            )
        )
    )
}

# Server-Modul ----------------------------------------------------------------
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # — Tabelle (unverändert) —
        if (exists("planer_overview")) {
            output$planer_table <- renderDT({
                datatable(
                    planer_overview,
                    options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip', scrollX = TRUE),
                    rownames = FALSE,
                    class    = "stripe hover cell-border"
                )
            })
        } else {
            output$planer_table <- renderDT({
                datatable(data.frame(Hinweis = "Keine Daten verfügbar"))
            })
        }
        
        # — Reactive Subset für den ausgewählten Planer —
        df_planer <- reactive({
            req(input$planer_select)
            all_data_finalized %>%
                filter(planer == input$planer_select)
        })
        
        # — Plot 1: Werke nach Anzahl der Aufträge —
        output$plot_werke <- renderPlotly({
            df <- df_planer() %>%
                count(werk, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(werk = factor(werk, levels = werk))
            
            p <- ggplot(df, aes(x = werk, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Werk", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # — Plot 2: Fertigungslinien nach Anzahl der Aufträge —
        output$plot_fertigungslinie <- renderPlotly({
            df <- df_planer() %>%
                count(fertigungslinie, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
            
            p <- ggplot(df, aes(x = fertigungslinie, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Fertigungslinie", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # — Plot 3: Top 15 Materialien nach Anzahl der Aufträge —
        output$plot_material <- renderPlotly({
            df <- df_planer() %>%
                count(materialnummer, name = "orders") %>%
                slice_max(order_by = orders, n = 15) %>%
                arrange(desc(orders)) %>%
                mutate(materialnummer = factor(materialnummer, levels = materialnummer))
            
            p <- ggplot(df, aes(x = materialnummer, y = orders, text = orders)) +
                geom_col() +
                labs(x = "Materialnummer", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            ggplotly(p, tooltip = "text")
        })
        
        # — Performance-Check Tabelle —
        output$perf_check_table <- renderDT({
            sel  <- input$planer_select
            df_s <- all_data_finalized %>% filter(planer == sel)
            df_o <- all_data_finalized %>% filter(planer != sel)
            
            # 1) Pünktlichkeitsrate (%)
            sel_pct    <- mean(df_s$abweichung <= 0, na.rm = TRUE) * 100
            other_pcts <- df_o %>%
                group_by(planer) %>%
                summarise(rate = mean(abweichung <= 0, na.rm = TRUE) * 100) %>%
                pull(rate)
            oth_pct    <- mean(other_pcts, na.rm = TRUE)
            dev_pct    <- sel_pct - oth_pct
            interp_pct <- if (dev_pct < 0) {
                '<span style="color:red">Im Vergleich zum Durchschnitt der übrigen Planer liegt die Pünktlichkeitsrate des ausgewählten Planers unter dem Durchschnitt</span>'
            } else if (dev_pct == 0) {
                'Die Terminzuverlässigkeit des ausgewählten Planers bewegt sich auf dem Niveau seiner Kollegen'
            } else {
                '<span style="color:green">Im Vergleich zu den anderen Planern schließt der ausgewählte Planer seine Aufträge überdurchschnittlich pünktlich ab</span>'
            }
            
            # 2) Ø Verzögerung (in Tagen)
            sel_del <- mean(df_s$abweichung[df_s$abweichung > 0], na.rm = TRUE)
            other_del <- df_o %>%
                filter(abweichung > 0) %>%
                group_by(planer) %>%
                summarise(avg = mean(abweichung, na.rm = TRUE)) %>%
                pull(avg) %>%
                mean(., na.rm = TRUE)
            dev_del    <- sel_del - other_del
            interp_del <- if (dev_del < 0) {
                '<span style="color:green">Die durchschnittliche Verspätung des ausgewählten Planers liegt unter dem Durchschnitt der übrigen Planer</span>'
            } else if (dev_del == 0) {
                'Die durchschnittliche Verspätung des ausgewählten Planers bewegt sich auf dem Niveau seiner Kollegen'
            } else {
                '<span style="color:red">Der ausgewählte Planer weist im Vergleich zu seinen Kollegen eine überdurchschnittliche Verspätung auf</span>'
            }
            
            # 3) Ø Anzahl Vorgänge pro Auftrag
            sel_ops <- df_s %>%
                mutate(ops = str_count(vorgangsfolge, "→") + 1) %>%
                summarise(avg = mean(ops, na.rm = TRUE)) %>%
                pull(avg)
            oth_ops <- df_o %>%
                mutate(ops = str_count(vorgangsfolge, "→") + 1) %>%
                summarise(avg = mean(ops, na.rm = TRUE)) %>%
                pull(avg)
            dev_ops    <- sel_ops - oth_ops
            interp_ops <- if (dev_ops < 0) {
                'Im Vergleich zum ausgewählten Planer bearbeiten die übrigen Planer im Schnitt aufwändigere bzw. komplexere Aufträge'
            } else if (dev_ops == 0) {
                'Die durchschnittliche Anzahl an Vorgängen pro Auftrag entspricht dem Wert der übrigen Planer'
            } else {
                'Der ausgewählte Planer betreut im Durchschnitt komplexere Aufträge als die übrigen Planer'
            }
            
            # 4) Anzahl an Aufträgen
            sel_n       <- nrow(df_s)
            other_counts <- df_o %>%
                group_by(planer) %>%
                summarise(n = n()) %>%
                pull(n)
            oth_n     <- mean(other_counts, na.rm = TRUE)
            dev_n     <- sel_n - oth_n
            interp_n  <- if (dev_n < 0) {
                'Die Anzahl der bearbeiteten Aufträge liegt beim ausgewählten Planer unter dem Durchschnitt der anderen Planer'
            } else if (dev_n == 0) {
                'Die Anzahl der bearbeiteten Aufträge durch den ausgewählten Planer entspricht dem Durchschnitt der übrigen Planer'
            } else {
                'Der ausgewählte Planer hat bislang mehr Aufträge abgeschlossen als der durchschnittliche Wert der übrigen Planer'
            }
            
            # Tabelle aufbauen mit tibble::tibble, damit `:=` funktioniert
            df_table <- tibble::tibble(
                KPI                                = c(
                    "Pünktlichkeitsrate (%)",
                    "Ø Verzögerung (in Tagen)",
                    "Ø Anzahl Vorgänge pro Auftrag",
                    "Anzahl an Aufträgen"
                ),
                !!sel                               := c(
                    round(sel_pct,  1),
                    round(sel_del,  1),
                    round(sel_ops,  1),
                    sel_n
                ),
                `Ø anderer Planer`                 = c(
                    round(oth_pct,  1),
                    round(other_del,1),
                    round(oth_ops,  1),
                    round(oth_n,    1)
                ),
                `Abweichung vom Ø anderer Planer`  = c(
                    round(dev_pct,  1),
                    round(dev_del,  1),
                    round(dev_ops,  1),
                    round(dev_n,    1)
                ),
                Interpretation                     = c(
                    interp_pct, interp_del, interp_ops, interp_n
                )
            )
            
            datatable(
                df_table,
                escape   = FALSE,
                rownames = FALSE,
                options  = list(dom = 't', autoWidth = TRUE)
            )
        })
        
    })
}

