# Packages --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

# Quellen laden (erstellt u. a. 'planer_overview' und 'all_data_finalized')
source("02_model/create_planer_overview.R", local = TRUE)
source("02_model/kpis_planer.R")

# UI-Modul --------------------------------------------------------------------
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Übersicht aller Planer
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
        
        # Detailansicht in einer blauen Box
        fluidRow(
            box(
                title       = "Detailansicht eines ausgewählten Planers",
                width       = 12,
                status      = "primary",
                solidHeader = TRUE,
                
                # Select Planner
                selectInput(
                    inputId = ns("planer_select"),
                    label   = "Select Planner:",
                    choices = sort(unique(all_data_finalized$planer))
                ),
                
                br(),
                
                # Drei interaktive Plots
                fluidRow(
                    box(title = "Werke nach Anzahl der Aufträge", width = 4,
                        plotlyOutput(ns("plot_werke"))),
                    box(title = "Fertigungslinien nach Anzahl der Aufträge", width = 4,
                        plotlyOutput(ns("plot_fertigungslinie"))),
                    box(title = "Top 15 Materialien nach Anzahl der Aufträge", width = 4,
                        plotlyOutput(ns("plot_material")))
                ),
                
                # Performance-Check Tabelle
                fluidRow(
                    box(title = "Planer Performance Check", width = 12,
                        DTOutput(ns("perf_check_table")))
                ),
                
                # Top Aufträge mit Verzögerung
                fluidRow(
                    box(title = "Top Aufträge mit Verzögerung", width = 12,
                        DTOutput(ns("delay_table")))
                ),
                
                # Top Aufträge mit frühzeitiger Fertigstellung
                fluidRow(
                    box(title = "Top Aufträge mit frühzeitiger Fertigstellung", width = 12,
                        DTOutput(ns("early_table")))
                )
            )
        )
    )
}

# Server-Modul ----------------------------------------------------------------
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # Übersichtstabelle
        output$planer_table <- renderDT({
            req(planer_overview)
            datatable(
                planer_overview,
                options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip', scrollX = TRUE),
                rownames = FALSE, class = "stripe hover cell-border"
            )
        })
        
        # Subset für den ausgewählten Planer
        df_planer <- reactive({
            req(input$planer_select)
            all_data_finalized %>% filter(planer == input$planer_select)
        })
        
        # Plot 1
        output$plot_werke <- renderPlotly({
            df <- df_planer() %>%
                count(werk, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(werk = factor(werk, levels = werk))
            ggplotly(
                ggplot(df, aes(werk, orders, text = orders)) +
                    geom_col() + labs(x="Werk", y="Anzahl der Aufträge") +
                    theme_minimal() + theme(axis.text.x = element_text(angle=45,hjust=1)),
                tooltip="text"
            )
        })
        
        # Plot 2
        output$plot_fertigungslinie <- renderPlotly({
            df <- df_planer() %>%
                count(fertigungslinie, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
            ggplotly(
                ggplot(df, aes(fertigungslinie, orders, text = orders)) +
                    geom_col() + labs(x="Fertigungslinie", y="Anzahl der Aufträge") +
                    theme_minimal() + theme(axis.text.x = element_text(angle=45,hjust=1)),
                tooltip="text"
            )
        })
        
        # Plot 3
        output$plot_material <- renderPlotly({
            df <- df_planer() %>%
                count(materialnummer, name = "orders") %>%
                slice_max(order_by = orders, n = 15) %>%
                arrange(desc(orders)) %>%
                mutate(materialnummer = factor(materialnummer, levels = materialnummer))
            ggplotly(
                ggplot(df, aes(materialnummer, orders, text = orders)) +
                    geom_col() + labs(x="Materialnummer", y="Anzahl der Aufträge") +
                    theme_minimal() + theme(axis.text.x = element_text(angle=45,hjust=1)),
                tooltip="text"
            )
        })
        
        # Performance-Check Tabelle
        output$perf_check_table <- renderDT({
            sel  <- input$planer_select
            df_s <- all_data_finalized %>% filter(planer == sel)
            df_o <- all_data_finalized %>% filter(planer != sel)
            
            # 1) Pünktlichkeitsrate (%)
            sel_pct    <- mean(df_s$abweichung <= 0, na.rm = TRUE) * 100
            oth_pct    <- df_o %>%
                group_by(planer) %>%
                summarise(rate = mean(abweichung <= 0, na.rm = TRUE)*100) %>%
                pull(rate) %>% mean(., na.rm = TRUE)
            dev_pct    <- ifelse(is.na(sel_pct - oth_pct), 0, sel_pct - oth_pct)
            interp_pct <- if (dev_pct < 0) {
                '<span style="color:red">Im Vergleich zum Durchschnitt der übrigen Planer liegt die Pünktlichkeitsrate des ausgewählten Planers unter dem Durchschnitt</span>'
            } else if (dev_pct == 0) {
                'Die Terminzuverlässigkeit des ausgewählten Planers bewegt sich auf dem Niveau seiner Kollegen'
            } else {
                '<span style="color:green">Im Vergleich zu den anderen Planern schließt der ausgewählte Planer seine Aufträge überdurchschnittlich pünktlich ab</span>'
            }
            
            # 2) Ø Verzögerung
            sel_del_raw   <- median(df_s$abweichung[df_s$abweichung > 0], na.rm = TRUE)
            sel_del       <- ifelse(is.nan(sel_del_raw), 0, sel_del_raw)
            other_del_raw <- df_o %>%
                filter(abweichung > 0) %>%
                group_by(planer) %>%
                summarise(avg = median(abweichung, na.rm=TRUE)) %>%
                pull(avg) %>% mean(., na.rm = TRUE)
            other_del     <- ifelse(is.nan(other_del_raw), 0, other_del_raw)
            dev_del       <- ifelse(is.na(sel_del - other_del), 0, sel_del - other_del)
            interp_del <- if (dev_del < 0) {
                '<span style="color:green">Die durchschnittliche Verspätung des ausgewählten Planers liegt unter dem Durchschnitt der übrigen Planer</span>'
            } else if (dev_del == 0) {
                'Die durchschnittliche Verspätung des ausgewählten Planers bewegt sich auf dem Niveau seiner Kollegen'
            } else {
                '<span style="color:red">Der Planer weist im Vergleich zu seinen Kollegen eine überdurchschnittliche Verspätung auf</span>'
            }
            
            # 3) Ø Vorgänge/Auftrag
            sel_ops_raw <- df_s %>%
                mutate(ops = str_count(vorgangsfolge,"→")+1) %>%
                summarise(avg = mean(ops, na.rm=TRUE)) %>%
                pull(avg)
            sel_ops     <- ifelse(is.nan(sel_ops_raw), 0, sel_ops_raw)
            oth_ops_raw <- df_o %>%
                mutate(ops = str_count(vorgangsfolge,"→")+1) %>%
                summarise(avg = mean(ops, na.rm=TRUE)) %>%
                pull(avg)
            oth_ops     <- ifelse(is.nan(oth_ops_raw), 0, oth_ops_raw)
            dev_ops     <- ifelse(is.na(sel_ops - oth_ops), 0, sel_ops - oth_ops)
            interp_ops  <- if (dev_ops < 0) {
                'Im Vergleich zum ausgewählten Planer bearbeiten die übrigen Planer im Schnitt aufwändigere Aufträge'
            } else if (dev_ops == 0) {
                'Die durchschnittliche Anzahl an Vorgängen pro Auftrag entspricht dem Wert der übrigen Planer'
            } else {
                'Der ausgewählte Planer betreut im Durchschnitt komplexere Aufträge als die übrigen Planer'
            }
            
            # 4) Anzahl Aufträge
            sel_n        <- nrow(df_s)
            oth_n        <- df_o %>% group_by(planer) %>% summarise(n=n()) %>% pull(n) %>% mean(., na.rm=TRUE)
            dev_n        <- ifelse(is.na(sel_n - oth_n), 0, sel_n - oth_n)
            interp_n     <- if (dev_n < 0) {
                'Die Anzahl der bearbeiteten Aufträge liegt beim ausgewählten Planer unter dem Durchschnitt der anderen Planer'
            } else if (dev_n == 0) {
                'Die Anzahl der bearbeiteten Aufträge durch den ausgewählten Planer entspricht dem Durchschnitt der übrigen Planer'
            } else {
                'Der ausgewählte Planer hat bislang mehr Aufträge abgeschlossen als der durchschnittliche Wert der übrigen Planer'
            }
            
            df_table <- tibble::tibble(
                KPI                               = c(
                    "Pünktlichkeitsrate (%)",
                    "Ø Verzögerung (Tage)",
                    "Ø Workflows/Auftrag",
                    "Anzahl Aufträge"
                ),
                !!sel                              := c(
                    round(sel_pct,1),
                    round(sel_del,1),
                    round(sel_ops,1),
                    sel_n
                ),
                `Ø anderer Planer`                = c(
                    round(oth_pct,1),
                    round(other_del,1),
                    round(oth_ops,1),
                    round(oth_n,1)
                ),
                `Abweichung vom Ø anderer Planer` = c(
                    round(dev_pct,1),
                    round(dev_del,1),
                    round(dev_ops,1),
                    round(dev_n,1)
                ),
                Interpretation                    = c(
                    interp_pct, interp_del, interp_ops, interp_n
                )
            )
            
            datatable(df_table,
                      escape=FALSE, rownames=FALSE,
                      options=list(dom='t', autoWidth=TRUE))
        })
        
        # Top 20 verspätete Aufträge
        output$delay_table <- renderDT({
            df <- df_planer() %>% filter(abweichung > 0)
            if (nrow(df)==0) {
                datatable(
                    data.frame(Hinweis="Für den ausgewählten Planer wurden bisher keine verspäteten Aufträge verzeichnet."),
                    options=list(dom='t')
                )
            } else {
                df %>%
                    arrange(desc(abweichung)) %>%
                    slice_head(n=20) %>%
                    transmute(
                        Auftrag             = auftragsnummer,
                        Materialnummer      = materialnummer,
                        `Verzögerung (Tage)`= abweichung
                    ) %>%
                    datatable(options=list(pageLength=20, dom='t'), rownames=FALSE)
            }
        })
        
        # Top 20 frühzeitige Fertigstellungen
        output$early_table <- renderDT({
            df <- df_planer() %>% filter(abweichung < 0)
            if (nrow(df)==0) {
                datatable(
                    data.frame(Hinweis="Für den ausgewählten Planer liegen bislang keine frühzeitig abgeschlossenen Aufträge vor."),
                    options=list(dom='t')
                )
            } else {
                df %>%
                    arrange(abweichung) %>%
                    slice_head(n=20) %>%
                    transmute(
                        Auftrag                               = auftragsnummer,
                        Materialnummer                        = materialnummer,
                        `Frühzeitige Fertigstellung (Tage)`  = abweichung
                    ) %>%
                    datatable(options=list(pageLength=20, dom='t'), rownames=FALSE)
            }
        })
        
    })
}