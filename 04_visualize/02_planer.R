# Packages --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(plotly)
library(ggplot2)

# Quellen laden (erstellt u. a. 'planer_overview' und 'auftraege_lt_unit')
source("02_model/create_planer_overview.R", local = TRUE)
source("01_transform/create_lt_unit.R")

# UI-Modul --------------------------------------------------------------------
planer_ui <- function(id) {
    ns <- NS(id)
    tagList(
        # Übersicht aller Planer
        fluidRow(
            box(
                title = "Übersicht aller Planer", width = 12, status = "primary", solidHeader = TRUE,
                DTOutput(ns("planer_table"))
            )
        ),
        # Detailansicht
        fluidRow(
            box(
                title = "Detailansicht eines ausgewählten Planers",
                width = 12, status = "primary", solidHeader = TRUE,
                selectInput(ns("planer_select"), "Select Planner:", choices = sort(unique(auftraege_lt_unit$planer))),
                br(),
                # drei Plots
                fluidRow(
                    box(title = "Werke nach Anzahl der Aufträge", width = 4, plotlyOutput(ns("plot_werke"))),
                    box(title = "Fertigungslinien nach Anzahl der Aufträge", width = 4, plotlyOutput(ns("plot_fertigungslinie"))),
                    box(title = "Top 15 Materialien nach Anzahl der Aufträge", width = 4, plotlyOutput(ns("plot_material")))
                ),
                # Performance-Check
                fluidRow(
                    box(title = "Planer Performance Check", width = 12, DTOutput(ns("perf_check_table")))
                ),
                # Quartils-Tabellen nebeneinander
                fluidRow(
                    box(
                        title = "Top Aufträge mit Verzögerung",
                        width = 6, status = "primary", solidHeader = TRUE,
                        height = "400px",
                        DTOutput(ns("delay_summary"))
                    ),
                    box(
                        title = "Top Aufträge mit frühzeitiger Fertigstellung",
                        width = 6, status = "primary", solidHeader = TRUE,
                        height = "400px",
                        DTOutput(ns("early_summary"))
                    )
                )
            )
        )
    )
}

# Server-Modul ----------------------------------------------------------------
planer_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Übersichtstabelle --------------------------------------------------------
        output$planer_table <- renderDT({
            datatable(
                planer_overview,
                options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip', scrollX = TRUE),
                rownames = FALSE, class = "stripe hover cell-border"
            )
        })
        
        # Daten für ausgewählten Planer -------------------------------------------
        df_planer <- reactive({
            req(input$planer_select)
            auftraege_lt_unit %>% filter(planer == input$planer_select)
        })
        
        # Plot: Werke nach Anzahl der Aufträge -------------------------------------
        output$plot_werke <- renderPlotly({
            df <- df_planer() %>%
                count(werk, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(werk = factor(werk, levels = werk))
            p <- ggplot(df, aes(werk, orders, text = orders)) +
                geom_col(fill = "grey") +
                labs(x = "Werk", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p, tooltip = "text")
        })
        
        # Plot: Fertigungslinien nach Anzahl der Aufträge -------------------------
        output$plot_fertigungslinie <- renderPlotly({
            df <- df_planer() %>%
                count(fertigungslinie, name = "orders") %>%
                arrange(desc(orders)) %>%
                mutate(fertigungslinie = factor(fertigungslinie, levels = fertigungslinie))
            p <- ggplot(df, aes(fertigungslinie, orders, text = orders)) +
                geom_col(fill = "grey") +
                labs(x = "Fertigungslinie", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p, tooltip = "text")
        })
        
        # Plot: Top 15 Materialien nach Anzahl der Aufträge ------------------------
        output$plot_material <- renderPlotly({
            df <- df_planer() %>%
                count(materialnummer, name = "orders") %>%
                slice_max(order_by = orders, n = 15) %>%
                arrange(desc(orders)) %>%
                mutate(materialnummer = factor(materialnummer, levels = materialnummer))
            p <- ggplot(df, aes(materialnummer, orders, text = orders)) +
                geom_col(fill = "grey") +
                labs(x = "Materialnummer", y = "Anzahl der Aufträge") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(p, tooltip = "text")
        })
        
        # Planer Performance Check -------------------------------------------------
        output$perf_check_table <- renderDT({
            sel  <- input$planer_select
            df_s <- auftraege_lt_unit %>% filter(planer == sel)
            df_o <- auftraege_lt_unit %>% filter(planer != sel)
            
            # 1) Pünktlichkeitsrate (%)
            sel_pct <- mean(df_s$abweichung_unit <= 0, na.rm = TRUE) * 100
            oth_pct <- df_o %>%
                group_by(planer) %>%
                summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE) * 100) %>%
                pull(rate) %>% mean(., na.rm = TRUE)
            dev_pct <- sel_pct - oth_pct
            interp_pct <- if (dev_pct < 0) {
                '<span style="color:red">Pünktlichkeitsrate unter Ø</span>'
            } else if (dev_pct == 0) {
                'Pünktlichkeitsrate auf Ø'
            } else {
                '<span style="color:green">Pünktlichkeitsrate über Ø</span>'
            }
            
            # 2) Ø Verzögerung
            sel_del <- median(df_s$abweichung_unit[df_s$abweichung_unit > 0], na.rm = TRUE)
            sel_del <- ifelse(is.nan(sel_del), 0, sel_del)
            oth_del <- df_o %>%
                filter(abweichung_unit > 0) %>%
                group_by(planer) %>%
                summarise(avg = median(abweichung_unit, na.rm = TRUE)) %>%
                pull(avg) %>% mean(., na.rm = TRUE)
            oth_del <- ifelse(is.nan(oth_del), 0, oth_del)
            dev_del <- sel_del - oth_del
            interp_del <- if (dev_del < 0) {
                '<span style="color:green">Verspätung unter Ø</span>'
            } else if (dev_del == 0) {
                'Verspätung auf Ø'
            } else {
                '<span style="color:red">Verspätung über Ø</span>'
            }
            
            # 3) Ø Workflows/Auftrag
            sel_ops <- df_s %>%
                mutate(ops = str_count(vorgangsfolge, "→") + 1) %>%
                summarise(avg = mean(ops, na.rm = TRUE)) %>%
                pull(avg)
            sel_ops <- ifelse(is.nan(sel_ops), 0, sel_ops)
            oth_ops <- df_o %>%
                mutate(ops = str_count(vorgangsfolge, "→") + 1) %>%
                summarise(avg = mean(ops, na.rm = TRUE)) %>%
                pull(avg)
            oth_ops <- ifelse(is.nan(oth_ops), 0, oth_ops)
            dev_ops <- sel_ops - oth_ops
            interp_ops <- if (dev_ops < 0) {
                'Weniger komplex als Ø'
            } else if (dev_ops == 0) {
                'Komplexität auf Ø'
            } else {
                'Komplexer als Ø'
            }
            
            # 4) Anzahl Aufträge
            sel_n <- nrow(df_s)
            oth_n <- df_o %>% group_by(planer) %>% summarise(n = n()) %>% pull(n) %>% mean(., na.rm = TRUE)
            dev_n <- sel_n - oth_n
            interp_n <- if (dev_n < 0) {
                'Weniger Aufträge als Ø'
            } else if (dev_n == 0) {
                'Anzahl auf Ø'
            } else {
                'Mehr Aufträge als Ø'
            }
            
            df_table <- tibble::tibble(
                KPI                               = c(
                    "Pünktlichkeitsrate (%)",
                    "Ø Verzögerung (Tage)",
                    "Ø Workflows/Auftrag",
                    "Anzahl Aufträge"
                ),
                !!sel                              := c(
                    round(sel_pct, 1),
                    round(sel_del, 1),
                    round(sel_ops, 1),
                    sel_n
                ),
                `Ø anderer Planer`                = c(
                    round(oth_pct, 1),
                    round(oth_del, 1),
                    round(oth_ops, 1),
                    round(oth_n, 1)
                ),
                `Abweichung vom Ø`                = c(
                    round(dev_pct, 1),
                    round(dev_del, 1),
                    round(dev_ops, 1),
                    round(dev_n, 1)
                ),
                Interpretation                    = c(
                    interp_pct, interp_del, interp_ops, interp_n
                )
            )
            
            datatable(
                df_table,
                escape = FALSE,
                rownames = FALSE,
                options = list(dom = 't', autoWidth = TRUE)
            )
        })
        
        # Hilfsfunktion: IDs vorbereiten -------------------------------------------
        make_id <- function(label) {
            label %>%
                str_replace_all("^>", "gt") %>%
                str_replace_all("-", "_")
        }
        
        # Verzögerungs-Quartile-Tabelle --------------------------------------------
        output$delay_summary <- renderDT({
            df <- df_planer() %>% filter(abweichung > 0)
            labels <- c(">10","5-10","3-4","1-2")
            counts <- c(
                sum(df$abweichung > 10),
                sum(df$abweichung >=5 & df$abweichung <=10),
                sum(df$abweichung >=3 & df$abweichung <=4),
                sum(df$abweichung >=1 & df$abweichung <=2)
            )
            total <- sum(counts)
            pct <- round(counts/total*100,1)
            summary_df <- tibble(
                `Verzögerungsbereich (Tage)` = labels,
                `Anzahl Aufträge`             = counts,
                pct_val                       = pct
            ) %>%
                mutate(
                    `Anteil (%)` = paste0(
                        pct_val, "%<br>",
                        "<div style='background:#eee;width:100px;height:12px;'>",
                        "<div style='background:#1f77b4;width:", pct_val, "%;height:12px;'></div></div>"
                    ),
                    Details = map_chr(labels, ~ as.character(
                        actionButton(
                            ns(paste0("abweichung_", make_id(.x))),
                            label = NULL,
                            icon = icon("search")
                        )
                    ))
                ) %>%
                select(-pct_val)
            
            datatable(
                summary_df,
                escape = FALSE,
                rownames = FALSE,
                options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
            )
        })
        
        # Frühzeitige Fertigstellungs-Quartile --------------------------------------
        output$early_summary <- renderDT({
            df <- df_planer() %>% filter(abweichung < 0)
            labels <- c(">10","5-10","3-4","1-2")
            counts <- c(
                sum(df$abweichung < -10),
                sum(df$abweichung <= -5 & df$abweichung >= -10),
                sum(df$abweichung <= -3 & df$abweichung >= -4),
                sum(df$abweichung <= -1 & df$abweichung >= -2)
            )
            total <- sum(counts)
            pct <- round(counts/total*100,1)
            summary_df <- tibble(
                `Frühzeitige Fertigstellung (Tage)` = labels,
                `Anzahl Aufträge`                    = counts,
                pct_val                              = pct
            ) %>%
                mutate(
                    `Anteil (%)` = paste0(
                        pct_val, "%<br>",
                        "<div style='background:#eee;width:100px;height:12px;'>",
                        "<div style='background:#1f77b4;width:", pct_val, "%;height:12px;'></div></div>"
                    ),
                    Details = map_chr(labels, ~ as.character(
                        actionButton(
                            ns(paste0("neg_dev_", make_id(.x))),
                            label = NULL,
                            icon = icon("search")
                        )
                    ))
                ) %>%
                select(-pct_val)
            
            datatable(
                summary_df,
                escape = FALSE,
                rownames = FALSE,
                options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE)
            )
        })
        
        # Detail-Modals Verzögerung -----------------------------------------------
        observeEvent(input$abweichung_gt10, {
            df_d <- df_planer() %>%
                filter(abweichung > 10) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(desc(Abweichung))
            showModal(modalDialog(
                title = "Aufträge mit Verzögerung > 10 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$abweichung_5_10, {
            df_d <- df_planer() %>%
                filter(abweichung >=5, abweichung <=10) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(desc(Abweichung))
            showModal(modalDialog(
                title = "Aufträge mit Verzögerung 5–10 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$abweichung_3_4, {
            df_d <- df_planer() %>%
                filter(abweichung >=3, abweichung <=4) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(desc(Abweichung))
            showModal(modalDialog(
                title = "Aufträge mit Verzögerung 3–4 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$abweichung_1_2, {
            df_d <- df_planer() %>%
                filter(abweichung >=1, abweichung <=2) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(desc(Abweichung))
            showModal(modalDialog(
                title = "Aufträge mit Verzögerung 1–2 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        
        # Detail-Modals Frühzeitig -----------------------------------------------
        observeEvent(input$neg_dev_gt10, {
            df_d <- df_planer() %>%
                filter(abweichung < -10) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(Abweichung)
            showModal(modalDialog(
                title = "Aufträge mit frühzeitiger Fertigstellung > 10 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$neg_dev_5_10, {
            df_d <- df_planer() %>%
                filter(abweichung <= -5, abweichung >= -10) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(Abweichung)
            showModal(modalDialog(
                title = "Aufträge mit frühzeitiger Fertigstellung 5–10 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$neg_dev_3_4, {
            df_d <- df_planer() %>%
                filter(abweichung <= -3, abweichung >= -4) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(Abweichung)
            showModal(modalDialog(
                title = "Aufträge mit frühzeitiger Fertigstellung 3–4 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        observeEvent(input$neg_dev_1_2, {
            df_d <- df_planer() %>%
                filter(abweichung <= -1, abweichung >= -2) %>%
                select(Auftragsnummer = auftragsnummer, Material = materialnummer, Abweichung = abweichung) %>%
                arrange(Abweichung)
            showModal(modalDialog(
                title = "Aufträge mit frühzeitiger Fertigstellung 1–2 Tage",
                DT::datatable(df_d, options = list(pageLength = 10, autoWidth = TRUE)),
                size = "l"
            ))
        })
        
    })
}

