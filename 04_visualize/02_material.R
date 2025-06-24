library(shiny)
library(argonDash)
library(DT)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(tidyr)

# @KASPAR: Hier in create_material_overview muss noch eine Übersichtstabelle für jedes einzelne Material ge-
# macht werden, ähnlich wie für die anderen Kategorien. war mir nur nicht sicher
# wie man das anbetracht der vielen mat-nr. machen soll. vllt abc?

# source("02_model/create_material_overview.R")
source("02_model/kpis_material.R")

# In material_ui kommt der ui‑teil des shinydashboards. 
# Den Namen material_ui nicht ändern!

#────────────────────────────── UI ──────────────────────────────
material_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h1("Materialnummern‑Analyse (A‑Fokus)", style = "font-weight: bold; margin-bottom: 20px;"),
        
        # Vier spezifische Alerts + bisheriger Outlier‑Alert
        bsAlert(ns("alert_termintreue")),   # 1) Termintreue
        bsAlert(ns("alert_liefertreue")),   # 2) Liefertreue
        bsAlert(ns("alert_speed_unit")),    # 3) Geschwindigkeit / Unit
        bsAlert(ns("alert_speed_order")),   # 4) Geschwindigkeit / Auftrag
        bsAlert(ns("outlier_alert")),       # bisheriger Sammel‑Alert
        
        fluidRow(
            infoBoxOutput(ns("material_servicelevel")),
            infoBoxOutput(ns("material_avg_delay")),
            infoBoxOutput(ns("material_avg_lt"))
        ),
        
        fluidRow(
            box(
                title = "ABC‑Klassen Übersicht", width = 12, status = "primary", solidHeader = TRUE,
                DTOutput(ns("abc_table")),
                br(),
                selectInput(ns("abc_select"), "ABC‑Klasse auswählen:", choices = c("A", "B", "C"), selected = "A"),
                DTOutput(ns("abc_class_table"))
            )
        ),
        
        fluidRow(
            box(
                title = "Gesamtmenge je ABC‑Klasse", width = 12, status = "success", solidHeader = TRUE,
                plotOutput(ns("abc_barplot"))
            )
        ),
        
        fluidRow(
            box(
                title = "Kennzahlenvergleich zwischen ABC‑Klassen", width = 12, status = "info", solidHeader = TRUE,
                plotOutput(ns("abc_kpi_plot"))
            )
        )
    )
}

#────────────────────────── SERVER ──────────────────────────────
material_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        #──── Globale Durchschnitte zur Alert‑Bewertung ────
        global_avg <- list(
            sl          = mean(materialnummer_overview$Anteil_pünktlich, na.rm = TRUE),           # Termintreue
            delay       = mean(materialnummer_overview$Ø_Abweichung,  na.rm = TRUE),             # Liefertreue (Ø‑Delay)
            speed_unit  = mean(materialnummer_overview$Ø_LT_pro_Unit,  na.rm = TRUE),            # LT / Unit
            speed_order = mean(auftraege_lt_unit$lt_ist_order,               na.rm = TRUE)       # LT / Auftrag
        )
        
        #──── Hilfsfunktion: Ausreißer‑Klassifikation ───────
        classify_outliers <- function(df) {
            df %>%
                mutate(
                    flag_sl    = Anteil_pünktlich < 0.50,
                    flag_delay = Ø_Abweichung     > 60,
                    flag_lt    = Ø_LT_pro_Unit    > 120,
                    Alert      = flag_sl | flag_delay | flag_lt,
                    Priority   = (flag_sl * 3) + (flag_delay * 2) + flag_lt,
                    Alert_Grund = paste(
                        ifelse(flag_sl,    "Servicelevel < 50 %",  ""),
                        ifelse(flag_delay, "Ø‑Abweichung > 60 s",   ""),
                        ifelse(flag_lt,    "Ø‑LT > 120 s",         ""),
                        sep = "; "
                    ) %>% gsub("(^; |; $)", "", .) %>% gsub("; ;", ";", .)
                )
        }
        
        #────────────────────── Infoboxen ───────────────────────────
        output$material_servicelevel <- renderInfoBox({
            sl_percent <- global_avg$sl * 100
            color <- if (sl_percent < 70) "red" else if (sl_percent < 95) "orange" else "green"
            infoBox("Overall Servicelevel",
                    paste0(formatC(sl_percent, format = "f", digits = 2, decimal.mark = ","), "%"),
                    icon  = icon("percent"), color = color, fill = TRUE)
        })
        
        output$material_avg_delay <- renderInfoBox({
            avg_delay <- global_avg$delay
            infoBox("Avg. Delay/Unit [s]",
                    formatC(avg_delay, format = "f", big.mark = ".", decimal.mark = ",", digits = 2),
                    icon  = icon("hourglass-half"), color = "light-blue", fill = TRUE)
        })
        
        output$material_avg_lt <- renderInfoBox({
            avg_lt <- global_avg$speed_unit
            infoBox("Avg LT/Unit [s]",
                    formatC(avg_lt, format = "f", big.mark = ".", decimal.mark = ",", digits = 2),
                    icon  = icon("clock"), color = "light-blue", fill = TRUE)
        })
        
        #────────────────── ABC‑Summary Tabelle ──────────────────────
        output$abc_table <- renderDT({
            abc_summary %>%
                mutate(Servicelevel = paste0(formatC(Anteil_pünktlich * 100, format = "f", digits = 2, decimal.mark = ","), "%")) %>%
                dplyr::select(
                    ABC_Klasse,
                    Anzahl_Materialien,
                    Gesamt_Anzahl,
                    Gesamtmenge,
                    Gesamtsollmenge,
                    Ø_Abweichung,
                    Ø_LT_pro_Unit,
                    Servicelevel
                ) %>%
                dplyr::rename(
                    `ABC-Klasse`            = ABC_Klasse,
                    Materialanzahl          = Anzahl_Materialien,
                    `Gesamtanzahl Aufträge` = Gesamt_Anzahl,
                    `Ø Abweichung [s]`      = Ø_Abweichung,
                    `Ø LT/Unit [s]`         = Ø_LT_pro_Unit
                ) %>%
                mutate(across(where(is.numeric), ~formatC(.x, format = "f", big.mark = ".", decimal.mark = ",", digits = 2))) %>%
                datatable(options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE, class = "stripe hover cell-border")
        })
        
        #──────────── Reaktive Tabelle mit Outlier‑Flags ─────────────
        annotated_materials <- reactive({
            req(input$abc_select)
            materialnummer_overview %>%
                filter(ABC_Klasse == input$abc_select) %>%
                classify_outliers()
        })
        
        #──────────── Vier thematische Alerts ────────────────────────
        observeEvent(annotated_materials(), {
            mats <- annotated_materials()
            
            # 1) Termintreue (Servicelevel)
            n_termintreue <- sum(mats$Anteil_pünktlich < global_avg$sl, na.rm = TRUE)
            if (n_termintreue > 0) {
                createAlert(session, anchorId = "alert_termintreue", alertId = "alert_termintreue",
                            title   = "Termintreue unter Durchschnitt",
                            content = paste("Bei", n_termintreue, "Materialien liegt die Termintreue unter dem Durchschnitt (",
                                            scales::percent(global_avg$sl, accuracy = 0.1), ")."),
                            style   = "danger", dismiss = TRUE)
            } else {
                closeAlert(session, "alert_termintreue")
            }
            
            # 2) Liefertreue (Ø‑Abweichung)
            n_liefertreue <- sum(mats$Ø_Abweichung > global_avg$delay, na.rm = TRUE)
            if (n_liefertreue > 0) {
                createAlert(session, anchorId = "alert_liefertreue", alertId = "alert_liefertreue",
                            title   = "Liefertreue unter Durchschnitt",
                            content = paste("Bei", n_liefertreue, "Materialien ist die Ø‑Abweichung höher als der Durchschnitt (",
                                            formatC(global_avg$delay, format = "f", digits = 2, decimal.mark = ","), " s)."),
                            style   = "warning", dismiss = TRUE)
            } else {
                closeAlert(session, "alert_liefertreue")
            }
            
            # 3) Geschwindigkeit pro Mengeneinheit (LT/Unit)
            n_speed_unit <- sum(mats$Ø_LT_pro_Unit > global_avg$speed_unit, na.rm = TRUE)
            if (n_speed_unit > 0) {
                createAlert(session, anchorId = "alert_speed_unit", alertId = "alert_speed_unit",
                            title   = "Geschwindigkeit pro Mengeneinheit unter Durchschnitt",
                            content = paste("Bei", n_speed_unit, "Materialien ist LT/Unit langsamer als der Durchschnitt (",
                                            formatC(global_avg$speed_unit, format = "f", digits = 2, decimal.mark = ","), " s)."),
                            style   = "warning", dismiss = TRUE)
            } else {
                closeAlert(session, "alert_speed_unit")
            }
            
            # 4) Geschwindigkeit pro Auftrag (LT/Order)
            speed_order_per_material <- auftraege_lt_unit %>%
                filter(materialnummer %in% mats$materialnummer) %>%
                group_by(materialnummer) %>%
                summarise(avg_lt_order = mean(lt_ist_order, na.rm = TRUE), .groups = "drop")
            
            n_speed_order <- sum(speed_order_per_material$avg_lt_order > global_avg$speed_order, na.rm = TRUE)
            if (n_speed_order > 0) {
                createAlert(session, anchorId = "alert_speed_order", alertId = "alert_speed_order",
                            title   = "Geschwindigkeit pro Auftrag unter Durchschnitt",
                            content = paste("Bei", n_speed_order, "Materialien ist LT/Auftrag langsamer als der Durchschnitt (",
                                            formatC(global_avg$speed_order, format = "f", digits = 2, decimal.mark = ","), " s)."),
                            style   = "warning", dismiss = TRUE)
            } else {
                closeAlert(session, "alert_speed_order")
            }
        }, ignoreNULL = FALSE)
        
        #──────────── Bisheriger Outlier‑Alert (unverändert) ──────────
        observeEvent(annotated_materials(), {
            out <- annotated_materials()
            if (any(out$Alert)) {
                createAlert(session, anchorId = "outlier_alert", alertId = "alert1",
                            title   = "Ausreißer erkannt",
                            content = paste("Es wurden", sum(out$Alert), "Materialien mit Ausreißern gefunden."),
                            style   = "danger", dismiss = TRUE)
            } else {
                closeAlert(session, "alert1")
            }
        }, ignoreNULL = FALSE)
        
        #──────────── Detailtabelle ──────────────────────────────────
        output$abc_class_table <- renderDT({
            annotated_materials() %>%
                arrange(desc(Alert), desc(Priority)) %>%
                transmute(
                    Materialnummer       = materialnummer,
                    `# Aufträge`          = formatC(Anzahl, format = "f", big.mark = ".", digits = 0),
                    `Gelieferte Menge`    = formatC(Gesamtmenge, format = "f", big.mark = ".", digits = 0),
                    Sollmenge            = formatC(Sollmenge, format = "f", big.mark = ".", digits = 0),
                    `Ø Abweichung [s]`    = formatC(Ø_Abweichung, format = "f", big.mark = ".", decimal.mark = ",", digits = 2),
                    `Ø LT/Unit [s]`       = formatC(Ø_LT_pro_Unit, format = "f", big.mark = ".", decimal.mark = ",", digits = 2),
                    Servicelevel         = paste0(formatC(Anteil_pünktlich * 100, format = "f", digits = 2, decimal.mark = ","), "%"),
                    Alert_Grund
                ) %>%
                datatable(options = list(pageLength = 15, scrollX = TRUE, order = list(list(0, "desc"))),
                          rownames = FALSE, class = "stripe hover cell-border") %>%
                formatStyle("Alert_Grund", target = "row",
                            backgroundColor = styleEqual(c(""), c(""), default = "#f8d7da"))
        })
        
        #──────────── Balkendiagramm ────────────────────────────────
        output$abc_barplot <- renderPlot({
            ggplot(abc_summary, aes(x = ABC_Klasse, y = Gesamtmenge, fill = ABC_Klasse)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = formatC(Gesamtmenge, format = "f", big.mark = ".", digits = 0)),
                          vjust = -0.2, size = 5) +
                labs(title = "Gesamtmenge je ABC‑Klasse", x = "ABC‑Klasse", y = "Gesamt gelieferte Menge") +
                theme_minimal(base_size = 15) +
                scale_fill_brewer(palette = "Set2")
        })
        
        #──────────── KPI‑Vergleich ─────────────────────────────────
        output$abc_kpi_plot <- renderPlot({
            abc_summary %>%
                dplyr::select(ABC_Klasse, Anzahl_Materialien, Ø_LT_pro_Unit, Ø_Abweichung, Anteil_pünktlich) %>%
                pivot_longer(-ABC_Klasse, names_to = "Kennzahl", values_to = "Wert") %>%
                mutate(
                    Wert_anzeige = ifelse(Kennzahl == "Anteil_pünktlich",
                                          paste0(formatC(Wert * 100, format = "f", digits = 2, decimal.mark = ","), "%"),
                                          formatC(Wert, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
                    Kennzahl = recode(Kennzahl,
                                      Anzahl_Materialien = "Materialanzahl",
                                      Ø_LT_pro_Unit      = "Ø LT/Unit [s]",
                                      Ø_Abweichung       = "Ø Abweichung [s]",
                                      Anteil_pünktlich   = "Servicelevel")
                ) %>%
                ggplot(aes(x = ABC_Klasse, y = Wert, fill = ABC_Klasse)) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(aes(label = Wert_anzeige), vjust = -0.2, size = 4) +
                facet_wrap(~Kennzahl, scales = "free_y") +
                labs(
                    title = "ABC‑Klassen: Materialanzahl, LT/Unit, Ø‑Abweichung, Servicelevel",
                    x = "ABC‑Klasse", y = "Wert"
                ) +
                theme_minimal(base_size = 13) +
                scale_fill_brewer(palette = "Set2")
        })
    })
}
