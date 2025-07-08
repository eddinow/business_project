library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(DT)
library(bsplus)
library(shinyBS)
library(echarts4r)
library(plotly)
library(ggbreak)

source("02_model/create_overview_tables.R")
source("01_transform/create_lt_unit.R")
source("02_model/kpis_material.R")

#Formel zur Berechnung des Modus
modus <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
}

# Designfunktion für die Plots
app_theme <- function(base_family = "Inter") {
    theme_minimal(base_family = base_family) +
        theme(
            # Schriftgröße & Farbe
            text = element_text(family = base_family, color = "#202124"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10, color = "#5f6368"),
            legend.text = element_text(size = 10, color = "#5f6368"),
            legend.title = element_text(size = 11),
            plot.caption = element_text(size = 9, color = "#9e9e9e", hjust = 1),
            
            # Layout
            plot.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_line(color = "#e0e0e0", size = 0.3),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA)
        )
}


#UI-----------------------------------------------------------------------------
klassifikationUI <- function() {
    tagList(
        
        # Main-Header mit Navigation
        div(
            style = "background-color: #f1f3f4; padding: 18px 32px; height: 72px;
         display: flex; align-items: center; justify-content: space-between;
         border-top: 1px solid #e0e0e0;",
            
            # Icon + Titel
            div(
                style = "display: flex; align-items: center; gap: 12px;",
                icon("arrow-right", class = NULL, style = "font-size: 20px; color: #5f6368;"),
                span(
                    style = "font-size: 20px; font-weight: 600; color: #202124;",
                    "Materialien"
                )
            ),
            
            # Material-Auswahl + Ansichtsauswahl
            div(
                style = "display: flex; align-items: center; gap: 24px;",
                
                # 1. Material auswählen
                div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span(
                        style = "font-size: 14px; color: #202124; font-weight: 500;",
                        "Material auswählen:"
                    ),
                    div(
                        style = "width: 180px;",
                        selectizeInput(
                            inputId = "selected_klassifikation",
                            label = NULL,
                            choices = c("A", "B", "C"),
                            selected = "A",
                            width = "100%"
                        )
                    )
                )
            )
        ),
        
        
        # Inhalt
        div(style = "max-width: 1100px; margin: 0 auto;",
            
            div(
                style = "padding: 48px 0 12px 0;",  # Abstand oben und unten
                uiOutput("klassifikation_title")
            ),
            
            # KPI Boxen (Anzahl Aufträge, Servicelevel, Bottleneck)
            fluidRow(
                column(
                    width = 4,
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_auftraege_material")
                    )
                ),
                column(
                    width = 4,
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_servicelevel_material")
                    )
                ),
                column(
                    width = 4,
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_bottleneck_material")
                    )
                )
            ),
            
            # Performance-KPIs des ausgewählten Materials
            fluidRow(
                column(
                    width = 12,
                    div(
                        class = "white-box",
                        style = "padding: 40px 32px; background-color: white;",
                        tagList(
                            div(
                                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 24px;",
                                tags$strong("Performance-Übersicht", 
                                            style = "font-weight: 600; font-size: 16px; color: #202124;")
                            ),
                            div(
                                style = "display: flex; justify-content: space-between;",
                                
                                # 1. Termintreue
                                div(
                                    style = "text-align: center; width: 24%;",
                                    echarts4rOutput("donut_termintreue_material", height = "160px"),
                                    div(
                                        style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                        uiOutput("termintreue_icon"),
                                        span(
                                            style = "display: flex; align-items: center; gap: 6px;",
                                            "Termintreue",
                                            tags$span(icon("circle-question"), id = "termintreue_info_material", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        bsPopover(
                                            id = "termintreue_info_material",
                                            title = "Termintreue",
                                            content = "Der prozentuale Anteil aller Aufträge, die bis zum geplanten Liefer- oder Fertigstellungstermin abgeschlossen wurden. Frühere Fertigstellungen werden dabei ebenfalls als termingerecht gewertet.",
                                            placement = "top",
                                            trigger = "hover"
                                        )
                                    )
                                ),
                                
                                # 2. Liefertreue
                                div(
                                    style = "text-align: center; width: 24%;",
                                    echarts4rOutput("donut_liefertreue_material", height = "160px"),
                                    div(
                                        style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                        uiOutput("liefertreue_icon"),
                                        span(
                                            style = "display: flex; align-items: center; gap: 6px;",
                                            "Liefertreue",
                                            tags$span(icon("circle-question"), id = "liefertreue_info_material", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "liefertreue_info_material",
                                            title = "Liefertreue",
                                            content = "Anteil der Aufträge, bei denen die gesamte Sollmenge geliefert wurde.",
                                            placement = "top",
                                            trigger = "hover"
                                        )
                                    )
                                ),
                                
                                # 3. Geschwindigkeit pro ME
                                div(
                                    style = "text-align: center; width: 24%;",
                                    echarts4rOutput("donut_geschwindigkeit_me_material", height = "160px"),
                                    div(
                                        style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                        uiOutput("geschwindigkeit_me_icon"),
                                        span(
                                            style = "display: flex; align-items: center; gap: 6px;",
                                            "Geschwindigkeit pro ME",
                                            tags$span(icon("circle-question"), id = "geschwindigkeit_me_info_material", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "geschwindigkeit_me_info_material",
                                            title = "Geschwindigkeit/ME [s]",
                                            content = "Gibt an, wie viel Zeit im Schnitt pro geliefertem Stück benötigt wurde. Dadurch können Aufträge mit unterschiedlichen Mengen vergleichbar gemacht und ineffiziente Prozesse leichter erkannt werden.",
                                            placement = "top",
                                            trigger = "hover"
                                        )
                                    )
                                ),
                                
                                # 4. Geschwindigkeit pro Auftrag
                                div(
                                    style = "text-align: center; width: 24%;",
                                    echarts4rOutput("donut_geschwindigkeit_auftrag_material", height = "160px"),
                                    div(
                                        style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                        uiOutput("geschwindigkeit_auftrag_icon"),
                                        span(
                                            style = "display: flex; align-items: center; gap: 6px;",
                                            "Geschwindigkeit pro Auftrag",
                                            tags$span(icon("circle-question"), id = "geschwindigkeit_auftrag_info_material", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "geschwindigkeit_auftrag_info_material",
                                            title = "Geschwindigkeit/Auftrag [Tage]",
                                            content = "Gibt an, wie lange eine Entität im Median für die Bearbeitung eines Auftrags benötigt. Dadurch kann die typische Durchlaufzeit erkannt und von extremen Einzelfällen abgegrenzt werden.",
                                            placement = "top",
                                            trigger = "hover"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            
            # Alert-Übersicht des ausgewählten Materials mit Filterfunktion
            fluidRow(
                column(
                    width = 12,
                    div(
                        class = "white-box",
                        tagList(
                            div(
                                style = "display: flex; align-items: center; justify-content: space-between;",
                                div(
                                    style = "display: flex; align-items: center;",
                                    span("Alerts", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                    tags$span(
                                        icon("circle-question"),
                                        id = "alerts_info_material",
                                        style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                    )
                                ),
                                selectInput(
                                    inputId = "alert_filter",
                                    label = NULL,
                                    choices = c("Alle", "Servicelevel unter Durchschnitt", "Ø‑Abweichung unter Durchschnitt", "Ø‑LT unter Durchschnitt"),
                                    selected = "Alle",
                                    width = "220px"
                                )
                                
                            ),
                            br(),
                            DTOutput("alert_table")
                        ),
                        bsPopover(
                            id = "alerts_info_material",
                            title = "Was wird hier gezeigt?",
                            content = "Die Tabelle zeigt alle Materialien, die in mindestens einem der drei Bereiche unterdurchschnittlich abschneiden: Servicelevel, mittlere Abweichung zur Soll-Lead Time oder durchschnittliche Lead Time pro Einheit. Je mehr Kriterien betroffen sind, desto höher die Priorität des Alerts. So können besonders kritische Materialien schnell identifiziert werden.",
                            placement = "right",
                            trigger = "hover"
                        )
                    )
                )
            ),
            
            # Lead Time Übersicht des ausgewählten Materials (ohne Ansicht als zweite Dimension)
            fluidRow(
                column(
                    width = 12,
                    div(
                        class = "white-box",
                        style = "background-color: rgba(255, 255, 255, 0.3);",
                        tagList(
                            
                            div(
                                style = "padding: 40px 0 15px 0;",
                                uiOutput("abweichung_title_material")
                            ),
                            
                            # Lead Time Abweichung im Zeitverlauf
                            fluidRow(
                                column(
                                    width = 12,
                                    div(
                                        class = "white-box",
                                        tagList(
                                            div(
                                                style = "display: flex; align-items: center;",
                                                span("Lead Time Abweichung im Zeitverlauf [Tage]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                                tags$span(
                                                    icon("circle-question"),
                                                    id = "abw_zeit_info_material",
                                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                                )
                                            ),
                                            br(),
                                            plotly::plotlyOutput("abweichung_time_plot_material", height = "240px"),
                                        ),
                                        
                                        bsPopover(
                                            id = "abw_zeit_info_material",
                                            title = "Was wird hier gezeigt?",
                                            content = "Die zeitliche Entwicklung der Lead Time Abweichung gibt Aufschluss darüber, ob eine Entität über aufeinanderfolgende Aufträge hinweg konstanter, ungenauer oder präziser arbeitet. Aufträge sind nach Starttermin sortiert, die y-Achse zeigt die absolute Abweichung in Tagen.",
                                            placement = "right",
                                            trigger = "hover"
                                        ),
                                    )
                                )
                            ),
                            
                            fluidRow(
                                column(
                                    # Lead Time Abweichung absolut
                                    width = 6,
                                    div(
                                        class = "white-box",
                                        tagList(
                                            div(
                                                style = "display: flex; align-items: center;",
                                                span("Lead Time Abweichung absolut", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                                tags$span(
                                                    icon("circle-question"),
                                                    id = "abw_abs_info_material",
                                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                                )
                                            ),
                                            br(),
                                            plotly::plotlyOutput("abweichung_hist_plot_material", height = "240px")
                                        ),
                                        
                                        bsPopover(
                                            id = "abw_abs_info_material",
                                            title = "Was wird hier gezeigt?",
                                            content = "Da die Lead Time Abweichungen je Entität keiner einheitlichen Verteilung folgen, sind Mittelwert und Standardabweichung oft wenig aussagekräftig. Stattdessen zeigt das Histogramm die tatsächliche Häufigkeitsverteilung – begrenzt auf das 2,5. bis 97,5. Perzentil, um extreme Ausreißer auszublenden. So lassen sich typische Muster und Verzögerungstendenzen erkennen.",
                                            placement = "right",
                                            trigger = "hover"
                                        ),
                                    )
                                ),
                                
                                # Lead Time Abweichung relativ
                                column(
                                    width = 6,
                                    div(
                                        class = "white-box",
                                        style = "min-height: 410px",
                                        tagList(
                                            div(
                                                style = "display: flex; align-items: center;",
                                                span("Lead Time Abweichung relativ", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                                tags$span(
                                                    icon("circle-question"),
                                                    id = "abw_rel_info_material",
                                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                                )
                                            ),
                                            br(),
                                            DT::DTOutput("abweichungstabelle_material")
                                        ),
                                        
                                        bsPopover(
                                            id = "abw_rel_info_material",
                                            title = "Was wird hier gezeigt?",
                                            content = "Die Darstellung zeigt die prozentuale Abweichung der Ist- von der Soll-Lead Time je Auftrag. Dadurch wird sichtbar, ob Verzögerungen systematisch auftreten und in welcher Größenordnung sie relativ zur geplanten Bearbeitungszeit liegen",
                                            placement = "right",
                                            trigger = "hover"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )}

#Server-------------------------------------------------------------------------
klassifikationServer <- function(input, output, session) {
    
    # Aufträge nach A,B,C-Material filtern
    observe({
        klassifikation <- unique(auftraege_lt_unit$klassifikation)
        
        updateSelectizeInput(
            session,
            inputId = "selected_klassifikation",
            choices = klassifikation,
            selected = "A", 
            server = TRUE
        )
    })
    
    output$klassifikation_title <- renderUI({
        req(input$selected_klassifikation)
        
        tags$div(
            style = "margin-top: 32px; margin-bottom: 32px;",
            tags$h2(
                paste("Details | Material", input$selected_klassifikation),
                style = "font-size: 25px; font-weight: 600; color: #202124; margin: 0;"
            )
        )
    })
    
    # KPIs der ausgewählten Ansicht
    
    # 1. Termintreue [1]
    output$donut_termintreue_material <- renderEcharts4r({
        sel <- input$selected_klassifikation
        data_selected <- auftraege_lt_unit %>% filter(klassifikation == sel)
        data_remaining <- auftraege_lt_unit %>% filter(klassifikation != sel)
        
        # Berechne Mittelwert aller Abweichungen kleiner oder gleich 0 - Anteil pünktlich
        termintreue_selected <- round(mean(data_selected$abweichung_unit <= 0, na.rm = TRUE) * 100, 1)
        termintreue_avg   <- round(data_remaining %>%
                                       group_by(klassifikation) %>%
                                       summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE)) %>%
                                       pull(rate) %>%
                                       mean(na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol_performance_vgl <- if (termintreue_selected > termintreue_avg) {
            "👑"
        } else if (termintreue_selected < termintreue_avg) {
            "⚠️"
        } else {
            ""
        }
        
        # Ablegen der Werte zum plotten
        df_termintreue <- tibble::tibble(
            category = c("Termintreu", "Verspätet"),
            count = c(termintreue_selected, 100 - termintreue_selected)
        )
        
        farbe_performance_vgl <- if (symbol_performance_vgl == "⚠️") {
            "#E57373"  # rot
        } else if (symbol_performance_vgl == "👑") {
            "#81C784"  # grün
        } else {
            "#cfcfcf"  # grau
        }
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_termintreue %>%
            e_charts(category) %>%
            e_pie(
                count,
                radius = c("75%", "90%"),
                label = list(show = FALSE),
                itemStyle = list(
                    color = htmlwidgets::JS(sprintf(
                        "function(params) {
                        let colors = %s;
                        return colors[params.dataIndex %% colors.length];
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", termintreue_selected, "%"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    # 2. Liefertreue [2]
    output$donut_liefertreue_material <- renderEcharts4r({
        sel <- input$selected_klassifikation
        data_selected <- auftraege_lt_unit %>% filter(klassifikation == sel)
        data_remaining <- auftraege_lt_unit %>% filter(klassifikation != sel)
        
        # Berechne Mittelwert aller gelieferten Mengen des gewählten Materials und 
        # aller Materialien größer oder gleich 0 - Anteil Mengentreue
        liefertreue_selected <- round(mean(data_selected$gelieferte_menge >= data_selected$sollmenge, na.rm = TRUE) * 100, 1)
        liefertreue_avg   <- round(mean(data_remaining$gelieferte_menge >= data_remaining$sollmenge, na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol_performance_vgl <- if (liefertreue_selected > liefertreue_avg) {
            "👑"
        } else if (liefertreue_selected < liefertreue_avg) {
            "⚠️"
        } else {
            ""
        }
        
        tooltip_text <- if (liefertreue_selected > liefertreue_avg) {
            paste0("Overperformance, durchschn. Liefertreue derzeit ", liefertreue_avg, "%")
        } else if (liefertreue_selected < liefertreue_avg) {
            paste0("Underperformance, durchschn. Liefertreue derzeit ", liefertreue_avg, "%")
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (symbol_performance_vgl == "⚠️") {
            "#E57373"
        } else if (symbol_performance_vgl == "👑") {
            "#81C784"
        } else {
            "#cfcfcf"
        }
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        # Ablegen der Werte zum plotten
        df_liefertreue <- tibble::tibble(
            category = c("Liefertreu", "Unvollständig"),
            count = c(liefertreue_selected, 100 - liefertreue_selected)
        )
        
        df_liefertreue %>%
            e_charts(category) %>%
            e_pie(
                count,
                radius = c("75%", "90%"),
                label = list(show = FALSE),
                itemStyle = list(
                    color = htmlwidgets::JS(sprintf(
                        "function(params) {
                        let colors = %s;
                        return colors[params.dataIndex %% colors.length];
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = sprintf(
                    "{a|%s} {b|%s%%}", symbol_performance_vgl, liefertreue_selected
                ),
                left = "center",
                top = "center",
                textStyle = list(
                    rich = list(
                        a = list(
                            fontSize = 20,
                            fontWeight = "bold",
                            color = "#202124",
                            backgroundColor = "#ffffff",
                            borderRadius = 5,
                            padding = 1,
                            fontFamily = "Segoe UI",
                            width = 20,
                            height = 20
                        ),
                        b = list(
                            fontSize = 20,
                            fontWeight = "bold",
                            color = "#202124"
                        )
                    )
                ),
                tooltip = list(
                    show = TRUE,
                    formatter = tooltip_text
                )
            ) %>%
            e_tooltip(trigger = "item") %>%
            e_legend(show = FALSE)
    })
    
    
    # 3. Geschwindigkeit pro ME
    output$donut_geschwindigkeit_me_material <- renderEcharts4r({
        req(input$selected_klassifikation)
        
        data_selected <- auftraege_lt_unit %>% filter(klassifikation == input$selected_klassifikation, !is.na(lt_ist_order))
        data_selected_valid <- auftraege_lt_unit %>% filter(!is.na(lt_ist_order))
        
        # Berechne Mittelwert aller Istzeiten für gewähltes Material und alle Materialien und rechne in min um
        geschw_sel <- round(mean(data_selected$lt_ist_order / 60, na.rm = TRUE), 1)
        geschw_all <- round(mean(data_selected_valid$lt_ist_order / 60, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol_performance_vgl <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (rel_diff > 0) {
            "#81C784" 
        } else if (rel_diff < 0) {
            "#E57373"  
        } else {
            "#cfcfcf" 
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        donut_fill <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        donut_fill <- max(min(donut_fill, 100), 0)
        
        # Ablegen der Werte zum plotten
        df_geschwindigkeit_me <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(donut_fill, 100 - donut_fill)
        )
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_geschwindigkeit_me %>%
            e_charts(category) %>%
            e_pie(
                count,
                radius = c("75%", "90%"),
                label = list(show = FALSE),
                itemStyle = list(
                    color = htmlwidgets::JS(sprintf(
                        "function(params) {
                        let colors = %s;
                        return colors[params.dataIndex %% colors.length];
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", geschw_sel, " min"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    # 4. Geschwindigkeit pro Auftrag
    output$donut_geschwindigkeit_auftrag_material <- renderEcharts4r({
        req(input$selected_klassifikation)
        
        data_selected <- auftraege_lt_unit %>% filter(klassifikation == input$selected_klassifikation, !is.na(lead_time_ist))
        data_selected_valid <- auftraege_lt_unit %>% filter(!is.na(lead_time_ist))
        
        # Berechne den Median aller Istzeiten für gewähltes Material und für alle Materialien
        geschw_sel <- round(median(data_selected$lead_time_ist, na.rm = TRUE), 1)
        geschw_all <- round(median(data_selected_valid$lead_time_ist, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol_performance_vgl <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (rel_diff > 0) {
            "#81C784"
        } else if (rel_diff < 0) {
            "#E57373"
        } else {
            "#cfcfcf"
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        donut_fill <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        donut_fill <- max(min(donut_fill, 100), 0)
        
        # Ablegen der Werte zum plotten
        df_geschwindigkeit_auftrag <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(donut_fill, 100 - donut_fill)
        )
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_geschwindigkeit_auftrag %>%
            e_charts(category) %>%
            e_pie(
                count,
                radius = c("75%", "90%"),
                label = list(show = FALSE),
                itemStyle = list(
                    color = htmlwidgets::JS(sprintf(
                        "function(params) {
                        let colors = %s;
                        return colors[params.dataIndex %% colors.length];
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", geschw_sel, " T"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    
    output$abweichung_title_material <- renderUI({
        req(input$selected_klassifikation)
        h4(
            paste0("Ansicht Lead Time Abweichung für Material ", input$selected_klassifikation),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    
    # KPI-Boxen
    
    # 1. Anzahl Aufträge für ausgewähltes Material
    output$livetracker_auftraege_material <- renderUI({
        req(input$selected_klassifikation)
        
        anzahl_auftraege <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            summarise(n = n_distinct(auftragsnummer)) %>%
            pull(n)
        
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                anzahl_auftraege
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                "# Aufträge"
            )
        )
    })
    
    # Servicelevel gesamt
    overall_servicelevel <- reactive({
        sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
    })
    
    # 2. Servicelevel ausgewähltes Material
    output$livetracker_servicelevel_material <- renderUI({
        req(input$selected_klassifikation)
        
        filtered <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation)
        
        if (nrow(filtered) == 0) {
            return(
                div(
                    style = "display: flex; flex-direction: column;",
                    span(style = "font-weight: 600; font-size: 24px; color: #9e9e9e;", "–"),
                    span("Servicelevel", style = "color: #5f6368; font-size: 14px;")
                )
            )
        }
        
        # Ermittle den Anteil aller Abweichungen kleiner oder gleich null (zu früh oder JIT)
        sl <- sum(filtered$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(filtered$auftragsnummer))
        overall_sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
        
        sl_percent <- paste0(round(sl * 100), "%")
        overall_text <- paste0("Overall Servicelevel = ", round(overall_sl * 100), "%")
        
        # Vergleich Performance mit Gesamtperformance
        if (sl > overall_sl) {
            icon_tag <- "<span id='servicelevel_icon' style='font-size: 24px; color: #34a853; margin-right: 6px;'>👑</span>"
            popover_text <- paste("Overperformance |", overall_text)
        } else {
            icon_tag <- "<span id='servicelevel_icon' style='font-size: 24px; color: #ea4335; margin-right: 6px;'>⚠️</span>"
            popover_text <- paste("Underperformance |", overall_text)
        }
        
        # User kann avg Servicelevel sehen, wenn er über das Icon hovert
        tagList(
            HTML(paste0(
                "<div style='display: flex; align-items: center;'>",
                icon_tag,
                "<span style='font-weight: 600; font-size: 24px; color: #202124;'>", sl_percent, "</span>",
                "</div>"
            )),
            span("Servicelevel", style = "font-size: 14px; color: #5f6368; margin-top: 4px;"),
            bsPopover(
                id = "servicelevel_icon",
                title = "Servicelevel-Vergleich",
                content = popover_text,
                placement = "top",
                trigger = "hover"
            )
        )
    })
    
    # 3. Bottleneck ausgewählte Ansicht
    output$livetracker_bottleneck_material <- renderUI({
        req(input$selected_klassifikation)
        
        # Ermittle Entität mit der höchsten mittleren Abweichung (Median) unter den verspäteten Aufträgen
        bottleneck_info <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation, abweichung > 0) %>%
            group_by(materialnummer) %>%
            summarise(
                median_abweichung = median(abweichung, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(desc(median_abweichung)) %>%
            slice(1)
        
        bottleneck_wert <- if (nrow(bottleneck_info) == 0 || is.na(bottleneck_info$materialnummer)) {
            "–"
        } else {
            paste0("Material ", bottleneck_info$materialnummer, " | ",
                   round(bottleneck_info$median_abweichung, 1), " Tage")
        }
        
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                bottleneck_wert
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                "Bottleneck | Verzögerung absolut"
            )
        )
    })
    
    
# Alert-Übersicht
    
    # Funktion zur Klassifikation auffälliger Materialien auf Basis von Mittelwertvergleichen
    classify_outliers <- function(material_summary) {
        
        # Durchschnittswerte als Referenz ermitteln
        mean_servicelevel <- mean(material_summary$Anteil_pünktlich, na.rm = TRUE)
        mean_deviation    <- mean(material_summary$Ø_Abweichung,     na.rm = TRUE)
        mean_lt_per_unit  <- mean(material_summary$Ø_LT_pro_Unit,    na.rm = TRUE)
        
        material_summary |>
            mutate(
                # Prüfe, ob Material in einem KPI unter dem Mittel liegt
                is_low_sl    = Anteil_pünktlich < mean_servicelevel,
                is_low_delay = Ø_Abweichung     < mean_deviation,
                is_low_lt    = Ø_LT_pro_Unit    < mean_lt_per_unit,
                
                # Flag & Priorität (gewichtete Summe für Sortierung)
                Alert    = is_low_sl | is_low_delay | is_low_lt,
                Priority = (is_low_sl * 3) + (is_low_delay * 2) + is_low_lt,
                
                # Textbasierte Begründung für Alert
                Alert_Grund = paste(
                    ifelse(is_low_sl,    "Servicelevel unter Durchschnitt", ""),
                    ifelse(is_low_delay, "Ø‑Abweichung unter Durchschnitt",  ""),
                    ifelse(is_low_lt,    "Ø‑LT unter Durchschnitt",          ""),
                    sep = "; "
                ) |>
                    gsub("(^; |; $)", "", x = _) |>
                    gsub("; ;", ";", x = _)
            )
    }
    
    # Reaktive Berechnung: Materialien + Alert-Kennzeichnung für ausgewählte Klassifikation
    annotated_materials <- reactive({
        req(input$selected_klassifikation)
        
        materialnummer_overview |>
            filter(ABC_Klasse == input$selected_klassifikation) |>
            classify_outliers()
    })
    
    # Reaktive Filterung nach Alert-Typ (Dropdown-Auswahl)
    filtered_alerts <- reactive({
        materials_with_alerts <- annotated_materials()
        
        if (is.null(input$alert_filter) || input$alert_filter == "Alle") {
            return(materials_with_alerts |> filter(Alert))
        }
        
        materials_with_alerts |> 
            filter(grepl(input$alert_filter, Alert_Grund, fixed = TRUE))
    })
    
    # Ausgabe der Alert-Tabelle (DataTable)
    output$alert_table <- renderDT({
        alert_table_data <- filtered_alerts()
        
        if (nrow(alert_table_data) == 0) {
            return(datatable(
                data.frame(Hinweis = "Keine Materialnummern mit dem gewählten Alert-Kriterium gefunden."),
                options = list(dom = 't'),
                rownames = FALSE
            ))
        }
        
        alert_table_data |>
            arrange(desc(Alert), desc(Priority)) |>
            transmute(
                Materialnummer     = materialnummer,
                `Ø Abweichung [s]` = round(Ø_Abweichung, 2),
                `Ø LT/Unit [s]`     = round(Ø_LT_pro_Unit, 2),
                Servicelevel        = paste0(round(Anteil_pünktlich * 100, 2), " %")
            ) |>
            datatable(
                options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
                rownames = FALSE,
                class = "hover"
            )
    })
    
    
    # Übersicht Lead Time Abweichung
    
    # 1. Abweichung im Zeitverlauf
    output$abweichung_time_plot_material <- renderPlotly({
        req(input$selected_klassifikation)
        
        # Sortiere nach tatsächlichem Starttermin, aber berücksichige nur jeden 
        # 10. Wert (aus Darstellungsgründen)
        df_abw_time_plot <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            arrange(starttermin_ist) %>%
            slice(seq(1, n(), by = 10))
        
        abw_time_plot <- ggplot(df_abw_time_plot, aes(x = starttermin_ist, y = abweichung)) +
            geom_smooth(
                method = "loess", se = FALSE, span = 0.2, color = "#6495ED", size = 0.7
            ) +
            labs(
                x = "Ist-Starttermin",
                y = "Abweichung von Soll-LT [d]"
            ) +
            app_theme()
        
        ggplotly(abw_time_plot, tooltip = c("x", "y")) %>%
            layout(
                font = list(family = "Inter", size = 10, color = "#5f6368"),
                xaxis = list(
                    titlefont = list(size = 10, color = "#202124"),
                    tickfont = list(size = 10, color = "#5f6368")
                ),
                yaxis = list(
                    titlefont = list(size = 10, color = "#202124"),
                    tickfont = list(size = 10, color = "#5f6368")
                )
            )
    })
    
    # 2. Lead Time Abweichung absolut
    plot_abweichung_histogram_material <- function(df, selected_klassifikation) {
        df_filtered <- df %>%
            filter(klassifikation == selected_klassifikation & !is.na(abweichung))
        
        if (nrow(df_filtered) == 0) return(NULL)
        
        # Dynamische Grenzen ohne obere und untere 2,5% (v.a. aus Darstellungsgründen)
        x_min <- quantile(df_filtered$abweichung, 0.025)
        x_max <- quantile(df_filtered$abweichung, 0.975)
        
        abw_abs_plot <- ggplot(df_filtered, aes(x = abweichung)) +
            geom_histogram(binwidth = 1, fill = "#cccccc", color = "white", boundary = 0) +
            labs(
                x = "Lead-Time-Abweichung [Tage]",
                y = "Anzahl Aufträge"
            ) +
            scale_x_continuous(limits = c(x_min, x_max)) +
            app_theme() 
        
        ggplotly(abw_abs_plot)
    }
    
    output$abweichung_hist_plot_material <- renderPlotly({
        req(input$selected_klassifikation)
        plot_abweichung_histogram_material(vorgaenge_sorted, input$selected_klassifikation)
    })
    
    
    # 3. Lead Time Abweichung relativ
    abweichung_tabelle_material <- reactive({
        req(input$selected_klassifikation)
        
        df_abw_rel <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            filter(!is.na(lt_ist_order), !is.na(lt_soll_order), lt_soll_order > 0) %>%
            
            # Setze Abweichung ins Verhältnis zur Sollzeit und klassifiziere die Aufträge
            mutate(
                abw_rel = (lt_ist_order - lt_soll_order) / lt_soll_order,
                kategorie = case_when(
                    abw_rel >= 1       ~ "≥ 100 % über Soll",
                    abw_rel >= 0.5     ~ "50–99 % über Soll",
                    abw_rel >= 0.25    ~ "25–49 % über Soll",
                    abw_rel >= 0       ~ "0–24 % über Soll",
                    abw_rel < 0        ~ "Unter Soll"
                )
            ) %>%
            group_by(kategorie) %>%
            summarise(Anzahl = n(), .groups = "drop") %>%
            mutate(
                Kategorie = factor(kategorie, levels = c(
                    "≥ 100 % über Soll",
                    "50–99 % über Soll",
                    "25–49 % über Soll",
                    "0–24 % über Soll",
                    "Unter Soll"
                )),
                Anteil_raw = round(100 * Anzahl / sum(Anzahl))
            ) %>%
            arrange(Kategorie) %>%
            mutate(
                `Lead Time Abweichung` = paste0(
                    "<div style='display: flex; align-items: center; gap: 8px;'>",
                    "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", Anteil_raw, "</span>",
                    "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                    "<div style='width:", Anteil_raw, "%; background-color: #4285F4; height: 100%;'></div>",
                    "</div>",
                    "</div>"
                )
            ) %>%
            dplyr::select(Kategorie, `Lead Time Abweichung`)
    })
    
    output$abweichungstabelle_material <- DT::renderDT({
        abweichung_tabelle_material()
    }, 
    options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        info = FALSE
    ), 
    escape = FALSE,
    rownames = FALSE,
    class = "hover"
    )
    
}
