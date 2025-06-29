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

source("02_model/create_workflows_overview.R", local = TRUE)
source("02_model/kpis_werke.R", local = TRUE)
source("01_transform/create_lt_unit.R", local = TRUE)


my_theme <- function(base_family = "Inter") {
    theme_minimal(base_family = base_family) +
        theme(
            # Einheitliche Schriftgröße & Farbe
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
klassifikation_ui <- fluidPage(
    
    # HEAD-Bereich mit Styles
    tags$head(
        tags$style(HTML("
      body {
        background-color: #f5f7fa;
        margin: 0;
        padding: 0;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }

      .navbar {
        width: 100%;
        display: flex;
        justify-content: space-between;
        align-items: center;
        background-color: white;
        border-bottom: 1px solid #ddd;
        padding: 1rem 2rem;
        font-weight: bold;
        font-size: 16px;
        position: relative;
      }

      .nav-tabs-custom {
        display: flex;
        gap: 32px;
        font-size: 14px;
        color: #5f6368;
        padding-top: 8px;
      }

      .nav-tabs-custom a {
        text-decoration: none;
        color: #5f6368;
        padding-bottom: 8px;
      }

      .nav-tabs-custom a.active {
        color: #1a73e8;
        font-weight: 600;
        border-bottom: 3px solid #1a73e8;
      }

      .navbar-right {
        display: flex;
        gap: 20px;
        align-items: center;
      }

      .navbar-logo {
        font-weight: 600;
        font-size: 18px;
        color: #202124;
      }

      .white-box {
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
        padding: 60px 68px;
        margin-bottom: 20px;
        width: 100%;
        min-height: 140px; 
      }

      .white-box h4 {
        font-size: 16px;
        font-weight: 600;
        margin-bottom: 20px;
        color: #202124;
      }

      table.dataTable {
        border-collapse: collapse !important;
        font-size: 12px;
      }

      table.dataTable.no-footer {
        border-bottom: none;
      }

      .dataTable th, .dataTable td {
        border: none !important;
        padding: 8px 12px !important;
      }

      table.dataTable tbody tr:hover {
        background-color: #f0f4f8 !important;
        cursor: pointer;
      }

      .stripe tbody tr:nth-child(odd) {
        background-color: #ffffff !important;
      }

      .stripe tbody tr:nth-child(even) {
        background-color: #f9fafb !important;
      }

      .dataTables_wrapper {
        border-radius: 12px;
        overflow: hidden;
      }

      .dataTable tbody td {
        border-bottom: 1px solid #e0e0e0 !important;
      }

      .selectize-input {
        padding-right: 30px !important;
      }

      .dataTables_wrapper .dataTables_paginate {
        font-size: 12px;
        margin-top: 8px;
        text-align: right;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 2px 6px;
        margin: 0 2px;
        border: 1px solid #ddd;
        border-radius: 4px;
        background-color: white;
        color: #444;
        font-size: 12px;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background-color: #e8f0fe;
        border-color: #4285f4;
        color: #1a73e8;
        font-weight: 600;
      }

      .dataTables_info {
        display: none !important;
      }
      
       /* Zentriert das Dropdown im Subheader */
  .subheader-dropdown .selectize-control.single {
    margin-top: 0 !important;
    display: flex;
    align-items: center;
  }

  .subheader-dropdown .selectize-input {
    margin: 0 !important;
    padding: 6px 10px !important;
    height: 36px !important;
    font-size: 14px;
  }

  .subheader-dropdown {
    display: flex;
    align-items: center;
    gap: 12px;
  }
    "))
    ),
    
    # NAVBAR OBEN
    div(
        style = "width: 100%; display: flex; flex-direction: column; background-color: white; border-bottom: 1px solid #ddd;",
        
        # Obere Zeile: Logo, Tabs, Icons
        div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 1rem 2rem;",
            
            # Links: Logo + Tabs
            div(style = "display: flex; align-items: center; gap: 32px;",
                div(class = "navbar-logo",
                    span(style = "color: #4285F4;", "True"),
                    span(style = "color: #EA4335;", "Time")
                ),
                div(class = "nav-tabs-custom",
                    a(id = "nav_material", href = "#", "Material"),
                    a(id = "nav_workflows", href = "#", class = "active", "Workflows"),
                    a(id = "nav_linien", href = "#", "Linien"),
                    a(id = "nav_werke", href = "#", "Werke")
                )
            ),
            
            # Rechts: Icons
            div(class = "navbar-right",
                actionButton("download_report", label = NULL, icon = icon("file-arrow-down"),
                             style = "background: none; border: none; color: #5f6368; font-size: 16px;"),
                tags$span(icon("user-circle"), style = "font-size: 20px; color: #5f6368; cursor: pointer;")
            )
        ),
        
        # Sub-Header direkt darunter (ohne Lücke)
        div(
            style = "background-color: #f1f3f4; padding: 18px 32px; height: 72px;
         display: flex; align-items: center; justify-content: space-between;
         border-top: 1px solid #e0e0e0;",
            
            # Linke Seite: Icon + Titel
            div(
                style = "display: flex; align-items: center; gap: 12px;",
                icon("arrow-right", class = NULL, style = "font-size: 20px; color: #5f6368;"),
                span(
                    style = "font-size: 20px; font-weight: 600; color: #202124;",
                    "Materialien"
                )
            ),
            
            # Rechte Seite: Linien-Auswahl + zweite Ansichtsauswahl
            div(
                style = "display: flex; align-items: center; gap: 24px;",
                
                # Linie auswählen
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
                            options = list(placeholder = ""),
                            width = "100%"
                        )
                    )
                )
            )
        )
        
    ),
    
    
    # INHALT: max-width Wrapper
    div(style = "max-width: 1100px; margin: 0 auto;",
        
        div(
            style = "padding: 48px 0 12px 0;",  # Abstand oben und unten
            uiOutput("klassifikation_title")
        ),
        
        #KPI Boxen
        fluidRow(
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_auftraege")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_servicelevel")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_bottleneck")
                )
            )
        ),
        
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "padding: 40px 32px; background-color: white;",
                    tagList(
                        
                        # Box-Überschrift
                        div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 24px;",
                            tags$strong("Performance-Übersicht", 
                                        style = "font-weight: 600; font-size: 16px; color: #202124;"),
                            tags$span(icon("circle-question"), id = "geschw_info", 
                                      style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                        ),
                        
                        # Alle 4 Donuts nebeneinander
                        div(
                            style = "display: flex; justify-content: space-between;",
                            
                            # Donut 1 – Termintreue
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_termintreue", height = "160px"),
                                div(
                                    style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                    uiOutput("termintreue_icon"),
                                    span(
                                        style = "display: flex; align-items: center; gap: 6px;",
                                        "Termintreue",
                                        tags$span(icon("circle-question"), id = "termintreue_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                    ),
                                    bsPopover(
                                        id = "termintreue_info",
                                        title = "Termintreue",
                                        content = "Der prozentuale Anteil aller Aufträge, die bis zum geplanten Liefer- oder Fertigstellungstermin abgeschlossen wurden. Frühere Fertigstellungen werden dabei ebenfalls als termingerecht gewertet.",
                                        placement = "top",
                                        trigger = "hover"
                                    )
                                )
                            ),
                            
                            # Donut 2 – Liefertreue
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_liefertreue", height = "160px"),
                                div(
                                    style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                    uiOutput("liefertreue_icon"),
                                    span(
                                        style = "display: flex; align-items: center; gap: 6px;",
                                        "Liefertreue",
                                        tags$span(icon("circle-question"), id = "liefertreue_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                    ),
                                    
                                    bsPopover(
                                        id = "liefertreue_info",
                                        title = "Liefertreue",
                                        content = "Asli Anteil der Aufträge, bei denen die gesamte Sollmenge geliefert wurde.",
                                        placement = "top",
                                        trigger = "hover"
                                    )
                                )
                            ),
                            
                            # Donut 3 – Geschwindigkeit pro ME
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_geschwindigkeit_me", height = "160px"),
                                div(
                                    style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                    uiOutput("geschwindigkeit_me_icon"),
                                    span(
                                        style = "display: flex; align-items: center; gap: 6px;",
                                        "Geschwindigkeit pro ME",
                                        tags$span(icon("circle-question"), id = "geschwindigkeit_me_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                    ),
                                    
                                    bsPopover(
                                        id = "geschwindigkeit_me_info",
                                        title = "Geschwindigkeit/ME [s]",
                                        content = "Julia",
                                        placement = "top",
                                        trigger = "hover"
                                    )
                                )
                            ),
                            
                            # Donut 4 – Geschwindigkeit pro Auftrag
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_geschwindigkeit_auftrag", height = "160px"),
                                div(
                                    style = "display: flex; justify-content: center; align-items: center; font-size: 13px; color: #555; margin-top: 4px;",
                                    uiOutput("geschwindigkeit_auftrag_icon"),
                                    span(
                                        style = "display: flex; align-items: center; gap: 6px;",
                                        "Geschwindigkeit pro Auftrag",
                                        tags$span(icon("circle-question"), id = "geschwindigkeit_auftrag_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                    ),
                                    
                                    bsPopover(
                                        id = "geschwindigkeit_auftrag_info",
                                        title = "Geschwindigkeit/Auftrag [Tage]",
                                        content = "Julia",
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
        
        
        bsPopover(
            id = "performance_vgl_info",
            title = "Was wird hier gezeigt?",
            content = "Eddi",
            placement = "right",
            trigger = "hover"
        ),
        
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
                                    id = "alerts_info",
                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                )
                            ),
                            selectInput(
                                inputId = "alert_filter",
                                label = NULL,
                                choices = c("Alle", "Servicelevel < 50 %", "Ø‑Abweichung > 60 s", "Ø‑LT > 120 s"),
                                selected = "Alle",
                                width = "220px"
                            )
                        ),
                        br(),
                        DTOutput("alert_table")
                    ),
                    bsPopover(
                        id = "alerts_info",
                        title = "Was wird hier gezeigt?",
                        content = "Materialien mit auffälligen Kennzahlen: niedrige Termintreue, hohe Abweichung oder lange Durchlaufzeit.",
                        placement = "right",
                        trigger = "hover"
                    )
                )
            )
        ),
        
        
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("abweichung_title")
                        ),
                        
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
                                                id = "abw_zeit_info",
                                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                            )
                                        ),
                                        br(),
                                        plotly::plotlyOutput("abweichung_time_plot", height = "240px"),
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_zeit_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Julia",
                                        placement = "right",
                                        trigger = "hover"
                                    ),
                                )
                            )
                        ),
                        
                        fluidRow(
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center;",
                                            span("Lead Time Abweichung absolut", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(
                                                icon("circle-question"),
                                                id = "abw_abs_info",
                                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                            )
                                        ),
                                        br(),
                                        plotly::plotlyOutput("abweichung_hist_plot", height = "240px")
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_abs_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Dieses Diagramm zeigt die Ist- und Soll-LTs in Abhängigkeit von der Sollmenge. So werden Unsicherheiten der einzelnen Workflows abhängig vom Auftragsvolumen sichtbar",
                                        placement = "right",
                                        trigger = "hover"
                                    ),
                                )
                            ),
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
                                                id = "abw_rel_info",
                                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                            )
                                        ),
                                        br(),
                                        DT::DTOutput("abweichungstabelle")
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_rel_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Julia",
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
)


#Server-------------------------------------------------------------------------
klassifikation_server <- function(input, output, session) {
    
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
    
    output$donut_termintreue <- renderEcharts4r({
        sel <- input$selected_klassifikation
        df_s <- auftraege_lt_unit %>% filter(klassifikation == sel)
        df_o <- auftraege_lt_unit %>% filter(klassifikation != sel)
        
        value <- round(mean(df_s$abweichung_unit <= 0, na.rm = TRUE) * 100, 1)
        avg   <- round(df_o %>%
                           group_by(klassifikation) %>%
                           summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE)) %>%
                           pull(rate) %>%
                           mean(na.rm = TRUE) * 100, 1)
        
        # 👑 oder ⚠️
        symbol <- if (value > avg) {
            "👑"
        } else if (value < avg) {
            "⚠️"
        } else {
            ""
        }
        
        df <- tibble::tibble(
            category = c("Termintreu", "Verspätet"),
            count = c(value, 100 - value)
        )
        
        farbe <- if (symbol == "⚠️") {
            "#E57373"  # rot
        } else if (symbol == "👑") {
            "#81C784"  # grün
        } else {
            "#cfcfcf"  # grau
        }
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", value, "%"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    output$donut_liefertreue <- renderEcharts4r({
        sel <- input$selected_klassifikation
        df_s <- auftraege_lt_unit %>% filter(klassifikation == sel)
        df_o <- auftraege_lt_unit %>% filter(klassifikation != sel)
        
        value <- round(mean(df_s$gelieferte_menge >= df_s$sollmenge, na.rm = TRUE) * 100, 1)
        avg   <- round(mean(df_o$gelieferte_menge >= df_o$sollmenge, na.rm = TRUE) * 100, 1)
        
        # Entscheidungssymbol & Farbe
        symbol <- if (value > avg) {
            "👑"
        } else if (value < avg) {
            "⚠️"
        } else {
            ""
        }
        
        tooltip_text <- if (value > avg) {
            paste0("Overperformance, durchschn. Liefertreue derzeit ", avg, "%")
        } else if (value < avg) {
            paste0("Underperformance, durchschn. Liefertreue derzeit ", avg, "%")
        } else {
            ""
        }
        
        farbe <- if (symbol == "⚠️") {
            "#E57373"
        } else if (symbol == "👑") {
            "#81C784"
        } else {
            "#cfcfcf"
        }
        
        farben <- c(farbe, "#f0f0f0")
        
        df <- tibble::tibble(
            category = c("Liefertreu", "Unvollständig"),
            count = c(value, 100 - value)
        )
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = sprintf(
                    "{a|%s} {b|%s%%}", symbol, value
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
    
    
    
    output$donut_geschwindigkeit_me <- renderEcharts4r({
        req(input$selected_klassifikation)
        
        df_sel <- auftraege_lt_unit %>% filter(klassifikation == input$selected_klassifikation, !is.na(lt_ist_order))
        df_all <- auftraege_lt_unit %>% filter(!is.na(lt_ist_order))
        
        geschw_sel <- round(mean(df_sel$lt_ist_order / 60, na.rm = TRUE), 1)
        geschw_all <- round(mean(df_all$lt_ist_order / 60, na.rm = TRUE), 1)
        
        rel_diff <- geschw_all - geschw_sel
        
        symbol <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe <- if (rel_diff > 0) {
            "#81C784"  # grün
        } else if (rel_diff < 0) {
            "#E57373"  # rot
        } else {
            "#cfcfcf"  # grau
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        prozent <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        prozent <- max(min(prozent, 100), 0)
        
        df <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(prozent, 100 - prozent)
        )
        
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", geschw_sel, " min"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    output$donut_geschwindigkeit_auftrag <- renderEcharts4r({
        req(input$selected_klassifikation)
        
        df_sel <- auftraege_lt_unit %>% filter(klassifikation == input$selected_klassifikation, !is.na(lead_time_ist))
        df_all <- auftraege_lt_unit %>% filter(!is.na(lead_time_ist))
        
        geschw_sel <- round(mean(df_sel$lead_time_ist, na.rm = TRUE), 1)
        geschw_all <- round(mean(df_all$lead_time_ist, na.rm = TRUE), 1)
        
        rel_diff <- geschw_all - geschw_sel
        
        symbol <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe <- if (rel_diff > 0) {
            "#81C784"
        } else if (rel_diff < 0) {
            "#E57373"
        } else {
            "#cfcfcf"
        }
        
        prozent <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        prozent <- max(min(prozent, 100), 0)
        
        df <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(prozent, 100 - prozent)
        )
        
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", geschw_sel, " T"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    output$abweichung_title <- renderUI({
        req(input$selected_klassifikation)
        h4(
            paste0("Ansicht Lead Time Abweichung für Material ", input$selected_klassifikation),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$performance_titel <- renderUI({
        h4(
            paste0("Ansicht Performance für Material ", input$selected_klassifikation),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$lt_title <- renderUI({
        req(input$selected_klassifikation)
        h4(
            paste("Lead Time- und Performanceübersicht Material", input$selected_klassifikation), 
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    # Mapping zwischen UI-Label und Datenspalte
    lt_map <- list(
        "Workflow" = "vorgangsfolge",
        "Werk"     = "werk",
        "Linie"    = "klassifikation",
        "Planer"   = "planer",
        "Material" = "materialnummer"
    )
    
    #Formel zur Berechnung des Modus
    modus <- function(x) {
        ux <- unique(x[!is.na(x)])
        ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    output$livetracker_auftraege <- renderUI({
        req(input$selected_klassifikation)
        
        anzahl <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            summarise(n = n_distinct(auftragsnummer)) %>%
            pull(n)
        
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                anzahl
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                "# Aufträge"
            )
        )
    })
    
    
    overall_servicelevel <- reactive({
        sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
    })
    
    
    output$livetracker_servicelevel <- renderUI({
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
        
        sl <- sum(filtered$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(filtered$auftragsnummer))
        overall_sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
        
        sl_percent <- paste0(round(sl * 100), "%")
        overall_text <- paste0("Overall Servicelevel = ", round(overall_sl * 100), "%")
        
        if (sl > overall_sl) {
            icon_tag <- "<span id='servicelevel_icon' style='font-size: 24px; color: #34a853; margin-right: 6px;'>👑</span>"
            popover_text <- paste("Overperformance |", overall_text)
        } else {
            icon_tag <- "<span id='servicelevel_icon' style='font-size: 24px; color: #ea4335; margin-right: 6px;'>⚠️</span>"
            popover_text <- paste("Underperformance |", overall_text)
        }
        
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
    
    
    output$livetracker_bottleneck <- renderUI({
        req(input$selected_klassifikation)
        
        bottleneck_info <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation, abweichung > 0) %>%
            group_by(materialnummer) %>%
            summarise(
                median_abweichung = median(abweichung, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(desc(median_abweichung)) %>%
            slice(1)
        
        wert <- if (nrow(bottleneck_info) == 0 || is.na(bottleneck_info$materialnummer)) {
            "–"
        } else {
            paste0("Material ", bottleneck_info$materialnummer, " | ",
                   round(bottleneck_info$median_abweichung, 1), " Tage")
        }
        
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                wert
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                "Bottleneck | Verzögerung absolut"
            )
        )
    })
    # Hilfsfunktion zur Alert-Klassifikation KASPAR ÄNDERN
    classify_outliers <- function(df) {
        df %>%
            mutate(
                flag_sl    = Anteil_pünktlich < 0.50,
                flag_delay = Ø_Abweichung     > 60,
                flag_lt    = Ø_LT_pro_Unit    > 120,
                Alert      = flag_sl | flag_delay | flag_lt,
                Priority   = (flag_sl * 3) + (flag_delay * 2) + flag_lt,
                Alert_Grund = (
                    paste(
                        ifelse(flag_sl,    "Servicelevel < 50 %",  ""),
                        ifelse(flag_delay, "Ø‑Abweichung > 60 s",  ""),
                        ifelse(flag_lt,    "Ø‑LT > 120 s",         ""),
                        sep = "; "
                    ) %>%
                        gsub("(^; |; $)", "", .) %>%
                        gsub("; ;", ";", .)
                )
            )
    }
    
    # Tabelle mit berechneten Kennzahlen je Material
    annotated_materials <- reactive({
        auftraege_lt_unit %>%
            group_by(materialnummer) %>%
            summarise(
                Anteil_pünktlich = mean(abweichung <= 0, na.rm = TRUE),
                Ø_Abweichung     = mean(abweichung, na.rm = TRUE),
                Ø_LT_pro_Unit    = mean(lt_ist_order, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            classify_outliers()
    })
    
    # Filterung nach ausgewähltem Alert-Grund
    filtered_alerts <- reactive({
        df <- annotated_materials()
        if (is.null(input$alert_filter) || input$alert_filter == "Alle") {
            return(df %>% filter(Alert))
        }
        df %>% filter(grepl(input$alert_filter, Alert_Grund, fixed = TRUE))
    })
    
    # Anzeige der Tabelle
    output$alert_table <- renderDT({
        df <- filtered_alerts()
        if (nrow(df) == 0) {
            return(datatable(
                data.frame(Hinweis = "Keine Materialnummern mit dem gewählten Alert-Kriterium gefunden."),
                options = list(dom = 't'),
                rownames = FALSE
            ))
        }
        
        df %>%
            arrange(desc(Alert), desc(Priority)) %>%
            transmute(
                Materialnummer       = materialnummer,
                `Ø Abweichung [s]`    = round(Ø_Abweichung, 2),
                `Ø LT/Unit [s]`       = round(Ø_LT_pro_Unit, 2),
                Servicelevel         = paste0(round(Anteil_pünktlich * 100, 2), " %"),
                Alert_Grund
            ) %>%
            datatable(
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                class = "stripe hover cell-border"
            )
    })
    
    
    
    output$abweichung_time_plot <- renderPlotly({
        req(input$selected_klassifikation)
        
        df <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            arrange(starttermin_ist) %>%
            slice(seq(1, n(), by = 10))
        
        p <- ggplot(df, aes(x = starttermin_ist, y = abweichung)) +
            geom_smooth(
                method = "loess", se = FALSE, span = 0.2, color = "#6495ED", size = 0.7
            ) +
            labs(
                x = "Ist-Starttermin",
                y = "Abweichung von Soll-LT [d]"
            ) +
            my_theme()  # 👈 hier deine Theme-Funktion
        
        ggplotly(p, tooltip = c("x", "y")) %>%
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
    
    plot_abweichung_histogram <- function(df, selected_klassifikation) {
        df_filtered <- df %>%
            filter(klassifikation == selected_klassifikation & !is.na(abweichung))
        
        if (nrow(df_filtered) == 0) return(NULL)
        
        # Dynamische Grenzen anhand 1% und 99% Quantil
        x_min <- quantile(df_filtered$abweichung, 0.025)
        x_max <- quantile(df_filtered$abweichung, 0.975)
        
        p <- ggplot(df_filtered, aes(x = abweichung)) +
            geom_histogram(binwidth = 1, fill = "#cccccc", color = "white", boundary = 0) +
            labs(
                x = "Abweichung (Ist - Soll) [Tage]",
                y = "Häufigkeit"
            ) +
            scale_x_continuous(limits = c(x_min, x_max)) +
            my_theme()  # 👈 Google-Stil hier anwenden
        
        ggplotly(p)
    }
    
    output$abweichung_hist_plot <- renderPlotly({
        req(input$selected_klassifikation)
        plot_abweichung_histogram(vorgaenge_sorted, input$selected_klassifikation)
    })
    
    abweichung_tabelle <- reactive({
        req(input$selected_klassifikation)
        
        df <- auftraege_lt_unit %>%
            filter(klassifikation == input$selected_klassifikation) %>%
            filter(!is.na(lt_ist_order), !is.na(lt_soll_order), lt_soll_order > 0) %>%
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
    
    output$abweichungstabelle <- DT::renderDT({
        abweichung_tabelle()
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

shinyApp(klassifikation_ui, klassifikation_server)
