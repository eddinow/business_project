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
fertigungslinie_ui <- fluidPage(
    
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
                icon("industry", class = NULL, style = "font-size: 20px; color: #5f6368;"),
                span(
                    style = "font-size: 20px; font-weight: 600; color: #202124;",
                    "Fertigungslinien"
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
                        "1. Fertigungslinie auswählen:"
                    ),
                    div(
                        style = "width: 180px;",
                        selectizeInput(
                            inputId = "selected_fertigungslinie",
                            label = NULL,
                            choices = NULL,
                            selected = "1",
                            options = list(placeholder = ""),
                            width = "100%"
                        )
                    )
                ),
                
                # 2. Ansicht auswählen
                div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span(
                        style = "font-size: 14px; color: #202124; font-weight: 500;",
                        "2. Ansicht auswählen:"
                    ),
                    div(
                        style = "width: 180px;",
                        selectInput(
                            inputId = "view_selection",
                            label = NULL,
                            choices = c("Workflow", "Planer", "Werk", "Material"),
                            selected = "Werk",
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
            uiOutput("fertigungslinie_title")
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
                                        content = "Eddi Anteil der Aufträge, die pünktlich zum gewünschten Liefertermin fertig wurden.",
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
                                        content = "Dieses Diagramm zeigt, bei wie vielen Aufträgen die komplette Sollmenge geliefert wurde. Liefertreue misst also, ob alle bestellten Teile vollständig angekommen sind – unabhängig vom Zeitpunkt.",
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
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("allocation_title")
                        ),
                        
                        
                        
                        fluidRow(
                            column(
                                width = 12,
                                div(
                                    class = "white-box",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Aktuelle Performance", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "performance_table_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        DTOutput("delay_table_shared"),
                                        
                                        bsPopover(
                                            id = "performance_table_info",
                                            title = "Aktuelle Performance",
                                            content = "Julia",
                                            placement = "top",
                                            trigger = "hover"
                                        ),
                                    )
                                )
                            )
                        ),
                        
                        # Zweite Zeile: Lead Time + Mengen – ebenfalls in eigenen Boxen
                        fluidRow(
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Verteilung der Aufträge", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "auftrverteilung_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "auftrverteilung_info",
                                            title = "Verteilung der Aufträge",
                                            content = "Dieses Kreisdiagramm zeigt, wie sich die Anzahl der Aufträge auf die ausgewählte Kategorie verteilt. Dadurch wird sichtbar, welche Bereiche besonders häufig oder selten im Workflow vertreten sind. So lassen sich Schwerpunkte im Workflow erkennen und Kapazitätsengpässe frühzeitig identifizieren.",
                                            placement = "top",
                                            trigger = "hover"
                                        ),
                                        
                                        echarts4rOutput("allocation_pie_shared", height = "300px")
                                    )
                                )
                            ),
                            
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    style = "min-height: 455px",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Top 200 Aufträge mit höchster Abweichung", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "topdelay_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "topdelay_info",
                                            title = "Top 200 Aufträge mit höchster Abweichung",
                                            content = "Eddi",
                                            placement = "top",
                                            trigger = "hover"
                                        ),
                                        DTOutput("delay_quartile_summary")
                                    )
                                )
                            )
                        )
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
fertigungslinie_server <- function(input, output, session) {
    
    observe({
        fertigungslinie <- unique(auftraege_lt_unit$fertigungslinie)
        
        updateSelectizeInput(
            session,
            inputId = "selected_fertigungslinie",
            choices = c("Fertigungslinie auswählen" = "", fertigungslinie),
            selected = "1001", 
            server = TRUE
        )
    })
    
    output$fertigungslinie_title <- renderUI({
        req(input$selected_fertigungslinie)
        
        tags$div(
            style = "margin-top: 32px; margin-bottom: 32px;",
            tags$h2(
                paste("Details | Fertigungslinie", input$selected_fertigungslinie),
                style = "font-size: 25px; font-weight: 600; color: #202124; margin: 0;"
            )
        )
    })
    
    output$donut_termintreue <- renderEcharts4r({
        sel <- input$selected_fertigungslinie
        df_s <- auftraege_lt_unit %>% filter(fertigungslinie == sel)
        df_o <- auftraege_lt_unit %>% filter(fertigungslinie != sel)
        
        value <- round(mean(df_s$abweichung_unit <= 0, na.rm = TRUE) * 100, 1)
        avg   <- round(df_o %>%
                           group_by(fertigungslinie) %>%
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
        sel <- input$selected_fertigungslinie
        df_s <- auftraege_lt_unit %>% filter(fertigungslinie == sel)
        df_o <- auftraege_lt_unit %>% filter(fertigungslinie != sel)
        
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
        req(input$selected_fertigungslinie)
        
        df_sel <- auftraege_lt_unit %>% filter(fertigungslinie == input$selected_fertigungslinie, !is.na(lt_ist_order))
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
        req(input$selected_fertigungslinie)
        
        df_sel <- auftraege_lt_unit %>% filter(fertigungslinie == input$selected_fertigungslinie, !is.na(lead_time_ist))
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
    
    
    output$performance_vgl <- renderUI({
        sel  <- input$selected_fertigungslinie
        df_s <- auftraege_lt_unit %>% filter(fertigungslinie == sel)
        df_o <- auftraege_lt_unit %>% filter(fertigungslinie != sel)
        
        # KPI-Werte berechnen (vereinfacht hier)
        kpis <- tibble::tibble(
            label = c("Pünktlichkeitsrate", "Ø Verzögerung (Tage)", "Ø Workflows/Auftrag", "Anzahl Aufträge"),
            value = c(
                mean(df_s$abweichung_unit <= 0, na.rm = TRUE) * 100,
                median(df_s$abweichung_unit[df_s$abweichung_unit > 0], na.rm = TRUE),
                df_s %>% mutate(ops = str_count(vorgangsfolge, "→") + 1) %>% summarise(avg = mean(ops, na.rm = TRUE)) %>% pull(avg),
                nrow(df_s)
            ),
            avg = c(
                df_o %>% group_by(fertigungslinie) %>% summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE)) %>% pull(rate) %>% mean(na.rm = TRUE) * 100,
                df_o %>% filter(abweichung_unit > 0) %>% group_by(fertigungslinie) %>% summarise(avg = median(abweichung_unit, na.rm = TRUE)) %>% pull(avg) %>% mean(na.rm = TRUE),
                df_o %>% mutate(ops = str_count(vorgangsfolge, "→") + 1) %>% summarise(avg = mean(ops, na.rm = TRUE)) %>% pull(avg),
                df_o %>% group_by(fertigungslinie) %>% summarise(n = n()) %>% pull(n) %>% mean(na.rm = TRUE)
            )
        )
        
        # Hilfsfunktion für ein KPI-Feld
        kpi_box <- function(value, avg, label) {
            diff <- value - avg
            icon <- if (round(diff, 1) > 0) {
                "<span style='color:green;font-size:24px'>&uarr;</span>"
            } else if (round(diff, 1) < 0) {
                "<span style='color:red;font-size:24px'>&darr;</span>"
            } else {
                "<span style='color:black;font-size:24px'>&rarr;</span>"
            }
            
            div(style = "
        background:white;
        border:1px solid #e0e0e0;
        border-radius:10px;
        padding:15px;
        margin:5px;
        text-align:center;
        width: 23%;
        box-shadow: 0px 2px 5px rgba(0,0,0,0.05);
        ",
                HTML(icon),
                div(style = "font-size:20px;font-weight:bold;margin-top:5px;", sprintf("%.1f", value)),
                div(style = "font-size:13px;color:#555;margin-top:2px;", label)
            )
        }
        
        # Vier Boxen nebeneinander anzeigen
        fluidRow(
            lapply(1:4, function(i) {
                column(
                    width = 3,
                    kpi_box(kpis$value[i], kpis$avg[i], kpis$label[i])
                )
            })
        )
    })
    
    
    
    output$allocation_title <- renderUI({
        req(input$selected_fertigungslinie, input$view_selection)
        h4(
            paste0("Ansicht ", input$view_selection, " für Fertigungslinie ", input$selected_fertigungslinie),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$abweichung_title <- renderUI({
        req(input$selected_fertigungslinie, input$view_selection)
        h4(
            paste0("Ansicht Lead Time Abweichung für Fertigungslinie ", input$selected_fertigungslinie),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$performance_titel <- renderUI({
        h4(
            paste0("Ansicht Performance für Fertigungslinie ", input$selected_fertigungslinie),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$lt_title <- renderUI({
        req(input$selected_fertigungslinie)
        h4(
            paste("Lead Time- und Performanceübersicht Fertigungslinie", input$selected_fertigungslinie), 
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    # Mapping zwischen UI-Label und Datenspalte
    lt_map <- list(
        "Workflow" = "vorgangsfolge",
        "Werk"     = "werk",
        "Linie"    = "fertigungslinie",
        "Planer"   = "planer",
        "Material" = "materialnummer"
    )
    
    #Formel zur Berechnung des Modus
    modus <- function(x) {
        ux <- unique(x[!is.na(x)])
        ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    output$delay_table_shared <- renderDT({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        col <- lt_map[[input$view_selection]]
        
        df <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie) %>%
            filter(if (input$view_selection == "Material") klassifikation == "A" else TRUE) %>%
            mutate(delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)) %>%
            group_by(value = .data[[col]]) %>%
            summarise(
                `Ist-LT [s/ME]` = round(median(lt_ist_order, na.rm = TRUE), 2),
                `Soll-LT [s/ME]` = round(as.numeric(modus(lt_soll_order)), 2),
                `Verzögerung [s/ME]` = round(pmax(`Ist-LT [s/ME]` - `Soll-LT [s/ME]`, 0), 2),
                `# Aufträge` = n(),
                .groups = "drop"
            ) %>%
            mutate(
                ampel_color = case_when(
                    `Verzögerung [s/ME]` <= 0.5 ~ "green",
                    `Verzögerung [s/ME]` <= 2   ~ "orange",
                    TRUE                         ~ "red"
                ),
                ampel = paste0(
                    "<div style='color: ", ampel_color, "; font-size: 20px; text-align: center;'>&#9679;</div>"
                )
            ) %>%
            dplyr::select(
                ampel_color, ampel,
                !!rlang::sym(input$view_selection) := value,
                `Verzögerung [s/ME]`,
                `Ist-LT [s/ME]`,
                `Soll-LT [s/ME]`,
                `# Aufträge`
            )
        
        datatable(
            df,
            escape = FALSE,
            options = list(
                pageLength = 6,
                dom = 'tip',
                ordering = TRUE,
                columnDefs = list(
                    list(visible = FALSE, targets = 0),
                    list(width = '25px', targets = 1),
                    list(orderData = 0, targets = 1),
                    list(title = "", targets = 1)
                )
            ),
            rownames = FALSE,
            class = "hover"
        )
    })
    
    
    output$allocation_pie_shared <- renderEcharts4r({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
        selected_col <- lt_map[[input$view_selection]]
        
        df <- auftraege_lt_unit %>%
            dplyr::filter(fertigungslinie == input$selected_fertigungslinie) %>%
            dplyr::filter(if (input$view_selection == "Material") klassifikation == "A" else TRUE) %>%
            dplyr::filter(!is.na(.data[[selected_col]])) %>%
            dplyr::group_by(category = .data[[selected_col]]) %>%
            dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(share = count / sum(count)) %>%
            dplyr::arrange(desc(share))
        
        df_main <- df %>% dplyr::filter(share >= 0.05)
        df_other <- df %>% dplyr::filter(share < 0.05)
        
        if (nrow(df_other) > 0) {
            other_total <- sum(df_other$count)
            other_label <- "Restliche"
            other_tooltip <- paste(df_other$category, collapse = ", ")
            
            df_main <- dplyr::bind_rows(
                df_main,
                tibble::tibble(category = other_label, count = other_total, share = other_total / sum(df$count))
            )
        } else {
            other_tooltip <- NULL
        }
        
        tooltip_formatter <- if (!is.null(other_tooltip)) {
            htmlwidgets::JS(sprintf(
                "function(params) {
         if(params.name === 'Restliche') {
           return 'Restliche: %s';
         } else {
           return params.name + ': ' + params.value;
         }
       }", other_tooltip
            ))
        } else {
            htmlwidgets::JS("function(params) { return params.name + ': ' + params.value; }")
        }
        
        df_main %>%
            echarts4r::e_charts(category) %>%
            echarts4r::e_pie(
                count,
                radius = "65%",
                label = list(
                    show = TRUE,
                    formatter = "{d}%", 
                    fontSize = 10
                ),
                itemStyle = list(
                    color = htmlwidgets::JS(
                        sprintf("function(params) {
            let colors = %s;
            return colors[params.dataIndex %% colors.length];
          }", jsonlite::toJSON(blau_palette, auto_unbox = TRUE))
                    )
                )
            ) %>%
            echarts4r::e_tooltip(formatter = tooltip_formatter) %>%
            echarts4r::e_legend(
                show = TRUE,
                orient = "horizontal",
                bottom = 0,
                textStyle = list(fontSize = 10)
            )
    })
    
    
    
    
    output$livetracker_auftraege <- renderUI({
        req(input$selected_fertigungslinie)
        
        anzahl <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie) %>%
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
        req(input$selected_fertigungslinie)
        
        filtered <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie)
        
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
        req(input$selected_fertigungslinie, input$view_selection)
        
        # Spaltenname aus vorhandenem Mapping lt_map
        selected <- lt_map[[input$view_selection]]
        label <- input$view_selection  
        
        bottleneck_info <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie, abweichung > 0) %>%
            filter(if (input$view_selection == "Material") klassifikation == "A" else TRUE) %>%
            filter(!is.na(.data[[selected]])) %>%
            group_by(group = .data[[selected]]) %>%
            summarise(
                median_abweichung = median(abweichung, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(desc(median_abweichung)) %>%
            slice(1)
        
        if (nrow(bottleneck_info) == 0 || is.na(bottleneck_info$group)) {
            wert <- "–"
        } else {
            wert <- paste0(label, " ", bottleneck_info$group, " | ", round(bottleneck_info$median_abweichung, 1), " Tage")
        }
        
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                wert
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                paste("Bottleneck | Verzögerung absolut")
            )
        )
    })
    
    output$top_delay_orders <- renderDT({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        df <- auftraege_lt_unit %>%
            filter(
                fertigungslinie == input$selected_fertigungslinie,
                !is.na(abweichung_unit)
            ) %>%
            filter(if (input$view_selection == "Material") klassifikation == "A" else TRUE) %>%
            arrange(desc(abweichung_unit)) %>%
            slice_head(n = 200) %>%
            transmute(
                `Auftragsnummer`     = auftragsnummer,
                `Abweichung [min/ME]`  = round(abweichung_unit, 2)/60
            )
        
        datatable(
            df,
            options = list(
                pageLength = 10,
                dom = 'tip',
                ordering = TRUE
            ),
            rownames = FALSE,
            class = "hover"
        )
    })
    
    ### 1. Neue Tabelle mit Verzögerungsbereichen erzeugen
    output$delay_quartile_summary <- renderDT({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        selected_col <- lt_map[[input$view_selection]]
        
        df <- auftraege_lt_unit %>%
            filter(
                fertigungslinie == input$selected_fertigungslinie,
                abweichung > 0,
                !is.na(abweichung),
                !is.na(.data[[selected_col]]),
                if (input$view_selection == "Material") klassifikation == "A" else TRUE
            )
        
        labels <- c("> 10", "10 bis 5", "5 bis 3", "3 bis 1")
        counts <- c(
            sum(df$abweichung > 10),
            sum(df$abweichung <= 10 & df$abweichung > 5),
            sum(df$abweichung <= 5 & df$abweichung > 3),
            sum(df$abweichung <= 3 & df$abweichung > 1)
        )
        pcts <- round(counts / sum(counts) * 100, 1)
        
        summary_df <- tibble(
            `Verzögerung [T]` = labels,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", pcts, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", pcts, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_q_10", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_105", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_53", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_31", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_df,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE,
                drawCallback = JS(
                    "function(settings){",
                    "  Shiny.unbindAll(this.api().table().node());",
                    "  Shiny.bindAll(this.api().table().node());",
                    "}"
                )
            )
        )
    })
    
    
    observeEvent(input$btn_q_10, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung > 10 Tage",
            DTOutput("modal_q10"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q10 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung > 10) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_105, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 5 und 10 Tagen",
            DTOutput("modal_q105"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q105 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit <= 10 & abweichung_unit > 5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_53, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 3 und 5 Tagen",
            DTOutput("modal_q53"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q53 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit <= 5 & abweichung_unit > 3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_31, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 1 und 3 Tagen",
            DTOutput("modal_q31"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q31 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit <= 3 & abweichung_unit > 1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    
    output$top_early_orders <- renderDT({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        df <- auftraege_lt_unit %>%
            filter(
                fertigungslinie == input$selected_fertigungslinie,
                !is.na(abweichung_unit),
                abweichung_unit < 0  # nur zu frühe
            ) %>%
            arrange(abweichung_unit) %>%  # früheste oben
            slice_head(n = 200) %>%
            transmute(
                `Auftrag`     = auftragsnummer,
                `Mat.`     = materialnummer,
                `Abweichung [min/ME]`  = round(abweichung_unit, 2)/60
            )
        
        datatable(
            df,
            options = list(
                pageLength = 10,
                dom = 'tip',
                ordering = TRUE
            ),
            rownames = FALSE,
            class = "hover"
        )
    })
    
    output$early_quartile_summary <- renderDT({
        req(input$selected_fertigungslinie)
        req(input$view_selection)
        
        selected_col <- lt_map[[input$view_selection]]
        
        df <- auftraege_lt_unit %>%
            filter(
                fertigungslinie == input$selected_fertigungslinie,
                abweichung_unit < 0,
                !is.na(abweichung_unit),
                !is.na(.data[[selected_col]])
            )
        
        labels <- c("< -10", "-10 bis -5", "-5 bis -3", "-3 bis -1")
        counts <- c(
            sum(df$abweichung_unit < -10),
            sum(df$abweichung_unit >= -10 & df$abweichung_unit < -5),
            sum(df$abweichung_unit >= -5 & df$abweichung_unit < -3),
            sum(df$abweichung_unit >= -3 & df$abweichung_unit < -1)
        )
        pcts <- round(counts / sum(counts) * 100, 1)
        
        summary_df <- tibble(
            `Verfrühung [T]` = labels,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", pcts, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", pcts, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_e_10", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_105", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_53", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_31", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_df,
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE,
                drawCallback = JS(
                    "function(settings){",
                    "  Shiny.unbindAll(this.api().table().node());",
                    "  Shiny.bindAll(this.api().table().node());",
                    "}"
                )
            )
        )
    })
    observeEvent(input$btn_e_10, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung < -10 Tage",
            DTOutput("modal_e10"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e10 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit < -10) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_105, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -10 und -5 Tagen",
            DTOutput("modal_e105"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e105 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit >= -10 & abweichung_unit < -5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_53, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -5 und -3 Tagen",
            DTOutput("modal_e53"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e53 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit >= -5 & abweichung_unit < -3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_31, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -3 und -1 Tagen",
            DTOutput("modal_e31"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e31 <- renderDT({
            req(input$selected_fertigungslinie)
            
            df <- auftraege_lt_unit %>%
                filter(fertigungslinie == input$selected_fertigungslinie, abweichung_unit >= -3 & abweichung_unit < -1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [s/ME]`   = round(lt_soll_order, 2),
                    `Ist-LT [s/ME]`    = round(lt_ist_order, 2),
                    `Abweichung [s/ME]` = round(abweichung_unit, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    output$abweichung_time_plot <- renderPlotly({
        req(input$selected_fertigungslinie)
        
        df <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie) %>%
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
    
    plot_abweichung_histogram <- function(df, selected_fertigungslinie) {
        df_filtered <- df %>%
            filter(fertigungslinie == selected_fertigungslinie & !is.na(abweichung))
        
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
        req(input$selected_fertigungslinie)
        plot_abweichung_histogram(vorgaenge_sorted, input$selected_fertigungslinie)
    })
    
    abweichung_tabelle <- reactive({
        req(input$selected_fertigungslinie)
        
        df <- auftraege_lt_unit %>%
            filter(fertigungslinie == input$selected_fertigungslinie) %>%
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

shinyApp(fertigungslinie_ui, fertigungslinie_server)
