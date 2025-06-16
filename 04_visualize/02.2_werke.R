library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(DT)
library(bsplus)
library(shinyBS)
library(echarts4r)
library(plotly)

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
werke_ui <- fluidPage(
    
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
                    a(id = "nav_linien", href = "#", "Fertigungslinien"),
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
                    "Werke"
                )
            ),
            
            # Rechte Seite: Werk-Auswahl + zweite Ansichtsauswahl
            div(
                style = "display: flex; align-items: center; gap: 24px;",
                
                # Werk auswählen
                div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span(
                        style = "font-size: 14px; color: #202124; font-weight: 500;",
                        "1. Werk auswählen:"
                    ),
                    div(
                        style = "width: 180px;",
                        selectizeInput(
                            inputId = "selected_werk",
                            label = NULL,
                            choices = NULL,
                            selected = "",
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
                            choices = c("Workflow", "Linie", "Planer", "Material"),
                            selected = "Linie",
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
            uiOutput("werk_title")
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
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("allocation_title")
                        ),
                        
                        
                        
                        fluidRow(
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Verteilung der Aufträge", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "allocation_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        echarts4rOutput("allocation_pie_shared", height = "300px")
                                    )
                                )
                            ),
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    style = "min-height: 450px;",  # <--- NEU hinzugefügt
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Aktuelle Performance", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "bottleneck_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        DTOutput("delay_table_shared")
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
                                            span("Ist- und Soll-Lead Time [Sek. pro ME]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "lt_kat_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        tags$span(
                                            icon("circle-question"),
                                            id = "lt_kat_info",
                                            style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                        ),
                                        bsPopover(
                                            id = "lt_kat_info",
                                            title = "Was wird hier gezeigt?",
                                            content = "Dieses Diagramm zeigt Median-Ist- und Soll-Lead Times je nach gewählter Kategorie.",
                                            placement = "right",
                                            trigger = "click"
                                        ),
                                        br(),
                                        plotOutput("leadtime_chart", height = "240px")
                                    )
                                )
                            ),
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Ist- und Sollmengen", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "mengen_kat_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        tags$span(
                                            icon("circle-question"),
                                            id = "mengen_kat_info",
                                            style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                        ),
                                        bsPopover(
                                            id = "mengen_kat_info",
                                            title = "Was wird hier gezeigt?",
                                            content = "Dieses Diagramm zeigt Solllmengen und gelieferte Mengen je nach gewählter Kategorie.",
                                            placement = "right",
                                            trigger = "click"
                                        ),
                                        br(),
                                        plotOutput("mengen_chart", height = "240px")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
            
            div(
                style = "padding: 48px 0 12px 0;",
                uiOutput("lt_title")
            ),
            
            fluidRow(
                
                column(
                    width = 6,
                    div(
                        class = "white-box",
                        tagList(
                            div(
                                style = "display: flex; align-items: center; margin-bottom: 16px;",
                                span("KPI-Übersicht", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                tags$span(icon("circle-question"), id = "kpi_donut_info",
                                          style = "color: #5f6368; margin-left: 8px; cursor: pointer;")
                            ),
                            bsPopover(
                                id = "kpi_donut_info",
                                title = "Was wird hier gezeigt?",
                                content = "Diese Donuts zeigen die KPI-Erfüllung für das gewählte Werk.",
                                placement = "right",
                                trigger = "click"
                            ),
                            fluidRow(
                                column(6,
                                       echarts4rOutput("kpi_donut_termintreue", height = "140px"),
                                       div(
                                           style = "text-align: center; margin-top: 6px;",
                                           span("Termintreue", style = "font-size: 13px; font-weight: 500; color: #202124;"),
                                           br(),
                                           span("Platzhaltertext für Termintreue", style = "font-size: 12px; color: #5f6368;")
                                       )
                                ),
                                column(6,
                                       echarts4rOutput("kpi_donut_liefertreue", height = "140px"),
                                       div(
                                           style = "text-align: center; margin-top: 6px;",
                                           span("Liefertreue", style = "font-size: 13px; font-weight: 500; color: #202124;"),
                                           br(),
                                           span("Platzhaltertext für Liefertreue", style = "font-size: 12px; color: #5f6368;")
                                       )
                                )
                            )
                        )
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
                            trigger = "click"
                        ),
                    )
                ),
                column(
                    width = 6,
                    div(
                        class = "white-box",
                        style = "min-height: 400px;",
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
                            trigger = "click"
                        ),
                    )
                )
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
                            trigger = "click"
                        )
                    )
                )
            )
        )
    )
    #Server-------------------------------------------------------------------------
    werke_server <- function(input, output, session) {
        
        observe({
            werk <- unique(auftraege_lt_unit$werk)
            
            updateSelectizeInput(
                session,
                inputId = "selected_werk",
                choices = c("Werk auswählen" = "", werk),
                selected = "1001", 
                server = TRUE
            )
        })
        
        output$werk_title <- renderUI({
            req(input$selected_werk)
            
            tags$div(
                style = "margin-top: 32px; margin-bottom: 32px;",
                tags$h2(
                    paste("Details | Werk", input$selected_werk),
                    style = "font-size: 25px; font-weight: 600; color: #202124; margin: 0;"
                )
            )
        })
        
        output$allocation_title <- renderUI({
            req(input$selected_werk, input$view_selection)
            h4(
                paste0("Ansicht ", input$view_selection, " für Werk ", input$selected_werk),
                style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
            )
        })
    
        output$lt_title <- renderUI({
            req(input$selected_werk)
            h4(
                paste("Lead Time- und Performanceübersicht Werk", input$selected_werk), 
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
        
        
        output$delay_table_shared <- renderDT({
            req(input$selected_werk)
            req(input$view_selection)
            
            col <- lt_map[[input$view_selection]]
            
            df <- auftraege_lt_unit %>%
                filter(werk == input$selected_werk) %>%
                mutate(delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)) %>%
                group_by(value = .data[[col]]) %>%
                summarise(`Verzögerung/ME [s]` = round(median(delay_capped, na.rm = TRUE), 2), .groups = "drop") %>%
                mutate(
                    ampel_color = case_when(
                        `Verzögerung/ME [s]` <= 0.5 ~ "green",
                        `Verzögerung/ME [s]` <= 2   ~ "orange",
                        TRUE                         ~ "red"
                    ),
                    ampel = paste0(
                        "<div style='color: ", ampel_color, "; font-size: 20px; text-align: center;'>&#9679;</div>"
                    )
                ) %>%
                dplyr::select(
                    ampel_color, ampel,
                    !!rlang::sym(input$view_selection) := value,
                    `Verzögerung/ME [s]`
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
            req(input$selected_werk)
            req(input$view_selection)
            
            blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
            column_map <- list("Workflow" = "vorgangsfolge", "Linie" = "fertigungslinie", "Planer" = "planer", "Material" = "materialnummer")
            selected_col <- column_map[[input$view_selection]]
            
            df <- auftraege_lt_unit %>%
                dplyr::filter(werk == input$selected_werk) %>%
                dplyr::filter(!is.na(.data[[selected_col]])) %>%
                dplyr::group_by(category = .data[[selected_col]]) %>%
                dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
                dplyr::mutate(share = count / sum(count)) %>%
                dplyr::arrange(desc(share))
            
            df_main <- df %>% dplyr::filter(share >= 0.05)
            df_other <- df %>% dplyr::filter(share < 0.05)
            
            if (nrow(df_other) > 0) {
                other_total <- sum(df_other$count)
                other_label <- "Weitere"
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
        if(params.name === 'Weitere') {
          return 'Weitere: %s';
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
                echarts4r::e_pie(count,
                                 radius = "65%",
                                 label = list(formatter = "{b}: {d}%", fontSize = 10),
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
                echarts4r::e_legend(show = FALSE)
        })
        
    
        output$livetracker_auftraege <- renderUI({
            req(input$selected_werk)
            
            anzahl <- auftraege_lt_unit %>%
                filter(werk == input$selected_werk) %>%
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
            req(input$selected_werk)
            
            filtered <- auftraege_lt_unit %>%
                filter(werk == input$selected_werk)
            
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
            req(input$selected_werk, input$view_selection)
            
            # Spaltenname aus vorhandenem Mapping lt_map
            selected <- lt_map[[input$view_selection]]
            label <- input$view_selection  # z. B. "Linie", "Planer" usw.
            
            bottleneck_info <- vorgaenge_sorted %>%
                filter(werk == input$selected_werk, abweichung > 0) %>%
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
        
        
        modus <- function(x) {
            ux <- unique(x[!is.na(x)])
            ux[which.max(tabulate(match(x, ux)))]
        }
        
        # Dynamischer Plot
        output$leadtime_chart <- renderPlot({
            req(input$selected_werk)
            req(input$view_selection)
            
            selected_col <- lt_map[[input$view_selection]]
            
            lt_agg <- auftraege_lt_unit %>%
                filter(
                    werk == input$selected_werk,  # <- ganz wichtig!
                    !is.na(.data[[selected_col]])
                ) %>%
                group_by(group = .data[[selected_col]]) %>%
                summarise(
                    ist_lt = median(lt_ist_order, na.rm = TRUE),
                    soll_lt = as.numeric(modus(lt_soll_order)),
                    .groups = "drop"
                ) %>%
                filter(!is.na(ist_lt), !is.na(soll_lt))
            
            ggplot(lt_agg, aes(x = factor(group))) +
                geom_col(aes(y = ist_lt), fill = "#cccccc", width = 0.15) +
                geom_segment(aes(
                    x = as.numeric(factor(group)) - 0.075,
                    xend = as.numeric(factor(group)) + 0.075,
                    y = soll_lt,
                    yend = soll_lt
                ), color = "#6495ED", linewidth = 0.6) +
                geom_text(aes(
                    y = soll_lt + max(ist_lt) * 0.05,
                    label = paste0(round(soll_lt, 2), " h")
                ), color = "black", size = 4) +
                labs(
                    x = input$view_selection,
                    y = "Lead Time per Unit [s]",
                    caption = paste("Balken = Median Ist-LT | Linie = Median Soll-LT | Werk:", input$selected_werk)
                ) +
                my_theme()
        })
        
        output$mengen_chart <- renderPlot({
            req(input$selected_werk)
            req(input$view_selection)
            
            selected_col <- lt_map[[input$view_selection]]
            
            lt_agg <- auftraege_lt_unit %>%
                filter(
                    werk == input$selected_werk,  # <- ganz wichtig!
                    !is.na(.data[[selected_col]])
                ) %>%
                group_by(group = .data[[selected_col]]) %>%
                summarise(
                    istmenge = median(gelieferte_menge, na.rm = TRUE),
                    sollmenge = median(sollmenge, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                filter(!is.na(istmenge), !is.na(sollmenge))
            
            ggplot(lt_agg, aes(x = factor(group))) +
                geom_col(aes(y = istmenge), fill = "#cccccc", width = 0.15) +
                geom_segment(aes(
                    x = as.numeric(factor(group)) - 0.075,
                    xend = as.numeric(factor(group)) + 0.075,
                    y = sollmenge,
                    yend = sollmenge
                ), color = "#6495ED", linewidth = 0.6) +
                geom_text(aes(
                    y = sollmenge + max(istmenge) * 0.05,
                    label = paste0(round(sollmenge, 2), " h")
                ), color = "black", size = 4) +
                labs(
                    x = input$view_selection,
                    y = "Mengen",
                    caption = paste("Balken = Median Istmenge | Linie = Median Sollmenge | Werk:", input$selected_werk)
                ) +
                my_theme()
        })
        
        # Dynamischer Titel über der Grafik
        output$lt_kategorie_title <- renderUI({
            req(input$view_selection)
            span(
                paste0("Lead Time nach ", input$view_selection, " [Sek. pro ME]"),
                style = "font-weight: 600; font-size: 16px; color: #202124;"
            )
        })
        
        # Reactive KPI-Werte aus werkefilter
        werke_kpi_level <- reactive({
            req(input$selected_werk)
            
            werke_overview %>%
                filter(werk == input$selected_werk) %>%
                summarise(
                    Termintreue = round(mean(Termintreue_prozent, na.rm = TRUE), 0),
                    Liefertreue = round(mean(Liefertreue_prozent, na.rm = TRUE), 0)
                )
        })
        
        kpi_for_view_selection <- reactive({
            req(input$selected_werk, input$view_selection)
            
            map <- list(
                "Workflow" = "vorgangsfolge",
                "Linie" = "fertigungslinie",
                "Planer" = "planer",
                "Material" = "materialnummer"
            )
            
            selected_col <- map[[input$view_selection]]
            
            auftraege_lt_unit %>%
                filter(
                    werk == input$selected_werk,
                    !is.na(.data[[selected_col]])
                ) %>%
                group_by(group = .data[[selected_col]]) %>%
                summarise(
                    Termintreue = mean(abweichung_unit <= 0, na.rm = TRUE),
                    Liefertreue = mean(gelieferte_menge >= sollmenge, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                summarise(
                    Termintreue = round(mean(Termintreue, na.rm = TRUE) * 100),
                    Liefertreue = round(mean(Liefertreue, na.rm = TRUE) * 100)
                )
        })
        
        
        # Donut für Termintreue
        output$kpi_donut_termintreue <- renderEcharts4r({
            value <- kpi_for_view_selection()$Termintreue
            
            tibble::tibble(name = c("Erfüllt", "Leer"),
                           value = c(value, 100 - value)) %>%
                e_charts(name) %>%
                e_pie(
                    value,
                    radius = c("75%", "90%"),
                    label = list(show = FALSE)
                ) %>%
                e_title(
                    text = paste0(value, "%"),
                    subtext = "Termintreue",
                    left = "center", top = "center",
                    textStyle = list(fontSize = 18, fontWeight = 600),
                    subtextStyle = list(fontSize = 12, color = "#5f6368")
                ) %>%
                e_legend(show = FALSE)
        })
        
        # Donut für Termintreue
        output$kpi_donut_termintreue <- renderEcharts4r({
            value <- werke_kpi_level()$Termintreue
            
            tibble::tibble(name = c("Erfüllt", "Leer"),
                           value = c(value, 100 - value)) %>%
                e_charts(name) %>%
                e_pie(
                    value,
                    radius = c("75%", "90%"),
                    label = list(show = FALSE)
                ) %>%
                e_color(c("#4285F4", "#e0e0e0")) %>%
                e_title(
                    text = paste0(value, "%"),
                    left = "center", top = "center",
                    textStyle = list(fontSize = 20, fontWeight = 600)
                ) %>%
                e_legend(show = FALSE)
        })
        
        # Donut für Liefertreue
        output$kpi_donut_liefertreue <- renderEcharts4r({
            value <- werke_kpi_level()$Liefertreue
            
            tibble::tibble(name = c("Erfüllt", "Leer"),
                           value = c(value, 100 - value)) %>%
                e_charts(name) %>%
                e_pie(
                    value,
                    radius = c("75%", "90%"),
                    label = list(show = FALSE)
                ) %>%
                e_color(c("#4285F4", "#e0e0e0")) %>%
                e_title(
                    text = paste0(value, "%"),
                    left = "center", top = "center",
                    textStyle = list(fontSize = 20, fontWeight = 600)
                ) %>%
                e_legend(show = FALSE)
        })
        
        output$abweichung_time_plot <- renderPlotly({
            req(input$selected_werk)
            
            df <- auftraege_lt_unit %>%
                filter(werk == input$selected_werk) %>%
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
        
        plot_abweichung_histogram <- function(df, selected_werk) {
            df_filtered <- df %>%
                filter(werk == selected_werk & !is.na(abweichung))
            
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
            req(input$selected_werk)
            plot_abweichung_histogram(vorgaenge_sorted, input$selected_werk)
        })
        
        abweichung_tabelle <- reactive({
            req(input$selected_werk)
            
            df <- auftraege_lt_unit %>%
                filter(werk == input$selected_werk) %>%
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
    shinyApp(werke_ui, werke_server)
    