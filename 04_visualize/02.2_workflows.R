library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(DT)
library(bsplus)
library(shinyBS)



source("02_model/create_workflows_overview.R", local = TRUE)
source("02_model/kpis_workflow_liegezeit.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)


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
workflows_ui <- fluidPage(
    
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
                icon("sitemap", class = NULL, style = "font-size: 20px; color: #5f6368;"),
                span(
                    style = "font-size: 20px; font-weight: 600; color: #202124;",
                    "Workflows"
                )
            ),
            
            # Rechte Seite: Label + Dropdown (perfekt ausgerichtet)
            div(
                style = "display: flex; align-items: center; gap: 12px;",
                
                span(
                    style = "font-size: 14px; color: #202124; font-weight: 500;",
                    "Workflow auswählen:"
                ),
                
                div(
                    style = "width: 200px;",
                    selectizeInput(
                        inputId = "selected_workflow",
                        label = NULL,
                        choices = NULL,
                        selected = "",
                        options = list(placeholder = ""),
                        width = "100%"
                    )
                )
            )
        )
    ),
    
    
    # INHALT: max-width Wrapper
    div(style = "max-width: 1100px; margin: 0 auto;",
        
        div(
            style = "padding: 48px 0 12px 0;",  # Abstand oben und unten
            uiOutput("workflow_title")
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
                    style = "min-height: 455px;",
                    
                    # Gemeinsamer Header mit Dropdown
                    div(
                        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
                        span("Quick View", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                        div(
                            style = "width: 200px;",
                            selectInput("dimension_selection", NULL,
                                        choices = c("Werk", "Linie", "Planer"),
                                        selected = "Werk")
                        )
                    ),
                    
                    # Inhalte: links Tabelle, rechts Pie
                    fluidRow(
                        column(
                            width = 6,
                            tagList(
                                bsPopover(
                                    id = "allocation_info",
                                    title = "Was wird hier gezeigt?",
                                    content = "Diese Grafik zeigt die Verteilung der Aufträge nach Werken, Linien oder Planern.",
                                    placement = "right",
                                    trigger = "click"
                                ),
                                div(
                                    style = "display: flex; align-items: center; margin-bottom: 16px;",
                                    span("Auftragsallokation", style = "font-weight: 500; font-size: 14px; color: #202124;"),
                                    tags$span(icon("circle-question"), id = "allocation_info",
                                              style = "color: #5f6368; margin-left: 8px; cursor: pointer;")
                                ),
                                echarts4rOutput("allocation_pie_shared", height = "300px")
                            )
                        ),
                        
                        column(
                            width = 6,
                            tagList(
                                bsPopover(
                                    id = "bottleneck_info",
                                    title = "Was wird hier gezeigt?",
                                    content = "Diese Tabelle zeigt, welche Werke, Linien und Planer besonders häufig Verzögerungen in der Bearbeitung verursachen.",
                                    placement = "right",
                                    trigger = "click"
                                ),
                                div(
                                    style = "display: flex; align-items: center; margin-bottom: 16px;",
                                    span("Verzögerungen/Bottlenecks", style = "font-weight: 500; font-size: 14px; color: #202124;"),
                                    tags$span(icon("circle-question"), id = "bottleneck_info",
                                              style = "color: #5f6368; margin-left: 8px; cursor: pointer;")
                                ),
                                DTOutput("delay_table_shared")
                            )
                        )
                    )
                )
            )
        ),
        
        # Neue Zeile mit zwei gleich großen Boxen
        fluidRow(
            column(
                width = 6,
                div(
                    class = "white-box",
                    tagList(
                        # Boxüberschrift mit Icon
                        div(
                            style = "display: flex; align-items: center;",
                            span("Lead Time nach Vorgang [Sek. pro ME]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                            tags$span(
                                icon("circle-question"),
                                id = "lt_vorgang_info",
                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            ),
                            
                            bsPopover(
                                id = "lt_vorgang_info",
                                title = "Was wird hier gezeigt?",
                                content = "Dieses Diagramm zeigt den Median der Ist-Lead Time und Soll-Lead Time aller Vorgangsnummern eines Workflows. Lead Times werden pro Mengeneinheit angegeben, um mengenunabhängig vergleichbar zu sein.",
                                placement = "right",
                                trigger = "click"
                            ),
                        ),
                        br(),
                        # Hier kommt dein Plot rein
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
                            style = "display: flex; align-items: center;",
                            span("Lead Time inkl. Liegezeit [Tag pro Auftrag]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                            tags$span(
                                icon("circle-question"),
                                id = "lz_info",
                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            )
                        )
                    ),
                    
                    bsPopover(
                        id = "lz_info",
                        title = "Was wird hier gezeigt?",
                        content = "Dieses Diagramm zeigt die Lead Times inklusive Liegezeiten auf Auftragsebene (Tage pro Auftrag).",
                        placement = "right",
                        trigger = "click"
                    ),
                    
                        br(),
                        plotOutput("balkenplot", height = "240px") 
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
                            span("Lead Time nach Sollmenge [Tage]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                            tags$span(
                                icon("circle-question"),
                                id = "lt_sollmenge_info",
                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            )
                        )
                    ),
                    
                    bsPopover(
                        id = "lt_sollmenge_info",
                        title = "Was wird hier gezeigt?",
                        content = "Dieses Diagramm zeigt die Ist- und Soll-LTs in Abhängigkeit von der Sollmenge. So werden Unsicherheiten der einzelnen Workflows abhängig vom Auftragsvolumen sichtbar",
                        placement = "right",
                        trigger = "click"
                    ),
                        br(),
                        plotly::plotlyOutput("workflow_plot", height = "240px")
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
                    ),
                )
            )
        )
    )
)      
            
workflows_server <- function(input, output, session) {
    
    observe({
        workflows <- unique(vorgaenge_lt_unit$vorgangsfolge)
        
        updateSelectizeInput(
            session,
            inputId = "selected_workflow",
            choices = c("Workflow auswählen" = "", workflows),
            selected = "0010",  # <- default value, bleibt so
            server = TRUE
        )
    })
    
    output$workflow_title <- renderUI({
        req(input$selected_workflow)
        
        tags$div(
            style = "margin-top: 32px; margin-bottom: 32px;",
            tags$h2(
                paste("Details | Workflow", input$selected_workflow),
                style = "font-size: 20px; font-weight: 600; color: #202124; margin: 0;"
            )
        )
    })
    
    
                # Reaktive Datenquelle, basierend auf dem aktiven Tab
                output$detail_table_werke <- renderDT({
                    req(input$selected_workflow)
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(werk) %>%
                        summarise(
                            #`Soll-LT/ME [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
                            #`Ist-LT/ME [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
                            `Avg. Delay/Unit [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Avg. Delay/Unit [s]` <= 0.5 ~ "green",
                                `Avg. Delay/Unit [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Werk = werk, `Avg. Delay/Unit [s]`)
                    
                    
                    datatable(
                        df,
                        escape = FALSE,
                        options = list(
                            pageLength = 6,
                            dom = 'tip',
                            ordering = TRUE,
                            pagingType = 'simple', 
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
                
                
                # Fertigungslinien
                output$detail_table_linien <- renderDT({
                    req(input$selected_workflow)
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(fertigungslinie) %>%
                        summarise(
                            #`Soll-LT/ME [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
                            #`Ist-LT/ME [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
                            `Avg. Delay/Unit [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Avg. Delay/Unit [s]` <= 0.5 ~ "green",
                                `Avg. Delay/Unit [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Linie = fertigungslinie, `Avg. Delay/Unit [s]`)
                    
                    
                    datatable(
                        df,
                        escape = FALSE,
                        options = list(
                            pageLength = 6,
                            dom = 'tip',
                            ordering = TRUE,
                            pagingType = 'simple', 
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
                
                # Planer
                output$detail_table_planer <- renderDT({
                    req(input$selected_workflow)
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(planer) %>%
                        summarise(
                            `Avg. Delay/Unit [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Avg. Delay/Unit [s]` <= 0.5 ~ "green",
                                `Avg. Delay/Unit [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Planer = planer, `Avg. Delay/Unit [s]`)
                    
                    datatable(
                        df,
                        escape = FALSE,
                        options = list(
                            pageLength = 6,
                            dom = 'tip',
                            ordering = TRUE,
                            pagingType = 'simple', 
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
                
                output$delay_table_shared <- renderDT({
                    req(input$selected_workflow)
                    req(input$dimension_selection)
                    
                    map <- list("Werk" = "werk", "Linie" = "fertigungslinie", "Planer" = "planer")
                    col <- map[[input$dimension_selection]]
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        mutate(delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)) %>%
                        group_by(value = .data[[col]]) %>%
                        summarise(`Avg. Delay/Unit [s]` = round(median(delay_capped, na.rm = TRUE), 2), .groups = "drop") %>%
                        mutate(
                            ampel_color = case_when(
                                `Avg. Delay/Unit [s]` <= 0.5 ~ "green",
                                `Avg. Delay/Unit [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(
                            ampel_color, ampel,
                            !!rlang::sym(input$dimension_selection) := value,
                            `Avg. Delay/Unit [s]`
                        )
                    
                    datatable(
                        df,
                        escape = FALSE,
                        options = list(
                            pageLength = 3,
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
                    req(input$selected_workflow)
                    req(input$dimension_selection)
                    
                    blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
                    column_map <- list("Werk" = "werk", "Linie" = "fertigungslinie", "Planer" = "planer")
                    selected_col <- column_map[[input$dimension_selection]]
                    
                    df <- auftraege_lt_unit %>%
                        dplyr::filter(vorgangsfolge == input$selected_workflow) %>%
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
                                         label = list(formatter = "{b}: {d}%"),
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
                
                
                output$allocation_pie <- echarts4r::renderEcharts4r({
                    req(input$selected_workflow)
                    req(input$allocation_dimension)
                    
                    # Eigene Farbtöne in Blauskala
                    blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
                    
                    # Spaltenmapping
                    column_map <- list(
                        "Werk" = "werk",
                        "Linie" = "fertigungslinie",
                        "Planer" = "planer"
                    )
                    
                    selected_col <- column_map[[input$allocation_dimension]]
                    
                    df <- auftraege_lt_unit %>%
                        dplyr::filter(vorgangsfolge == input$selected_workflow) %>%
                        dplyr::filter(!is.na(.data[[selected_col]])) %>%
                        dplyr::group_by(category = .data[[selected_col]]) %>%
                        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
                        dplyr::mutate(share = count / sum(count)) %>%
                        dplyr::arrange(desc(share))
                    
                    # Unterteilung nach Anteil
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
                    
                    # Tooltip dynamisch mit JS-Funktion
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
                                         label = list(formatter = "{b}: {d}%"),
                                         itemStyle = list(
                                             color = htmlwidgets::JS(
                                                 sprintf("function(params) {
                         let colors = %s;
                         return colors[params.dataIndex %% colors.length];
                       }", jsonlite::toJSON(blau_palette, auto_unbox = TRUE))
                                             )
                                         )
                        ) %>%
                        echarts4r::e_tooltip(formatter = tooltip_formatter)
                    echarts4r::e_legend(show = FALSE)
                })
                
                
                
                
                output$livetracker_auftraege <- renderUI({
                    req(input$selected_workflow)
                    
                    anzahl <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
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
                    req(input$selected_workflow)
                    
                    filtered <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow)
                    
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
                    req(input$selected_workflow)
                    
                    bottleneck_info <- vorgaenge_sorted %>%
                        filter(vorgangsfolge == input$selected_workflow & abweichung > 0) %>%
                        group_by(Vorgangsnummer) %>%
                        summarise(median_abweichung = median(abweichung, na.rm = TRUE), .groups = "drop") %>%
                        arrange(desc(median_abweichung)) %>%
                        slice(1)
                    
                    if (nrow(bottleneck_info) == 0) {
                        wert <- "–"
                    } else {
                        wert <- paste0(bottleneck_info$Vorgangsnummer, " | ", round(bottleneck_info$median_abweichung, 1), " Tage")
                    }
                    
                    tags$div(
                        style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
                        tags$span(
                            style = "font-weight: 600; font-size: 22px; color: #202124;",
                            wert
                        ),
                        tags$span(
                            style = "font-size: 14px; color: #5f6368;",
                            "Bottleneck | Verzögerung"
                        )
                    )
                })
                
                
                
                
                
                
                
                modus <- function(x) {
                    ux <- unique(x[!is.na(x)])
                    ux[which.max(tabulate(match(x, ux)))]
                }
                
                output$leadtime_chart <- renderPlot({
                    req(input$selected_workflow)
                    
                    lt_agg <- vorgaenge_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        group_by(Vorgangsnummer) %>%
                        summarise(
                            ist_lt = median(lt_ist_order, na.rm = TRUE),
                            soll_lt = as.numeric(modus(lt_soll_order)),
                            .groups = "drop"
                        ) |>
                        filter(!is.na(ist_lt), !is.na(soll_lt))
                    
                    ggplot(lt_agg, aes(x = factor(Vorgangsnummer))) +
                        geom_col(aes(y = ist_lt), fill = "#cccccc", width = 0.15) +
                        geom_segment(aes(
                            x = as.numeric(factor(Vorgangsnummer)) - 0.075,
                            xend = as.numeric(factor(Vorgangsnummer)) + 0.075,
                            y = soll_lt,
                            yend = soll_lt
                        ), color = "#6495ED", linewidth = 0.6) +
                        geom_text(aes(
                            y = soll_lt + max(ist_lt) * 0.05,
                            label = paste0(round(soll_lt, 2), " h")
                        ), color = "black", size = 4, family = "Inter") +
                        labs(
                            x = "Vorgangsnummer",
                            y = "Lead Time per Unit [s]",
                            caption = "Balken = Median Ist-LT | Linie = Median Soll-LT"
                        ) +
                        my_theme()
                })
                
                
                # Liegezeiten
                data_input <- reactive({
                    ausgabe_df %>% 
                        filter(!is.na(vorgangsfolge))
                })
                
                aggregated_data <- reactive({
                    req(input$selected_workflow)
                    
                    data_input() %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
                        group_by(Vorgangsnummer) %>%
                        summarise(median_istdauer = median(istdauer, na.rm = TRUE)) %>%
                        ungroup()
                })
                
                output$balkenplot <- renderPlot({
                    df_plot <- aggregated_data()
                    
                    ggplot(df_plot, aes(x = Vorgangsnummer, y = median_istdauer)) +
                        geom_col(fill = "#cccccc", width = 0.15) +
                        labs(
                            x = "Vorgang / Liegezeit",
                            y = "Ist-LT [d]"
                        ) +
                        my_theme()
                })
                
                est_plot_obj <- reactive({
                    req(input$selected_workflow)
                    create_est_lt_combined(auftraege_lt_unit, input$selected_workflow, session = session)
                })
                
                output$workflow_plot <- plotly::renderPlotly({
                    result <- est_plot_obj()
                    req(result)
                    plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
                })
                
                output$abweichung_time_plot <- renderPlotly({
                    req(input$selected_workflow)
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
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
                
                plot_abweichung_histogram <- function(df, selected_workflow) {
                    df_filtered <- df %>%
                        filter(vorgangsfolge == selected_workflow & !is.na(abweichung))
                    
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
                    req(input$selected_workflow)
                    plot_abweichung_histogram(vorgaenge_sorted, input$selected_workflow)
                })
                
                abweichung_tabelle <- reactive({
                    req(input$selected_workflow)
                    
                    df <- auftraege_lt_unit %>%
                        filter(vorgangsfolge == input$selected_workflow) %>%
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
            
            shinyApp(workflows_ui, workflows_server)