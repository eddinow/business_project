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


my_theme <- function() {
    theme_minimal(base_family = "Inter") +
        theme(
            axis.title = element_text(size = 12, color = "#202124"),     # Dunkles Grau (fast Schwarz)
            axis.text = element_text(size = 10, color = "#5f6368"),      # Mittelgrau
            plot.caption = element_text(size = 9, color = "#9e9e9e", hjust = 1),  # Hellgrau, rechtsbündig
            plot.title = element_blank(),                                # Kein Titel im Plot
            legend.position = "none",                                    # Keine Legende
            panel.grid.major = element_line(color = "#e0e0e0", size = 0.3), # Sehr feines Raster
            panel.grid.minor = element_blank(),                          # Keine kleinen Rasterlinien
            axis.line = element_blank(),                                 # Keine Achsenlinien
            axis.ticks = element_blank(),                                # Keine Ticks
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
    "))
    ),
    
    # NAVBAR OBEN
    div(class = "navbar",
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
    
    # INHALT: max-width Wrapper
    div(style = "max-width: 1100px; margin: 0 auto;",
        
        # KPI-Boxen nebeneinander
        fluidRow(
            column(width = 4,  # Box mit "Workflows"
                   div(
                       class = "white-box",
                       style = "height: 180px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                       span(
                           style = "font-weight: 600; font-size: 32px; line-height: 1.2; color: #202124;",
                           "Workflows"
                       )
                   )
            ),
            column(width = 4, uiOutput("overall_servicelevel")),
            column(width = 4, uiOutput("ist_lt"))
        ),
        
        fluidRow(
            column(
                width = 12,
                div(
                    style = "background-color: white;
               border-radius: 12px;
               box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
               padding: 0 20px;
               margin-bottom: 20px;
               height: 70px;
               display: flex;
               align-items: center;
               justify-content: flex-start;
               max-width: 100%;",
                    div(
                        style = "width: 480px;",
                        selectInput(
                            inputId = "selected_workflow",
                            label = NULL,
                            choices = c("Workflow auswählen" = ""),
                            selected = "0010",
                            width = "100%"
                        )
                    )
                )
            )
        ),
        
        # Tabelle mit Header und Download
        fluidRow(
            column(
                width = 12,
                tags$details(
                    open = TRUE,  # oder FALSE, wenn sie standardmäßig zu sein soll
                    div(
                        class = "white-box",
                        tagList(
                            tags$summary(
                                style = "font-weight: 600; font-size: 16px; color: #202124; margin-bottom: 12px; cursor: pointer;",
                                span("Lead Time nach Workflows"),
                                tags$span(
                                    icon("circle-question"),
                                    id = "workflows_table_info",
                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            ),
                            
                            bsPopover(
                                id = "workflows_table_info",
                                title = "Was wird hier gezeigt?",
                                content = "Diese Tabelle zeigt die wichtigsten Kennzahlen aller Workflows. Bei einem Servicelevel unter 70% zeigt die Kontrollleuchte rot, bis 95% orange und über 95% grün.",
                                placement = "right",
                                trigger = "click"
                            ),
                            
                            div(
                                style = "display: flex; justify-content: flex-end; align-items: baseline; margin-bottom: 12px;",
                                div(style = "margin-right: 12px;",
                                    selectInput("sortierung_workflows", label = NULL,
                                                choices = c("Top", "Kritisch"), selected = "Top", width = "140px")
                                ),
                                downloadButton(
                                    "download_csv_workflows", label = NULL, icon = icon("download"),
                                    style = "padding: 6px 10px;"
                                )
                            ),
                            DTOutput("workflow_table")
                        )
                    )
                )
            )
        ),
        
        fluidRow(
            # Linke große Box
            column(
                width = 8,
                div(
                    style = "height: 455px;",
                    div(
                        class = "white-box",
                        style = "height: 100%;",
                        tagList(
                            div(
                                style = "display: flex; justify-content: space-between; align-items: center;",
                                div(
                                    style = "display: flex; align-items: center;",
                                    span("Quick View Verzögerungen", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                    tags$span(
                                        icon("circle-question"),
                                        id = "bottleneck_info",
                                        style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                    )
                                )
                            ),
                            
                            bsPopover(
                                id = "bottleneck_info",
                                title = "Was wird hier gezeigt?",
                                content = "Diese Tabelle zeigt, welche Werke, Linien und Planer besonders häufig Verzögerungen in der Bearbeitung verursachen.",
                                placement = "right",
                                trigger = "click"
                            ),
                            br(),
                            tabsetPanel(
                                type = "tabs",
                                tabPanel("Werke", DTOutput("detail_table_werke")),
                                tabPanel("Linien", DTOutput("detail_table_linien")),
                                tabPanel("Planer", DTOutput("detail_table_planer"))
                            )
                        )
                    )
                )
            ),
            
# Rechte Spalte
            column(
                width = 4,
                div(
                    # Box 1
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_auftraege")
                    ),
                    
                    # Box 2
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_servicelevel")
                    ),
                    
                    # Box 3 (letzte ohne margin-bottom)
                    div(
                        class = "white-box",
                        style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                        uiOutput("livetracker_bottleneck")
                    )
                )
            )
            
        )
        ,
        
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
                    updateSelectInput(
                        session,
                        inputId = "selected_workflow",
                        choices = c("Workflow auswählen" = "", workflows),
                        selected = "0010"
                    )
                })
                
                output$overall_servicelevel <- renderUI({
                    sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
                        sum(!is.na(auftraege_lt_unit$abweichung_unit))
                    
                    sl_percent <- round(sl * 100)
                    
                    farbe <- if (sl_percent < 70) {
                        "#ea4335"  # Google Rot
                    } else if (sl_percent < 95) {
                        "#fbbc04"  # Google Orange
                    } else {
                        "#34a853"  # Google Grün
                    }
                    
                    div(
                        class = "white-box",
                        div(
                            style = "display: flex; flex-direction: column; align-items: flex-start;",
                            span(
                                style = paste0("font-weight: 600; font-size: 32px; line-height: 1.2; color: ", farbe, ";"),
                                paste0(sl_percent, "%")
                            ),
                            span(
                                style = "color: #5f6368; font-size: 16px; font-weight: 400;",
                                "Servicelevel"
                            )
                        )
                    )
                })

                
                output$ist_lt <- renderUI({
                    ist <- median(auftraege_lt_unit$lt_ist_order, na.rm = TRUE)
                    
                    div(
                        class = "white-box",
                        div(
                            style = "display: flex; flex-direction: column; align-items: flex-start;",
                            span(
                                style = "font-weight: 600; font-size: 32px; line-height: 1.2; color: #202124;",
                                paste0(round(ist, 2), " s")
                            ),
                            span(
                                style = "color: #5f6368; font-size: 16px; font-weight: 400;",
                                "Ist-LT/ME"
                            )
                        )
                    )
                })
                
                #Übersichtstableau
                output$workflow_table <- renderDT({
                    df <- workflows_overview
                    
                    # Umwandlung Servicelevel in numerisch
                    df$servicelevel_numeric <- as.numeric(gsub("%", "", df$Servicelevel)) / 100
                    
                    # Sortierung basierend auf Auswahl
                    if (input$sortierung_workflows == "Top") {
                        df <- df[order(-df$servicelevel_numeric), ]
                    } else {
                        df <- df[order(df$servicelevel_numeric), ]
                    }
                    df <- df |> dplyr::select(-servicelevel_numeric)
                    datatable(
                        df,
                        escape = which(colnames(df) != "ampel"),
                        options = list(
                            pageLength = 10,          
                            lengthChange = FALSE,     
                            dom = 'tip',              
                            ordering = FALSE,
                            columnDefs = list(
                                list(visible = FALSE, targets = 0),
                                list(width = '20px', targets = 1),
                                list(orderData = 0, targets = 1),
                                list(title = "", targets = 1),
                                list(className = 'dt-center', targets = 1)
                            )
                        ),
                        rownames = FALSE,
                        class = "hover",
                        width = "100%"
                    )
                })
                
                output$download_csv_workflows <- downloadHandler(
                    filename = function() {
                        paste0("workflows_", Sys.Date(), ".csv")
                    },
                    content = function(file) {
                        df <- workflows_overview
                        df$servicelevel_numeric <- as.numeric(gsub("%", "", df$Servicelevel)) / 100
                        if (input$sortierung == "Top") {
                            df <- df[order(-df$servicelevel_numeric), ]
                        } else {
                            df <- df[order(df$servicelevel_numeric), ]
                        }
                        write.csv(df, file, row.names = FALSE)
                    }
                )
                
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
                            pageLength = 3,
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
                            pageLength = 3,
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
                            pageLength = 3,
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
                        sum(!is.na(filtered$abweichung_unit))
                    
                    sl_percent <- paste0(round(sl * 100), "%")
                    
                    farbe <- if (sl < 0.7) {
                        "#ea4335"  # Rot
                    } else if (sl < 0.95) {
                        "#fbbc04"  # Orange
                    } else {
                        "#34a853"  # Grün
                    }
                    
                    div(
                        style = "display: flex; flex-direction: column;",
                        span(style = paste0("font-weight: 600; font-size: 24px; color: ", farbe, ";"), sl_percent),
                        span("Servicelevel", style = "color: #5f6368; font-size: 14px;")
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
                            "Bottleneck"
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
                        ), color = "black", size = 4) +
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