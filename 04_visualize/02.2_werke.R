    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)
    library(shinydashboardPlus)
    library(DT)
    library(bsplus)
    library(shinyBS)
    library(plotly)
    
    
    source("02_model/create_workflows_overview.R", local = TRUE)
    source("02_model/kpis_werke.R", local = TRUE)
    source("01_transform/create_lt_unit.R", local = TRUE)
    
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
                               "Werke"
                           )
                       )
                ),
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
                                        span("Allokation Aufträge", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                        tags$span(
                                            icon("circle-question"),
                                            id = "allocation_info",
                                            style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                        )
                                    )
                                ),
                                
                                bsPopover(
                                    id = "allocation_info",
                                    title = "Was wird hier gezeigt?",
                                    content = "Diese Tabelle zeigt die Allokation aller Aufträge auf alle vorhandenen Werke.",
                                    placement = "right",
                                    trigger = "click"
                                ),
                                br(),
                                DT::dataTableOutput("allocation_table")
                            )
                        )
                    )
                )
            ),
            
            fluidRow(
                column(
                    width = 12,
                    div(
                        style = "background-color: white;
                   border-radius: 12px;
                   box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
                   padding: 24px 32px;
                   margin-bottom: 20px;
                   height: 100px;
                   display: flex;
                   align-items: center;
                   gap: 48px;",  # Abstand zwischen Titel und Dropdown
                        
                        # Links: Fetter Titel "Werke"
                        span(
                            style = "font-weight: 700; font-size: 24px; color: #202124;",
                            "Werke"
                        ),
                        
                        # Direkt daneben: Dropdown mit Label darüber
                        div(
                            style = "display: flex; flex-direction: column; align-items: flex-start; width: 240px;",
                            span("Werk auswählen", style = "font-size: 14px; font-weight: 500; color: #5f6368; margin-bottom: 4px;"),
                            selectInput(
                                inputId = "selected_werk",
                                label = NULL,
                                choices = c("1001", "1002", "1003"),
                                selected = "1001",
                                width = "100%"
                            )
                        )
                    )
                )
            )
            ,
            
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
                                    content = "Diese Tabelle zeigt, welche Workflows, Linien und Planer besonders häufig Verzögerungen in der Bearbeitung verursachen.",
                                    placement = "right",
                                    trigger = "click"
                                ),
                                br(),
                                tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Workflows", DTOutput("detail_table_workflows")),
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
                
            ),
            
            fluidRow(
                column(
                    width = 6,
                    div(
                        class = "white-box",
                        tagList(
                            # Boxüberschrift mit Icon
                            div(
                                style = "display: flex; align-items: center;",
                                span("Workflows nach Werk", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                tags$span(
                                    icon("circle-question"),
                                    id = "werk_workflows_info",
                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                ),
                                
                                bsPopover(
                                    id = "werk_workflows_info",
                                    title = "Was wird hier gezeigt?",
                                    content = "Asli.",
                                    placement = "right",
                                    trigger = "click"
                                ),
                            ),
                            br(),
                            # Hier kommt dein Plot rein
                            #plotOutput("donut_top_vorgaenge", height = "240px")
                            #plotlyOutput("donut_top_vorgaenge", height = "240px")
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
                                span("Lead Time nach Workflows [Sek. pro ME]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                tags$span(
                                    icon("circle-question"),
                                    id = "lt_wf_info",
                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                )
                            )
                        ),
    
                        bsPopover(
                            id = "lt_wf_info",
                            title = "Was wird hier gezeigt?",
                            content = "Julia.",
                            placement = "right",
                            trigger = "click"
                        ),
    
                        br(),
                        plotOutput("leadtime_chart", height = "240px")
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
                                span("Überschrift anpassen", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                tags$span(
                                    icon("circle-question"),
                                    id = "werke_level_info",
                                    style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                                )
                            ),
                            br(),
                            DT::DTOutput("leveltable")
                        ),
                        
                        bsPopover(
                            id = "werke_level_info",
                            title = "Was wird hier gezeigt?",
                            content = "Asli",
                            placement = "right",
                            trigger = "click"
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
                        )
                    )
                )
            )
        )
    )
        
            
            
            
            werke_server <- function(input, output, session) {
                
                
                
                observe({
                    werke <- unique(auftraege_lt_unit$werk)
                    updateSelectInput(
                        session,
                        inputId = "selected_werk",
                        choices = c("Werk auswählen" = "", werke),
                        selected = "1001"
                    )
                })
                
                daten_gefiltert <- reactive({
                    req(input$selected_werk)
                    werke_overview %>%
                        filter(werk == input$selected_werk) %>%
                        arrange(desc(Anzahl)) %>%
                        mutate(Anteil_prozent = round(Anzahl / sum(Anzahl) * 100, 1)) %>%
                        slice_max(order_by = Anzahl, n = 10)
                })
                
                allocation_werke <- function(input, output, session, data) {
                    output$allocation_table <- DT::renderDataTable({
                        req(data())
                        
                        data() %>%
                            dplyr::count(werk, name = "Anzahl_Auftraege") %>%
                            dplyr::mutate(
                                Gesamt = sum(Anzahl_Auftraege),
                                Prozent = round(100 * Anzahl_Auftraege / Gesamt, 1)
                            ) %>%
                            dplyr::mutate(
                                `Auftragsanteil` = paste0(
                                    "<div style='display: flex; align-items: center; gap: 8px;'>",
                                    "<span style='color: #9e9e9e; font-size: 12px; min-width: 32px;'>", Prozent, "%</span>",
                                    "<div style='background-color: #e0e0e0; width: 100px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                                    "<div style='width:", Prozent, "%; background-color: #4285F4; height: 100%;'></div>",
                                    "</div>",
                                    "</div>"
                                )
                            ) %>%
                            dplyr::select(Werk = werk, `Auftragsanteil`) %>%
                            DT::datatable(escape = FALSE, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
                    })
                }
                
                allocation_werke(input, output, session, data = reactive({ auftraege_lt_unit }))
                
                
                # output$overall_servicelevel <- renderUI({
                #     sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
                #         sum(!is.na(auftraege_lt_unit$abweichung_unit))
                #     
                #     sl_percent <- round(sl * 100)
                #     
                #     farbe <- if (sl_percent < 70) {
                #         "#ea4335"  # Google Rot
                #     } else if (sl_percent < 95) {
                #         "#fbbc04"  # Google Orange
                #     } else {
                #         "#34a853"  # Google Grün
                #     }
                #     
                #     div(
                #         class = "white-box",
                #         div(
                #             style = "display: flex; flex-direction: column; align-items: flex-start;",
                #             span(
                #                 style = paste0("font-weight: 600; font-size: 32px; line-height: 1.2; color: ", farbe, ";"),
                #                 paste0(sl_percent, "%")
                #             ),
                #             span(
                #                 style = "color: #5f6368; font-size: 16px; font-weight: 400;",
                #                 "Servicelevel"
                #             )
                #         )
                #     )
                # })
                # 
                # 
                # output$ist_lt <- renderUI({
                #     ist <- median(auftraege_lt_unit$lt_ist_order, na.rm = TRUE)
                #     
                #     div(
                #         class = "white-box",
                #         div(
                #             style = "display: flex; flex-direction: column; align-items: flex-start;",
                #             span(
                #                 style = "font-weight: 600; font-size: 32px; line-height: 1.2; color: #202124;",
                #                 paste0(round(ist, 2), " s")
                #             ),
                #             span(
                #                 style = "color: #5f6368; font-size: 16px; font-weight: 400;",
                #                 "Ist-LT/ME"
                #             )
                #         )
                #     )
                # })
                # 
                output$detail_table_workflows <- renderDT({
                    req(input$selected_werk)
                    
                    df <- auftraege_lt_unit %>%
                        filter(werk == input$selected_werk) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(vorgangsfolge) %>%
                        summarise(
                            #`Soll-LT/ME [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
                            #`Ist-LT/ME [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
                            `Verzögerung/ME [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Verzögerung/ME [s]` <= 0.5 ~ "green",
                                `Verzögerung/ME [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Workflows = vorgangsfolge, `Verzögerung/ME [s]`)
                    
                    
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
                    req(input$selected_werk)
                    
                    df <- auftraege_lt_unit %>%
                        filter(werk == input$selected_werk) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(fertigungslinie) %>%
                        summarise(
                            #`Soll-LT/ME [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
                            #`Ist-LT/ME [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
                            `Verzögerung/ME [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Verzögerung/ME [s]` <= 0.5 ~ "green",
                                `Verzögerung/ME [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Linie = fertigungslinie, `Verzögerung/ME [s]`)
                    
                    
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
                    req(input$selected_werk)
                    
                    df <- auftraege_lt_unit %>%
                        filter(werk == input$selected_werk) %>%
                        mutate(
                            delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                        ) %>%
                        group_by(planer) %>%
                        summarise(
                            `Verzögerung/ME [s]` = round(median(delay_capped, na.rm = TRUE), 2),
                            .groups = "drop"
                        ) %>%
                        mutate(
                            ampel_color = case_when(
                                `Verzögerung/ME [s]` <= 0.5 ~ "green",
                                `Verzögerung/ME [s]` <= 2   ~ "orange",
                                TRUE                         ~ "red"
                            ),
                            ampel = paste0(
                                "<div style='color: ", ampel_color, 
                                "; font-size: 20px; text-align: center;'>&#9679;</div>"
                            )
                        ) %>%
                        dplyr::select(ampel_color, ampel, Planer = planer, `Verzögerung/ME [s]`)
                    
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
                    
                    # Vergleichswert: Durchschnitt über alle Werke
                    sl_durchschnitt <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
                        sum(!is.na(auftraege_lt_unit$abweichung_unit))
                    
                    # Servicelevel für aktuelles Werk
                    sl_werk <- sum(filtered$abweichung_unit <= 0, na.rm = TRUE) / 
                        sum(!is.na(filtered$abweichung_unit))
                    
                    sl_percent <- paste0(round(sl_werk * 100), "%")
                    
                    # Icon je nach Vergleich
                    icon_name <- if (sl_werk >= sl_durchschnitt) "arrow-up" else "arrow-down"
                    icon_color <- if (sl_werk >= sl_durchschnitt) "#34a853" else "#ea4335"
                    
                    div(
                        style = "display: flex; flex-direction: column;",
                        
                        # Icon + Wert nebeneinander
                        div(
                            style = "display: flex; align-items: center; gap: 6px;",
                            icon(icon_name, style = paste0("color:", icon_color, "; font-size: 16px;")),
                            span(style = "font-weight: 600; font-size: 24px; color: #202124;", sl_percent)
                        ),
                        
                        # Label
                        span("Servicelevel", style = "color: #5f6368; font-size: 14px;")
                    )
                })
                
                
                output$livetracker_bottleneck <- renderUI({
                    req(input$selected_werk)
                    
                    bottleneck_info <- vorgaenge_sorted %>%
                        filter(werk == input$selected_werk & abweichung > 0) %>%
                        group_by(vorgangsfolge) %>%
                        summarise(median_abweichung = median(abweichung, na.rm = TRUE), .groups = "drop") %>%
                        arrange(desc(median_abweichung)) %>%
                        slice(1)
                    
                    if (nrow(bottleneck_info) == 0) {
                        wert <- "–"
                    } else {
                        wert <- paste0(bottleneck_info$vorgangsfolge, " | ", round(bottleneck_info$median_abweichung, 1), " Tage")
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
                
                output$donut_top_vorgaenge <- renderPlotly({
                    df <- daten_gefiltert()
                    plot_ly(
                        data = df,
                        labels = ~paste0(vorgangsfolge, " (", Anteil_prozent, "%)"),
                        values = ~Anteil_prozent,
                        type = 'pie',
                        hole = 0.5,
                        textinfo = "label+percent"
                    )
                })
                
                modus <- function(x) {
                    ux <- unique(x[!is.na(x)])
                    ux[which.max(tabulate(match(x, ux)))]
                }
                
                output$leadtime_chart <- renderPlot({
                    req(input$selected_werk)
                    
                    lt_agg <- auftraege_lt_unit %>%
                        filter(werk == input$selected_werk) %>%
                        group_by(vorgangsfolge) %>%
                        summarise(
                            ist_lt = median(lt_ist_order, na.rm = TRUE),
                            soll_lt = as.numeric(modus(lt_soll_order)),
                            .groups = "drop"
                        ) |>
                        filter(!is.na(ist_lt), !is.na(soll_lt))
                    
                    ggplot(lt_agg, aes(x = factor(vorgangsfolge))) +
                        geom_col(aes(y = ist_lt), fill = "#cccccc", width = 0.15) +
                        geom_segment(aes(
                            x = as.numeric(factor(vorgangsfolge)) - 0.075,
                            xend = as.numeric(factor(vorgangsfolge)) + 0.075,
                            y = soll_lt,
                            yend = soll_lt
                        ), color = "#6495ED", linewidth = 0.6) +
                        geom_text(aes(
                            y = soll_lt + max(ist_lt) * 0.05,
                            label = paste0(round(soll_lt, 2), " h")
                        ), color = "black", size = 4) +
                        labs(
                            x = "Workflow",
                            y = "Lead Time per Unit [s]",
                            caption = "Balken = Median Ist-LT | Linie = Median Soll-LT"
                        ) +
                        my_theme()
                })
                
                werke_kpi_level <- reactive({
                    req(input$selected_werk)
    
                    # Hole den richtigen Datensatz für das ausgewählte Werk
                    df <- werke_overview %>%
                        filter(werk == input$selected_werk) %>%
                        summarise(
                            Termintreue = round(mean(Termintreue_prozent, na.rm = TRUE), 0),
                            Liefertreue = round(mean(Liefertreue_prozent, na.rm = TRUE), 0),
                            Servicelevel = round(mean(Servicelevel_prozent, na.rm = TRUE), 0)
                        ) %>%
                        tidyr::pivot_longer(
                            cols = everything(),
                            names_to = "Kategorie",
                            values_to = "Wert"
                        ) %>%
                        mutate(
                            `KPI-Darstellung` = paste0(
                                "<div style='display: flex; align-items: center; gap: 8px;'>",
                                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", Wert, "</span>",
                                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                                "<div style='width:", Wert, "%; background-color: #4285F4; height: 100%;'></div>",
                                "</div>",
                                "</div>"
                            )
                        ) %>%
                        dplyr::select(Kategorie, `KPI-Darstellung`)
                })
    
    
    
                output$leveltable <- DT::renderDT({
                    werke_kpi_level()
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
