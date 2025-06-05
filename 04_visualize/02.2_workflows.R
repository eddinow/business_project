library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(DT)
library(bsplus)

source("02_model/create_workflows_overview.R", local = TRUE)

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
                            selected = "",
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
                                span("Lead Times nach Workflows"),
                                tags$span(icon("circle-question"), id = "workflows_info", 
                                          style = "color: #5f6368; margin-left: 8px;") %>%
                                    bs_embed_popover(title = "Workflows Übersicht", 
                                                     content = "Beschreibung hier...",
                                                     placement = "right", trigger = "focus")
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
                        selected = ""
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
            }
            
            shinyApp(workflows_ui, workflows_server)