# Initialize -------------------------------------------------------------------
rm(list = ls())
set.seed(1)
setwd("~/Documents/R Projekte/R Tutorial")

# Packages ---------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(bsplus)

source("02_model/create_workflows_overview.R", local = TRUE)

# UI -----------------------------------------------------------------------------
start_ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      .navbar {
        width: 100%;
        display: flex;
        align-items: center;
        background-color: white;
        border-bottom: 1px solid #ddd;
        padding: 1rem 2rem;
        font-weight: bold;
        font-size: 16px;
        position: relative;
      }
      .navbar-logo {
        font-weight: 600;
        font-size: 18px;
        color: #202124;
        flex: 0 0 auto;
      }
      .nav-tabs-custom {
        display: flex;
        flex: 1;
        justify-content: center;
        gap: 32px;
        font-size: 14px;
        color: #5f6368;
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
        flex: 0 0 auto;
        gap: 20px;
        align-items: center;
      }
      body {
        background-color: #f5f7fa;
        margin: 0;
        padding: 0;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .white-box {
        background-color: white;
        border-radius: 12px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.05);
        padding: 60px 68px;
        margin-bottom: 20px;
        width: 100%;
      }
      table.dataTable {
        border-collapse: collapse !important;
        border-spacing: 0;
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
    
    # Navbar ----------------------------------------------------------------------
    div(class = "navbar",
        div(class = "navbar-logo",
            span(style = "color: #4285F4;", "True"),
            span(style = "color: #EA4335;", "Time")
        ),
        div(class = "nav-tabs-custom",
            a(id = "nav_material", href = "#", "Material"),
            a(id = "nav_workflows", href = "#", class = "active", "Workflows"),
            a(id = "nav_linien", href = "#", "Fertigungslinien"),
            a(id = "nav_werke", href = "#", "Werke"),
            a(id = "nav_planer", href = "#", "Planer")
        ),
        div(class = "navbar-right",
            actionButton("download_report", label = NULL, icon = icon("file-arrow-down"),
                         style = "background: none; border: none; color: #5f6368; font-size: 16px;"),
            tags$span(icon("user-circle"), style = "font-size: 20px; color: #5f6368; cursor: pointer;")
        )
    ),
    
    # Main Container --------------------------------------------------------------
    div(style = "max-width: 1100px; margin: 0 auto;",
        fluidRow(
            column(6, uiOutput("overall_servicelevel")),
            column(6, uiOutput("soll_lt"))
        ),
        fluidRow(
            column(6, uiOutput("overall_delay")),
            column(6, uiOutput("ist_lt"))
        ),
        
        # Workflows Box ------------------------------------------------------------
        fluidRow(
            column(12,
                   div(class = "white-box",
                       tagList(
                           div(style = "display: flex; justify-content: space-between; align-items: center;",
                               div(style = "display: flex; align-items: center;",
                                   span("Lead Times nach Workflows",
                                        style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                   tags$span(icon("circle-question"), id = "workflows_info",
                                             style = "color: #5f6368; margin-left: 8px;") %>%
                                       bs_embed_popover(
                                           title   = "Workflows Übersicht",
                                           content = "Sie können für einzelne Workflows (Abfolge an Vorgangsnummern eines Auftrags) die Median-Lead Times pro gefertigtem Stück sehen. Durch diese Rate kann die Lead Time Performance trotz unterschiedlicher Durchlaufvolumina der Workflows verglichen werden. Der Servicelevel gibt den Anteil an Aufträgen an, die zu früh oder rechtzeitig fertig wurden.",
                                           placement = "right", trigger = "focus"
                                       )
                               ),
                               div(style = "display: flex; align-items: center;",
                                   div(style = "margin-right: 12px;",
                                       selectInput("sortierung_workflows", label = NULL,
                                                   choices = c("Top", "Kritisch"),
                                                   selected = "Top", width = "140px")
                                   ),
                                   downloadButton("download_csv_workflows", label = NULL,
                                                  icon = icon("download"),
                                                  style = "padding: 6px 10px; margin-top: -16px;")
                               )
                           ),
                           br(),
                           DTOutput("workflow_table")
                       )
                   )
            )
        ),
        
        # Linien Box ---------------------------------------------------------------
        fluidRow(
            column(12,
                   div(class = "white-box",
                       tagList(
                           div(style = "display: flex; justify-content: space-between; align-items: center;",
                               div(style = "display: flex; align-items: center;",
                                   span("Lead Times nach Fertigungslinien",
                                        style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                   tags$span(icon("circle-question"), id = "linien_info",
                                             style = "color: #5f6368; margin-left: 8px;") %>%
                                       bs_embed_popover(
                                           title   = "Workflows Übersicht",
                                           content = "Sie können für einzelne Fertigungslinien die Median-Lead Times pro gefertigtem Stück sehen. Durch diese Rate kann die Lead Time Performance trotz unterschiedlicher Durchlaufvolumina der Workflows verglichen werden. Der Servicelevel gibt den Anteil an Aufträgen an, die zu früh oder rechtzeitig fertig wurden.",
                                           placement = "right", trigger = "focus"
                                       )
                               ),
                               div(style = "display: flex; align-items: center;",
                                   div(style = "margin-right: 12px;",
                                       selectInput("sortierung_linien", label = NULL,
                                                   choices = c("Top", "Kritisch"),
                                                   selected = "Top", width = "140px")
                                   ),
                                   downloadButton("download_csv_linien", label = NULL,
                                                  icon = icon("download"),
                                                  style = "padding: 6px 10px; margin-top: -16px;")
                               )
                           ),
                           br(),
                           DTOutput("linien_table")
                       )
                   )
            )
        ),
        
        # Werke Box ---------------------------------------------------------------
        fluidRow(
            column(12,
                   div(class = "white-box",
                       tagList(
                           div(style = "display: flex; justify-content: space-between; align-items: center;",
                               div(style = "display: flex; align-items: center;",
                                   span("Lead Times nach Werken",
                                        style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                   tags$span(icon("circle-question"), id = "werke_info",
                                             style = "color: #5f6368; margin-left: 8px;") %>%
                                       bs_embed_popover(
                                           title   = "Werke Übersicht",
                                           content = "Sie können für einzelne Werke die Median-Lead Times pro gefertigtem Stück sehen. Durch diese Rate kann die Lead Time Performance trotz unterschiedlicher Durchlaufvolumina der Workflows verglichen werden. Der Servicelevel gibt den Anteil an Aufträgen an, die zu früh oder rechtzeitig fertig wurden.",
                                           placement = "right", trigger = "focus"
                                       )
                               ),
                               div(style = "display: flex; align-items: center;",
                                   div(style = "margin-right: 12px;",
                                       selectInput("sortierung_werke", label = NULL,
                                                   choices = c("Top", "Kritisch"),
                                                   selected = "Top", width = "140px")
                                   ),
                                   downloadButton("download_csv_werke", label = NULL,
                                                  icon = icon("download"),
                                                  style = "padding: 6px 10px; margin-top: -16px;")
                               )
                           ),
                           br(),
                           DTOutput("werke_table")
                       )
                   )
            )
        )
    )
)

# Server ------------------------------------------------------------------------
start_server <- function(input, output, session) {
    
    # KPI: Service Level ---------------------------------------------------------
    output$overall_servicelevel <- renderUI({
        sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(auftraege_lt_unit$abweichung_unit))
        sl_percent <- round(sl * 100)
        farbe <- if (sl_percent < 70) {
            "#ea4335"
        } else if (sl_percent < 95) {
            "#fbbc04"
        } else {
            "#34a853"
        }
        div(
            class = "white-box",
            div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                span(style = paste0("font-weight: 600; font-size: 32px; color: ", farbe, ";"),
                     paste0(sl_percent, "%")),
                span(style = "color: #5f6368; font-size: 16px;",
                     "Servicelevel")
            )
        )
    })
    
    # KPI: Soll-LT/ME ------------------------------------------------------------
    output$soll_lt <- renderUI({
        soll <- median(auftraege_lt_unit$lt_soll_order, na.rm = TRUE)
        div(
            class = "white-box",
            div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                span(style = "font-weight: 600; font-size: 32px; color: #202124;",
                     paste0(round(soll, 2), " s")),
                span(style = "color: #5f6368; font-size: 16px;",
                     "Soll-LT/ME")
            )
        )
    })
    
    # KPI: Delay/ME ---------------------------------------------------------------
    output$overall_delay <- renderUI({
        delay <- median(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE)
        div(
            class = "white-box",
            div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                span(style = "font-weight: 600; font-size: 32px; color: #202124;",
                     paste0(round(delay, 2), " s")),
                span(style = "color: #5f6368; font-size: 16px;",
                     "Delay/ME")
            )
        )
    })
    
    # KPI: Ist-LT/ME -------------------------------------------------------------
    output$ist_lt <- renderUI({
        ist <- median(auftraege_lt_unit$lt_ist_order, na.rm = TRUE)
        div(
            class = "white-box",
            div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                span(style = "font-weight: 600; font-size: 32px; color: #202124;",
                     paste0(round(ist, 2), " s")),
                span(style = "color: #5f6368; font-size: 16px;",
                     "Ist-LT/ME")
            )
        )
    })
    
    # Tabellen: renderDT & downloadHandler für Workflows, Linien, Werke -----------
    render_table_and_download <- function(data, input_id, out_table, out_dl, prefix) {
        output[[out_table]] <- renderDT({
            df <- data
            df$servicelevel_numeric <- as.numeric(gsub("%", "", df$Servicelevel)) / 100
            sel <- input[[input_id]]
            if (sel == "Top") {
                df <- df[order(-df$servicelevel_numeric), ]
            } else {
                df <- df[order(df$servicelevel_numeric), ]
            }
            df <- df |> dplyr::select(-servicelevel_numeric)
            datatable(
                df,
                escape = which(colnames(df) != "ampel"),
                options = list(
                    pageLength   = 10,
                    lengthChange = FALSE,
                    dom          = 'tip',
                    ordering     = FALSE,
                    columnDefs   = list(
                        list(visible = FALSE, targets = 0),
                        list(width   = '20px', targets = 1),
                        list(orderData = 0, targets = 1),
                        list(title     = '', targets = 1),
                        list(className = 'dt-center', targets = 1)
                    )
                ),
                rownames = FALSE,
                class = 'hover',
                width = '100%'
            )
        })
        output[[out_dl]] <- downloadHandler(
            filename = function() paste0(prefix, "_", Sys.Date(), ".csv"),
            content = function(file) {
                df <- data
                df$servicelevel_numeric <- as.numeric(gsub("%", "", df$Servicelevel)) / 100
                sel <- input[[input_id]]
                if (sel == "Top") {
                    df <- df[order(-df$servicelevel_numeric), ]
                } else {
                    df <- df[order(df$servicelevel_numeric), ]
                }
                write.csv(df, file, row.names = FALSE)
            }
        )
    }
    
    render_table_and_download(workflows_overview,  "sortierung_workflows",
                              "workflow_table",      "download_csv_workflows", "workflows")
    render_table_and_download(linien_overview,    "sortierung_linien",
                              "linien_table",        "download_csv_linien", "linien")
    render_table_and_download(werke_overview,     "sortierung_werke",
                              "werke_table",         "download_csv_werke",  "werke")
}

# Run App -----------------------------------------------------------------------
shinyApp(start_ui, start_server)