# Packages ---------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)           # für JS-Klassenmanipulation
library(DT)
library(bsplus)

source("02_model/create_workflows_overview.R", local = TRUE)

# UI -----------------------------------------------------------------------------
start_ui <- fluidPage(
    useShinyjs(),             # shinyjs initialisieren
    
    tags$head(
        # CSS inkl. Hover/Focus-Reset für Logo und Tabs
        tags$style(HTML("
      .navbar {
        width: 100%;
        display: flex;
        align-items: center;
        background-color: white;
        /* entferne die alte graue Linie */
        border-bottom: none;
        padding: 1rem 2rem;
        font-weight: bold;
        font-size: 16px;
        position: relative;
      }
      /* Neue, einzige graue Linie ganz unten */
      .navbar::after {
        content: '';
        position: absolute;
        bottom: 0;
        left: 0;
        right: 0;
        height: 1px;
        background-color: #ddd;
      }
      /* Logo als Klick-Button mit gleichem Padding wie die Tabs */
      .navbar-logo.action-button {
        display: flex;
        align-items: center;
        padding: 8px 0;          /* einheitliche Höhe für Unterstrich */
        text-decoration: none;
        cursor: pointer;
      }
      /* Entferne Browser-Fokus-/Hover-Underline */
      .navbar-logo.action-button:hover,
      .navbar-logo.action-button:focus {
        text-decoration: none !important;
        outline: none !important;
      }
      /* Aktiver Zustand für Logo: Unterstrich */
      .navbar-logo.action-button.active {
        border-bottom: 3px solid #1a73e8;
      }
      .navbar-logo span {
        font-weight: 600;
        font-size: 18px;
      }
      .nav-tabs-custom {
        display: flex;
        flex: 1;
        justify-content: center;
        align-items: center;
        gap: 32px;
        font-size: 14px;
        color: #5f6368;
      }
      /* Gleicher Padding-Wert wie beim Logo */
      .nav-tabs-custom .action-button {
        text-decoration: none;
        color: #5f6368;
        padding: 8px 0;
      }
      /* Entferne Browser-Fokus-/Hover-Underline für Tabs */
      .nav-tabs-custom .action-button:hover,
      .nav-tabs-custom .action-button:focus {
        text-decoration: none !important;
        outline: none !important;
      }
      /* Aktiver Zustand für Tabs: Farbe + Unterstrich */
      .nav-tabs-custom .action-button.active {
        color: #1a73e8;
        font-weight: 600;
        border-bottom: 3px solid #1a73e8;
      }
      .navbar-right {
        display: flex;
        flex: 0 0 auto;
        gap: 20px;
        align-items: center;
        margin-right: 1rem;
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
      .dataTable th, .dataTable td {
        border: none !important;
        padding: 8px 12px !important;
      }
      table.dataTable tbody tr:hover {
        background-color: #f0f4f8 !important;
        cursor: pointer;
      }
      .stripe tbody tr:nth-child(odd)  { background-color: #ffffff !important; }
      .stripe tbody tr:nth-child(even) { background-color: #f9fafb  !important; }
      .dataTables_wrapper { border-radius: 12px; overflow: hidden; }
      .dataTable tbody td { border-bottom: 1px solid #e0e0e0 !important; }
      .dataTables_wrapper .dataTables_paginate {
        font-size: 12px; margin-top: 8px; text-align: right;
      }
      .dataTables_wrapper .paginate_button {
        padding: 2px 6px; margin: 0 2px;
        border: 1px solid #ddd; border-radius: 4px;
        background-color: white; color: #444; font-size: 12px;
      }
      .dataTables_wrapper .current {
        background-color: #e8f0fe; border-color: #4285f4;
        color: #1a73e8; font-weight: 600;
      }
      .dataTables_info { display: none !important; }
    ")),
        # JS für automatische .active-Klassenpflege
        tags$script(HTML("
      $(document).on('click',
        '.navbar-logo.action-button, .nav-tabs-custom .action-button',
        function(){
          $('.navbar-logo.action-button, .nav-tabs-custom .action-button')
            .removeClass('active');
          $(this).addClass('active');
        }
      );
    "))
    ),
    
    # Navbar ----------------------------------------------------------------------
    div(class = "navbar",
        # Logo als klickbarer Link
        actionLink("nav_home",
                   div(class = "navbar-logo action-button",
                       span(style = "color: #4285F4;", "True"),
                       span(style = "color: #EA4335; margin-left:4px;", "Time")
                   )
        ),
        # Mittlere Tabs
        div(class = "nav-tabs-custom",
            actionLink("nav_material",  "Material"),
            actionLink("nav_workflows", "Workflows"),
            actionLink("nav_linien",    "Fertigungslinien"),
            actionLink("nav_werke",     "Werke"),
            actionLink("nav_planer",    "Planer")
        ),
        # Rechte Icons
        div(class = "navbar-right",
            actionButton("download_report", label = NULL,
                         icon = icon("file-arrow-down"),
                         style = "background: none; border: none; color: #5f6368; font-size: 16px;"),
            tags$span(icon("user-circle"),
                      style = "font-size: 20px; color: #5f6368; cursor: pointer;")
        )
    ),
    
    # Unsichtbares Tab-Panel für Dashboards -------------------------------------
    tabsetPanel(id = "main_tabs", type = "hidden",
                
                # Home / Hauptseite --------------------------------------------------------
                tabPanel("home", value = "home",
                         div(style = "max-width: 1100px; margin: 0 auto;",
                             
                             # KPI-Zeile 1
                             fluidRow(
                                 column(6, uiOutput("overall_servicelevel")),
                                 column(6, uiOutput("soll_lt"))
                             ),
                             # KPI-Zeile 2
                             fluidRow(
                                 column(6, uiOutput("overall_delay")),
                                 column(6, uiOutput("ist_lt"))
                             ),
                             # Workflows-Box
                             fluidRow(
                                 column(12,
                                        div(class = "white-box",
                                            tagList(
                                                div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                    div(style = "display:flex; align-items:center;",
                                                        span("Lead Times nach Workflows",
                                                             style = "font-weight:600;font-size:16px;color:#202124;"),
                                                        tags$span(icon("circle-question"), id = "workflows_info",
                                                                  style = "color:#5f6368;margin-left:8px;") %>%
                                                            bs_embed_popover(
                                                                title   = "Workflows Übersicht",
                                                                content = "Sie können für einzelne Workflows …",
                                                                placement = "right", trigger = "focus"
                                                            )
                                                    ),
                                                    div(style = "display:flex; align-items:center;",
                                                        div(style = "margin-right:12px;",
                                                            selectInput("sortierung_workflows", label = NULL,
                                                                        choices = c("Top","Kritisch"),
                                                                        selected = "Top", width = "140px")
                                                        ),
                                                        downloadButton("download_csv_workflows", label = NULL,
                                                                       icon = icon("download"),
                                                                       style = "padding:6px 10px; margin-top:-16px;")
                                                    )
                                                ),
                                                br(),
                                                DTOutput("workflow_table")
                                            )
                                        )
                                 )
                             ),
                             # Fertigungslinien-Box
                             fluidRow(
                                 column(12,
                                        div(class = "white-box",
                                            tagList(
                                                div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                    div(style = "display:flex; align-items:center;",
                                                        span("Lead Times nach Fertigungslinien",
                                                             style = "font-weight:600;font-size:16px;color:#202124;"),
                                                        tags$span(icon("circle-question"), id = "linien_info",
                                                                  style = "color:#5f6368;margin-left:8px;") %>%
                                                            bs_embed_popover(
                                                                title   = "Linien Übersicht",
                                                                content = "Sie können für einzelne Fertigungslinien …",
                                                                placement = "right", trigger = "focus"
                                                            )
                                                    ),
                                                    div(style = "display:flex; align-items:center;",
                                                        div(style = "margin-right:12px;",
                                                            selectInput("sortierung_linien", label = NULL,
                                                                        choices = c("Top","Kritisch"),
                                                                        selected = "Top", width = "140px")
                                                        ),
                                                        downloadButton("download_csv_linien", label = NULL,
                                                                       icon = icon("download"),
                                                                       style = "padding:6px 10px; margin-top:-16px;")
                                                    )
                                                ),
                                                br(),
                                                DTOutput("linien_table")
                                            )
                                        )
                                 )
                             ),
                             # Werke-Box
                             fluidRow(
                                 column(12,
                                        div(class = "white-box",
                                            tagList(
                                                div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                    div(style = "display:flex; align-items:center;",
                                                        span("Lead Times nach Werken",
                                                             style = "font-weight:600;font-size:16px;color:#202124;"),
                                                        tags$span(icon("circle-question"), id = "werke_info",
                                                                  style = "color:#5f6368;margin-left:8px;") %>%
                                                            bs_embed_popover(
                                                                title   = "Werke Übersicht",
                                                                content = "Sie können für einzelne Werke …",
                                                                placement = "right", trigger = "focus"
                                                            )
                                                    ),
                                                    div(style = "display:flex; align-items:center;",
                                                        div(style = "margin-right:12px;",
                                                            selectInput("sortierung_werke", label = NULL,
                                                                        choices = c("Top","Kritisch"),
                                                                        selected = "Top", width = "140px")
                                                        ),
                                                        downloadButton("download_csv_werke", label = NULL,
                                                                       icon = icon("download"),
                                                                       style = "padding:6px 10px; margin-top:-16px;")
                                                    )
                                                ),
                                                br(),
                                                DTOutput("werke_table")
                                            )
                                        )
                                 )
                             )
                         )
                ),
                
                # Platzhalter für weitere Dashboards ----------------------------------------
                tabPanel("material",   value = "material",  h2("Material-Dashboard (in Arbeit)")),
                tabPanel("workflows",  value = "workflows", h2("Workflows-Dashboard (in Arbeit)")),
                tabPanel("linien",     value = "linien",    h2("Fertigungslinien-Dashboard (in Arbeit)")),
                tabPanel("werke",      value = "werke",     h2("Werke-Dashboard (in Arbeit)")),
    )
)

# Server ------------------------------------------------------------------------
start_server <- function(input, output, session) {
    
    # KPI: Service Level ---------------------------------------------------------
    output$overall_servicelevel <- renderUI({
        sl_pct <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(auftraege_lt_unit$abweichung_unit))
        sl_val <- round(sl_pct * 100)
        color  <- if (sl_val < 70) "#ea4335" else if (sl_val < 95) "#fbbc04" else "#34a853"
        div(class = "white-box",
            div(style = "display:flex; flex-direction:column;",
                span(style = paste0("font-weight:600;font-size:32px;color:",color,";"),
                     paste0(sl_val,"%")),
                span(style = "font-size:16px;color:#5f6368;","Service Level")
            )
        )
    })
    
    # KPI: Planned LT per Unit --------------------------------------------------
    output$soll_lt <- renderUI({
        val <- median(auftraege_lt_unit$lt_soll_order, na.rm = TRUE)
        div(class = "white-box",
            div(style = "display:flex; flex-direction:column;",
                span(style = "font-weight:600;font-size:32px;color:#202124;",
                     paste0(round(val,2)," s")),
                span(style = "font-size:16px;color:#5f6368;","Planned LT/Unit")
            )
        )
    })
    
    # KPI: Delay per Unit --------------------------------------------------------
    output$overall_delay <- renderUI({
        val <- median(auftraege_lt_unit$abweichung_unit, na.rm = TRUE)
        div(class = "white-box",
            div(style = "display:flex; flex-direction:column;",
                span(style = "font-weight:600;font-size:32px;color:#202124;",
                     paste0(round(val,2)," s")),
                span(style = "font-size:16px;color:#5f6368;","Delay/Unit")
            )
        )
    })
    
    # KPI: Actual LT per Unit ---------------------------------------------------
    output$ist_lt <- renderUI({
        val <- median(auftraege_lt_unit$lt_ist_order, na.rm = TRUE)
        div(class = "white-box",
            div(style = "display:flex; flex-direction:column;",
                span(style = "font-weight:600;font-size:32px;color:#202124;",
                     paste0(round(val,2)," s")),
                span(style = "font-size:16px;color:#5f6368;","Actual LT/Unit")
            )
        )
    })
    
    # Helper: render & download für die Tabellen --------------------------------
    render_table_and_download <- function(data, sel_id, tbl_out, dl_out, prefix) {
        output[[tbl_out]] <- renderDT({
            df <- data
            df$servicelevel_numeric <- as.numeric(gsub('%','',df$Servicelevel))/100
            df <- if (input[[sel_id]]=="Top") df[order(-df$servicelevel_numeric),] else df[order(df$servicelevel_numeric),]
            df <- df |> dplyr::select(-servicelevel_numeric)
            datatable(df, escape = which(names(df)!="ampel"),
                      options = list(pageLength=10, dom='tip', ordering=FALSE),
                      rownames=FALSE, class='hover', width='100%')
        })
        output[[dl_out]] <- downloadHandler(
            filename = function() paste0(prefix,"_",Sys.Date(),".csv"),
            content = function(file) write.csv(data, file, row.names=FALSE)
        )
    }
    
    render_table_and_download(workflows_overview, "sortierung_workflows",
                              "workflow_table","download_csv_workflows","workflows")
    render_table_and_download(linien_overview,   "sortierung_linien",
                              "linien_table",  "download_csv_linien",  "linien")
    render_table_and_download(werke_overview,    "sortierung_werke",
                              "werke_table",   "download_csv_werke",   "werke")
    
    # Navigation: nur updateTabsetPanel; visuelle Hervorhebung per JS ----------
    observeEvent(input$nav_home,      updateTabsetPanel(session,"main_tabs","home"))
    observeEvent(input$nav_material,  updateTabsetPanel(session,"main_tabs","material"))
    observeEvent(input$nav_workflows, updateTabsetPanel(session,"main_tabs","workflows"))
    observeEvent(input$nav_linien,    updateTabsetPanel(session,"main_tabs","linien"))
    observeEvent(input$nav_werke,     updateTabsetPanel(session,"main_tabs","werke"))
    observeEvent(input$nav_planer, {
        # statt nur Tab wechseln → Browser auf die Planer-App umleiten
        runjs("window.location.href = '02.2_planer.html';")
    })
    
    # Starte immer auf Home ------------------------------------------------------
    isolate({
        updateTabsetPanel(session,"main_tabs","home")
        runjs("$('.navbar-logo.action-button').addClass('active');")
    })
}

# Run App -----------------------------------------------------------------------
shinyApp(start_ui, start_server)