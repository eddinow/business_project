rm(list = ls())
set.seed(1)

# Packages ---------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)           
library(DT)
library(bsplus)

source("02_model/create_overview_tables.R")
source("04_visualize/dashboard_planer.R")
source("04_visualize/dashboard_werke.R")
source("04_visualize/dashboard_linien.R")
source("04_visualize/dashboard_workflows.R")
source("04_visualize/dashboard_material.R")

# Definiert, welcher Text im UI welcher Spalte in den Daten entspricht
lt_map <- list(
    "Workflow" = "vorgangsfolge",
    "Werk"     = "werk",
    "Linie"    = "fertigungslinie",
    "Planer"   = "planer",
    "A-Material" = "materialnummer"
)

# UI ---------------------------------------------------------------------------
start_ui <- fluidPage(
    useShinyjs(),             
    
    tags$head(
        # Benutzerdefiniertes CSS für Navigationsleiste, Tabs, Logo und Datentabellen
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
        # JS für automatisches Hinzufügen/Entfernen der 'active'-Klassen
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
    
    # Navbar (obere Navigationsleiste)------------------------------------------
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
 
    ),
    
    # Unsichtbares Tab-Panel für Dashboards ------------------------------------
    tabsetPanel(id = "main_tabs", type = "hidden",
                # … Home, Material, Workflows, Linien, Werke …
                tabPanel("Material", value = "material", klassifikationUI()),
                tabPanel("Workflows", value = "workflows", vorgangsfolgeUI()),
                tabPanel("Linien", value = "linien", fertigungslinieUI()),
                tabPanel("Werke", value = "werke", werkUI()),
                tabPanel("Planer", value = "planer", planerUI()),
                
                # Home / Hauptseite --------------------------------------------------------
                tabPanel("home", value = "home",
                         div(style = "max-width: 1100px; margin: 0 auto;",
                             
                             fluidRow(
                                 column(
                                     width = 6,
                                     div(
                                         class = "white-box",
                                         style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                                         uiOutput("livetracker_auftraege")
                                     )
                                 ),
                                 column(
                                     width = 6,
                                     div(
                                         class = "white-box",
                                         style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                                         uiOutput("livetracker_overall_servicelevel")
                                     )
                                 )
                             ),
                             
                             
                             # Material-Box
                             fluidRow(
                                 column(12,
                                        div(class = "white-box",
                                            tagList(
                                                div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                    div(style = "display:flex; align-items:center;",
                                                        span("Lead Times nach Material (A,B,C)",
                                                             style = "font-weight:600;font-size:16px;color:#202124;"),
                                                        tags$span(icon("circle-question"), id = "linien_info",
                                                                  style = "color:#5f6368;margin-left:8px;") %>%
                                                            bs_embed_popover(
                                                                title   = "Material Übersicht",
                                                                content = "Sie können für einzelne Fertigungslinien …",
                                                                placement = "right", trigger = "focus"
                                                            )
                                                    ),
                                                    div(style = "display:flex; align-items:center;",
                                                        div(style = "margin-right:12px;",
                                                            selectInput("sortierung_material", label = NULL,
                                                                        choices = c("Top","Kritisch"),
                                                                        selected = "Top", width = "140px")
                                                        ),

                                                    )
                                                ),
                                                br(),
                                                DTOutput("material_table")
                                            )
                                        )
                                 )
                             ),
                             
                             #Planer-Box
                             fluidRow(
                                 column(12,
                                        div(class = "white-box",
                                            tagList(
                                                div(style = "display:flex; justify-content:space-between; align-items:center;",
                                                    div(style = "display:flex; align-items:center;",
                                                        span("Lead Times nach Planern",
                                                             style = "font-weight:600;font-size:16px;color:#202124;"),
                                                        tags$span(icon("circle-question"), id = "linien_info",
                                                                  style = "color:#5f6368;margin-left:8px;") %>%
                                                            bs_embed_popover(
                                                                title   = "Planer Übersicht",
                                                                content = "Sie können für einzelne Fertigungslinien …",
                                                                placement = "right", trigger = "focus"
                                                            )
                                                    ),
                                                    div(style = "display:flex; align-items:center;",
                                                        div(style = "margin-right:12px;",
                                                            selectInput("sortierung_planer", label = NULL,
                                                                        choices = c("Top","Kritisch"),
                                                                        selected = "Top", width = "140px")
                                                        ),

                                                    )
                                                ),
                                                br(),
                                                DTOutput("planer_table")
                                            )
                                        )
                                 )
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
    )
)


# Server ------------------------------------------------------------------------
start_server <- function(input, output, session) {
    
    # Berechnung der Anzahl an Aufträgen 
    output$livetracker_auftraege <- renderUI({
        
        anzahl <- auftraege_lt_unit %>%
            summarise(n = n_distinct(auftragsnummer)) %>%
            pull(n)
        
        # UI-Ausgabe der Anzahl an Aufträgen
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
    
    
    # overall_servicelevel <- reactive({
    #     sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
    #         sum(!is.na(auftraege_lt_unit$auftragsnummer))
    # })
    
    
    output$livetracker_overall_servicelevel <- renderUI({
        
        # Berechnung des Overall Servicelevels
        overall_sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
        
        sl_percent <- paste0(round(overall_sl * 100), "%")
        
        # UI-Ausgabe des Servicelevels
        tags$div(
            style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: center;",
            tags$span(
                style = "font-weight: 600; font-size: 22px; color: #202124;",
                sl_percent
            ),
            tags$span(
                style = "font-size: 14px; color: #5f6368;",
                "Servicelevel (gesamt)"
            )
        )
    })
    
    # Erstellung einer sortiebaren und formartierten Datentablle ---------------
    render_table <- function(data, sel_id, tbl_out) {
        output[[tbl_out]] <- renderDT({
            df_datatable <- data
            df_datatable$servicelevel_numeric <- as.numeric(gsub('%','',df_datatable$Servicelevel))/100
            df_datatable <- if (input[[sel_id]]=="Top") df_datatable[order(-df_datatable$servicelevel_numeric),] else df_datatable[order(df_datatable$servicelevel_numeric),]
            df_datatable <- df_datatable |> dplyr::select(-servicelevel_numeric)
            datatable(df_datatable, escape = which(names(df_datatable)!="Status"),
                      options = list(pageLength=10, dom='tip', ordering=FALSE),
                      rownames=FALSE, class='hover', width='100%')
        })
        
    }
    
    # Tabellen für alle Übersichten erstellen --------------------
    render_table(workflows_overview, "sortierung_workflows",
                              "workflow_table")
    render_table(linien_overview, "sortierung_linien",
                              "linien_table")
    render_table(werke_overview,    "sortierung_werke",
                              "werke_table")
    render_table(planer_overview,    "sortierung_planer",
                              "planer_table")
    render_table(material_overview,    "sortierung_material",
                              "material_table")
    
    # Navigation: Tabs wechseln und Module starten -----------------------------
    observeEvent(input$nav_home,      updateTabsetPanel(session,"main_tabs","home"))
    
    observeEvent(input$nav_material,  updateTabsetPanel(session,"main_tabs","material"))
    klassifikationServer(input, output, session)
    
    observeEvent(input$nav_workflows, updateTabsetPanel(session,"main_tabs","workflows"))
    vorgangsfolgeServer(input, output, session)
    
    observeEvent(input$nav_linien,    updateTabsetPanel(session,"main_tabs","linien"))
    fertigungslinieServer(input, output, session)
    
    observeEvent(input$nav_werke,     updateTabsetPanel(session,"main_tabs","werke"))
    werkServer(input, output, session)
    
    observeEvent(input$nav_planer,    updateTabsetPanel(session,"main_tabs","planer"))
    planerServer(input, output, session)
    
    # Starte immer auf der Startseite ------------------------------------------
    isolate({
        updateTabsetPanel(session,"main_tabs","home")
        runjs("$('.navbar-logo.action-button').addClass('active');")
    })
}

# Run App -----------------------------------------------------------------------
shinyApp(start_ui, start_server)