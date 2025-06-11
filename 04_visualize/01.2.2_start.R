library(shiny)
library(shinyWidgets)
library(DT)
library(bsplus)

# Euer bestehender Data-Prep-Code
source("02_model/create_workflows_overview.R", local = TRUE)

# UI ---------------------------------------------------------------------------
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      .navbar {
        display: flex; align-items: center;
        background-color: #fff; border-bottom: 1px solid #ddd;
        padding: 1rem 2rem; font-weight: bold; font-size: 16px;
      }
      .navbar-logo { display: flex; align-items: center; flex:0 0 auto; }
      .navbar-logo img { height:24px; margin-right:8px; }
      .navbar-logo .home-text { position: relative; color:#5f6368; text-decoration:none; }
      .navbar-logo .home-text.active::after {
        content:''; position:absolute; bottom:-4px; left:0; right:0;
        height:3px; background-color:#1a73e8;
      }
      .nav-tabs-custom {
        flex:1; display:flex; justify-content:center; align-items:center; gap:32px;
      }
      .nav-tabs-custom .tab-item {
        position: relative; color:#5f6368; text-decoration:none; padding:8px 0;
      }
      .nav-tabs-custom .tab-item.active {
        color:#1a73e8; font-weight:600;
      }
      .nav-tabs-custom .tab-item.active::after {
        content:''; position:absolute; bottom:-4px; left:0; right:0;
        height:3px; background-color:#1a73e8;
      }
      .navbar-right { display:flex; gap:20px; align-items:center; margin-right:1rem; }
      body { background-color:#f5f7fa; margin:0; padding:0;
             font-family:'Helvetica Neue',Helvetica,Arial,sans-serif; }
    "))
    ),
    
    # 1) Dynamische Navbar
    uiOutput("navbarUI"),
    
    # 2) Versteckte Seiten
    tabsetPanel(
        id = "page",
        type = "hidden",
        
        # -- Hauptseite (aktuell Euer Workflows-Dashboard) --
        tabPanel(
            value = "home",
            div(style="max-width:1100px; margin:20px auto;",
                fluidRow(
                    column(6, uiOutput("overall_servicelevel")),
                    column(6, uiOutput("soll_lt"))
                ),
                fluidRow(
                    column(6, uiOutput("overall_delay")),
                    column(6, uiOutput("ist_lt"))
                ),
                # hier könntet Ihr noch mehr Komponenten der Hauptseite packen…
                fluidRow(
                    column(12,
                           div(class="white-box",
                               tagList(
                                   div(style="display:flex;justify-content:space-between;align-items:center;",
                                       div(style="display:flex;align-items:center;",
                                           span("Lead Times nach Workflows",
                                                style="font-weight:600;font-size:16px;color:#202124;"),
                                           tags$span(icon("circle-question"), id="workflows_info",
                                                     style="color:#5f6368;margin-left:8px;") %>%
                                               bs_embed_popover(
                                                   title="Workflows Übersicht",
                                                   content="…", placement="right", trigger="focus"
                                               )
                                       ),
                                       div(style="display:flex;align-items:center;",
                                           selectInput("sortierung_workflows", label=NULL,
                                                       choices=c("Top","Kritisch"),
                                                       selected="Top", width="140px"),
                                           downloadButton("download_csv_workflows", label=NULL,
                                                          icon=icon("download"),
                                                          style="margin-left:12px;")
                                       )
                                   ),
                                   DTOutput("workflow_table")
                               )
                           )
                    )
                )
            )
        ),
        
        # -- Platzhalter für die späteren Dashboards --
        tabPanel(value = "material",  div(style="padding:40px;", h2("Material (in Arbeit)"))),
        tabPanel(value = "workflows", div(style="padding:40px;", h2("Workflows (in Arbeit)"))),
        tabPanel(value = "linien",    div(style="padding:40px;", h2("Fertigungslinien (in Arbeit)"))),
        tabPanel(value = "werke",     div(style="padding:40px;", h2("Werke (in Arbeit)"))),
        tabPanel(value = "planer",    div(style="padding:40px;", h2("Planer (in Arbeit)")))
    )
)

# SERVER -----------------------------------------------------------------------
server <- function(input, output, session) {
    
    # 1) Navbar rendern
    output$navbarUI <- renderUI({
        tabs <- c(material="Material",
                  workflows="Workflows",
                  linien="Fertigungslinien",
                  werke="Werke",
                  planer="Planer")
        
        tags$div(class="navbar",
                 # Logo + Home
                 tags$div(class="navbar-logo",
                          tags$img(src="logo.png"),
                          tags$a(
                              class = if (input$page=="home") "home-text active" else "home-text",
                              href  = "#", "TrueTime",
                              onclick = "Shiny.setInputValue('nav_home', Math.random())"
                          )
                 ),
                 # Mittlere Tabs
                 tags$div(class="nav-tabs-custom",
                          lapply(names(tabs), function(name) {
                              tags$a(
                                  class   = if (input$page==name) "tab-item active" else "tab-item",
                                  href    = "#",
                                  tabs[[name]],
                                  onclick = sprintf("Shiny.setInputValue('nav_%s', Math.random())", name)
                              )
                          })
                 ),
                 # Icons rechts
                 tags$div(class="navbar-right",
                          actionButton("download_report", NULL, icon=icon("file-arrow-down"),
                                       style="background:none; border:none; color:#5f6368;"),
                          tags$span(icon("user-circle"), style="font-size:20px; color:#5f6368;")
                 )
        )
    })
    
    # 2) Klick-Events → Seitenwechsel
    observeEvent(input$nav_home,     updateTabsetPanel(session, "page", "home"))
    observeEvent(input$nav_material, updateTabsetPanel(session, "page", "material"))
    observeEvent(input$nav_workflows,updateTabsetPanel(session, "page", "workflows"))
    observeEvent(input$nav_linien,   updateTabsetPanel(session, "page", "linien"))
    observeEvent(input$nav_werke,    updateTabsetPanel(session, "page", "werke"))
    observeEvent(input$nav_planer,   updateTabsetPanel(session, "page", "planer"))
    
    # 3) Eure KPI-Boxen (unverändert)
    output$overall_servicelevel <- renderUI({
        sl_pct <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm=TRUE) /
            sum(!is.na(auftraege_lt_unit$abweichung_unit))
        sl_val <- round(sl_pct*100)
        color  <- if (sl_val<70) "#ea4335" else if (sl_val<95) "#fbbc04" else "#34a853"
        div(class="white-box",
            div(style="display:flex;flex-direction:column;",
                span(style=paste0("font-weight:600;font-size:32px;color:",color,";"),
                     paste0(sl_val, "%")),
                span(style="font-size:16px;color:#5f6368;", "Service Level")
            )
        )
    })
    # … und analog output$soll_lt, output$overall_delay, output$ist_lt …
    
    # 4) Eure Tabellen & DownloadHandler
    output$workflow_table <- renderDT({
        df <- workflows_overview
        df$servicelevel_numeric <- as.numeric(gsub("\\%","",df$Servicelevel))/100
        if (input$sortierung_workflows == "Top") {
            df <- df[order(-df$servicelevel_numeric), ]
        } else {
            df <- df[order(df$servicelevel_numeric), ]
        }
        df <- df[, setdiff(names(df), "servicelevel_numeric")]
        datatable(df, escape = which(names(df)!="ampel"),
                  options = list(pageLength=10, dom='tip', ordering=FALSE), class='hover')
    })
    output$download_csv_workflows <- downloadHandler(
        filename = function() paste0("workflows_", Sys.Date(), ".csv"),
        content = function(file) {
            df <- workflows_overview
            df$servicelevel_numeric <- as.numeric(gsub("\\%","",df$Servicelevel))/100
            if (input$sortierung_workflows == "Top") {
                df <- df[order(-df$servicelevel_numeric), ]
            } else {
                df <- df[order(df$servicelevel_numeric), ]
            }
            write.csv(df, file, row.names = FALSE)
        }
    )
    
    # Für Linienstabelle und Werke-Tabelle analog:
    # output$linien_table + download_csv_linien …
    # output$werke_table + download_csv_werke …
}

# App starten ------------------------------------------------------------------
shinyApp(ui, server)