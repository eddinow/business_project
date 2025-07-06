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

source("02_model/create_workflows_overview.R")
source("02_model/kpis_workflow_liegezeit.R")
source("01_transform/create_lt_unit.R")
source("01_transform/create_est_lt_per_workflow.R")

# Eigenes Mapping für Workflows, weil Daten aus anderem DF gesourced werden, als 
# bei den anderen Seiten. 
lt_map_workflow <- list(
    "Arbeitsschritte" = "Vorgangsnummer",
    "Arbeitsplatz"     = "Arbeitsplatz",
    "A-Material"    = "materialnummer"
)

#Formel zur Berechnung des Modus
modus <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
}

# Designfunktion für die Plots
app_theme <- function(base_family = "Inter") {
    theme_minimal(base_family = base_family) +
        theme(
            # Schriftgröße & Farbe
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
vorgangsfolgeUI <- function() {
    tagList(
    
        
        # Sub-Header
        div(
            style = "background-color: #f1f3f4; padding: 18px 32px; height: 72px;
         display: flex; align-items: center; justify-content: space-between;
         border-top: 1px solid #e0e0e0;",
            
            # Icon + Titel
            div(
                style = "display: flex; align-items: center; gap: 12px;",
                icon("project-diagram", class = NULL, style = "font-size: 20px; color: #5f6368;"),
                span(
                    style = "font-size: 20px; font-weight: 600; color: #202124;",
                    "Workflows"
                )
            ),
            
            # Workflow-Auswahl + Ansichtsauswahl
            div(
                style = "display: flex; align-items: center; gap: 24px;",
                
                # 1. Workflow auswählen
                div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    span(
                        style = "font-size: 14px; color: #202124; font-weight: 500;",
                        "1. Workflow auswählen:"
                    ),
                    div(
                        style = "width: 180px;",
                        selectizeInput(
                            inputId = "selected_vorgangsfolge",
                            label = NULL,
                            choices = NULL,
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
                            inputId = "view_selection_workflow",
                            label = NULL,
                            choices = c("Arbeitsschritte", "Arbeitsplatz", "A-Material"),
                            selected = "Arbeitsschritte",
                            width = "100%"
                        )
                    )
                )
            )
        ),
    
    
    # Inhalt
    div(style = "max-width: 1100px; margin: 0 auto;",
        
        div(
            style = "padding: 48px 0 12px 0;",  # Abstand oben und unten
            uiOutput("vorgangsfolge_title")
        ),
        
        #KPI Boxen (Anzahl Aufträge, Servicelevel, Bottleneck)
        fluidRow(
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_auftraege_workflow")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_servicelevel_workflow")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_bottleneck_workflow")
                )
            )
        ),
        
        # Performance-KPIs des ausgewählten Workflows
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "padding: 40px 32px; background-color: white;",
                    tagList(
                        div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 24px;",
                            tags$strong("Performance-Übersicht", 
                                        style = "font-weight: 600; font-size: 16px; color: #202124;")
                        ),
                        div(
                            style = "display: flex; justify-content: space-between;",
                            
                            # 1. Termintreue
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_termintreue_workflow", height = "160px"),
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
                                        content = "Der prozentuale Anteil aller Aufträge, die bis zum geplanten Liefer- oder Fertigstellungstermin abgeschlossen wurden. Frühere Fertigstellungen werden dabei ebenfalls als termingerecht gewertet.",
                                        placement = "top",
                                        trigger = "hover"
                                    )
                                )
                            ),
                            
                            # 2. Liefertreue
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_liefertreue_workflow", height = "160px"),
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
                            
                            # 3. Geschwindigkeit pro ME
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_geschwindigkeit_me_workflow", height = "160px"),
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
                                        content = "Gibt an, wie viel Zeit im Schnitt pro geliefertem Stück benötigt wurde. Dadurch können Aufträge mit unterschiedlichen Mengen vergleichbar gemacht und ineffiziente Prozesse leichter erkannt werden.",
                                        placement = "top",
                                        trigger = "hover"
                                    )
                                )
                            ),
                            
                            # 4. Geschwindigkeit pro Auftrag
                            div(
                                style = "text-align: center; width: 24%;",
                                echarts4rOutput("donut_geschwindigkeit_auftrag_workflow", height = "160px"),
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
                                        content = "Gibt an, wie lange eine Entität im Median für die Bearbeitung eines Auftrags benötigt. Dadurch kann die typische Durchlaufzeit erkannt und von extremen Einzelfällen abgegrenzt werden.",
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
        
        #KPIs der ausgewählten Ansicht
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("allocation_title_workflow")
                        ),
                        
                        
                        # Übersichtstabelle mit Verzögerungen, Lead Times und Servicelevel
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
                                        DTOutput("delay_table_shared_workflow"),
                                        
                                        bsPopover(
                                            id = "performance_table_info",
                                            title = "Aktuelle Performance",
                                            content = "In der Tabelle wird die ausgewählte Ansicht (z. B. Werke, Linien, Materialien) für jede gewählte Entität (z. B. Planer, Workflows) dargestellt. Dadurch kann nachvollzogen werden, wie sich einzelne Entitäten auf unterschiedliche Strukturebenen auswirken. Grün steht für geringe (< 0,5 s/ME), Gelb für moderate (bis 2 s/ME) und Rot für kritische Verzögerungen (> 2 s/ME). Zusätzlich lassen sich die durchschnittlichen Ist- und Soll-Lead Times pro Mengeneinheit sowie die zugehörige Auftragsanzahl ablesen, um Zusammenhänge zwischen Auslastung und Performance sichtbar zu machen.",
                                            placement = "top",
                                            trigger = "hover"
                                        ),
                                    )
                                )
                            )
                        ),
                        
                        # Verteilung der Aufträge
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
                                        
                                        echarts4rOutput("allocation_pie_shared_workflow", height = "300px")
                                    )
                                )
                            ),
                            
                            # Aufträge mit starken Abweichungen
                            column(
                                width = 6,
                                div(
                                    class = "white-box",
                                    style = "min-height: 455px",
                                    tagList(
                                        div(
                                            style = "display: flex; align-items: center; gap: 6px; margin-bottom: 16px;",
                                            span("Top 200 Aufträge mit Abweichung", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                                            tags$span(icon("circle-question"), id = "topdelay_info", style = "color: #5f6368; font-size: 14px; cursor: pointer;")
                                        ),
                                        
                                        bsPopover(
                                            id = "topdelay_info",
                                            title = "Verfrühung & Verzögerung",
                                            content = "Passend zu der oben getroffenen Auswahl sieht man in dieser Darstellung die prozentualen Anteile von Aufträgen, die in vier Gruppen nach Dauer der Verzögerung bzw. Verfrühung kategorisiert sind. Je nach Bedarf kann man über die Lupen-Icons weiterführende Details zu den einzelnen Aufträgen einsehen und daraus gezielte Optimierungsmaßnahmen ableiten.",
                                            placement = "top",
                                            trigger = "hover"
                                        ),
                                        
                                        tabsetPanel(
                                            id = "abweichung_tabs",
                                            type = "tabs",
                                            
                                            # Tab 1: Verzögerungsverteilung mit Lupenbuttons
                                            tabPanel(
                                                "Verzögerungen",
                                                DTOutput("delay_quartile_summary_workflow")
                                            ),
                                            
                                            # Tab 2: Verfrühungsverteilung mit Lupenbuttons
                                            tabPanel(
                                                "Verfrühungen",
                                                DTOutput("early_quartile_summary_workflow")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        
    # Lead Time Übersicht des ausgewählten Workflows (ohne Ansicht als zweite Dimension)
        
        # Bearbeitungs- und Liegezeiten des ausgewählten Workflows
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    tagList(
                        div(
                            style = "display: flex; align-items: center;",
                            span("Bearbeitungs- und Liegezeiten [Tage]", style = "font-weight: 600; font-size: 16px; color: #202124;"),
                            tags$span(
                                icon("circle-question"),
                                id = "liegezeiten_info",
                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            )
                        ),
                        br(),
                        plotOutput("balkenplot_workflow", height = "240px"),
                    ),
                    
                    bsPopover(
                        id = "liegezeiten_info",
                        title = "Was wird hier gezeigt?",
                        content = "Zeigt die Ist-Lead Times inklusive Liegezeiten als kumulierte Werte. Der blaue Anteil symbol_performance_vglisiert dabei die Lead Time des jeweiligen Prozessschrittes. Diese genaue Dauer steht als Wert ablesbar auch über den Balken. User kriegen so einen Eindruck, ob und in welchem Umfang Liegezeiten die Bearbeitung eines Auftrags treiben. Ausgangspunkt für qualitative Ursachenanalysen.",
                        placement = "right",
                        trigger = "hover"
                    ),
                )
            )
        ),
        
        # Lead Time des ausgewählten Workflows nach der Sollmenge
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
                                id = "abw_menge_info",
                                style = "color: #5f6368; margin-left: 8px; cursor: pointer;"
                            )
                        ),
                        br(),
                        plotly::plotlyOutput("workflow_plot", height = "240px"),
                    ),
                    
                    bsPopover(
                        id = "abw_menge_info",
                        title = "Was wird hier gezeigt?",
                        content = "Julia",
                        placement = "right",
                        trigger = "hover"
                    ),
                )
            )
        ),
        
        # Details zur Lead Time Abweichung
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("abweichung_title_workflow")
                        ),
                        
                        # Lead Time Abweichung im Zeitverlauf
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
                                        plotly::plotlyOutput("abweichung_time_plot_workflow", height = "240px"),
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_zeit_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Die zeitliche Entwicklung der Lead Time Abweichung gibt Aufschluss darüber, ob eine Entität über aufeinanderfolgende Aufträge hinweg konstanter, ungenauer oder präziser arbeitet. Aufträge sind nach Starttermin sortiert, die y-Achse zeigt die absolute Abweichung in Tagen.",
                                        placement = "right",
                                        trigger = "hover"
                                    ),
                                )
                            )
                        ),
                        
                        fluidRow(
                            #Lead Time Abweichung absolut
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
                                        plotly::plotlyOutput("abweichung_hist_plot_workflow", height = "240px")
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_abs_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Da die Lead Time Abweichungen je Entität keiner einheitlichen Verteilung folgen, sind Mittelwert und Standardabweichung oft wenig aussagekräftig. Stattdessen zeigt das Histogramm die tatsächliche Häufigkeitsverteilung – begrenzt auf das 2,5. bis 97,5. Perzentil, um extreme Ausreißer auszublenden. So lassen sich typische Muster und Verzögerungstendenzen erkennen.",
                                        placement = "right",
                                        trigger = "hover"
                                    ),
                                )
                            ),
                            
                            # Lead Time Abweichung relativ
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
                                        DT::DTOutput("abweichungstabelle_workflow")
                                    ),
                                    
                                    bsPopover(
                                        id = "abw_rel_info",
                                        title = "Was wird hier gezeigt?",
                                        content = "Die Darstellung zeigt die prozentuale Abweichung der Ist- von der Soll-Lead Time je Auftrag. Dadurch wird sichtbar, ob Verzögerungen systematisch auftreten und in welcher Größenordnung sie relativ zur geplanten Bearbeitungszeit liegen",
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
)}

#Server-------------------------------------------------------------------------
vorgangsfolgeServer <- function(input, output, session) {
    
    # Aufträge nach Workflow filtern
    observe({
        vorgangsfolge <- unique(vorgaenge_sorted$vorgangsfolge)
        
        updateSelectizeInput(
            session,
            inputId = "selected_vorgangsfolge",
            choices = c("Workflow auswählen" = "", vorgangsfolge),
            selected = "0010", 
            server = TRUE
        )
    })
    
    # Beschränken auf A-Materialien
    get_filtered_data_workflow <- function(df, selected_vorgangsfolge, selected_view_vorgangsfolge) {
        df_filtered <- df %>%
            filter(vorgangsfolge == selected_vorgangsfolge)
        
        if (selected_view_vorgangsfolge == "A-Material") {
            df_filtered <- df_filtered %>%
                filter(klassifikation == "A")
        }
        
        return(df_filtered)
    }
    
    
    output$vorgangsfolge_title <- renderUI({
        req(input$selected_vorgangsfolge)
        
        tags$div(
            style = "margin-top: 32px; margin-bottom: 32px;",
            tags$h2(
                paste("Details | Workflow", input$selected_vorgangsfolge),
                style = "font-size: 25px; font-weight: 600; color: #202124; margin: 0;"
            )
        )
    })
    
# KPIs der ausgewählten Ansicht
    
    # 1. Termintreue
    output$donut_termintreue_workflow <- renderEcharts4r({
        sel <- input$selected_vorgangsfolge
        data_selected <- vorgaenge_sorted %>% filter(vorgangsfolge == sel)
        data_remaining <- vorgaenge_sorted %>% filter(vorgangsfolge != sel)
        
        # Berechne Mittelwert aller Abweichungen kleiner oder gleich 0 - Anteil pünktlich
        termintreue_selected <- round(mean(data_selected$abweichung_unit <= 0, na.rm = TRUE) * 100, 1)
        termintreue_avg   <- round(data_remaining %>%
                           group_by(vorgangsfolge) %>%
                           summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE)) %>%
                           pull(rate) %>%
                           mean(na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol_performance_vgl <- if (termintreue_selected > termintreue_avg) {
            "👑"
        } else if (termintreue_selected < termintreue_avg) {
            "⚠️"
        } else {
            ""
        }
        
        # Ablegen der Daten zum plotten
        df_termintreue <- tibble::tibble(
            category = c("Termintreu", "Verspätet"),
            count = c(termintreue_selected, 100 - termintreue_selected)
        )
        
        farbe_performance_vgl <- if (symbol_performance_vgl == "⚠️") {
            "#E57373"  # rot
        } else if (symbol_performance_vgl == "👑") {
            "#81C784"  
        } else {
            "#cfcfcf"  
        }
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_termintreue %>%
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
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", termintreue_selected, "%"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    # 2. Liefertreue 
    output$donut_liefertreue_workflow <- renderEcharts4r({
        sel <- input$selected_vorgangsfolge
        data_selected <- vorgaenge_sorted %>% filter(vorgangsfolge == sel)
        data_remaining <- vorgaenge_sorted %>% filter(vorgangsfolge != sel)
        
        # Berechne Mittelwert aller gelieferten Mengen größer oder gleich 0 - Anteil Mengentreue
        liefertreue_selected <- round(mean(data_selected$`Gutmenge Vorgang` >= data_selected$sollmenge, na.rm = TRUE) * 100, 1)
        liefertreue_avg   <- round(mean(data_remaining$`Gutmenge Vorgang` >= data_remaining$sollmenge, na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol_performance_vgl <- if (liefertreue_selected > liefertreue_avg) {
            "👑"
        } else if (liefertreue_selected < liefertreue_avg) {
            "⚠️"
        } else {
            ""
        }
        
        tooltip_text <- if (liefertreue_selected > liefertreue_avg) {
            paste0("Overperformance, durchschn. Liefertreue derzeit ", liefertreue_avg, "%")
        } else if (liefertreue_selected < liefertreue_avg) {
            paste0("Underperformance, durchschn. Liefertreue derzeit ", liefertreue_avg, "%")
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (symbol_performance_vgl == "⚠️") {
            "#E57373"
        } else if (symbol_performance_vgl == "👑") {
            "#81C784"
        } else {
            "#cfcfcf"
        }
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        # Ablegen der Daten zum plotten
        df_liefertreue <- tibble::tibble(
            category = c("Liefertreu", "Unvollständig"),
            count = c(liefertreue_selected, 100 - liefertreue_selected)
        )
        
        df_liefertreue %>%
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
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = sprintf(
                    "{a|%s} {b|%s%%}", symbol_performance_vgl, liefertreue_selected
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
    
    
    # 3. Geschwindigkeit pro ME
    output$donut_geschwindigkeit_me_workflow <- renderEcharts4r({
        req(input$selected_vorgangsfolge)
        
        data_selected <- vorgaenge_sorted %>% filter(vorgangsfolge == input$selected_vorgangsfolge, !is.na(lt_ist_order))
        data_selected_valid <- vorgaenge_sorted %>% filter(!is.na(lt_ist_order))
        
        # Berechne Mittelwert aller Istzeiten und rechne in min um
        geschw_sel <- round(mean(data_selected$lt_ist_order / 60, na.rm = TRUE), 1)
        geschw_all <- round(mean(data_selected_valid$lt_ist_order / 60, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol_performance_vgl <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (rel_diff > 0) {
            "#81C784"  # grün
        } else if (rel_diff < 0) {
            "#E57373"  
        } else {
            "#cfcfcf"
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        donut_fill <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        donut_fill <- max(min(donut_fill, 100), 0)
        
        # Ablegen der Daten zum plotten
        df_geschwindigkeit_me <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(donut_fill, 100 - donut_fill)
        )
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_geschwindigkeit_me %>%
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
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", geschw_sel, " min"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
    # 4. Geschwindigkeit pro Auftrag
    output$donut_geschwindigkeit_auftrag_workflow <- renderEcharts4r({
        req(input$selected_vorgangsfolge)
        
        data_selected <- vorgaenge_sorted %>% filter(vorgangsfolge == input$selected_vorgangsfolge, !is.na(istdauer))
        data_selected_valid <- vorgaenge_sorted %>% filter(!is.na(istdauer))
        
        # Berechne den Median aller Istzeiten
        geschw_sel <- round(median(data_selected$istdauer, na.rm = TRUE), 1)
        geschw_all <- round(median(data_selected_valid$istdauer, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol_performance_vgl <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe_performance_vgl <- if (rel_diff > 0) {
            "#81C784"
        } else if (rel_diff < 0) {
            "#E57373"
        } else {
            "#cfcfcf"
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        donut_fill <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        donut_fill <- max(min(donut_fill, 100), 0)
        
        # Ablegen der Daten zum plotten
        df_geschwindigkeit_auftrag <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(donut_fill, 100 - donut_fill)
        )
        
        farben_performance_vgl <- c(farbe_performance_vgl, "#f0f0f0")
        
        df_geschwindigkeit_auftrag %>%
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
                    }", jsonlite::toJSON(farben_performance_vgl, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol_performance_vgl, " ", geschw_sel, " T"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    
# KPI-Boxen 
    
    # 1. Anzahl Aufträge für ausgewählten Workflow
    output$livetracker_auftraege_workflow <- renderUI({
        req(input$selected_vorgangsfolge)
        
        anzahl <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            summarise(n = n_distinct(Auftragsnummer)) %>%
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
    
    # Servicelevel gesamt
    overall_servicelevel <- reactive({
        sum(vorgaenge_sorted$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(vorgaenge_sorted$Auftragsnummer))
    })
    
    # 2. Servicelevel ausgewählter Workflow
    output$livetracker_servicelevel_workflow <- renderUI({
        req(input$selected_vorgangsfolge)
        
        filtered <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge)
        
        if (nrow(filtered) == 0) {
            return(
                div(
                    style = "display: flex; flex-direction: column;",
                    span(style = "font-weight: 600; font-size: 24px; color: #9e9e9e;", "–"),
                    span("Servicelevel", style = "color: #5f6368; font-size: 14px;")
                )
            )
        }
        
        # Ermittle den Anteil aller Abweichungen kleiner oder gleich null (zu früh oder JIT)
        sl <- sum(filtered$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(filtered$Auftragsnummer))
        overall_sl <- sum(vorgaenge_sorted$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(vorgaenge_sorted$Auftragsnummer))
        
        sl_percent <- paste0(round(sl * 100), "%")
        overall_text <- paste0("Overall Servicelevel = ", round(overall_sl * 100), "%")
        
        # Vergleich Performance mit Gesamtperformance
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
    
    # 3. Bottleneck der ausgewählten Ansicht
    output$livetracker_bottleneck_workflow <- renderUI({
        req(input$selected_vorgangsfolge, input$view_selection_workflow)
        
        selected <- lt_map_workflow[[input$view_selection_workflow]]
        label <- input$view_selection_workflow  
        
        # Ermittle Entität mit der höchsten mittleren Abweichung (Median) unter den verspäteten Aufträgen
        bottleneck_info <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung > 0) %>%
            filter(if (input$view_selection_workflow == "A-Material") klassifikation == "A" else TRUE) %>%
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
    
    output$allocation_title_workflow <- renderUI({
        req(input$selected_vorgangsfolge, input$view_selection_workflow)
        h4(
            paste0("Ansicht ", input$view_selection_workflow, " für Workflow ", input$selected_vorgangsfolge),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$abweichung_title_workflow <- renderUI({
        req(input$selected_vorgangsfolge, input$view_selection_workflow)
        h4(
            paste0("Ansicht Lead Time Abweichung für Workflow ", input$selected_vorgangsfolge),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    
    output$lt_title_workflow <- renderUI({
        req(input$selected_vorgangsfolge)
        h4(
            paste("Lead Time- und Performanceübersicht Workflow", input$selected_vorgangsfolge), 
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })

    
# Zweidimensionale KPIs (Linie + ausgewählte Ansicht)
    
    # 1. Tabelle mit aktuellen Verzögerungen, LTs, Servicelevel, # Aufträge
    output$delay_table_shared_workflow <- renderDT({
        req(input$selected_vorgangsfolge)
        req(input$view_selection_workflow)
        
        col_delay_table <- lt_map_workflow[[input$view_selection_workflow]]
        
        df_delay_table <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            filter(if (input$view_selection_workflow == "A-Material") klassifikation == "A" else TRUE) %>%
            # Nur positive Abweichungen als Verzögerung bewerten, sonst NA
            mutate(delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)) %>%
            group_by(value = .data[[col_delay_table]]) %>%
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
                !!rlang::sym(input$view_selection_workflow) := value,
                `Verzögerung [s/ME]`,
                `Ist-LT [s/ME]`,
                `Soll-LT [s/ME]`,
                `# Aufträge`
            )
        
        datatable(
            df_delay_table,
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
    
    # 2. Kuchendiagramm mit Verteilung der Aufträge 
    output$allocation_pie_shared_workflow <- renderEcharts4r({
        req(input$selected_vorgangsfolge)
        req(input$view_selection_workflow)
        
        blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
        col_allocation_pie <- lt_map_workflow[[input$view_selection_workflow]]
        
        df_allocation_pie <- vorgaenge_sorted %>%
            dplyr::filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            dplyr::filter(if (input$view_selection_workflow == "A-Material") klassifikation == "A" else TRUE) %>%
            dplyr::filter(!is.na(.data[[col_allocation_pie]])) %>%
            dplyr::group_by(category = .data[[col_allocation_pie]]) %>%
            dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(share = count / sum(count)) %>%
            dplyr::arrange(desc(share))
        
        # Splitten der Daten in Gruppen: Jede Linie, Workflow usw. dessen Anteil am 
        # Gesamtanteil aller Aufträge über 5% ausmacht wird einzeln abgebildet, alle 
        # anderen, die unter 5% ausmachen werden in einer Gruppe zusammengefasst
        df_main_groups <- df_allocation_pie %>% dplyr::filter(share >= 0.05)
        df_small_groups <- df_allocation_pie %>% dplyr::filter(share < 0.05)
        
        if (nrow(df_small_groups) > 0) {
            small_groups_total <- sum(df_small_groups$count)
            small_groups_label <- "Restliche"
            small_groups_tooltip <- paste(df_small_groups$category, collapse = ", ")
            
            df_main_groups <- dplyr::bind_rows(
                df_main_groups,
                tibble::tibble(category = small_groups_label, count = small_groups_total, share = small_groups_total / sum(df$count))
            )
        } else {
            small_groups_tooltip <- NULL
        }
        
        tooltip_formatter <- if (!is.null(small_groups_tooltip)) {
            htmlwidgets::JS(sprintf(
                "function(params) {
         if(params.name === 'Restliche') {
           return 'Restliche: %s';
         } else {
           return params.name + ': ' + params.value;
         }
       }", small_groups_tooltip
            ))
        } else {
            htmlwidgets::JS("function(params) { return params.name + ': ' + params.value; }")
        }
        
        df_main_groups %>%
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
    

    # 3.1 Verspätungen
    
    # 3.1.1 Übersichtstabelle Verteilung der Verzögerungen
    output$delay_quartile_summary_workflow <- renderDT({
        req(input$selected_vorgangsfolge)
        req(input$view_selection_workflow)
        
        col_delay_quartile_summary <- lt_map_workflow[[input$view_selection_workflow]]
        
        df_delay_quartile_summary <- vorgaenge_sorted %>%
            filter(
                vorgangsfolge == input$selected_vorgangsfolge,
                abweichung > 0,
                !is.na(abweichung),
                !is.na(.data[[col_delay_quartile_summary]]),
                if (input$view_selection_workflow == "A-Material") klassifikation == "A" else TRUE
            )
        
        # Einteilen der verspäteten Aufträge nach Stärke der Abweichung
        labels_delay_quartile_summary <- c("> 10", "10 bis 5", "5 bis 3", "3 bis 1")
        counts_delay_quartile_summary <- c(
            sum(df_delay_quartile_summary$abweichung > 10),
            sum(df_delay_quartile_summary$abweichung <= 10 & df_delay_quartile_summary$abweichung > 5),
            sum(df_delay_quartile_summary$abweichung <= 5 & df_delay_quartile_summary$abweichung > 3),
            sum(df_delay_quartile_summary$abweichung <= 3 & df_delay_quartile_summary$abweichung > 1)
        )
        share_delay_quartile_summary <- round(counts_delay_quartile_summary / sum(counts_delay_quartile_summary) * 100, 1)
        
        summary_delay_quartile_summary <- tibble(
            `Verzögerung [T]` = labels_delay_quartile_summary,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", share_delay_quartile_summary, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", share_delay_quartile_summary, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_q_10_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_105_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_53_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_31_workflow", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_delay_quartile_summary,
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
    
    # 3.1.2 Detail-Icons, die zu Detailtabellen führen
    
    # Verspätung über 10 Tage
    observeEvent(input$btn_q_10_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung > 10 Tage",
            DTOutput("modal_q10_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q10_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_10t_versp <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung > 10) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_10t_versp, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verspätung 5-10 Tage
    observeEvent(input$btn_q_105_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 5 und 10 Tagen",
            DTOutput("modal_q105_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q105_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_5_10t_versp <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit <= 10 & abweichung_unit > 5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_5_10t_versp, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verspätung 3-5 Tage
    observeEvent(input$btn_q_53_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 3 und 5 Tagen",
            DTOutput("modal_q53_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q53_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_3_5t_versp <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit <= 5 & abweichung_unit > 3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_3_5t_versp, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verspätung 1-3 Tage
    observeEvent(input$btn_q_31_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 1 und 3 Tagen",
            DTOutput("modal_q31_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q31_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_1_3t_versp <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit <= 3 & abweichung_unit > 1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_1_3t_versp, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    

    # 3.2 Verfrühungen
    
    # 3.2.1 Übersichtstabelle Verteilung der Verzögerungen
    output$early_quartile_summary_workflow <- renderDT({
        req(input$selected_vorgangsfolge)
        req(input$view_selection_workflow)
        
        col_early_quartile_summary <- lt_map_workflow[[input$view_selection_workflow]]
        
        df_early_quartile_summary <- vorgaenge_sorted %>%
            filter(
                vorgangsfolge == input$selected_vorgangsfolge,
                abweichung_unit < 0,
                !is.na(abweichung_unit),
                !is.na(.data[[col_early_quartile_summary]])
            )
        
        # Einteilen der verfrühten Aufträge nach Stärke der Abweichung
        labels_early_quartile_summary <- c("< -10", "-10 bis -5", "-5 bis -3", "-3 bis -1")
        counts_early_quartile_summary <- c(
            sum(df_early_quartile_summary$abweichung_unit < -10),
            sum(df_early_quartile_summary$abweichung_unit >= -10 & df_early_quartile_summary$abweichung_unit < -5),
            sum(df_early_quartile_summary$abweichung_unit >= -5 & df_early_quartile_summary$abweichung_unit < -3),
            sum(df_early_quartile_summary$abweichung_unit >= -3 & df_early_quartile_summary$abweichung_unit < -1)
        )
        share_early_quartile_summary <- round(counts_early_quartile_summary / sum(counts_early_quartile_summary) * 100, 1)
        
        summary_early_quartile_summary <- tibble(
            `Verfrühung [T]` = labels_early_quartile_summary,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", share_early_quartile_summary, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", share_early_quartile_summary, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_e_10_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_105_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_53_workflow", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_31_workflow", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_early_quartile_summary,
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
    
    
    # 3.2.2 Detail-Icons, die zu Detailtabellen führen
    
    # Verfrühung über 10 Tage
    observeEvent(input$btn_e_10_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung über 10 Tage",
            DTOutput("modal_e10_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e10_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_10t_verfr <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit < -10) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_10t_verfr, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verfrühung 5-10 Tage
    observeEvent(input$btn_e_105_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung von 5-10 Tagen",
            DTOutput("modal_e105_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e105_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_5_10t_verfr <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit >= -10 & abweichung_unit < -5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_5_10t_verfr, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verfrühung 3-5 Tage
    observeEvent(input$btn_e_53_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung von 3-5 Tagen",
            DTOutput("modal_e53_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e53_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_3_5t_verfr <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit >= -5 & abweichung_unit < -3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_3_5t_verfr, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    # Verfrühung 1-3 Tage
    observeEvent(input$btn_e_31_workflow, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung von 1-3 Tagen",
            DTOutput("modal_e31_workflow"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e31_workflow <- renderDT({
            req(input$selected_vorgangsfolge)
            
            df_order_1_3t_verfr <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_vorgangsfolge, abweichung_unit >= -3 & abweichung_unit < -1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df_order_1_3t_verfr, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    
    
# Plot Bearbeitungs- und Liegezeiten
    data_input <- reactive({
        ausgabe_df %>%
            filter(!is.na(vorgangsfolge))
    })
    
    aggregated_data <- reactive({
        req(input$selected_vorgangsfolge)
        
        data_input() %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            group_by(Vorgangsnummer) %>%
            summarise(median_istdauer = median(istdauer, na.rm = TRUE)) %>%
            ungroup()
    })
    
    output$balkenplot_workflow <- renderPlot({
        df_plot <- aggregated_data() %>%
            arrange(Vorgangsnummer) %>%
            mutate(
                kumulierte_dauer = cumsum(median_istdauer),
                grauanteil = kumulierte_dauer - median_istdauer,
                blauanteil = median_istdauer
            ) %>%
            pivot_longer(
                cols = c("grauanteil", "blauanteil"),
                names_to = "farbe",
                values_to = "dauer"
            )
        
        ggplot(df_plot, aes(x = Vorgangsnummer, y = dauer, fill = farbe)) +
            geom_col(width = 0.3) +
            geom_text(
                data = df_plot %>% filter(farbe == "blauanteil"),
                aes(label = round(dauer, 1), y = cumsum(dauer)), 
                vjust = -0.3,
                size = 3
            ) +
            scale_fill_manual(values = c(
                "grauanteil" = "#cccccc",
                "blauanteil" = "#6495ED"
            )) +
            labs(
                x = "Vorgang / Liegezeit",
                y = "Ist-LT [d]"
            ) +
            app_theme()
    })
    
    
 # Plot Sollmengenabhängige Lead Times
    
    est_plot_obj <- reactive({
        req(input$selected_vorgangsfolge)
        create_est_lt_combined(vorgaenge_sorted, input$selected_vorgangsfolge, session = session)
    })
    
    output$workflow_plot <- plotly::renderPlotly({
        result <- est_plot_obj()
        req(result)
        
        plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color")) %>%
            plotly::layout(
                legend = list(
                    traceorder = "normal",
                    title = list(text = "Variante")
                )
            ) %>%
            plotly::style(
                name = "Lead Time Ist", traces = 1
            ) %>%
            plotly::style(
                name = "Lead Time Soll", traces = 2
            )
    })
    
    
    
# Übersicht Lead Time Abweichung
    
    # 1. Abweichung im Zeitverlauf
    output$abweichung_time_plot_workflow <- renderPlotly({
        req(input$selected_vorgangsfolge)
        
        # Sortiere nach tatsächlichem Starttermin, aber berücksichige nur jeden 
        # 10. Wert (aus Darstellungsgründen)
        df_abweichung_time_plot <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            arrange(`Iststart Vorgang`) %>%
            slice(seq(1, n(), by = 10))
        
        plot_abweichung_time <- ggplot(df_abweichung_time_plot, aes(x = `Iststart Vorgang`, y = abweichung)) +
            geom_smooth(
                method = "loess", se = FALSE, span = 0.2, color = "#6495ED", size = 0.7
            ) +
            labs(
                x = "Ist-Starttermin",
                y = "Abweichung von Soll-LT [d]"
            ) +
            app_theme()  
        
        ggplotly(plot_abweichung_time, tooltip = c("x", "y")) %>%
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
    
    # 2. Lead Time Abweichung absolut
    plot_abweichung_histogram_workflow <- function(df, selected_vorgangsfolge) {
        df_filtered <- df %>%
            filter(vorgangsfolge == selected_vorgangsfolge & !is.na(abweichung))
        
        if (nrow(df_filtered) == 0) return(NULL)
        
        # Dynamische Grenzen ohne obere und untere 2,5% (v.a. aus Darstellungsgründen)
        abw_min <- quantile(df_filtered$abweichung, 0.025)
        abw_max <- quantile(df_filtered$abweichung, 0.975)
        
        plot_abweichung_histogram <- ggplot(df_filtered, aes(x = abweichung)) +
            geom_histogram(binwidth = 1, fill = "#cccccc", color = "white", boundary = 0) +
            labs(
                x = "Lead-Time-Abweichung [Tage]",
                y = "Anzahl Aufträge"
            ) +
            scale_x_continuous(limits = c(abw_min, abw_max)) +
            app_theme() 
        
        ggplotly(plot_abweichung_histogram)
    }
    
    output$abweichung_hist_plot_workflow <- renderPlotly({
        req(input$selected_vorgangsfolge)
        plot_abweichung_histogram_workflow(vorgaenge_sorted, input$selected_vorgangsfolge)
    })
    
    # 3. Lead Time Abweichung relativ
    abweichung_tabelle_workflow <- reactive({
        req(input$selected_vorgangsfolge)
        
        df_abw_rel <- vorgaenge_sorted %>%
            filter(vorgangsfolge == input$selected_vorgangsfolge) %>%
            filter(!is.na(lt_ist_order), !is.na(lt_soll_order), lt_soll_order > 0) %>%
            
            # Setze Abweichung ins Verhältnis zur Sollzeit und klassifiziere die Aufträge
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
    
    output$abweichungstabelle_workflow <- DT::renderDT({
        abweichung_tabelle_workflow()
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
