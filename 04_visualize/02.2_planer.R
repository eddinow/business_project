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
source("02_model/kpis_werke.R")
source("01_transform/create_lt_unit.R")

# Mapping zwischen UI-Label und Datenspalte
lt_map <- list(
    "Workflow" = "vorgangsfolge",
    "Werk"     = "werk",
    "Linie"    = "klassifikation",
    "Planer"   = "planer",
    "Material" = "materialnummer"
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


# UI -----------------------------------------------------------------------------
planerUI <- function() {
    tagList(
    
    # Sub-Header 
    div(
        style = "background-color: #f1f3f4; padding: 18px 32px; height: 72px;
         display: flex; align-items: center; justify-content: space-between;
         border-top: 1px solid #e0e0e0;",
        # Linke Seite: Icon + Titel
        div(
            style = "display: flex; align-items: center; gap: 12px;",
            icon("user-cog", class = NULL, style = "font-size: 20px; color: #5f6368;"),
            span(
                style = "font-size: 20px; font-weight: 600; color: #202124;",
                "Planer"
            )
        ),
        # Planer-Auswahl + Ansichtsauswahl
        div(
            style = "display: flex; align-items: center; gap: 24px;",
            # 1. Planer auswählen
            div(
                style = "display: flex; align-items: center; gap: 8px;",
                span(
                    style = "font-size: 14px; color: #202124; font-weight: 500;",
                    "1. Planer auswählen:"
                ),
                div(
                    style = "width: 180px;",
                    selectizeInput(
                        inputId = "selected_planer",
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
                        inputId = "view_selection_planer",
                        label = NULL,
                        choices = c("Workflow", "Linie", "Werk", "A-Material"),
                        selected = "Linie",
                        width = "100%"
                    )
                )
            )
        )
    ),
    
    # Inhalt
    div(style = "max-width: 1100px; margin: 0 auto;",
        # Rest des UI bleibt unverändert, beginnend mit Titel, KPI-Boxes, etc.
        div(
            style = "padding: 48px 0 12px 0;",
            uiOutput("planer_title")
        ),
        
        # KPI Boxen (Anzahl Aufträge, Servicelevel, Bottleneck)
        fluidRow(
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_auftraege_planer")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_servicelevel_planer")
                )
            ),
            column(
                width = 4,
                div(
                    class = "white-box",
                    style = "height: 60px; display: flex; justify-content: flex-start; align-items: center; padding-left: 68px; padding-right: 16px; margin-bottom: 20px;",
                    uiOutput("livetracker_bottleneck_planer")
                )
            )
        ),
        
        # Performance-KPIs des ausgewählten Planers
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
                                echarts4rOutput("donut_termintreue_planer", height = "160px"),
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
                                echarts4rOutput("donut_liefertreue_planer", height = "160px"),
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
                                echarts4rOutput("donut_geschwindigkeit_me_planer", height = "160px"),
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
                                echarts4rOutput("donut_geschwindigkeit_auftrag_planer", height = "160px"),
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
        
        # KPIs der ausgewählten Ansicht
        fluidRow(
            column(
                width = 12,
                div(
                    class = "white-box",
                    style = "background-color: rgba(255, 255, 255, 0.3);",
                    tagList(
                        
                        div(
                            style = "padding: 40px 0 15px 0;",
                            uiOutput("allocation_title_planer")
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
                                        DTOutput("delay_table_shared_planer"),
                                        
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
                                        
                                        echarts4rOutput("allocation_pie_shared_planer", height = "300px")
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
                                                DTOutput("delay_quartile_summary_planer")
                                            ),
                                            
                                            # Tab 2: Verfrühungsverteilung mit Lupenbuttons
                                            tabPanel(
                                                "Verfrühungen",
                                                DTOutput("early_quartile_summary_planer")
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
                            uiOutput("abweichung_title_planer")
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
                                        plotly::plotlyOutput("abweichung_time_plot_planer", height = "240px"),
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
                            # Lead Time Abweichung absolut
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
                                        plotly::plotlyOutput("abweichung_hist_plot_planer", height = "240px")
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
                                        DT::DTOutput("abweichungstabelle_planer")
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
planerServer <- function(input, output, session) {
    
    # Aufträge nach Planern filtern
    observe({
        planer <- unique(auftraege_lt_unit$planer)
        
        updateSelectizeInput(
            session,
            inputId = "selected_planer",
            choices = c("Planer auswählen" = "", planer),
            selected = "FST1", 
            server = TRUE
        )
    })
    
    # Beschränken auf A-Materialien
    get_filtered_data_planer <- function(df, selected_planer, selected_view) {
        df_filtered <- df %>%
            filter(planer == selected_planer)
        
        if (selected_view == "Material") {
            df_filtered <- df_filtered %>%
                filter(klassifikation == "A")
        }
        
        return(df_filtered)
    }
    
    
    output$planer_title <- renderUI({
        req(input$selected_planer)
        
        tags$div(
            style = "margin-top: 32px; margin-bottom: 32px;",
            tags$h2(
                paste("Details | Planer", input$selected_planer),
                style = "font-size: 25px; font-weight: 600; color: #202124; margin: 0;"
            )
        )
    })
    
# KPIs der ausgewählten Ansicht
    
    # 1. Termintreue [1]
    output$donut_termintreue_planer <- renderEcharts4r({
        sel <- input$selected_planer
        df_s <- auftraege_lt_unit %>% filter(planer == sel)
        df_o <- auftraege_lt_unit %>% filter(planer != sel)
        
        # Berechne Mittelwert aller Abweichungen kleiner oder gleich 0 - Anteil pünktlich
        value <- round(mean(df_s$abweichung_unit <= 0, na.rm = TRUE) * 100, 1)
        avg   <- round(df_o %>%
                           group_by(planer) %>%
                           summarise(rate = mean(abweichung_unit <= 0, na.rm = TRUE)) %>%
                           pull(rate) %>%
                           mean(na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol <- if (value > avg) {
            "👑"
        } else if (value < avg) {
            "⚠️"
        } else {
            ""
        }
        
        df <- tibble::tibble(
            category = c("Termintreu", "Verspätet"),
            count = c(value, 100 - value)
        )
        
        farbe <- if (symbol == "⚠️") {
            "#E57373"  # rot
        } else if (symbol == "👑") {
            "#81C784"  # grün
        } else {
            "#cfcfcf"  # grau
        }
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", value, "%"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    # 2. Liefertreue [2]
    output$donut_liefertreue_planer <- renderEcharts4r({
        sel <- input$selected_planer
        df_s <- auftraege_lt_unit %>% filter(planer == sel)
        df_o <- auftraege_lt_unit %>% filter(planer != sel)
        
        # Berechne Mittelwert aller gelieferten Mengen größer oder gleich 0 - Anteil Mengentreue
        value <- round(mean(df_s$gelieferte_menge >= df_s$sollmenge, na.rm = TRUE) * 100, 1)
        avg   <- round(mean(df_o$gelieferte_menge >= df_o$sollmenge, na.rm = TRUE) * 100, 1)
        
        # Vergleich von Performance mit Gesamtperformance
        symbol <- if (value > avg) {
            "👑"
        } else if (value < avg) {
            "⚠️"
        } else {
            ""
        }
        
        tooltip_text <- if (value > avg) {
            paste0("Overperformance, durchschn. Liefertreue derzeit ", avg, "%")
        } else if (value < avg) {
            paste0("Underperformance, durchschn. Liefertreue derzeit ", avg, "%")
        } else {
            ""
        }
        
        farbe <- if (symbol == "⚠️") {
            "#E57373"
        } else if (symbol == "👑") {
            "#81C784"
        } else {
            "#cfcfcf"
        }
        
        farben <- c(farbe, "#f0f0f0")
        
        df <- tibble::tibble(
            category = c("Liefertreu", "Unvollständig"),
            count = c(value, 100 - value)
        )
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = sprintf(
                    "{a|%s} {b|%s%%}", symbol, value
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
    
    
    # 3. Geschwindigkeit pro ME [3]
    output$donut_geschwindigkeit_me_planer <- renderEcharts4r({
        req(input$selected_planer)
        
        df_sel <- auftraege_lt_unit %>% filter(planer == input$selected_planer, !is.na(lt_ist_order))
        df_all <- auftraege_lt_unit %>% filter(!is.na(lt_ist_order))
        
        # Berechne Mittelwert aller Istzeiten und rechne in min um
        geschw_sel <- round(mean(df_sel$lt_ist_order / 60, na.rm = TRUE), 1)
        geschw_all <- round(mean(df_all$lt_ist_order / 60, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe <- if (rel_diff > 0) {
            "#81C784"  # grün
        } else if (rel_diff < 0) {
            "#E57373"  # rot
        } else {
            "#cfcfcf"  # grau
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        prozent <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        prozent <- max(min(prozent, 100), 0)
        
        df <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(prozent, 100 - prozent)
        )
        
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", geschw_sel, " min"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
    # 4. Geschwindigkeit pro Auftrag [4]
    output$donut_geschwindigkeit_auftrag_planer <- renderEcharts4r({
        req(input$selected_planer)
        
        df_sel <- auftraege_lt_unit %>% filter(planer == input$selected_planer, !is.na(lead_time_ist))
        df_all <- auftraege_lt_unit %>% filter(!is.na(lead_time_ist))
        
        # Berechne den Median aller Istzeiten
        geschw_sel <- round(median(df_sel$lead_time_ist, na.rm = TRUE), 1)
        geschw_all <- round(median(df_all$lead_time_ist, na.rm = TRUE), 1)
        rel_diff <- geschw_all - geschw_sel
        
        # Vergleich Performance mit Gesamtperformance
        symbol <- if (rel_diff > 0) {
            "👑"
        } else if (rel_diff < 0) {
            "⚠️"
        } else {
            ""
        }
        
        farbe <- if (rel_diff > 0) {
            "#81C784"
        } else if (rel_diff < 0) {
            "#E57373"
        } else {
            "#cfcfcf"
        }
        
        # Prozentfüllung basierend auf +/- 8-fachem Durchschnitt
        prozent <- (1 - (geschw_sel / (8 * geschw_all))) * 100
        prozent <- max(min(prozent, 100), 0)
        
        df <- tibble::tibble(
            category = c("Aktueller Wert", "Rest"),
            count = c(prozent, 100 - prozent)
        )
        
        farben <- c(farbe, "#f0f0f0")
        
        df %>%
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
                    }", jsonlite::toJSON(farben, auto_unbox = TRUE)
                    ))
                )
            ) %>%
            e_title(
                text = paste0(symbol, " ", geschw_sel, " T"),
                left = "center",
                top = "center",
                textStyle = list(fontSize = 20, fontWeight = "bold")
            ) %>%
            e_tooltip(show = FALSE) %>%
            e_legend(show = FALSE)
    })
    
 # KPI-Boxen 

    # 1. Anzahl Aufträge für ausgewählten Planer
    output$livetracker_auftraege_planer <- renderUI({
        req(input$selected_planer)
        
        anzahl <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer) %>%
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
    
    # Servicelevel gesamt
    overall_servicelevel <- reactive({
        sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) /
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
    })
    
    # 2. Servicelevel ausgewählter Planer
    output$livetracker_servicelevel_planer <- renderUI({
        req(input$selected_planer)
        
        filtered <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer)
        
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
            sum(!is.na(filtered$auftragsnummer))
        overall_sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(auftraege_lt_unit$auftragsnummer))
        
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
    output$livetracker_bottleneck_planer <- renderUI({
        req(input$selected_planer, input$view_selection_planer)
        
        selected <- lt_map[[input$view_selection_planer]]
        label <- input$view_selection_planer  
        
        # Ermittle Entität mit der höchsten mittleren Abweichung (Median) unter den verspäteten Aufträgen
        bottleneck_info <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer, abweichung > 0) %>%
            filter(if (input$view_selection_planer == "Material") klassifikation == "A" else TRUE) %>%
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
    
    output$allocation_title_planer <- renderUI({
        req(input$selected_planer, input$view_selection_planer)
        h4(
            paste0("Ansicht ", input$view_selection_planer, " für Planer ", input$selected_planer),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$abweichung_title_planer <- renderUI({
        req(input$selected_planer, input$view_selection_planer)
        h4(
            paste0("Ansicht Lead Time Abweichung für Planer ", input$selected_planer),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$performance_title_planer <- renderUI({
        h4(
            paste0("Ansicht Performance für Planer ", input$selected_planer),
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })
    
    output$lt_title_planer <- renderUI({
        req(input$selected_planer)
        h4(
            paste("Lead Time- und Performanceübersicht Planer", input$selected_planer), 
            style = "margin-bottom: 48px; font-weight: 600; color: #202124; font-size: 20px;"
        )
    })

    
    output$delay_table_shared_planer <- renderDT({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        col <- lt_map[[input$view_selection_planer]]
        
        df <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer) %>%
            filter(if (input$view_selection_planer == "Material") klassifikation == "A" else TRUE) %>%
            mutate(delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)) %>%
            group_by(value = .data[[col]]) %>%
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
                !!rlang::sym(input$view_selection_planer) := value,
                `Verzögerung [s/ME]`,
                `Ist-LT [s/ME]`,
                `Soll-LT [s/ME]`,
                `# Aufträge`
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
    
    
    output$allocation_pie_shared_planer <- renderEcharts4r({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        blau_palette <- c("#DCEEFF", "#A0C4FF", "#87BFFF", "#6495ED", "#1A73E8", "#4285F4", "#2B63B9", "#0B47A1")
        selected_col <- lt_map[[input$view_selection_planer]]
        
        df <- auftraege_lt_unit %>%
            dplyr::filter(planer == input$selected_planer) %>%
            dplyr::filter(if (input$view_selection_planer == "Material") klassifikation == "A" else TRUE) %>%
            dplyr::filter(!is.na(.data[[selected_col]])) %>%
            dplyr::group_by(category = .data[[selected_col]]) %>%
            dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(share = count / sum(count)) %>%
            dplyr::arrange(desc(share))
        
        df_main <- df %>% dplyr::filter(share >= 0.05)
        df_other <- df %>% dplyr::filter(share < 0.05)
        
        if (nrow(df_other) > 0) {
            other_total <- sum(df_other$count)
            other_label <- "Restliche"
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
         if(params.name === 'Restliche') {
           return 'Restliche: %s';
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
    
    
    
    output$top_delay_orders_planer <- renderDT({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        df <- auftraege_lt_unit %>%
            filter(
                planer == input$selected_planer,
                !is.na(abweichung_unit)
            ) %>%
            filter(if (input$view_selection_planer == "Material") klassifikation == "A" else TRUE) %>%
            arrange(desc(abweichung_unit)) %>%
            slice_head(n = 200) %>%
            transmute(
                `Auftragsnummer`     = auftragsnummer,
                `Abweichung [min/ME]`  = round(abweichung_unit, 2)/60
            )
        
        datatable(
            df,
            options = list(
                pageLength = 10,
                dom = 'tip',
                ordering = TRUE
            ),
            rownames = FALSE,
            class = "hover"
        )
    })
    
    ### 1. Neue Tabelle mit Verzögerungsbereichen erzeugen
    output$delay_quartile_summary_planer <- renderDT({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        selected_col <- lt_map[[input$view_selection_planer]]
        
        df <- auftraege_lt_unit %>%
            filter(
                planer == input$selected_planer,
                abweichung > 0,
                !is.na(abweichung),
                !is.na(.data[[selected_col]]),
                if (input$view_selection_planer == "Material") klassifikation == "A" else TRUE
            )
        
        labels <- c("> 10", "10 bis 5", "5 bis 3", "3 bis 1")
        counts <- c(
            sum(df$abweichung > 10),
            sum(df$abweichung <= 10 & df$abweichung > 5),
            sum(df$abweichung <= 5 & df$abweichung > 3),
            sum(df$abweichung <= 3 & df$abweichung > 1)
        )
        pcts <- round(counts / sum(counts) * 100, 1)
        
        summary_df <- tibble(
            `Verzögerung [T]` = labels,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", pcts, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", pcts, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_q_10_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_105_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_53_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_q_31_planer", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_df,
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
    
    
    observeEvent(input$btn_q_10_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung > 10 Tage",
            DTOutput("modal_q10_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q10_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung > 10) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_105_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 5 und 10 Tagen",
            DTOutput("modal_q105_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q105_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit <= 10 & abweichung_unit > 5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_53_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 3 und 5 Tagen",
            DTOutput("modal_q53_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q53_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit <= 5 & abweichung_unit > 3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_q_31_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verzögerung zwischen 1 und 3 Tagen",
            DTOutput("modal_q31_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_q31_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit <= 3 & abweichung_unit > 1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    
    output$top_early_orders_planer <- renderDT({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        df <- auftraege_lt_unit %>%
            filter(
                planer == input$selected_planer,
                !is.na(abweichung_unit),
                abweichung_unit < 0  # nur zu frühe
            ) %>%
            arrange(abweichung_unit) %>%  # früheste oben
            slice_head(n = 200) %>%
            transmute(
                Auftragsnummer     = auftragsnummer,
                Materialnummer     = materialnummer,
                `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                `Abweichung [T/Auftr.]` = round(abweichung, 2)
            )
        
        datatable(
            df,
            options = list(
                pageLength = 10,
                dom = 'tip',
                ordering = TRUE
            ),
            rownames = FALSE,
            class = "hover"
        )
    })
    
    output$early_quartile_summary_planer <- renderDT({
        req(input$selected_planer)
        req(input$view_selection_planer)
        
        selected_col <- lt_map[[input$view_selection_planer]]
        
        df <- auftraege_lt_unit %>%
            filter(
                planer == input$selected_planer,
                abweichung_unit < 0,
                !is.na(abweichung_unit),
                !is.na(.data[[selected_col]])
            )
        
        labels <- c("< -10", "-10 bis -5", "-5 bis -3", "-3 bis -1")
        counts <- c(
            sum(df$abweichung_unit < -10),
            sum(df$abweichung_unit >= -10 & df$abweichung_unit < -5),
            sum(df$abweichung_unit >= -5 & df$abweichung_unit < -3),
            sum(df$abweichung_unit >= -3 & df$abweichung_unit < -1)
        )
        pcts <- round(counts / sum(counts) * 100, 1)
        
        summary_df <- tibble(
            `Verfrühung [T]` = labels,
            `Anteil [%]` = paste0(
                "<div style='display: flex; align-items: center; gap: 8px;'>",
                "<span style='color: #9e9e9e; font-size: 12px; min-width: 24px;'>", pcts, "%</span>",
                "<div style='background-color: #e0e0e0; width: 80px; height: 8px; border-radius: 4px; overflow: hidden;'>",
                "<div style='width:", pcts, "%; background-color: #4285F4; height: 100%;'></div>",
                "</div>",
                "</div>"
            ),
            Details = c(
                as.character(actionButton("btn_e_10_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_105_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_53_planer", label = NULL, icon = icon("search"))),
                as.character(actionButton("btn_e_31_planer", label = NULL, icon = icon("search")))
            )
        )
        
        datatable(
            summary_df,
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
    observeEvent(input$btn_e_10_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung < -10 Tage",
            DTOutput("modal_e10_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e10_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_105_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -10 und -5 Tagen",
            DTOutput("modal_e105_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e105_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit >= -10 & abweichung_unit < -5) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_53_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -5 und -3 Tagen",
            DTOutput("modal_e53_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e53_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit >= -5 & abweichung_unit < -3) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    observeEvent(input$btn_e_31_planer, {
        showModal(modalDialog(
            title = "Aufträge mit Verfrühung zwischen -3 und -1 Tagen",
            DTOutput("modal_e31_planer"),
            size = "l", easyClose = TRUE, footer = modalButton("Schließen")
        ))
        
        output$modal_e31_planer <- renderDT({
            req(input$selected_planer)
            
            df <- auftraege_lt_unit %>%
                filter(planer == input$selected_planer, abweichung_unit >= -3 & abweichung_unit < -1) %>%
                transmute(
                    Auftragsnummer     = auftragsnummer,
                    Materialnummer     = materialnummer,
                    `Soll-LT [T/Auftr.]`   = round(lead_time_soll, 2),
                    `Ist-LT [T/Auftr.]`    = round(lead_time_ist, 2),
                    `Abweichung [T/Auftr.]` = round(abweichung, 2)
                )
            
            datatable(df, options = list(pageLength = 10, dom = 'lfrtip'), rownames = FALSE, class = "cell-border hover nowrap")
        })
    })
    
    
    
# Übersicht Lead Time Abweichung
    
    # 1. Abweichung im Zeitverlauf
    output$abweichung_time_plot_planer <- renderPlotly({
        req(input$selected_planer)
        
        # Sortiere nach tatsächlichem Starttermin, aber berücksichige nur jeden 
        # 10. Wert (aus Darstellungsgründen)
        df <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer) %>%
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
            app_theme()  # 👈 hier deine Theme-Funktion
        
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
    
    # 2. Lead Time Abweichung absolut
    plot_abweichung_histogram_planer <- function(df, selected_planer) {
        df_filtered <- df %>%
            filter(planer == selected_planer & !is.na(abweichung))
        
        if (nrow(df_filtered) == 0) return(NULL)
        
        # Dynamische Grenzen ohne obere und untere 2,5% (v.a. aus Darstellungsgründen)
        x_min <- quantile(df_filtered$abweichung, 0.025)
        x_max <- quantile(df_filtered$abweichung, 0.975)
        
        p <- ggplot(df_filtered, aes(x = abweichung)) +
            geom_histogram(binwidth = 1, fill = "#cccccc", color = "white", boundary = 0) +
            labs(
                x = "Lead-Time-Abweichung [Tage]",
                y = "Anzahl Aufträge"
            ) +
            scale_x_continuous(limits = c(x_min, x_max)) +
            app_theme() 
        
        ggplotly(p)
    }
    
    output$abweichung_hist_plot_planer <- renderPlotly({
        req(input$selected_planer)
        plot_abweichung_histogram_planer(vorgaenge_sorted, input$selected_planer)
    })
    
    
    # 3. Lead Time Abweichung relativ
    abweichung_tabelle_planer <- reactive({
        req(input$selected_planer)
        
        df <- auftraege_lt_unit %>%
            filter(planer == input$selected_planer) %>%
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
    
    output$abweichungstabelle_planer <- DT::renderDT({
        abweichung_tabelle_planer()
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
