# Import------------------------------------------------------------------------
library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)
library(DescTools)

# Tidy--------------------------------------------------------------------------


# Transform---------------------------------------------------------------------
source("02_model/create_workflows_overview.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)
source("01_transform/create_lt_unit.R", local = TRUE)
source("02_model/kpis_workflow_arbeitsplatz.R", local = TRUE)
source("02_model/kpis_workflow_liegezeit.R", local = TRUE)

# Visualize & Communicate-------------------------------------------------------

# UI----------------------------------------------------------------------------
workflows_ui <- function(id) {

    ns <- NS(id)
    tagList(
        h1("Workflows", style = "font-weight: bold; margin-bottom: 40px;"),
        
        fluidRow(
            infoBoxOutput(ns("overall_servicelevel")),
            infoBoxOutput(ns("avg_delay")),
            infoBoxOutput(ns("avg_lt"))
        ),
        
        fluidRow(
            box(title = "Quick view", width = 12, status = "primary",
                DT::DTOutput(ns("workflows_table"), width = "100%")
            )
        ),
        
        div(style = "margin-top: 20px;",
            fluidRow(
                box(title = "Workflow auswählen", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput(ns("selected_workflow"), "Workflow auswählen:", choices = NULL, width = "100%")
                )
            )
        ),
        
        uiOutput(ns("workflow_detail_title")),
        
        fluidRow(
            infoBoxOutput(ns("workflow_detail_value2")),
            infoBoxOutput(ns("workflow_detail_value3")),
            infoBoxOutput(ns("workflow_detail_value1"))
        ),
        
        fluidRow(
            column(width = 4,
                   box(title = "Werke", width = NULL, status = "primary", solidHeader = FALSE, 
                       DTOutput(ns("detail_table_a"))
                   )),
            column(width = 4,
                   box(title = "Linien", width = NULL, status = "primary", solidHeader = FALSE,
                       DTOutput(ns("detail_table_b"))
                   )),
            column(width = 4,
                   box(title = "Planer", width = NULL, status = "primary", solidHeader = FALSE,
                       DTOutput(ns("detail_table_c"))
                   ))
        ),
        
        fluidRow(
            box(title = "Lead Times", width = 12, status = "primary", solidHeader = FALSE,
                div(
                    tags$h4(
                        "Lead Time nach Sollmenge [d]", 
                        icon("info-circle", id = ns("info_leadtimes")),
                        style = "font-weight: bold; font-size: 16px;"),
                    
                    bsPopover(
                        id = ns("info_leadtimes"),
                        title = "Was wird hier gezeigt?",
                        content = "Ist- und Soll-LT für jeden Vorgang des ausgewählten Workflows in der Einheit LT/unit [s]",
                        placement = "right",
                        trigger = "hover"
                    ),
                    plotlyOutput(ns("workflow_plot")),
                    br()
                ),
                
                br(),
                br(),
                br(),
                
                
                fluidRow( 
                    column(
                        width = 6,  
                        div(
                            tags$h4(
                                "Lead Time nach Vorgangsnummer [s/unit] ",
                                icon("info-circle", id = ns("info_leadtimes")),
                                style = "font-weight: bold; font-size: 16px;"
                            ),
                            bsPopover(
                                id = ns("info_leadtimes"),
                                title = "Was wird hier gezeigt?",
                                content = "Ist- und Soll-LT für jeden Vorgang des ausgewählten Workflows in der Einheit LT/unit [s]",
                                placement = "right",
                                trigger = "hover"
                            ),
                            plotOutput(ns("leadtime_chart"), height = "400px"),
                            br()
                    )
                ),
                
                column(
                    width = 6,
                    div(
                        tags$h4(
                            "Lead Time inkl. Liegezeiten [d/auftrag]",
                            icon("info-circle", id = ns("info_leadtimes")),
                            style = "font-weight: bold; font-size: 16px;"
                        ),
                        bsPopover(
                            id = ns("info_lztimes"),
                            title = "Was wird hier gezeigt?",
                            content = "Durchschnittliche LT für jeden Vorgang des ausgewählten Workflows in der Einheit LT [d].",
                            placement = "right",
                            trigger = "hover"
                        ),
                        plotOutput(ns("balkenplot"), height = "400px"),
                        br()
                        )
                    )
                )
                    
                )
            ),
        
        fluidRow(
            box(title = "Performance", width = 12, status = "primary", solidHeader = FALSE,
                column(
                    width = 12,
                    div(
                        tags$h4(
                            "Abweichung im Zeitverlauf",
                            icon("info-circle", id = ns("info_performance")),
                            style = "font-weight: bold; font-size: 16px;"
                        ),
                        bsPopover(
                            id = ns("info_performance"),
                            title = "Was wird hier gezeigt?",
                            content = "Abweichung zwischen Ist- und Soll-LT seit Beobachtungsbeginn. Positive Werte stellen Verzögerungen dar (Ist>Soll).",
                            placement = "right",
                            trigger = "hover"
                        ),
                        plotlyOutput(ns("abweichung_time_plot")),
                        br()
                    )
                ),
                
                br(),
                br(),
                br(),

                fluidRow(
                    column(
                        width = 6,
                        div(
                            tags$h4(
                                "Verteilung der Abweichung",
                                icon("info-circle", id = ns("info_streuung")),
                                style = "font-weight: bold; font-size: 16px;"
                            ),
                            bsPopover(
                                id = ns("info_streuung"),
                                title = "Was wird hier gezeigt?",
                                content = "Streuung der auftretenden Lead Time-Abweichungen. Positive Werte stellen Verzögerungen dar (Ist>Soll).",
                                placement = "right",
                                trigger = "hover"
                            ),
                            plotlyOutput(ns("abweichung_hist_plot"))
                        )
                    )
                )
            )
        )
    )
}


# Server------------------------------------------------------------------------
workflows_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$overall_servicelevel <- renderInfoBox({
            sl <- sum(auftraege_lt_unit$abweichung_unit <= 0, na.rm = TRUE) / 
                sum(!is.na(auftraege_lt_unit$abweichung_unit))
            
            sl_percent <- round(sl * 100)
            
            color <- if (sl_percent < 70) {
                "red"
            } else if (sl_percent < 95) {
                "orange"
            } else {
                "green"
            }
            
            infoBox(
                title = "Overall Servicelevel",
                value = paste0(sl_percent, "%"),
                icon = icon("percent"),
                color = color,
                fill = TRUE
            )
        })
        
        
        output$avg_delay <- renderInfoBox({
            median_delay <- round(
                median(workflows_overview$`Avg Delay/Unit [s]`, na.rm = TRUE),
                2
            )
            
            infoBox(
                title = "Avg. Delay/Unit [s]",
                value = paste0(median_delay),
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        output$avg_lt <- renderInfoBox({
            lt <- round(median(workflows_overview$`Ist-LT/Unit [s]`, na.rm = TRUE), 2)
            
            infoBox(
                title = "Avg LT/Unit [s]",
                value = round(lt, 1),
                icon = icon("clock"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        

      
        output$workflows_table <- renderDT({
            datatable(
                workflows_overview,
                escape = which(colnames(workflows_overview) != "ampel"),
                options = list(
                    pageLength = 10,
                    dom = 'tip',
                    scrollX = TRUE,
                    columnDefs = list(
                        list(visible = FALSE, targets = 0),       # ampel_color
                        list(width = '25px', targets = 1),        # ampel
                        list(orderData = 0, targets = 1),         # sortieren nach Farbe
                        list(title = "", targets = 1)             # keine Spaltenüberschrift
                    )
                ),
                rownames = FALSE,
                class = "stripe hover cell-border",
                width = "100%"
            )
        }, server = FALSE, fillContainer = TRUE)

        observe({
            workflows <- unique(all_data_finalized$vorgangsfolge)
            updateSelectInput(session, "selected_workflow", choices = workflows)
        })
        
        # User können sehen, in welche Werke, Linien und Planer dem ausgewählten
        # Workflow zugeodnet sind. Avg. LT/Order und Delay pro Werk, Linie, Planer 
        # wird angezeigt. Als Delays gelten alle Aufträge, die zu spät fertig
        # wurden (Abweichung Ist-Soll<0). Bottlenecks werden aufgedeckt.
        
        # Werke
        output$detail_table_a <- renderDT({
            req(input$selected_workflow)
            
            df <- auftraege_lt_unit %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                mutate(
                    delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                ) %>%
                group_by(werk) %>%
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
                        "<div style='background-color: ", ampel_color, 
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
                class = "stripe hover cell-border"
            )
        })
        
        
        # Fertigungslinien
        output$detail_table_b <- renderDT({
            req(input$selected_workflow)
            
            df <- auftraege_lt_unit %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                mutate(
                    delay_capped = ifelse(abweichung_unit < 0, NA, abweichung_unit)
                ) %>%
                group_by(fertigungslinie) %>%
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
                class = "stripe hover cell-border"
            )
        })
        
        # Planer
        output$detail_table_c <- renderDT({
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
                class = "stripe hover cell-border"
            )
        })
        
        
    # Plot Liegezeiten u Bearbeitungszeiten
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

        
    # PRÜFEN-------------
        # Plot: Soll vs. Ist
        est_plot_obj <- reactive({
            req(input$selected_workflow)
            create_est_lt_combined(all_data_finalized, input$selected_workflow, session = session)
        })
        
        output$workflow_plot <- plotly::renderPlotly({
            result <- est_plot_obj()
            req(result)
            plotly::ggplotly(result$plot, tooltip = c("x", "y", "fill", "color"))
        })
        
        # Abbilden Plot aus vorgaenge_lz_bz (Übersicht Bearbeitungs- u. Liegezeit)
        output$stacked_bar_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow)
            
            plot_workflow_structure(df)
        })
        
        # Abbilden Histogramm der Abweichungen
        output$abweichung_hist_plot <- renderPlotly({
            req(input$selected_workflow)
            plot_abweichung_histogram(vorgaenge_sorted, input$selected_workflow)
        })
        
        # Detailtabelle Zeitanteile, gehört zu vorgaenge_lz_bz Plot
        output$workflow_table_detail <- renderDT({
            req(input$selected_workflow)
            
            df <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                pivot_longer(
                    cols = ends_with("_median"),
                    names_to = "Vorgang",
                    values_to = "Time"
                ) %>%
                filter(!is.na(Time)) %>%
                mutate(
                    Vorgang = str_replace(Vorgang, "_median", ""),
                    Vorgang = ifelse(Vorgang == "Liegedauer_gesamt", "Avg. Delay", Vorgang)
                ) %>%
                arrange(desc(Time)) %>%
                mutate(
                    `Avg. LT (d)` = round(Time, 1),
                    `Avg. LT (%)` = round(Time / sum(Time) * 100, 1)
                ) %>%
                dplyr::select(Vorgang, `Avg. LT (d)`, `Avg. LT (%)`)

            total_row <- df %>%
                summarise(
                    Vorgang = "Gesamt",
                    `Avg. LT (d)` = sum(`Avg. LT (d)`, na.rm = TRUE),
                    `Avg. LT (%)` = sum(`Avg. LT (%)`, na.rm = TRUE)
                )
            
            df <- bind_rows(df, total_row)
            
            datatable(
                df,
                escape = FALSE,
                rownames = FALSE,
                options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE
                ),
                class = "stripe cell-border compact"
            ) %>%
                formatStyle(
                    columns = names(df),
                    target = "row",
                    fontWeight = styleEqual("Gesamt", "bold"),
                    backgroundColor = styleEqual("Gesamt", "#f0f0f0"),
                    color = styleEqual("Gesamt", "black")
                ) %>%
                formatStyle(
                    columns = names(df),
                    textAlign = 'center'
                )
        })
        
# Detailansicht ausgewählter Workflow-------------------------------------------
        output$table_title <- renderText({
            req(input$selected_workflow)
            input$selected_workflow
        })
        
        output$workflow_detail_title <- renderUI({
            req(input$selected_workflow)
            h2(paste("Detailansicht Workflow", input$selected_workflow), style = "margin-top: 40px; margin-bottom: 20px;")
        })
        
        # Infobox Avg. LT
        output$workflow_value1 <- renderInfoBox({
            infoBox(title = "Avg. LT", value = "", icon = icon("clock"), color = "light-blue", fill = FALSE)
        })
        
        # Infobox Avg. Delay
        output$workflow_value2 <- renderInfoBox({
            infoBox(title = "Avg. Delay", value = "", icon = icon("hourglass-half"), color = "yellow", fill = FALSE)
        })
        
        # Infobox Servicelevel
        output$workflow_value4 <- renderInfoBox({
            infoBox(title = "Servicelevel", value = "", icon = icon("percent"), color = "teal", fill = FALSE)
        })
        
        # Infobox Anzahl Aufträge
       output$workflow_value3 <- renderInfoBox({
            infoBox(title = "# Orders", 
                    value = if (!is.null(input$selected_workflow)) {
                        nrow(vorgaenge_sorted[vorgaenge_sorted$vorgangsfolge == input$selected_workflow, ])
                    } else {
                        ""
                    }, 
                    icon = icon("list-ol"), color = "green", fill = FALSE)
        })
        
       output$workflow_detail_value2 <- renderInfoBox({
           req(input$selected_workflow)
           
           # Detail-Infobox Servicelevel
           servicelevel <- auftraege_lt_unit %>%
               filter(vorgangsfolge == input$selected_workflow) %>%
               summarise(
                   servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
                       sum(!is.na(abweichung_unit))
               ) %>%
               pull(servicelevel_numeric)
           
           sl_percent <- round(servicelevel * 100, 0)
           
           color <- if (sl_percent < 70) {
               "red"
           } else if (sl_percent < 95) {
               "orange"
           } else {
               "green"
           }
           
           infoBox(
               title = "Servicelevel",
               value = paste0(sl_percent, "%"),
               icon = icon("percent"),
               color = color,
               fill = FALSE
           )
       })
       
       #Detail-Infobox Bottleneck
       output$workflow_detail_value3 <- renderInfoBox({
           req(input$selected_workflow)
           
           bottleneck_info <- vorgaenge_sorted %>%
               filter(vorgangsfolge == input$selected_workflow & abweichung > 0) %>%
               group_by(Vorgangsnummer) %>%
               summarise(median_abweichung = median(abweichung, na.rm = TRUE), .groups = "drop") %>%
               arrange(desc(median_abweichung)) %>%
               slice(1)
           
           if (nrow(bottleneck_info) == 0) {
               infoBox(
                   title = "Bottleneck",
                   value = "Kein positiver Wert",
                   icon = icon("exclamation-triangle"),
                   color = "light-blue",
                   fill = FALSE
               )
           } else {
               infoBox(
                   title = "Bottleneck",
                   value = paste0("Vg. ", bottleneck_info$Vorgangsnummer, ": currently ", round(bottleneck_info$median_abweichung, 1), " Tage Delay"),
                   icon = icon("exclamation-triangle"),
                   color = "light-blue",
                   fill = FALSE
               )
           }
       })
        
       #Detail-Infobox Anzahl Aufträge
        output$workflow_detail_value1 <- renderInfoBox({
            req(input$selected_workflow)
            count <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                nrow()
            
            infoBox(
                title = "# Orders",
                value = count,
                icon = icon("list-ol"),
                color = "light-blue",
                fill = FALSE
            )
        })
        
       
       # Diagramm Vergleich Soll- und Ist-LTs 
        output$workflow_ist_soll_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- vorgaenge_sorted %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                group_by(vorgangsnummer) %>%
                summarise(
                    Ist = median(istdauer, na.rm = TRUE),
                    Soll = median(solldauer, na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                filter(!is.na(Ist), !is.na(Soll))
            
            p <- ggplot(df, aes(x = factor(vorgangsnummer))) +
                geom_col(aes(y = Ist), fill = "#002366", width = 0.6) +
                geom_segment(aes(x = as.numeric(factor(vorgangsnummer)) - 0.3,
                                 xend = as.numeric(factor(vorgangsnummer)) + 0.3,
                                 y = Soll, yend = Soll),
                             color = "darkred", linewidth = 1) +
                labs(
                    x = "Vorgang",
                    y = "Median Dauer [Tage]"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = c("x", "y"))
        })
        
        # Diagramm Bearbeitungs- und Liegezeiten
        output$balkenplot <- renderPlot({
            df_plot <- aggregated_data()
            
            ggplot(df_plot, aes(x = Vorgangsnummer, y = median_istdauer)) +
                geom_col(fill = "#002366", width = 0.15) +
                labs(
                    x = "Vorgang / Liegezeit",
                    y = "Ist-LT [d]"
                ) +
                theme_minimal() +
                theme(axis.text.x = element_text(hjust = 1))
        })
        
        
        # Diagramm Zeitverlauf Abweichung Soll-Ist
        output$abweichung_time_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- all_data_finalized %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                arrange(starttermin_ist) %>%
                slice(seq(1, n(), by = 10))  # 👈 nur jeden 10. Wert behalten
            
            p <- ggplot(df, aes(x = starttermin_ist, y = abweichung)) +
                # geom_line(color = "#002366", size = 0.2) +
                # geom_point(color = "#002366", size = 0.3, alpha = 0.6) +
                geom_smooth(
                    method = "loess", se = FALSE, span = 0.2, color = "#002366", size = 0.7
                ) +
                labs(
                    x = "Ist-Starttermin",
                    y = "Abweichung von Soll-LT [d]"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = c("x", "y"))
        })
        
        # Diagramm Bearbeitungszeit + Liegezeit
        plot_workflow_structure <- function(df) {
            df <- df %>%
                pivot_longer(
                    cols = ends_with("_median"),
                    names_to = "Vorgang",
                    values_to = "Time"
                ) %>%
                filter(!is.na(Time)) %>%
                mutate(
                    Vorgang = str_replace(Vorgang, "_median", ""),
                    Vorgang = ifelse(Vorgang == "Liegedauer_gesamt", "Avg. Delay", Vorgang)
                )
            
            ggplot(df, aes(x = "", y = Time, fill = Vorgang)) +
                geom_bar(stat = "identity", width = 1) +
                coord_flip() +
                theme_void() +
                theme(legend.position = "none") +
                scale_fill_manual(values = rep("#bdd7e7", nrow(df))) +
                geom_text(aes(label = Vorgang), color = "white", size = 5, position = position_stack(vjust = 0.5))
        }
        
# PRÜFEN----------------------------------
        
        output$leadtime_chart <- renderPlot({
            req(input$selected_workflow)
            
            # Aggregation
            lt_agg <- vorgaenge_lt_unit %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                group_by(Vorgangsnummer) %>%
                summarise(
                    ist_lt = median(lt_ist_order, na.rm = TRUE),
                    soll_lt = Mode(lt_soll_order, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                filter(!is.na(ist_lt), !is.na(soll_lt))
            
            
            # Plot
            ggplot(lt_agg, aes(x = factor(Vorgangsnummer))) +
                # Balken: halb so breit, etwas weniger transparent
                geom_col(aes(y = ist_lt), fill = "#002366", alpha = 0.6, width = 0.15) +
                
                # Linie: ebenfalls nur halb so breit (x-Spanne) und dünner
                geom_segment(aes(
                    x = as.numeric(factor(Vorgangsnummer)) - 0.075,
                    xend = as.numeric(factor(Vorgangsnummer)) + 0.075,
                    y = soll_lt,
                    yend = soll_lt
                ), color = "darkred", linewidth = 0.6) +
                
                # Text: klein, leserlich
                geom_text(aes(
                    y = soll_lt + max(ist_lt) * 0.05,
                    label = paste0(round(soll_lt, 2), " h")
                ), color = "black", size = 4) +
                
                # Layout
                labs(
                    x = "Vorgangsnummer",
                    y = "Lead Time per Unit [s]",
                    caption = "Balken = Median Ist-LT | Linie = Median Soll-LT"
                ) +
                theme_minimal() +
                theme(
                    plot.caption = element_text(hjust = 1, size = 8),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 11))
        })
        
    })
}
