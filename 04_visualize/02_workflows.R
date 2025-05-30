library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)
library(DescTools)

source("02_model/create_workflows_overview.R", local = TRUE)
source("01_transform/create_est_lt_per_workflow.R", local = TRUE)
source("01_transform/create_lt_unit.R", local = TRUE)
source("02_model/kpis_workflow_arbeitsplatz.R", local = TRUE)

# UI---------------------------------------------
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
                
                # Mengenabhängige Lead Time
                div(
                    tags$h4("Mengenabhängige Lead Time", style = "font-weight: bold; font-size: 16px;"),
                    plotlyOutput(ns("workflow_plot")),
                    br()
                ),
                
                br(),  # Abstand vor nächstem Diagramm
                
                fluidRow(
                    column(
                        width = 6,  # 👈 vorher 8
                        div(
                            tags$h4(
                                "Detailansicht Ist-Lead Times ",
                                icon("info-circle", id = ns("info_leadtimes")),
                                style = "font-weight: bold; font-size: 16px;"
                            ),
                            bsPopover(
                                id = ns("info_leadtimes"),
                                title = "Was wird hier gezeigt?",
                                content = "Durchschnittliche LT für jeden Vorgang des ausgewählten Workflows inkl. eventuelle durchschn. Liegezeiten.",
                                placement = "right",
                                trigger = "hover"
                            ),
                            plotlyOutput(ns("stacked_bar_plot"), height = "100px")  # 👈 z. B. schmaler
                        )
                    ),
                    column(
                        width = 6,  # ggf. Tabelle etwas vergrößern
                        div(
                            DTOutput(ns("workflow_table_detail"))
                        )
                    ),
                    
                ),
                
                fluidRow( 
                    column(
                        width = 6,  # 75 % Breite von 12
                        div(
                            tags$h4(
                                "Lead Time je Vorgang ",
                                icon("info-circle", id = ns("info_leadtimes")),
                                style = "font-weight: bold; font-size: 16px;"
                            ),
                            bsPopover(
                                id = ns("info_leadtimes"),
                                title = "Was wird hier gezeigt?",
                                content = "Durchschnittliche LT für jeden Vorgang des ausgewählten Workflows inkl. eventuelle durchschn. Liegezeiten.",
                                placement = "right",
                                trigger = "hover"
                            ),
                            plotOutput(ns("leadtime_chart"), height = "400px"),
                            br()
                    )
                ),
                    
                )
            )
        ),
        
        fluidRow(
            box(title = "Performance", width = 12, status = "primary", solidHeader = FALSE,
                column(
                    width = 12,
                    div(
                        tags$h4(
                            "Zeitverlauf der Lead Time-Abweichung",
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
                

                fluidRow(
                    column(
                        width = 6,
                        div(
                            tags$h4(
                                "Streuung der Lead Time-Abweichung",
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
                    ),
                    column(
                        width = 6,
                        div(
                            tags$h4("Soll-Ist Vergleich pro Vorgang", style = "font-weight: bold; font-size: 16px;"),
                            plotlyOutput(ns("workflow_ist_soll_plot"))
                        )
                    )
                )
            )
        ),
        
    )  # schließt tagList
    
}


# Server----------------------------------------------------
workflows_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$overall_servicelevel <- renderInfoBox({
            sl <- mean(all_data_finalized$abweichung <= 0, na.rm = TRUE)
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
            delay <- median(all_data_finalized$abweichung, na.rm = TRUE)
            
            infoBox(
                title = "Avg. Delay/Order [d]",
                value = round(delay, 1),
                icon = icon("hourglass-half"),
                color = "light-blue",
                fill = TRUE
            )
        })
        
        output$avg_lt <- renderInfoBox({
            lt <- median(all_data_finalized$lead_time_ist, na.rm = TRUE)
            
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
                    colnames = c(
                        "ampel" = "ampel",
                        "Workflow" = "Workflow",
                        "Avg LT/Unit [s]" = "Avg LT/Unit [s]",
                        "Avg Delay/Unit [s]" = "Avg Delay/Unit [s]",
                        "# Orders" = "# Orders",
                        "Servicelevel" = "Servicelevel"
                    ),
                    options = list(
                        pageLength = 10,
                        dom = 'tip',
                        scrollX = TRUE,
                        columnDefs = list(
                            list(visible = FALSE, targets = 0),  # ampel_color
                            list(width = '25px', targets = 1),   # ampel
                            list(orderData = 0, targets = 1)     # sortiere nach ampel_color
                        )
                    ),
                    rownames = FALSE,
                    class = "stripe hover cell-border",
                    width = "100%"
                )
            }, server = FALSE, fillContainer = TRUE)
            
        # Dropdown füllen
        observe({
            workflows <- unique(all_data_finalized$vorgangsfolge)
            updateSelectInput(session, "selected_workflow", choices = workflows)
        })
        
        # Wir geben für jedes Werk/Linie/Planer die Avg. LT/Order an. Damit man
        # weiß ob akuter delay herrscht bewerten wir alle Abweichungen kleiner 0
        # zu früh gefertigte Aufträge) und nehmen dann erst den Mittelwert
        
        output$detail_table_a <- renderDT({
            req(input$selected_workflow)
            
            df <- all_data_finalized %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                mutate(
                    delay_capped = ifelse(abweichung < 0, 0, abweichung)
                ) %>%
                group_by(werk) %>%
                summarise(
                    `Avg. Delay/Order [d]` = round(mean(delay_capped, na.rm = TRUE), 1),
                    .groups = "drop"
                ) %>%
                mutate(
                    ampel_color = case_when(
                        `Avg. Delay/Order [d]` <= 0 ~ "green",
                        `Avg. Delay/Order [d]` <= 3 ~ "orange",
                        TRUE ~ "red"
                    ),
                    ampel = paste0(
                        "<div style='color: ", ampel_color, 
                        "; font-size: 20px; text-align: center;'>&#9679;</div>"
                    )
                ) %>%
                dplyr::select(ampel_color, ampel, Werk = werk, `Avg. Delay/Order [d]`)
            
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
        
        output$detail_table_b <- renderDT({
            req(input$selected_workflow)
            
            df <- all_data_finalized %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                mutate(delay_capped = ifelse(abweichung < 0, 0, abweichung)) %>%
                group_by(fertigungslinie) %>%
                summarise(
                    `Avg. Delay/Order [d]` = round(mean(delay_capped, na.rm = TRUE), 1),
                    .groups = "drop"
                ) %>%
                mutate(
                    ampel_color = dplyr::case_when(
                        `Avg. Delay/Order [d]` <= 0 ~ "green",
                        `Avg. Delay/Order [d]` <= 3 ~ "orange",
                        TRUE ~ "red"
                    ),
                    ampel = paste0(
                        "<div style='color: ", ampel_color, 
                        "; font-size: 20px; text-align: center;'>&#9679;</div>"
                    )
                ) %>%
                dplyr::select(ampel_color, ampel, Linie = fertigungslinie, `Avg. Delay/Order [d]`)
            
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
        
        output$detail_table_c <- renderDT({
            req(input$selected_workflow)
            
            df <- all_data_finalized %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                mutate(delay_capped = ifelse(abweichung < 0, 0, abweichung)) %>%
                group_by(planer) %>%
                summarise(
                    `Avg. Delay/Order [d]` = round(mean(delay_capped, na.rm = TRUE), 1),
                    .groups = "drop"
                ) %>%
                mutate(
                    ampel_color = dplyr::case_when(
                        `Avg. Delay/Order [d]` <= 0 ~ "green",
                        `Avg. Delay/Order [d]` <= 3 ~ "orange",
                        TRUE ~ "red"
                    ),
                    ampel = paste0(
                        "<div style='color: ", ampel_color, 
                        "; font-size: 20px; text-align: center;'>&#9679;</div>"
                    )
                ) %>%
                dplyr::select(ampel_color, ampel, Planer = planer, `Avg. Delay/Order [d]`)
            
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
        
        # Balkendiagramm Zeitanteile
        output$stacked_bar_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- vorgaenge_lz_bz %>%
                filter(vorgangsfolge == input$selected_workflow)
            
            plot_workflow_structure(df)
        })
        
        # Histogramm der Abweichungen
        output$abweichung_hist_plot <- renderPlotly({
            req(input$selected_workflow)
            plot_abweichung_histogram(vorgaenge_sorted, input$selected_workflow)
        })
        
        # Detailtabelle Zeitanteile
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
            
            # Summenzeile hinzufügen
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
        
        output$table_title <- renderText({
            req(input$selected_workflow)
            input$selected_workflow
        })
        
        output$workflow_detail_title <- renderUI({
            req(input$selected_workflow)
            h2(paste("Detailansicht Workflow", input$selected_workflow), style = "margin-top: 40px; margin-bottom: 20px;")
        })
        
        # InfoBoxes (initial leer)
        output$workflow_value1 <- renderInfoBox({
            infoBox(title = "Avg. LT", value = "", icon = icon("clock"), color = "light-blue", fill = FALSE)
        })
        
        output$workflow_value2 <- renderInfoBox({
            infoBox(title = "Avg. Delay", value = "", icon = icon("hourglass-half"), color = "yellow", fill = FALSE)
        })
        
        output$workflow_value4 <- renderInfoBox({
            infoBox(title = "Servicelevel", value = "", icon = icon("percent"), color = "teal", fill = FALSE)
        })
        
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
           servicelevel <- vorgaenge_sorted %>%
               filter(vorgangsfolge == input$selected_workflow) %>%
               summarise(Servicelevel = mean(abweichung <= 0, na.rm = TRUE)) %>%
               pull(Servicelevel)
           
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
                             color = "red", linewidth = 1) +
                labs(
                    x = "Vorgang",
                    y = "Median Dauer [Tage]"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = c("x", "y"))
        })
        
        
        output$abweichung_time_plot <- renderPlotly({
            req(input$selected_workflow)
            
            df <- all_data_finalized %>%
                filter(vorgangsfolge == input$selected_workflow) %>%
                arrange(starttermin_ist) %>%
                slice(seq(1, n(), by = 10))  # 👈 nur jeden 10. Wert behalten
            
            p <- ggplot(df, aes(x = starttermin_ist, y = abweichung)) +
                geom_line(color = "#002366", size = 0.2) +
                geom_point(color = "#002366", size = 0.3, alpha = 0.6) +
                geom_smooth(
                    method = "loess", se = FALSE, span = 0.2, color = "darkred", size = 0.7
                ) +
                labs(
                    x = "Starttermin (Ist)",
                    y = "Abweichung [Tage]"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = c("x", "y"))
        })
        
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
        
        output$leadtime_chart <- renderPlot({
            req(input$selected_workflow)
            
            # Aggregation
            lt_agg <- lt_per_unit_workflows |>
                filter(vorgangsfolge == input$selected_workflow) |>
                group_by(Vorgangsnummer) |>
                summarise(
                    ist_lt = median(lt_ist_order, na.rm = TRUE) * 24 * 60 * 60,  # Sekunden
                    soll_lt = Mode(lt_soll_order, na.rm = TRUE) * 24 * 60 * 60,
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

