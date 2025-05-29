delay_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Verspätungsauswertung je Dimension"),
        fluidRow(
            column(4, DTOutput(ns("table_werke"))),
            column(4, DTOutput(ns("table_linien"))),
            column(4, DTOutput(ns("table_planer")))
        )
    )
}

delay_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        color_column <- function(values) {
            sapply(values, function(v) {
                if (v > 3) "🔴"
                else if (v > 1) "🟠"
                else "🟢"
            })
        }
        
        output$table_werke <- renderDT({
            df <- delay_summary %>%
                select(Werk = werk, `Avg. Delay [d]` = Avg_Delay) %>%
                mutate(Ampel = color_column(`Avg. Delay [d]`))
            
            datatable(df, escape = FALSE, rownames = FALSE, options = list(dom = 'tip'))
        })
        
        output$table_linien <- renderDT({
            df <- delay_summary %>%
                select(Linie = fertigungslinie, `Avg. Delay [d]` = Avg_Delay) %>%
                mutate(Ampel = color_column(`Avg. Delay [d]`))
            
            datatable(df, escape = FALSE, rownames = FALSE, options = list(dom = 'tip'))
        })
        
        output$table_planer <- renderDT({
            df <- delay_summary %>%
                select(Planer = planer, `Avg. Delay [d]` = Avg_Delay) %>%
                mutate(Ampel = color_column(`Avg. Delay [d]`))
            
            datatable(df, escape = FALSE, rownames = FALSE, options = list(dom = 'tip'))
        })
    })
}
