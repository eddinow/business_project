output$werke_table <- renderDT({
    df <- daten_gefiltert() %>%
        select(
            vorgangsfolge, werk, Anzahl,
            Median_LT, Durchschnitt_Abweichung,
            Average_Delay, Servicelevel_prozent
        )
    
    datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        class = "stripe hover cell-border"
    )
})
