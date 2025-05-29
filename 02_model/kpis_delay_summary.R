delay_summary <- all_data_finalized %>%
    filter(!is.na(abweichung)) %>%
    mutate(delay = ifelse(abweichung > 0, abweichung, 0)) %>%
    group_by(werk, fertigungslinie, planer) %>%
    summarise(
        Avg_Delay = round(mean(delay, na.rm = TRUE), 1),
        .groups = "drop"
    )
