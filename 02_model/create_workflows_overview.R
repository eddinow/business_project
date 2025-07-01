# Initialisierung --------------------------------------------------------------

library(dplyr)
library(readxl)

# Daten laden ------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Transform------------------

# In dem df create_lt_unit ergänzen wir die bereinigten Auftrags- und Vorgangs-
# Frames um die notwendigen Spalten (u.a. LT's auf Unit-Ebene, Abweichungen usw.)

source("01_transform/create_lt_unit.R")

# Model ------------------------------------------------------------------------

#Overview-Table ist für alle Kategorieseiten gleich gehalten: Ausprägung, zugehörige
# Anzahl an Aufträgen (# Orders), durchschnittliche Soll-LT per unit [s] (Median 
# der Soll-LT/unit aller zugehörigen Aufträge), durchschnittliche Ist-LT per unit [s] 
# (Median der Ist-LT/unit aller zugehörigen Aufträge), durchschnittliche Verzögerung 
# per unit [s] (Median aller LT-Abweichungen, die negativ sind, also nur Verspätungen), 
# Servicelevel (Anteil der Aufträge, die rechtzeitig oder zu früh fertig sind). 
# Nutzer können so die verschiedenen Ausprägungen einer Kategorie schnell miteinander 
# vergleichen.

# Workflows---------------------------------------------------------------------
workflows_overview <- auftraege_lt_unit %>%
    filter(!is.na(lt_ist_order), !is.na(lt_soll_order)) %>%
    group_by(vorgangsfolge) %>%
    summarise(
        `Soll-LT/Unit [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
        `Ist-LT/Unit [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
        `Avg Delay/Unit [s]` = round(
            pmax(`Ist-LT/Unit [s]` - `Soll-LT/Unit [s]`, 0), 2
        ),
        `# Orders` = n(),
        servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(abweichung_unit)),
        .groups = "drop"
    ) %>%
    
    # Visualize --------------------------------------------------------------------

mutate(
    `Servicelevel` = paste0(round(servicelevel_numeric * 100, 0), "%"),
    ampel_color = case_when(
        servicelevel_numeric >= 0.95 ~ "green",
        servicelevel_numeric >= 0.7  ~ "orange",
        TRUE                         ~ "red"
    ),
    Status = paste0(
        "<div style='color: ", ampel_color, 
        "; font-size: 20px; display: inline-block; text-align: center;'>&#9679;</div>"
    )
) %>%
    rename(`Workflow` = vorgangsfolge) %>%
    dplyr::select(
        Status, Workflow,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(desc(`# Orders`))

#Fertigungslinien---------------------------------------------------------------

linien_overview <- vorgaenge_lt_unit %>%
    filter(!is.na(lt_ist_order), !is.na(lt_soll_order)) %>%
    group_by(fertigungslinie) %>%
    summarise(
        `Soll-LT/Unit [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
        `Ist-LT/Unit [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
        `Avg Delay/Unit [s]` = round(
            pmax(`Ist-LT/Unit [s]` - `Soll-LT/Unit [s]`, 0), 4
        ),
        `# Orders` = n(),
        servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(abweichung_unit)),
        .groups = "drop"
    ) %>%
    
    # Visualize --------------------------------------------------------------------

mutate(
    `Servicelevel` = paste0(round(servicelevel_numeric * 100, 0), "%"),
    ampel_color = case_when(
        servicelevel_numeric >= 0.95 ~ "green",
        servicelevel_numeric >= 0.7  ~ "orange",
        TRUE                         ~ "red"
    ),
    Status = paste0(
        "<div style='color: ", ampel_color, 
        "; font-size: 20px; display: inline-block; text-align: center;'>&#9679;</div>"
    )
) %>%
    rename(`Fertigungslinie` = fertigungslinie) %>%
    dplyr::select(
        Status, Fertigungslinie,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(desc(`# Orders`))

#Werke--------------------------------------------------------------------------
werke_overview <- auftraege_lt_unit %>%
    filter(!is.na(lt_ist_order), !is.na(lt_soll_order)) %>%
    group_by(werk) %>%
    summarise(
        `Soll-LT/Unit [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
        `Ist-LT/Unit [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
        `Avg Delay/Unit [s]` = round(
            pmax(`Ist-LT/Unit [s]` - `Soll-LT/Unit [s]`, 0), 2
        ),
        `# Orders` = n(),
        servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(abweichung_unit)),
        .groups = "drop"
    ) %>%
    
    # Visualize --------------------------------------------------------------------

mutate(
    `Servicelevel` = paste0(round(servicelevel_numeric * 100, 0), "%"),
    ampel_color = case_when(
        servicelevel_numeric >= 0.95 ~ "green",
        servicelevel_numeric >= 0.7  ~ "orange",
        TRUE                         ~ "red"
    ),
    Status = paste0(
        "<div style='color: ", ampel_color, 
        "; font-size: 20px; display: inline-block; text-align: center;'>&#9679;</div>"
    )
) %>%
    rename(`Werk` = werk) %>%
    dplyr::select(
        Status, Werk,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(desc(`# Orders`))




#Planer---------------------------------------------------------------

planer_overview <- auftraege_lt_unit %>%
    filter(!is.na(lt_ist_order), !is.na(lt_soll_order)) %>%
    group_by(planer) %>%
    summarise(
        `Soll-LT/Unit [s]` = round(median(lt_soll_order, na.rm = TRUE), 2),
        `Ist-LT/Unit [s]`  = round(median(lt_ist_order, na.rm = TRUE), 2),
        `Avg Delay/Unit [s]` = round(
            pmax(`Ist-LT/Unit [s]` - `Soll-LT/Unit [s]`, 0), 4
        ),
        `# Orders` = n(),
        servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(abweichung_unit)),
        .groups = "drop"
    ) %>%
    
    # Visualize --------------------------------------------------------------------

mutate(
    `Servicelevel` = paste0(round(servicelevel_numeric * 100, 0), "%"),
    ampel_color = case_when(
        servicelevel_numeric >= 0.95 ~ "green",
        servicelevel_numeric >= 0.7  ~ "orange",
        TRUE                         ~ "red"
    ),
    Status = paste0(
        "<div style='color: ", ampel_color, 
        "; font-size: 20px; display: inline-block; text-align: center;'>&#9679;</div>"
    )
) %>%
    rename(`Planer` = planer) %>%
    dplyr::select(
        Status, Planer,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(desc(`# Orders`))

# Material

material_overview <- auftraege_lt_unit %>%
    filter(!is.na(lt_ist_order), !is.na(lt_soll_order)) %>%
    group_by(klassifikation) %>%
    summarise(
        lt_soll_median = median(lt_soll_order, na.rm = TRUE),
        lt_ist_median  = median(lt_ist_order,  na.rm = TRUE),
        `# Orders` = n(),
        servicelevel_numeric = sum(abweichung_unit <= 0, na.rm = TRUE) / 
            sum(!is.na(abweichung_unit)),
        .groups = "drop"
    ) %>%
    mutate(
        `Soll-LT/Unit [s]`     = round(lt_soll_median, 2),
        `Ist-LT/Unit [s]`      = round(lt_ist_median, 2),
        `Avg Delay/Unit [s]`   = round(pmax(lt_ist_median - lt_soll_median, 0), 4),
        `Servicelevel`         = paste0(round(servicelevel_numeric * 100, 0), "%"),
        ampel_color = case_when(
            servicelevel_numeric >= 0.95 ~ "green",
            servicelevel_numeric >= 0.7  ~ "orange",
            TRUE                         ~ "red"
        ),
        Status = paste0(
            "<div style='color: ", ampel_color, 
            "; font-size: 20px; display: inline-block; text-align: center;'>&#9679;</div>"
        )
    ) %>%
    rename(`Material (A,B,C)` = klassifikation) %>%
    dplyr::select(
        Status, `Material (A,B,C)`,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(`Material (A,B,C)`)



