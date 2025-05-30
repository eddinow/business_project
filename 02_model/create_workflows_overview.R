# Initialisierung ----------------------------------------------------------------
# 
# rm(list = ls())
# set.seed(1)

library(dplyr)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
source("01_transform/create_lt_unit.R")

# Transform------------------
# Für die Übersichtsseite der Workflows erstellen wir ein Tableau mit allen WFs.
# Für jeden WF wird die Avg LT berechnet (Median aller LT). Avg Delay ist Median
# aller Abweichung, no of orders die Anzahl der Aufträge. Servicelevel = Anzahl
#Aufträge mit Abweichung größer gleich 0 / Anzahl aller Aufträge

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
    mutate(
        `Servicelevel` = paste0(round(servicelevel_numeric * 100, 0), "%"),
        ampel_color = case_when(
            servicelevel_numeric >= 0.95 ~ "green",
            servicelevel_numeric >= 0.7  ~ "orange",
            TRUE                         ~ "red"
        ),
        ampel = paste0(
            "<div style='color: ", ampel_color, 
            "; font-size: 20px; text-align: center;'>&#9679;</div>"
        )
    ) %>%
    rename(`Workflow` = vorgangsfolge) %>%
    dplyr::select(
        ampel_color, ampel, Workflow,
        `Soll-LT/Unit [s]`, `Ist-LT/Unit [s]`, `Avg Delay/Unit [s]`,
        `# Orders`, `Servicelevel`
    ) %>%
    arrange(desc(`# Orders`))



