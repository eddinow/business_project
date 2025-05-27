# Initialisierung ----------------------------------------------------------------
# 
# rm(list = ls())
# set.seed(1)

library(dplyr)
library(readxl)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Transform------------------
# Für die Übersichtsseite der Workflows erstellen wir ein Tableau mit allen WFs.
# Für jeden WF wird die Avg LT berechnet (Median aller LT). Avg Delay ist Median
# aller Abweichung, no of orders die Anzahl der Aufträge. Servicelevel = Anzahl
#Aufträge mit Abweichung größer gleich 0 / Anzahl aller Aufträge

workflows_overview <- all_data_finalized %>%
    group_by(vorgangsfolge) %>%
    summarise(
        `Avg LT/Order [d]` = median(lead_time_ist, na.rm = TRUE),
        `Avg Delay/Order [d]` = round(min(median(abweichung, na.rm = TRUE), 0), 0),
        `# Orders` = n(),
        servicelevel_numeric = mean(abweichung <= 0, na.rm = TRUE),
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
    dplyr::select(ampel_color, ampel, Workflow, `Avg LT/Order [d]`, `Avg Delay/Order [d]`, `# Orders`, `Servicelevel`) %>%
    arrange(desc(`# Orders`))

# 👉 HTML in Spaltenüberschriften (Info-Icons einfügen)
colnames(workflows_overview)[colnames(workflows_overview) == "Avg LT/Order [d]"] <-
    HTML('Avg LT/Order [d] <i id="info_avglt" class="fa fa-info-circle"></i>')

colnames(workflows_overview)[colnames(workflows_overview) == "Avg Delay/Order [d]"] <-
    HTML('Avg Delay/Order [d] <i id="info_avgdelay" class="fa fa-info-circle"></i>')

colnames(workflows_overview)[colnames(workflows_overview) == "# Orders"] <-
    HTML('# Orders <i id="info_orders" class="fa fa-info-circle"></i>')

colnames(workflows_overview)[colnames(workflows_overview) == "Servicelevel"] <-
    HTML('Servicelevel <i id="info_servicelevel" class="fa fa-info-circle"></i>')