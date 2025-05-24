# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)
library(stringr)
library(plotly)

# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")
vorgaenge_raw <- read_excel("vorgaenge_sap_raw.xlsx")


# BALKENDIAGRAMM ZEITEN FÜR WORKFLOWS
#Um wieder die Lead Times auf Vorgangs- und Arbeitsplatzebene sehen zu können, 
# müssen die sap_vorgaenge cleanen und dann pro Vorgang u Arbeitsplatz wieder die LT
# ermitteln. Wir ermitteln außerdem die Liegezeiten als Differenz zwischen Enddatum
# Vorgang 1 und Startdatum Folgevorgang. Wir ermitteln LT/Unit!

vorgaenge_raw <- vorgaenge_raw %>%
    filter(Auftragsnummer %in% all_data_finalized$auftragsnummer)

vorgaenge_cleaned <- vorgaenge_raw %>%
    left_join(
        all_data_finalized %>%
            dplyr::select(
                auftragsnummer,
                materialnummer,
                werk,
                starttermin_soll,
                endtermin_soll,
                fertigungslinie,
                planer,
                vorgangsfolge,
                arbeitsplatzfolge
            ),
        by = c("Auftragsnummer" = "auftragsnummer")
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    dplyr::select(-`ME`, -`Gutmenge Vorgang`, -`Ausschuss Vorgang`)

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        `Iststart Vorgang` = as.Date(`Iststart Vorgang`),
        `Istende Vorgang`  = as.Date(`Istende Vorgang`)
    )

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(
        solldauer = as.numeric(difftime(endtermin_soll, starttermin_soll, units = "days")),
        istdauer = as.numeric(difftime(`Istende Vorgang`, `Iststart Vorgang`, units = "days")),
        abweichung = istdauer - solldauer
    )
    

vorgaenge_cleaned <- vorgaenge_cleaned %>%
    arrange(vorgangsfolge, Vorgangsnummer)
    
vorgaenge_cleaned <- vorgaenge_cleaned %>%
    mutate(gesamtdauer = as.numeric(`Istende Vorgang` - `Iststart Vorgang`))

vorgaenge_sorted <- vorgaenge_cleaned %>%
    arrange(Auftragsnummer, `Iststart Vorgang`)

liegezeiten_plot <- vorgaenge_sorted %>%
    group_by(Auftragsnummer) %>%
    arrange(`Iststart Vorgang`) %>%
    mutate(
        vorheriges_ende = lag(`Istende Vorgang`),
        aktuelles_start = `Iststart Vorgang`
    ) %>%
    filter(!is.na(vorheriges_ende) & aktuelles_start > vorheriges_ende) %>%
    mutate(
        Typ = "Liegezeit",
        Start = vorheriges_ende,
        Ende = aktuelles_start
    ) %>%
    dplyr::select(Auftragsnummer, Typ, Start, Ende)


vorgaenge_plot <- vorgaenge_sorted %>%
    dplyr::select(
        Auftragsnummer,
        Vorgangsnummer,
        Typ = Vorgangsnummer,
        Start = `Iststart Vorgang`,
        Ende = `Istende Vorgang`
    )

workflow_plot_data <- bind_rows(vorgaenge_plot, liegezeiten_plot) %>%
    arrange(Auftragsnummer, Start) %>%
    group_by(Auftragsnummer) %>%
    mutate(Schritt = row_number()) %>%
    ungroup()


liegezeiten_df <- vorgaenge_sorted %>%
    group_by(Auftragsnummer) %>%
    mutate(
        vorheriges_ende = lag(`Istende Vorgang`),
        aktuelles_start = `Iststart Vorgang`,
        liegedauer = as.numeric(aktuelles_start - vorheriges_ende)
    ) %>%
    filter(!is.na(liegedauer) & liegedauer > 0) %>%  # Nur echte Liegezeiten
    summarise(
        Liegezeit_start = min(vorheriges_ende, na.rm = TRUE),
        Liegezeit_ende   = max(aktuelles_start, na.rm = TRUE),
        Liegedauer_gesamt = sum(liegedauer, na.rm = TRUE),
        .groups = "drop"
    )

# Alle Aufträge mit Vorgangsdauern (auch ohne Liegezeiten)
auftrags_dauer <- vorgaenge_sorted %>%
    dplyr::select(Auftragsnummer, Vorgangsnummer, istdauer) %>%
    pivot_wider(
        names_from = Vorgangsnummer,
        values_from = istdauer
    )

auftrags_dauer <- vorgaenge_sorted %>%
    dplyr::select(Auftragsnummer, Vorgangsnummer, istdauer) %>%
    pivot_wider(names_from = Vorgangsnummer, values_from = istdauer)

# 2. Kombinieren mit Liegezeiten (auch wenn keine vorhanden)
liegezeit_u_auftragszeit <- auftrags_dauer %>%
    left_join(liegezeiten_df, by = "Auftragsnummer")



# Zusammenfassen der Aufträgen nach Vorgangsnummern um die Daten den einzelnen
# Workflows zuordnen zu können. Hier nutzen wir Median Werte für Liegezeit und
# Bearbeitungszeit

liegezeiten_df_erweitert <- liegezeit_u_auftragszeit %>%
    left_join(
        vorgaenge_cleaned %>% dplyr::select(Auftragsnummer, vorgangsfolge) %>% distinct(),
        by = "Auftragsnummer"
    )


vorgaenge_lz_bz <- liegezeiten_df_erweitert %>%
    group_by(vorgangsfolge) %>%
    summarise(across(
        .cols = c(Liegedauer_gesamt, `0010`, `0020`, `0030`, `0040`, `0050`, `0060`, `0070`, `0005`, `0032`),
        .fns = ~ median(.x, na.rm = TRUE),
        .names = "{.col}_median"
    )) 



df <- vorgaenge_lz_bz

# Funktion zur Umwandlung einer Zeile in langes Format
transform_row_to_long <- function(row) {
    row <- as_tibble(row)  # 🔧 Liste zu Tibble umwandeln
    id <- row$vorgangsfolge
    row_long <- row %>%
        dplyr::select(ends_with("_median")) %>%
        pivot_longer(
            cols = everything(),
            names_to = "Step",
            values_to = "Time"
        ) %>%
        filter(!is.na(Time)) %>%
        mutate(
            Step = str_replace(Step, "_median", ""),
            vorgangsfolge = id
        )
    return(row_long)
}

plot_workflow_structure <- function(df) {
    if (nrow(df) == 0) return(NULL)
    
    df_long <- df %>%
        pivot_longer(
            cols = ends_with("_median"),
            names_to = "Step",
            values_to = "Time"
        ) %>%
        filter(!is.na(Time)) %>%
        mutate(
            Step = str_replace(Step, "_median", ""),
            Step = ifelse(Step == "Liegedauer_gesamt", "Avg. Delay", Step),
            text_color = unname(ifelse(Step == "Avg. Delay", "black", "white"))
        ) %>%
        group_by(vorgangsfolge) %>%
        mutate(
            Total = sum(Time),
            pct = Time / Total,
            label = unname(paste0(
                "<b>", Step, "</b><br>",
                ifelse(Step == "Avg. Delay", "Avg. Delay [d]: ", "Avg. LT [d]: "),
                round(Time, 1), "<br>", round(pct * 100, 1), "%"
            )),
            xmin = cumsum(lag(pct, default = 0)),
            xmax = cumsum(pct)
        ) %>%
        ungroup()
    
    color_map <- c(
        "0010" = "#c6dbef", "0020" = "#9ecae1", "0030" = "#6baed6",
        "0040" = "#4292c6", "0050" = "#2171b5", "0060" = "#08519c",
        "0070" = "#08306b", "0005" = "#b3cde3", "0032" = "#a6bddb",
        "Avg. Delay" = "#f5f7fa"
    )
    
    p <- ggplot(df_long) +
        geom_rect(
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = Step, text = label),
            color = NA
        ) +
        geom_text(
            data = df_long %>% filter(pct > 0.05),
            aes(x = (xmin + xmax) / 2, y = 0.5, label = Step, color = text_color),
            size = 3.5,
            show.legend = FALSE
        ) +
        scale_fill_manual(values = color_map, guide = "none") +  # 🔧 Legende deaktiviert
        scale_color_identity() +
        facet_wrap(~ vorgangsfolge, ncol = 1, scales = "free") +
        theme_minimal(base_size = 14) +  # 🔧 Minimal statt Void, um gezielt zu löschen
        theme(
            panel.grid = element_blank(),       # 🔧 Hintergrundlinien aus
            axis.title = element_blank(),       # 🔧 Achsentitel aus
            axis.text = element_blank(),        # 🔧 Achsentext aus
            axis.ticks = element_blank(),       # 🔧 Ticks aus
            strip.text = element_text(hjust = 0.5, face = "bold"),
            legend.position = "none",           # 🔧 Sicherstellung: keine Legende
            plot.margin = margin(5, 5, 5, 5)
        )
    
    ggplotly(p, tooltip = "text") %>%
        layout(
            margin = list(t = 20, b = 5, l = 5, r = 5),
            height = 120
        )
}