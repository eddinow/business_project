# Erzeugen der Soll- und Ist-LT's in Abhängigkeit von der Sollmenge auf Workflow-
# ebene.

# Initialisierung --------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)
library(readxl)

# Tidy -------------------------------------------------------------------------

# Bereinigtes Datenset via all_data_finalized zugänglich
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")

# Transform --------------------------------------------------------------------

# Data Frames in create_lt_unit und kpis_workflow_arbeitsplatz für Analyse auf
# Workflowebene aufbereitet
source("02_model/kpis_workflow_arbeitsplatz.R", local = TRUE)  
source("01_transform/create_lt_unit.R", local = TRUE)

# Model ------------------------------------------------------------------------

# Bilden perzentilabhängiger Cutoffs, um Ausreißer zu entfernen. Kategorisieren
# der Sollmengen in dynamischen Bins. Dann bilden des Medians und der Ober- und
# Unterperzentile für jeden Bin. User kann so mengenabhängig die LT abschätzen.

create_est_lt_combined <- function(df, selected_vorgangsfolge, fallback_bin_size = 100000, session = NULL) {
    compute_variant <- function(df, selected_vorgangsfolge, col_name, label) {
        data_valid_to_use <- df %>%
            filter(vorgangsfolge == selected_vorgangsfolge, !is.na(sollmenge), !is.na(.data[[col_name]]))
        
        # Median-Berechnung nur, wenn mehr als 3 Datenpunkte vorhanden
        if (nrow(data_valid_to_use) < 3) return(NULL)
        
        # Ermitteln der optimalen Bin-Breite abhängig von der Sollmenge (Freedman-Diaconis-Regel)
        iqr_est_lt <- IQR(data_valid_to_use$sollmenge, na.rm = TRUE)
        n_est_lt <- sum(!is.na(data_valid_to_use$sollmenge))
        bin_size_est_lt <- 2 * iqr_est_lt / (n_est_lt^(1/3))
        if (is.na(bin_size_est_lt) || bin_size_est_lt <= 0) bin_size_est_lt <- fallback_bin_size
        
        # Erstellen von Klassengrenzen beginnend bei 0 bis zur maximalen Sollmenge im Abstand
        # der berechneten Bin-sizes; Nur wenn mindestens zwei breaks vorhanden sind
        breaks <- seq(0, max(data_valid_to_use$sollmenge, na.rm = TRUE) + bin_size, by = bin_size)
        if (length(breaks) < 2) return(NULL)
        
        # Erstellen einer neuen Kategorie-Spalte, in der jeder Auftrag dann einer Mengen-
        # klasse zugeordnet wird
        df_quantity_binned <- data_valid_to_use %>%
            mutate(bin = cut(sollmenge, breaks = breaks, include.lowest = TRUE, right = FALSE))
        
        # Berechnen der mittleren Lead Time, oberen Grenze (90%) und unteren Grenze
        # (10%) für jede Mengenklasse
        lt_by_bin <- df_quantity_binned %>%
            group_by(bin) %>%
            summarise(
                median_lt = median(.data[[col_name]], na.rm = TRUE),
                p10 = quantile(.data[[col_name]], 0.10, na.rm = TRUE),
                p90 = quantile(.data[[col_name]], 0.90, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            filter(!is.na(bin))
        
        # Aufbereiten der Bin-Beschriftungen für bessere Lesbarkeit im Plot
        bin_levels <- levels(df_quantity_binned$bin)
        bin_starts <- as.numeric(gsub("^\\[|\\(|,.*$", "", bin_levels))
        bin_ends   <- as.numeric(gsub("^.*,(.*)\\]$", "\\1", bin_levels))
        
        # Ablegen der Bin-Grenzen und Labels in separaten DF als Vorbereitung
        # zum plotten; hinzufügen zu Haupttabelle; Ergänzen der Variante (Ist/Soll)
        bin_bounds <- tibble(bin = factor(bin_levels, levels = bin_levels)) %>%
            mutate(
                bin_start = bin_starts,
                bin_end = bin_ends,
                bin_label = paste0(
                    formatC(bin_start / 1000, format = "f", digits = 0, big.mark = "."), "k"
                )
            ) %>%
            filter(bin %in% lt_by_bin$bin)
        
        lt_by_bin <- lt_by_bin %>%
            left_join(bin_bounds, by = "bin") %>%
            mutate(
                bin_label = factor(bin_label, levels = unique(bin_bounds$bin_label)),
                variante = label
            )
        
        return(lt_by_bin)
    }
    
    df_soll <- compute_variant(df, selected_vorgangsfolge, "solldauer", "Soll")
    df_ist  <- compute_variant(df, selected_vorgangsfolge, "istdauer", "Ist")
    if (is.null(df_soll) || is.null(df_ist)) return(NULL)
    df_ist_soll_mengenabhg <- bind_rows(df_soll, df_ist)
    
# Visualize---------------------------------------------------------------------
    if (!is.null(session)) {
        updateSliderInput(session, "selected_sollmenge",
                          min = min(df_ist_soll_mengenabhg$bin_start, na.rm = TRUE),
                          max = max(df_ist_soll_mengenabhg$bin_end, na.rm = TRUE),
                          value = min(df_ist_soll_mengenabhg$bin_start, na.rm = TRUE),
                          step = 1000)
    }
    
    # Nur jeden 5. Bin als Label anzeigen (aus Darstellungsgründen)
    all_labels <- levels(df_ist_soll_mengenabhg$bin_label)
    selected_labels <- all_labels[seq(1, length(all_labels), by = ceiling(length(all_labels) / 5))]
    
    total_points <- nrow(df_ist_soll_mengenabhg)
    
    # Dummy-Daten nur für die Legende; damit Farbe und Text sauber in Legende steht
    leg_df <- data.frame(
        variante = c("Ist", "Soll"),
        bin_label = factor(rep(df_ist_soll_mengenabhg$bin_label[1], 2), levels = levels(df_ist_soll_mengenabhg$bin_label)),
        median_lt = c(0, 0)  # z. B. 0 oder ein realistischer, kleiner Wert
    )
    
    plot_est_lt_sollmenge <- ggplot(df_ist_soll_mengenabhg, aes(x = bin_label, group = variante)) +
        geom_ribbon(aes(ymin = p10, ymax = p90, fill = variante), alpha = 0.08, show.legend = FALSE) +
        geom_line(aes(y = median_lt, color = variante), linewidth = 0.3, show.legend = FALSE) +
        geom_point(aes(y = median_lt, color = variante), size = 0.4, show.legend = FALSE) +
        
        # Dummy-Linien nur für die Legende
        geom_line(data = leg_df, aes(y = median_lt, color = variante), linewidth = 1, na.rm = TRUE) +
        
        scale_color_manual(
            values = c("Soll" = "#8B0000", "Ist" = "#6495ED"),
            labels = c("Soll" = "LT Soll", "Ist" = "LT Ist")
        ) +
        scale_fill_manual(
            values = c("Soll" = "#8B0000", "Ist" = "#6495ED"),
            guide = "none"  
        ) +
        labs(
            x = "Sollmengen-Bereich",
            y = "Lead Time",
            color = "Variante",
            caption = paste("Aufträge ges.:", total_points)
        ) +
        scale_x_discrete(breaks = selected_labels) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 1, face = "italic", size = 9),
            legend.position = "right",
            legend.box = "vertical"
        )
    
    return(list(
        plot = plot_est_lt_sollmenge,
        table = df_ist_soll_mengenabhg %>%
            dplyr::select(bin_label, bin_start, bin_end, variante, lt_median = median_lt, lt_lower = p10, lt_upper = p90)
    ))
}