# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)

# Daten laden -------------------------------------------------------------------
source("00_tidy/create_all_data_finalized.R") 

# Wir machen perzentil-abhängige Cutoffs, um Ausreißer zu entfernen. Wir packen 
# dann die Sollmengen in Bins. Dann bilden wir für jeden Bin den Median und
# oberes bzw unteres Perzentil. So können wir einigermaßen mengenabhängig die LT 
# prognostizieren.

create_est_lt <- function(df, vorgangsfolge_id, fallback_bin_size = 100000) {
    df_step <- df %>%
        filter(vorgangsfolge == vorgangsfolge_id, !is.na(sollmenge), !is.na(lead_time_soll))
    
    
    
    # Cutoffs
    cutoffs <- df_step %>%
        summarise(
            menge_min = quantile(sollmenge, 0.01),
            menge_max = quantile(sollmenge, 0.99),
            lt_min    = quantile(lead_time_soll, 0.01),
            lt_max    = quantile(lead_time_soll, 0.99)
        )
    
    df_clean <- df_step %>%
        filter(
            between(sollmenge, cutoffs$menge_min, cutoffs$menge_max),
            between(lead_time_soll, cutoffs$lt_min, cutoffs$lt_max)
        )
    
   
    
    # Bin-Breite automatisch bestimmen (mit fallback)
    iqr <- IQR(df_clean$sollmenge, na.rm = TRUE)
    n <- sum(!is.na(df_clean$sollmenge))
    bin_size <- 2 * iqr / (n^(1/3))
    if (is.na(bin_size) || bin_size <= 0) bin_size <- fallback_bin_size
    
    breaks <- seq(0, max(df_clean$sollmenge, na.rm = TRUE) + bin_size, by = bin_size)
    if (length(breaks) < 2) return(NULL)
    
    df_binned <- df_clean %>%
        mutate(
            bin = cut(sollmenge, breaks = breaks, include.lowest = TRUE, right = FALSE)
        )
    
    lt_by_bin <- df_binned %>%
        group_by(bin) %>%
        summarise(
            median_lt = median(lead_time_soll, na.rm = TRUE),
            p10 = quantile(lead_time_soll, 0.10, na.rm = TRUE),
            p90 = quantile(lead_time_soll, 0.90, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(!is.na(bin))
    
    # 3. Hole Breaks und berechne Labels
    bin_levels <- levels(df_binned$bin)
    
    # Saubere Extraktion von Start- und Endwerten pro Bin
    bin_starts <- as.numeric(gsub("^\\[|\\(|,.*$", "", bin_levels))
    bin_ends   <- as.numeric(gsub("^.*,(.*)\\]$", "\\1", bin_levels))
    
    # Bin-Bounds mit Labels
    bin_bounds <- tibble(bin = factor(bin_levels, levels = bin_levels)) %>%
        mutate(
            bin_start = bin_starts,
            bin_end = bin_ends,
            bin_label = paste0(
                formatC(bin_start / 1000, format = "f", digits = 0, big.mark = "."), "k – ",
                formatC(bin_end / 1000, format = "f", digits = 0, big.mark = "."), "k"
            )
        ) %>%
        filter(bin %in% lt_by_bin$bin)
    
    # 4. Join der Labels
    lt_by_bin <- lt_by_bin %>%
        left_join(bin_bounds, by = "bin") %>%
        mutate(bin_label = factor(bin_label, levels = bin_label))
    # 📊 Plot erstellen
    p <- ggplot(lt_by_bin, aes(x = bin_label)) +
        geom_ribbon(aes(ymin = p10, ymax = p90, group = 1), fill = "#002366", alpha = 0.2) +
        geom_line(aes(y = median_lt, group = 1), color = "#002366", linewidth = 1) +
        geom_point(aes(y = median_lt), color = "#002366", size = 2) +
        labs(
            title = paste("Lead Time je Losgrößen-Bin –", vorgangsfolge_id),
            x = "Sollmengen-Bereich",
            y = "Lead Time (soll)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Rückgabe als Liste mit Plot UND Tabelle
    return(list(
        plot = p,
        table = lt_by_bin %>%
            dplyr::select(bin_label, lt_median = median_lt, lt_lower = p10, lt_upper = p90)
    ))
}