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

# Dateireinigung in all_data_finalized abgeschlossen
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

#SOLLZEITEN--------

# Tidy
create_est_lt <- function(df, selected_vorgangsfolge, fallback_bin_size = 100000) {
    df_step <- df %>%
        filter(vorgangsfolge == selected_vorgangsfolge, !is.na(sollmenge), !is.na(solldauer))

    # Cutoffs
    cutoffs <- df_step %>%
        summarise(
            menge_min = quantile(sollmenge, 0.01),
            menge_max = quantile(sollmenge, 0.99),
            lt_min    = quantile(solldauer, 0.01),
            lt_max    = quantile(solldauer, 0.99)
        )

    df_clean <- df_step %>%
        filter(
            between(sollmenge, cutoffs$menge_min, cutoffs$menge_max),
            between(solldauer, cutoffs$lt_min, cutoffs$lt_max)
        )
    
# Transform
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

# Model
    lt_by_bin <- df_binned %>%
        group_by(bin) %>%
        summarise(
            median_lt = median(solldauer, na.rm = TRUE),
            p10 = quantile(solldauer, 0.10, na.rm = TRUE),
            p90 = quantile(solldauer, 0.90, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(!is.na(bin))

    bin_levels <- levels(df_binned$bin)
    bin_starts <- as.numeric(gsub("^\\[|\\(|,.*$", "", bin_levels))
    bin_ends   <- as.numeric(gsub("^.*,(.*)\\]$", "\\1", bin_levels))

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

# Visualize
    lt_by_bin <- lt_by_bin %>%
        left_join(bin_bounds, by = "bin") %>%
        mutate(bin_label = factor(bin_label, levels = bin_label))
    
    p <- ggplot(lt_by_bin, aes(x = bin_label)) +
        geom_ribbon(aes(ymin = p10, ymax = p90, group = 1), fill = "#002366", alpha = 0.2) +
        geom_line(aes(y = median_lt, group = 1), color = "#002366", linewidth = 1) +
        geom_point(aes(y = median_lt), color = "#002366", size = 2) +
        labs(
            x = "Sollmengen-Bereich",
            y = "Lead Time (soll)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(list(
        plot = p,
        table = lt_by_bin %>%
            dplyr::select(bin_label, lt_median = median_lt, lt_lower = p10, lt_upper = p90)
    ))
}



# #ISTZEITEN------------------------------

# Tidy
create_est_lt_ist <- function(df, selected_vorgangsfolge, fallback_bin_size = 100000) {
    df_step_ist <- df %>%
        filter(vorgangsfolge == selected_vorgangsfolge, !is.na(sollmenge), !is.na(istdauer))

    # Cutoffs
    cutoffs_ist <- df_step_ist %>%
        summarise(
            menge_min = quantile(sollmenge, 0.01),
            menge_max = quantile(sollmenge, 0.99),
            lt_min    = quantile(istdauer, 0.01),
            lt_max    = quantile(istdauer, 0.99)
        )

    df_clean_ist <- df_step_ist %>%
        filter(
            between(sollmenge, cutoffs_ist$menge_min, cutoffs_ist$menge_max),
            between(istdauer, cutoffs_ist$lt_min, cutoffs_ist$lt_max)
        )

# Transform
    iqr_ist <- IQR(df_clean_ist$sollmenge, na.rm = TRUE)
    n_ist <- sum(!is.na(df_clean_ist$sollmenge))
    bin_size_ist <- 2 * iqr_ist / (n_ist^(1/3))
    if (is.na(bin_size_ist) || bin_size_ist <= 0) bin_size_ist <- fallback_bin_size

    breaks_ist <- seq(0, max(df_clean_ist$sollmenge, na.rm = TRUE) + bin_size_ist, by = bin_size_ist)
    if (length(breaks_ist) < 2) return(NULL)

    df_binned_ist <- df_clean_ist %>%
        mutate(
            bin = cut(sollmenge, breaks = breaks_ist, include.lowest = TRUE, right = FALSE)
        )

    lt_by_bin_ist <- df_binned_ist %>%
        group_by(bin) %>%
        summarise(
            median_lt = median(istdauer, na.rm = TRUE),
            p10 = quantile(istdauer, 0.10, na.rm = TRUE),
            p90 = quantile(istdauer, 0.90, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        filter(!is.na(bin))

    bin_levels_ist <- levels(df_binned_ist$bin)
    bin_starts_ist <- as.numeric(gsub("^\\[|\\(|,.*$", "", bin_levels_ist))
    bin_ends_ist   <- as.numeric(gsub("^.*,(.*)\\]$", "\\1", bin_levels_ist))

    bin_bounds_ist <- tibble(bin = factor(bin_levels_ist, levels = bin_levels_ist)) %>%
        mutate(
            bin_start = bin_starts_ist,
            bin_end = bin_ends_ist,
            bin_label = paste0(
                formatC(bin_start / 1000, format = "f", digits = 0, big.mark = "."), "k – ",
                formatC(bin_end / 1000, format = "f", digits = 0, big.mark = "."), "k"
            )
        ) %>%
        filter(bin %in% lt_by_bin_ist$bin)

# Model
    lt_by_bin_ist <- lt_by_bin_ist %>%
        left_join(bin_bounds_ist, by = "bin") %>%
        mutate(bin_label = factor(bin_label, levels = bin_label))

# Visualize
    p_ist <- ggplot(lt_by_bin_ist, aes(x = bin_label)) +
        geom_ribbon(aes(ymin = p10, ymax = p90, group = 1), fill = "#002366", alpha = 0.2) +
        geom_line(aes(y = median_lt, group = 1), color = "#002366", linewidth = 1) +
        geom_point(aes(y = median_lt), color = "#002366", size = 2) +
        labs(
           # title = paste("Lead Time (IST) je Losgrößen-Bin –", selected_vorgangsfolge),
            x = "Sollmengen-Bereich",
            y = "Lead Time (ist)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(list(
        plot = p_ist,
        table = lt_by_bin_ist %>%
            dplyr::select(bin_label, lt_median = median_lt, lt_lower = p10, lt_upper = p90)
    ))
}


# SOLL- UND IST ------------

# Model
create_est_lt_combined <- function(df, selected_vorgangsfolge, fallback_bin_size = 100000, session = NULL) {
    compute_variant <- function(df, selected_vorgangsfolge, col_name, label) {
        df_step <- df %>%
            filter(vorgangsfolge == selected_vorgangsfolge, !is.na(sollmenge), !is.na(.data[[col_name]]))
        
        if (nrow(df_step) < 3) return(NULL)
        
        iqr <- IQR(df_step$sollmenge, na.rm = TRUE)
        n <- sum(!is.na(df_step$sollmenge))
        bin_size <- 2 * iqr / (n^(1/3))
        if (is.na(bin_size) || bin_size <= 0) bin_size <- fallback_bin_size
        
        breaks <- seq(0, max(df_step$sollmenge, na.rm = TRUE) + bin_size, by = bin_size)
        if (length(breaks) < 2) return(NULL)
        
        df_binned <- df_step %>%
            mutate(bin = cut(sollmenge, breaks = breaks, include.lowest = TRUE, right = FALSE))
        
        lt_by_bin <- df_binned %>%
            group_by(bin) %>%
            summarise(
                median_lt = median(.data[[col_name]], na.rm = TRUE),
                p10 = quantile(.data[[col_name]], 0.10, na.rm = TRUE),
                p90 = quantile(.data[[col_name]], 0.90, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            filter(!is.na(bin))
        
        bin_levels <- levels(df_binned$bin)
        bin_starts <- as.numeric(gsub("^\\[|\\(|,.*$", "", bin_levels))
        bin_ends   <- as.numeric(gsub("^.*,(.*)\\]$", "\\1", bin_levels))
        
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
    
    df_combined <- bind_rows(df_soll, df_ist)
    
# Visualize
    if (!is.null(session)) {
        updateSliderInput(session, "selected_sollmenge",
                          min = min(df_combined$bin_start, na.rm = TRUE),
                          max = max(df_combined$bin_end, na.rm = TRUE),
                          value = min(df_combined$bin_start, na.rm = TRUE),
                          step = 1000)
    }
    
    
    all_labels <- levels(df_combined$bin_label)
    selected_labels <- all_labels[seq(1, length(all_labels), by = ceiling(length(all_labels) / 5))]
    
    total_points <- nrow(df_combined)
    
    p <- ggplot(df_combined, aes(x = bin_label, group = variante)) +
        geom_ribbon(aes(ymin = p10, ymax = p90, fill = variante), alpha = 0.08) +
        geom_line(aes(y = median_lt, color = variante), linewidth = 0.3) +
        geom_point(aes(y = median_lt, color = variante), size = 0.4) +
        scale_color_manual(values = c("Soll" = "#002366", "Ist" = "#6495ED")) +
        scale_fill_manual(values = c("Soll" = "#002366", "Ist" = "#6495ED")) +
        labs(
            #title = paste("Soll- und Ist-Lead Time", selected_vorgangsfolge),
            x = "Sollmengen-Bereich",
            y = "Lead Time",
            color = "Variante",
            fill = "Variante",
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
        plot = p,
        table = df_combined %>%
            dplyr::select(bin_label, bin_start, bin_end, variante, lt_median = median_lt, lt_lower = p10, lt_upper = p90)
    ))
}

# Communicate ------------------------------------------------------------------
