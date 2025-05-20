# Initialisierung ----------------------------------------------------------------

# rm(list = ls())
# set.seed(1)

library(dplyr)
library(tidyr)
library(purrr)
library(scales)


# Daten laden -------------------------------------------------------------------
all_data_finalized <- read_xlsx("00_tidy/all_data_finalized.xlsx")



# Wir machen perzentil-abhängige Cutoffs, um Ausreißer zu entfernen. Wir packen 
# dann die Sollmengen in Bins. Dann bilden wir für jeden Bin den Median und
# oberes bzw unteres Perzentil. So können wir einigermaßen mengenabhängig die LT 
# prognostizieren.

# 0010-------------------------------
df_0010 <- all_data_finalized %>%
    filter(vorgangsfolge == "0010", !is.na(sollmenge), !is.na(lead_time_soll))

# Berechne 1. bis 99. Perzentil – gegen Extremwerte
cutoffs <- df_0010 %>%
    summarise(
        menge_min = quantile(sollmenge, 0.01),
        menge_max = quantile(sollmenge, 0.99),
        lt_min    = quantile(lead_time_soll, 0.01),
        lt_max    = quantile(lead_time_soll, 0.99)
    )

# Daten beschneiden anhand der Schwellen
df_0010_clean <- df_0010 %>%
    filter(
        sollmenge >= cutoffs$menge_min,
        sollmenge <= cutoffs$menge_max,
        lead_time_soll >= cutoffs$lt_min,
        lead_time_soll <= cutoffs$lt_max
    )


#bin_size <- 250000
iqr <- IQR(df_0010_clean$sollmenge, na.rm = TRUE)
n <- sum(!is.na(df_0010_clean$sollmenge))
bin_size <- 2 * iqr / (n^(1/3))

# 1. Binning mit automatisch berechneter Breite
df_binned <- df_0010_clean %>%
    mutate(
        bin = cut(
            sollmenge,
            breaks = seq(0, max(sollmenge, na.rm = TRUE) + bin_size, by = bin_size),
            include.lowest = TRUE,
            right = FALSE
        )
    )

# 2. Statistiken je Bin
lt_by_bin <- df_binned %>%
    group_by(bin) %>%
    summarise(
        median_lt = median(lead_time_soll, na.rm = TRUE),
        p10 = quantile(lead_time_soll, 0.10, na.rm = TRUE),
        p90 = quantile(lead_time_soll, 0.90, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(!is.na(bin))

# 3. Bin-Grenzen berechnen
bin_levels <- levels(df_binned$bin)
bin_bounds <- tibble(bin = factor(bin_levels, levels = bin_levels)) %>%
    mutate(
        bin_start = as.numeric(sub("\\[(.*),.*", "\\1", bin_levels)),
        bin_end   = as.numeric(sub(".*,([^]]*)\\)", "\\1", bin_levels)),
        bin_label = paste0(
            formatC(bin_start / 1000, format = "f", digits = 0, big.mark = "."), "k – ",
            formatC(bin_end / 1000, format = "f", digits = 0, big.mark = "."), "k"
        )
    ) %>%
    filter(bin %in% lt_by_bin$bin)

# 4. Labels korrekt anhängen
lt_by_bin <- lt_by_bin %>%
    left_join(bin_bounds, by = "bin") %>%
    mutate(bin_label = factor(bin_label, levels = bin_label))

# 5. Plot erzeugen
ggplot(lt_by_bin, aes(x = bin_label)) +
    geom_ribbon(aes(ymin = p10, ymax = p90, group = 1), fill = "#002366", alpha = 0.2) +
    geom_line(aes(y = median_lt, group = 1), color = "#002366", linewidth = 1) +
    geom_point(aes(y = median_lt), color = "#002366", size = 2) +
    labs(
        title = "Lead Time je Losgrößen-Bin (Median & 10–90% Perzentilbereich)",
        x = "Sollmengen-Bereich",
        y = "Lead Time (soll)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

est_lt_0010 <- lt_by_bin %>%
    select(bin_label, lt_median = median_lt, lt_lower = p10, lt_upper = p90)

view(est_lt_0010)
# 0010_0020--------------------------


