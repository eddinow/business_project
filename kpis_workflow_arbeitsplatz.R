# Initialisierung ----------------------------------------------------------------

rm(list = ls())
set.seed(1)

library(tidyverse)
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(shiny)
library(DT)
library(stringr)

# Daten laden -------------------------------------------------------------------
source("create_all_data_finalized.R")  # erzeugt 'all_data_finalized'
vorgaenge_raw <- read_excel("2025-04-08_Vorgänge SAP.xlsx")


# Wir wollen ein Ampelsystem für alle Arbeitsplätze hinsichtlich ihrer Performance
# schaffen. Negative Abweichungen über 60% = rot, 20-60% = orange, 0-10%=grün).
# Dazu müssen wir zuerst die Spalte Abweichung aus all_data_finalized abhängig 
# von der 'Auftragsnummer' dem df vorgaenge_raw hinzufügen



vorgaenge_raw <- vorgaenge_raw %>%
    left_join(
        dplyr::select(all_data_finalized, auftragsnummer, abweichung, lead_time_soll),
        by = c("Auftragsnummer" = "auftragsnummer")
    )


df_long <- all_data_finalized %>%
    mutate(
        vorgangsfolge_list = str_split(vorgangsfolge, " → "),
        arbeitsplatzfolge_list = str_split(arbeitsplatzfolge, " → ")
    ) %>%
    unnest_longer(vorgangsfolge_list) %>%
    unnest_longer(arbeitsplatzfolge_list) %>%
    rename(
        Einzelvorgang = vorgangsfolge_list,
        Einzelarbeitsplatz = arbeitsplatzfolge_list
    ) %>%
    mutate(Auftragsnummer = auftragsnummer)

# Join mit vorgaenge_raw – basierend auf Einzelwerten
arbeitsplatz_all_data_finalized <- df_long %>%
    left_join(
        dplyr::select(
            vorgaenge_raw,
            Auftragsnummer,
            Vorgangsnummer,
            Arbeitsplatz,
            `Iststart Vorgang`,
            `Istende Vorgang`
        ),
        by = c("Auftragsnummer", "Einzelvorgang" = "Vorgangsnummer", "Einzelarbeitsplatz" = "Arbeitsplatz")
    )

# abweichungen_je_vorgangsfolge <- arbeitsplatz_all_data_finalized %>%
#     filter(!is.na(abweichung)) %>%
#     group_by(vorgangsfolge, Einzelvorgang) %>%
#     summarise(
#         durchschnitt_abweichung = mean(abweichung, na.rm = TRUE),
#         anzahl = n(),
#         .groups = "drop"
#     ) %>%
#     arrange(vorgangsfolge, Einzelvorgang)

abweichungen_je_vorgangsfolge <- arbeitsplatz_all_data_finalized %>%
    filter(!is.na(abweichung) & abweichung < 0) %>%  # nur negative
    group_by(vorgangsfolge, Einzelvorgang) %>%
    summarise(
        durchschnitt_abweichung = mean(abweichung, na.rm = TRUE),
        anzahl = n(),
        .groups = "drop"
    )

# Pro Vorgangsfolge den Einzelvorgang mit höchster Abweichung auswählen
max_abweichung_je_folge <- abweichungen_je_vorgangsfolge %>%
    group_by(vorgangsfolge) %>%
    slice_max(durchschnitt_abweichung, with_ties = FALSE) %>%
    ungroup()

kritischster_vorgang <- abweichungen_je_vorgangsfolge %>%
    group_by(vorgangsfolge) %>%
    slice_min(durchschnitt_abweichung, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr:select(vorgangsfolge, Einzelvorgang) %>%
    rename(kritischster_vorgang = Einzelvorgang)


# ggplot(max_abweichung_je_folge, aes(x = reorder(vorgangsfolge, durchschnitt_abweichung), 
#                                     y = durchschnitt_abweichung,
#                                     fill = Einzelvorgang)) +
#     geom_col() +
#     coord_flip() +
#     labs(
#         title = "Einzelvorgang mit höchster Abweichung je Vorgangsfolge",
#         x = "Vorgangsfolge",
#         y = "Ø Abweichung (Tage)",
#         fill = "Einzelvorgang"
#     ) +
#     theme_minimal()

# ⏱ Durchschnittliche Lead Time je Einzelvorgang pro Vorgangsfolge
lt_pro_vorgang <- arbeitsplatz_all_data_finalized %>%
    filter(!is.na(`Iststart Vorgang`) & !is.na(`Istende Vorgang`)) %>%
    mutate(lt = as.numeric(difftime(`Istende Vorgang`, `Iststart Vorgang`, units = "days"))) %>%
    group_by(vorgangsfolge, Einzelvorgang) %>%
    summarise(
        durchschnitt_lt = mean(lt, na.rm = TRUE),
        durchschnitt_abweichung = mean(abweichung, na.rm = TRUE),
        .groups = "drop"
    )

# 🧮 Durchschnittliche Gesamt-LT pro Vorgangsfolge
lt_gesamt_pro_folge <- arbeitsplatz_all_data_finalized %>%
    filter(!is.na(`Iststart Vorgang`) & !is.na(`Istende Vorgang`)) %>%
    group_by(vorgangsfolge, Auftragsnummer) %>%
    summarise(
        gesamt_lt = as.numeric(difftime(max(`Istende Vorgang`), min(`Iststart Vorgang`), units = "days")),
        .groups = "drop"
    ) %>%
    group_by(vorgangsfolge) %>%
    summarise(
        durchschnitt_gesamt_lt = mean(gesamt_lt, na.rm = TRUE),
        .groups = "drop"
    )

# 🔴 Einzelvorgang mit höchster Abweichung je Folge
# kritischster_vorgang <- lt_pro_vorgang %>%
#     group_by(vorgangsfolge) %>%
#     slice_max(durchschnitt_abweichung, with_ties = FALSE) %>%
#     ungroup() %>%
#     dplyr::select(vorgangsfolge, Einzelvorgang) %>%
#     rename(kritischster_vorgang = Einzelvorgang)

# 🔗 Alles zusammenführen
# plot_df <- lt_pro_vorgang %>%
#     left_join(lt_gesamt_pro_folge, by = "vorgangsfolge") %>%
#     left_join(kritischster_vorgang, by = "vorgangsfolge") %>%
#     mutate(
#         ist_kritisch = Einzelvorgang == kritischster_vorgang
#     )

plot_df <- lt_pro_vorgang %>%
    left_join(lt_gesamt_pro_folge, by = "vorgangsfolge") %>%
    left_join(kritischster_vorgang, by = "vorgangsfolge") %>%
    mutate(
        ist_kritisch = Einzelvorgang == kritischster_vorgang
    )

plot_vorgangsfolge <- function(folge_name) {
    df <- filter(plot_df, vorgangsfolge == folge_name)
    
    ggplot(df, aes(x = Einzelvorgang, y = durchschnitt_lt, fill = ist_kritisch)) +
        geom_col() +
        geom_hline(aes(yintercept = durchschnitt_gesamt_lt), color = "black", linetype = "dashed", size = 1) +
        scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
        labs(
            title = paste("Vorgangsfolge:", folge_name),
            y = "Ø Lead Time (Tage)",
            x = "Einzelvorgang",
            fill = "Höchste Abweichung"
        ) +
        theme_minimal()
}

plot_vorgangsfolge("0010 → 0020 → 0030")

