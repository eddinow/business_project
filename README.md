# SAP Lead Time Analyse App

Dieses Projekt beinhaltet eine interaktive Shiny-App zur Analyse und Visualisierung 
von SAP-Auftragsdaten. Ziel ist es, Abweichungen in der Durchlaufzeit (Lead Time) 
entlang verschiedener Kategorien (z. B. Werke, Linien, Planer, Materialien) transparent 
darzustellen.

---

## Projektstruktur

- `00_tidy/` – Datenbereinigung & Vorbereitung
- `01_transform/` – Feature Engineering & Transformation
- `02_model/` – Modelllogik (z. B. Klassifikation, Ausreißererkennung)
- `04_visualize/` – Visualisierungen & App-Komponenten
- `OPEN APP HERE/` – UI und Server der Shiny-App

---

## Start der App

1. RStudio öffnen
2. Projektdatei `business_project.Rproj` öffnen
3. In `OPEN APP HERE/app.R` gehen
4. App starten mit `Run App` (grüner Button oben rechts)