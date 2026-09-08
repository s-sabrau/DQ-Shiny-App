# Interactive Medical Data App

**Version**: 1.0.0
**Date**: January 8, 2025
**Contact**: [sarah.braun@med.uni-greifswald.de](mailto:sarah.braun@med.uni-greifswald.de)

---

## Authors

* Sarah Braun
* Christian Draeger
* Lea Michaelis
* Sherry Freiesleben
* Dagmar Waltemath
* Matthias Löbe
* Judith Wodke

---

## Abstract

> This Shiny application provides an interactive platform for uploading, integrating, visualizing, and summarizing heterogeneous medical datasets (CSV, JSON, FHIR). Built on state-of-the-art R packages (e.g., **shiny**, **fhircrackr**, **ggplot2**, **leaflet**), it enables researchers and clinical IT teams to explore data quality, distributions, and shared categories across multiple sources with full reproducibility and modularity.

---

## Background

* **Heterogeneous Data Sources**
  Clinical and research data often exist in disparate formats: CSV exports, JSON-formatted histograms, and HL7 FHIR APIs.

* **Need for Integration**
  Comparative analyses require harmonization of these diverse formats into a unified view.

* **Interactive Dashboards**
  Shiny apps facilitate real-time exploration for non-technical users.

---

## Features

| **Category**                       | **Description**                                                                                                                                             |
|------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Data Import**                    | • Upload multiple CSV, JSON, or FHIR® bundle JSON files (e.g. pre-fetched from a HAPI Test Server)<br>• Live FHIR server connection code exists (`fhircrackr`/`httr`) but is currently not wired into the UI — no immediate use case was found, so it's unused for now    |
| **Column Mapping**                 | • Dynamically map ‘Category’ and ‘Count’ columns when CSV headers differ                                                                                   |
| **Visual Exploration**             | • Draggable mini-plots (Histogram, Pie Chart, Line Chart) with adjustable transparency<br>• “Stack All” and “Stack Selected” controls                       |
| **Data Combination & Intersection**| • Stacked-bar combination of selected categories across datasets<br>• Identify and export category intersections as JSON                                     |
| **Statistical Overview**           | • Auto-generated tables: dataset sizes, mean counts<br>• Color-coded summary of category prevalence (all/multiple/single sources)                         |
| **Geospatial Visualization**       | • *Currently disabled*: code for an interactive map of German Data Integration Centers (**leaflet** + **geodata**) exists but is commented out and not part of the running app                                                                      |
| **FHIR Bin Reports**               | • Bin FHIR attribute values (text, numeric, boolean) and export as a FHIR `MeasureReport` (separate or composite stratifier format)                        |

---

## System Architecture

This application adopts a **modular Shiny framework** for clarity, testability, and maintainability:

1. **UI Layer**
   Defined via `fluidPage()` and `navbarPage()`, grouping functionality into:

   * Data Upload
   * Census Data
   * FHIR in bins
   * Visualization
   * Combined Data
   * Statistics

2. **Server Layer**

   * Reactivity: `reactive()`, `eventReactive()` ensure immediate UI updates
   * Observers: `observe()`, `observeEvent()` handle user-driven events

3. **Helper Modules**

   * `loadJsonData()`, `loadCsvData()`, `make_safe_id()` encapsulate parsing, validation, sanitization

4. **Plotting Components**

   * `ggplot2` charts via separate render functions (`renderPlot()`); a **leaflet** map render function exists but is currently commented out (see Geospatial Visualization above)

5. **Data Integration Pipeline**
   Central reactive `allData` unifies datasets from uploads, powering both visualization and statistics without redundant computations.

6. **Extensibility & Testing**

   * Modular structure allows adding new data sources or plot types
   * Supports unit testing of individual functions independent of UI

---

## Requirements

* **R** ≥ 4.2
* **OS**: Linux, macOS, Windows
* **Packages** (installed automatically via `ensure_pkg()`):

  ```r
  install.packages(c(
    "shiny", "shinythemes", "shinyjqui",
    "jsonlite", "readr", "fhircrackr", "httr",
    "dplyr", "tidyr", "ggplot2", "leaflet",
    "DT"
  ))
  ```

---

## Installation

1. **Clone repository**

   ```bash
   git clone https://git.uni-greifswald.de/MILA_public/DQ-App.git
   cd DQ-App
   ```

2. **Install & Run**

   ```r
   source("app.R")  # ensure_pkg() installs missing dependencies
   runApp("app.R")
   ```

> **Need a ready-made R environment?**
> If you don't have a suitable R installation, use a container image from the
> [Rocker Project](https://rocker-project.org/). The `rocker/shiny` or
> `rocker/tidyverse` images provide R with Shiny preinstalled and give you a
> reproducible environment for running the app.

---

## Usage

1. Run the app (see Installation above) — Shiny will open it automatically in a browser window/tab.
2. Navigate tabs:

   * **Data Upload**: Upload CSV/JSON/FHIR bundle files
   * **Census Data**: Visualize census population data and uploaded FHIR patient data
   * **FHIR in bins**: Bin FHIR attribute values and export as a FHIR `MeasureReport`
   * **Visualization**: Arrange & filter mini-plots
   * **Combined Data**: Combine categories, download JSON
   * **Statistics**: View summaries & category presence

---

## Quality Assurance & Reproducibility

* **Version Control**: Git with feature branches & peer review
* **Unit Testing**: Planned `testthat` coverage for core functions
* **Documentation**: Inline comments + precise README ensure transparency

---

## Future Directions

* **Extended FHIR Support**: Add Observations, Conditions
* **Automated Testing**: Full `testthat` suite integration

---

## License & Citation

* **License**: [MIT](LICENSE)
* **Citation**:

  > Braun S., Draeger C., Michaelis L., et al. (2025). *Interactive Medical Data App*. [https://git.uni-greifswald.de/MILA_public/DQ-App](https://git.uni-greifswald.de/MILA_public/DQ-App)

---

> **Note**: A live FHIR-server connection was implemented and works, but is currently not exposed in the UI — no immediate use case was found for it. FHIR data is currently provided via uploading pre-fetched bundle JSON files instead.


