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
| **Data Import**                    | • Upload multiple CSV or JSON files<br>• Connect to any FHIR® server (default: HAPI Test Server – placeholder; replace with your internal FHIR endpoint)    |
| **Column Mapping**                 | • Dynamically map ‘Category’ and ‘Count’ columns when CSV headers differ                                                                                   |
| **Visual Exploration**             | • Draggable mini-plots (Histogram, Pie Chart, Line Chart) with adjustable transparency<br>• “Stack All” and “Stack Selected” controls                       |
| **Data Combination & Intersection**| • Stacked-bar combination of selected categories across datasets<br>• Identify and export category intersections as JSON                                     |
| **Statistical Overview**           | • Auto-generated tables: dataset sizes, mean counts<br>• Color-coded summary of category prevalence (all/multiple/single sources)                         |
| **Geospatial Visualization**       | • Interactive map of German Data Integration Centers using **leaflet** and **geodata**                                                                      |

---

```
                               - Connect to any FHIR® server (default: HAPI Test Server – _placeholder; replace with your internal FHIR endpoint_)
```

\| **Column Mapping**            | Dynamic mapping of CSV columns when `Category` and `Count` are not standard               |
\| **Visual Exploration**        | - Draggable mini-plots (Histogram, Pie, Line) with transparency control
\- “Stack All” / “Stack Selected” controls                                               |
\| **Data Combination & Intersection** | - Stacked-bar combination of selected categories across files
\- Identification & export of category intersections                                    |
\| **Statistical Overview**      | - Tables of dataset sizes, means, category presence
\- Color-coded summary (all / multiple / single sources)                                |
\| **Geospatial Visualization**  | Map of German Data Integration Centers (via **leaflet**, **geodata**)                     |

---

## System Architecture

This application adopts a **modular Shiny framework** for clarity, testability, and maintainability:

1. **UI Layer**
   Defined via `fluidPage()` and `navbarPage()`, grouping functionality into:

   * Data Upload
   * Visualization
   * Combined Data
   * Statistics

2. **Server Layer**

   * Reactivity: `reactive()`, `eventReactive()` ensure immediate UI updates
   * Observers: `observe()`, `observeEvent()` handle user-driven events

3. **Helper Modules**

   * `loadJsonData()`, `loadCsvData()`, `make_safe_id()` encapsulate parsing, validation, sanitization

4. **Plotting Components**

   * `ggplot2` charts and **leaflet** maps via separate render functions (`renderPlot()`, `renderLeaflet()`)

5. **Data Integration Pipeline**
   Central reactive `allData` unifies datasets from uploads and FHIR requests, powering both visualization and statistics without redundant computations.

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
    "geodata", "terra"
  ))
  ```

---

## Installation

1. **Clone repository**

   ```bash
   git clone https://github.com/YourOrg/medical-data-dashboard.git
   cd medical-data-dashboard
   ```

2. **Install & Run**

   ```r
   source("app.R")  # ensure_pkg() installs missing dependencies
   runApp("app.R")
   ```

---

## Usage

1. Open `(...)` in your browser.
2. Navigate tabs:

   * **Data Upload**: Upload CSV/JSON or connect to FHIR (*replace placeholder URL*)
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
* **CI/CD**: 

---

## License & Citation

* **License**: (...)
* **Citation**:

  > Braun S., Draeger C., Michaelis L., et al. (2025). *Interactive Medical Data App*. GitHub. [https://github.com/YourOrg/medical-data-dashboard](https://github.com/YourOrg/medical-data-dashboard)

---

> **Note**: Default FHIR server (`http://hapi.fhir.org/baseR4`) is a placeholder. Replace with your institution’s internal FHIR endpoint in production.


