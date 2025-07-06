# Interactive Medical Data App

**Version**: 1.0.0  
**Date**: January 8, 2025  
**Contact**: sarah.braun@med.uni-greifswald.de  

---

## Authors

- Sarah Braun  
- Christian Draeger  
- Lea Michaelis  
- Sherry Freiesleben  
- Dagmar Waltemath  
- Matthias Löbe  
- Judith Wodke  

---

## Abstract

This Shiny application provides an interactive platform for uploading, integrating, visualizing, and summarizing heterogeneous medical datasets (CSV, JSON, FHIR). Built on state-of-the-art R packages (e.g., **shiny**, **fhircrackr**, **ggplot2**, **leaflet**), it enables researchers and clinical IT teams to explore data quality, distributions, and shared categories across multiple sources with full reproducibility and modularity.

---

## Background

1. **Heterogeneous Data Sources**  
   Clinical and research data often exist in disparate formats: CSV exports from laboratory information systems, JSON-formatted histograms, and HL7 FHIR servers.  
2. **Need for Integration**  
   Comparative analyses require harmonization of these diverse formats into a unified view.  
3. **Interactive Dashboards**  
   Shiny apps facilitate non-technical users’ exploration of complex datasets in real time.

---

## Features

1. **Data Import**  
   - Upload multiple CSV or JSON files  
   - Connect to any FHIR® server (default: HAPI Test Server – placeholder; replace with your internal FHIR endpoint in production)  
2. **Column Mapping**  
   - Dynamic mapping of CSV columns when “Category” and “Count” are not standard  
3. **Visual Exploration**  
   - Draggable mini-plots (Histogram, Pie Chart, Line Chart) with adjustable transparency  
   - “Stack All” or “Stack Selected” controls for plot arrangement  
4. **Data Combination & Intersection**  
   - Stacked-bar combination of selected categories across files  
   - Identification and export of category intersections  
5. **Statistical Overview**  
   - Automatic tables of dataset sizes, means, and category presence  
   - Color-coded summary indicating categories present in all, multiple, or single sources  
6. **Geospatial Visualization**  
   - Map of German Data Integration Centers using **leaflet** and **geodata**

---

## System Architecture

  ┌───────────────────┐
  │   Shiny UI (R)    │
  └────────┬──────────┘
           │
           ▼
  ┌───────────────────┐
  │   Server Logic    │
  │                   │
  │  • Data Loading   │
  │  • Data Cleaning  │
  │  • Reactive Core  │
  │  • ggplot2 Plots  │
  │  • Leaflet Map    │
  └───────────────────┘


- **Reactivity**: All data objects are reactive (`reactive()`, `eventReactive()`) to ensure instantaneous UI updates.  
- **Modularity**: Helper functions (e.g., `loadJsonData()`, `loadCsvData()`, `make_safe_id()`) encapsulate repeated tasks.  
- **UI Design**: Based on **shinythemes** (“spacelab”) with custom CSS/JS for enhanced interactivity.

---

## Requirements

- **R** ≥ 4.2  
- Operating System: Linux, macOS, or Windows  
- R packages (installed automatically via `ensure_pkg()`):
  ```r
  install.packages(c(
    "shiny", "shinythemes", "shinyjqui",
    "jsonlite", "readr", "fhircrackr", "httr",
    "dplyr", "tidyr", "ggplot2", "leaflet",
    "geodata", "terra"
  ))

**Installation:

git clone: (...)

Usage

Launch the app and open (...) in your browser.
Navigate through the tabs:
Data Upload: Upload CSV/JSON files or connect to FHIR (replace default HAPI Test Server URL).
Visualization: Arrange and filter mini-plots.
Combined Data: Combine selected categories and download JSON.
Statistics: View statistical summaries and category presence table.
Quality Assurance & Reproducibility

Version Control: Git branching model with peer reviews for feature development.
Planned Unit Tests: Use testthat for key functions (e.g., data loading).
Documentation: Inline code comments and this scientifically precise README ensure transparency.
Future Directions

Extended FHIR Support: Add resources such as Observations and Conditions.
Automated Testing: Full testthat suite integration.
CI/CD Pipeline: Dockerized deployment for automated testing and hosting.
License & Citation

License: (...)
How to Cite:
Braun S., Draeger C., Michaelis L., et al. (2025). Interactive Medical Data App. GitHub. https://github.com/YourOrg/medical-data-dashboardUsage


