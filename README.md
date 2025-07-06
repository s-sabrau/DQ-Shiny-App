Interactive Medical Data App

Version: 1.0.0Date: January 8, 2025
Contact: sarah.braun@med.uni-greifswald.de

Authors

Sarah Braun

Christian Draeger

Lea Michaelis

Sherry Freiesleben

Dagmar Waltemath

Matthias Löbe

Judith Wodke

Abstract

This Shiny application provides an interactive platform for uploading, integrating, visualizing, and summarizing heterogeneous medical datasets (CSV, JSON, FHIR). Built on state-of-the-art R packages (e.g., shiny, fhircrackr, ggplot2, leaflet), it enables researchers and clinical IT teams to explore data quality, distributions, and shared categories across multiple sources with full reproducibility and modularity.

Background

Heterogeneous Data SourcesClinical and research data often exist in disparate formats: CSV exports from laboratory information systems, JSON-formatted histograms, and HL7 FHIR servers.

Need for IntegrationComparative analyses require harmonization of these diverse formats into a unified view.

Interactive DashboardsShiny apps facilitate non-technical users’ exploration of complex datasets in real time.

Features

Data Import

Upload multiple CSV or JSON files

Connect to any FHIR® server (default: HAPI Test Server – placeholder; replace with your internal FHIR endpoint in production)

Column Mapping

Dynamic mapping of CSV columns when “Category” and “Count” are not standard

Visual Exploration

Draggable mini-plots (Histogram, Pie Chart, Line Chart) with adjustable transparency

“Stack All” or “Stack Selected” controls for plot arrangement

Data Combination & Intersection

Stacked-bar combination of selected categories across files

Identification and export of category intersections

Statistical Overview

Automatic tables of dataset sizes, means, and category presence

Color-coded summary indicating categories present in all, multiple, or single sources

Geospatial Visualization

Map of German Data Integration Centers using leaflet and geodata

System Architecture

This application adopts a modular Shiny-based framework designed for clarity, testability, and maintainability:

UI Layer: Defined by fluidPage() and navbarPage(), organizing functionality into clear tabs (Data Upload, Visualization, Combined Data, Statistics).

Server Layer: Implements reactive (reactive(), eventReactive()) and observer (observe(), observeEvent()) constructs to handle data loading, mapping, combination, and plotting in response to user inputs.

Helper Modules: Encapsulated functions (loadJsonData(), loadCsvData(), make_safe_id()) ensure consistent data parsing, validation, and sanitization across different file formats.

Plotting Components: Separate reactive outputs for ggplot2-based charts and leaflet maps, each with their own render functions (renderPlot(), renderLeaflet()) to maintain loose coupling.

Data Integration Pipeline: Central reactive object (allData) unifies datasets from file uploads and FHIR requests, driving both visualization and statistical summaries without redundant computations.

Extensibility & Testing: Modular structure allows insertion of new data sources or plot types via additional helper modules, and supports unit testing of individual functions without UI dependencies.

By replacing the previous monolithic depiction with this modular breakdown, the architecture remains robust, scalable, and aligned with best practices for scientific reproducibility.

Requirements

R ≥ 4.2

Operating System: Linux, macOS, or Windows

R packages (installed automatically via ensure_pkg()):

install.packages(c(
  "shiny", "shinythemes", "shinyjqui",
  "jsonlite", "readr", "fhircrackr", "httr",
  "dplyr", "tidyr", "ggplot2", "leaflet",
  "geodata", "terra"
))

Installation

Clone repository
(...)

Install dependencies & run

source("app.R")  # ensure_pkg() will install missing packages automatically
runApp("app.R")

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

How to Cite:Braun S., Draeger C., Michaelis L., et al. (2025). Interactive Medical Data App. GitHub. https://github.com/YourOrg/medical-data-dashboard

Note: The default FHIR server (http://hapi.fhir.org/baseR4) is provided as an example only. In a production environment, replace this URL with your institution’s internal FHIR endpoint.

