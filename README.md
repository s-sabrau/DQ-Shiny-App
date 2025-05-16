# Medical Data Visualization Dashboard

A Shiny-based application for interactive upload, exploration, combination and statistical summarization of medical data in JSON and CSV formats. Dynamic CSV mapping ensures compatibility with non-standard column naming, while a rich UI fosters intuitive, drag-and-drop visual analysis.

---

## Table of Contents

- [Purpose](#purpose)  
- [Key Features](#key-features)  
- [Installation](#installation)  
- [Usage](#usage)  
- [Application Structure](#application-structure)   
- [Authors & Contact](#authors--contact)  
- [License](#license)  

---

## Purpose

This script boots up and configures all required R packages to run the dashboard application. It empowers users to:

1. **Upload** JSON or CSV files containing medical histogram data.  
2. **Dynamically map** non-standard CSV column names to required fields (`Category` and `Count`).  
3. **Visualize** data via stacked histograms, pie charts, and line charts in draggable plot boxes.  
4. **Combine** multiple datasets into unified and intersection views with download capability.  
5. **Summarize** key statistics and category distributions across all uploaded files.

---

## Key Features

- **Flexible Data Upload**  
  - Accepts multiple `.json` and `.csv` files in a single session.  
  - Automatic JSON parsing; user-guided CSV column mapping when headers differ.

- **Interactive Mapping UI**  
  - Generates per-file mapping controls only if expected columns are absent.  
  - Ensures robust handling of heterogeneous CSV schemas.

- **Dynamic Visualization**  
  - Four main tabs: Upload, Visualization, Combined Data, Statistics.  
  - Draggable, transparent plot boxes for side-by-side comparison.  
  - “Stack All” and “Stack Selected” functions for rapid overlay.

- **Combined & Intersection Views**  
  - Select files and categories to merge into a stacked bar plot.  
  - Identify and plot categories common to all selected datasets.  
  - JSON download handlers for both combined and intersection results.

- **Statistical Summary**  
  - Tabular display of per-file count and mean values.  
  - Color-coded category presence matrix (green/yellow/red) for cross-file comparison.

- **Geospatial Context**  
  - Embedded Leaflet map showing all German data integration centers.

---

## Installation

1. **Clone the repository**  
  install.packages(c(
  "shiny",
  "jsonlite",
  "ggplot2",
  "leaflet",
  "shinythemes",
  "shinyjqui",
  "geodata"
))
library(shiny)
runApp("app.R")

Application Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.

Dynamic CSV Mapping

When a CSV file lacks the standard Category or Count headers, the app:

    Detects missing columns.

    Renders two selectInput() controls for the user to map their column names to Category and Count.

    Waits (req()) until both mappings are provided before proceeding with data loading.

This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

  Author: Sarah Braun

  Date Created: January 8, 2025

  Email: sarah.braun@med.uni-greifswald.de

    Application Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.

Dynamic CSV Mapping

When a CSV file lacks the standard Category or Count headers, the app:

    Detects missing columns.

    Renders two selectInput() controls for the user to map their column names to Category and Count.

    Waits (req()) until both mappings are provided before proceeding with data loading.

This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

    Original Author: Sarah Braun

    Date Created: January 8, 2025

    Email: sarah.braun@med.uni-greifswald.de

    RApplication Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.

Application Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.


This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

    Original Author: Sarah Braun

    Date Created: January 8, 2025

    Email: sarah.braun@med.uni-greifswald.de

    Repository: !noch eintragen!



This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

    Authors: Sarah BraunApplication Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.

Dynamic CSV Mapping

When a CSV file lacks the standard Category or Count headers, the app:

    Detects missing columns.

    Renders two selectInput() controls for the user to map their column names to Category and Count.

    Waits (req()) until both mappings are provided before proceeding with data loading.

This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

    Original Application Structure

    UI (ui)

        Uses shinytheme("spacelab") for a clean, responsive design.

        Custom CSS and JavaScript for draggable/stackable plot boxes.

    Server (server)

        Data Loading: loadJsonData() and loadCsvData() handle parsing.

        Reactivity: Central allData() reactive to track uploads.

        Visualization: Per-file renderPlot() functions with unified y-axis scaling.

        Combination Logic: observeEvent(input$combineData) and input$combineIntersection.

        Statistics Tab: Builds HTML table with inline color coding for category presence.

Dynamic CSV Mapping

When a CSV file lacks the standard Category or Count headers, the app:

    Detects missing columns.

    Renders two selectInput() controls for the user to map their column names to Category and Count.

    Waits (req()) until both mappings are provided before proceeding with data loading.

This ensures scientific rigor and prevents pipeline breaks due to inconsistent input schemas.
Authors & Contact

    Original Author: Sarah Braun

    Date Created: January 8, 2025

    Email: sarah.braun@med.uni-greifswald.de

    Repository: https://github.com/yourusername/medical-data-dashboard
Author: Sarah Braun

    Date Created: January 8, 2025

    Email: sarah.braun@med.uni-greifswald.de

    Repository: https://github.com/yourusername/medical-data-dashboard


    Date Created: January 8, 2025

    Email: sarah.braun@med.uni-greifswald.de

    Repository: https://github.com/yourusername/medical-data-dashboard




