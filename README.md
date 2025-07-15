# Interactive Medical Data App

Version: 1.0.0
Date: January 8, 2025
Contact: sarah.braun@med.uni-greifswald.de

## Description

The Interactive Medical Data App is a modular R Shiny dashboard for uploading, harmonizing, visualizing, and exporting heterogeneous medical datasets.
It supports CSV, JSON, and HL7 FHIR® data sources and is designed for rapid, interactive exploration of data distributions, quality, and category overlaps—ideal for clinical research, IT, and data integration projects.

## Features

### Flexible Data Import:
Upload multiple CSV/JSON files or connect to any FHIR® server (default: HAPI Test Server, easily replaceable)
Map custom column names to 'Category' and 'Count' for heterogeneous CSVs
### Visual Exploration:
Arrange draggable mini-plots (histogram, pie, line) per dataset
Adjustable transparency and filtering for each plot
Stack all plots or selected pairs for comparison
Data Combination & Intersection:
Combine selected categories across datasets (stacked bar plot)
Identify and export category intersections to JSON
### Statistics:
Dataset size and mean count summary tables
Color-coded prevalence overview of categories (across all, multiple, or single files)
### Geospatial Visualization:
Interactive map of German Data Integration Centers using leaflet and geodata

## Prerequisites

R (version ≥ 4.2)
OS: Linux, macOS, or Windows
R Packages:
All required packages are installed automatically by the app on startup:
shiny, shinythemes, shinyjqui, jsonlite, readr, fhircrackr, httr, dplyr, tidyr, ggplot2, leaflet, geodata, terra
Installation & Usage

### 1. Clone the repository
git clone https://github.com/YourOrg/medical-data-dashboard.git
cd medical-data-dashboard
2. Start the app
You can run the app directly via R or RStudio. All dependencies are checked and installed on launch.

### From the project directory:
source("app.R")
shiny::runApp("app.R")
Alternatively, open app.R in RStudio and click "Run App".

### 2. App Workflow
Data Upload: Import CSV, JSON, or connect to FHIR; map columns as needed
Visualization: Explore individual and combined plots, filter categories, adjust transparency
Combined Data: Select and combine categories across datasets; export as JSON
Statistics: Review dataset summaries and category prevalence
Map: Visualize German Data Integration Centers

## Troubleshooting

### 1. Missing Packages:
The app installs missing packages on first run. If installation fails, check your internet connection and CRAN mirrors.
FHIR Connection Issues:
Ensure the FHIR server URL is reachable from your machine.
The default HAPI Test Server (http://hapi.fhir.org/baseR4) is a demo endpoint; for real data, replace with your institution's FHIR API.
Plot or Data Loading Errors:
Make sure uploaded files are correctly formatted.
For custom CSVs, ensure correct mapping to 'Category' and 'Count' columns.
Port Conflicts:
If running multiple Shiny apps, check and adjust the port parameter in runApp().
Architecture

The application is organized into clear UI and server layers:

### UI:

Modular navigation (Data Upload, Visualization, Combined Data, Statistics, Map)
Dynamic controls and plot arrangement
### Server:

Data loading and harmonization from multiple sources
Real-time reactivity for user-driven filtering and visualization
Encapsulated helpers for file parsing, FHIR integration, and ID sanitization
Modular plotting with ggplot2 and leaflet
Extensibility:

Easily add new data sources, categories, or plot types
Codebase supports isolated testing of logic functions

## Citation

If you use this app in your research or project, please cite as:

Braun S., Draeger C., Michaelis L., Freiesleben S., Waltemath D., Löbe M., Wodke J. (2025).
Interactive Medical Data App. GitHub. https://github.com ... 


