# Interactive Medical Data App

Version: 1.0.0
Date: January 8, 2025
Contact: sarah.braun@med.uni-greifswald.de

## Description
The Interactive Medical Data App is a modular R Shiny dashboard for uploading,visualizing, and exporting heterogeneous medical datasets.
It supports CSV, JSON, and HL7 FHIR® data sources, enabling rapid and interactive exploration of data distributions, quality, and category overlaps. The app is designed for clinical research, IT, and medical data integration projects.


## Features
### Flexible Data Import:

- Upload multiple CSV/JSON files
- Connect to any HL7 FHIR® server (default: HAPI Test Server; easily configurable)
- Custom mapping for 'Category' and 'Count' columns in heterogeneous CSVs
  
### Interactive Visualization:
- Draggable mini-plots (histogram, pie, line) for each dataset
- Adjustable plot transparency and category filtering
- Stack/overlay plots for comparison
  
### Data Combination & Intersection:
- Combine categories across datasets (stacked bar plot)
- Identify and export category intersections to JSON
  
### Descriptive Statistics:
- Summary tables for dataset size and mean counts
- Color-coded overview of category prevalence


## Prerequisites
R (version ≥ 4.2)
OS: Linux, macOS, or Windows
R Packages: Installed automatically on app startup:
shiny, shinythemes, shinyjqui, jsonlite, readr, fhircrackr, httr, dplyr, tidyr, ggplot2, leaflet

## Installation
### 1. Clone the repository:


```bash
git clone https://github.com/[ aktuelles repo]
```


### Start the app:
In R or RStudio, from the project directory:

2. Start the app:
```r
source("aktuelle  App.R")
shiny::runApp("aktuelle App.R")
```


### Quick Start:
#### Data Upload:
Select "File (CSV/JSON)" or "FHIR (HAPI Test Server)" as data source
For custom CSVs, map columns to 'Category' and 'Count' as prompted.

#### Visualization:
Drag, arrange, and filter plots
Adjust transparency, select categories, compare distributions.

#### Data Combination:
Select datasets and categories to combine; view as stacked bar plot
Export combined/intersection results as JSON.

#### Statistics:
View dataset metrics and category overlap

## Troubleshooting
#### Missing R Packages:
App installs missing dependencies automatically.
If installation fails, check your internet connection and CRAN mirrors.

#### FHIR Server Connection Issues:
Ensure the FHIR server URL is reachable from your machine
The default HAPI Test Server (http://hapi.fhir.org/baseR4) is for testing; replace with your institutional endpoint as needed.

#### File Import Errors:
Make sure files are correctly formatted and columns are mapped
For custom CSVs, ensure correct assignment to 'Category' and 'Count'.

#### Port Conflicts:
If you have multiple Shiny apps running, adjust the port in runApp().

## Architecture
The Interactive Medical Data App consists of:

### UI Layer:
Modular navigation: Data Upload, Visualization, Combined Data, Statistics,
Dynamic controls and drag-and-drop plotting.

### Server Layer:
Data loading and harmonization from multiple sources (CSV, JSON, FHIR)
Real-time reactivity for user-driven filtering and visualization
Encapsulated helpers for file parsing, FHIR integration, and ID sanitization
Modular plotting with ggplot2 and leaflet.

### Extensibility:


## Citation
If you use this app in your research or project, please cite as:

Braun S., Draeger C., Michaelis L., Freiesleben S., Waltemath D., Löbe M., Wodke J. (2025).
Interactive Medical Data App. GitHub. https://github.com ... 


