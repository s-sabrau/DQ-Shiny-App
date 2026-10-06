# Interactive Medical Data App

**Version**: 1.0.1

---

## Authors

* Tom Gebhardt
* Sarah Braun
* Christian Draeger
* Lea Michaelis
* Sherry Freiesleben
* Dagmar Waltemath
* Matthias Löbe
* Judith Wodke

---

## Abstract

> This Shiny application (IQRviz, *Interoperable data Quality Report visualization*) provides an interactive platform for uploading, comparing, aggregating and summarizing data quality reports and value distributions from heterogeneous sources: CSV and JSON distributions, FHIR® MeasureReports (e.g. DQ summary reports, DQ-SR, and census reports) and FHIR bundles. Built on R packages such as **shiny**, **ggplot2**, **ggiraph** and **fhircrackr**, it lets researchers compare value distributions across sites, exclude or aggregate sites, and export the result as CSV, JSON or a FHIR `MeasureReport`.

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
| **Data Import**                    | • Upload multiple CSV, JSON, FHIR® `MeasureReport` and FHIR bundle files (e.g. pre-fetched from a HAPI test server)<br>• Live FHIR server connection code exists (`fhircrackr`/`httr`) but is currently not wired into the UI |
| **MeasureReports / DQ-SRs**        | • Any `MeasureReport` is read from its FHIR structure alone (R4 and R5): every stratifier becomes a category/count distribution<br>• Reports with several stratifiers get a selector for the distribution to use |
| **Census Reports**                 | • Age × gender `MeasureReport`s in *composite* and *separate* style (see [Census report styles](#census-report-styles)) |
| **Column Mapping**                 | • Map the category column (and optionally a count column) when CSV headers differ from `Category`/`Count`                                                  |
| **FHIR in Bins**                   | • Bin the values of a FHIR attribute (text, numeric, boolean)<br>• Export the bins as a FHIR `MeasureReport` (separate or composite stratifier format)       |
| **Compare**                        | • Compare sources side by side or overlaid (lines, difference to a reference, grouped or transparent bars); the overlay highlights a source on hover<br>• Bins from a census report, from the categories of any report, or from *FHIR in bins*<br>• Aggregate sources into a combined source<br>• Sources that cannot be mapped to the bins are excluded and listed in an info box<br>• Export as CSV, JSON or FHIR `MeasureReport` |
| **Data Combination & Intersection**| • Stacked-bar combination of selected categories across datasets<br>• Identify and export category intersections as JSON                                     |
| **Statistical Overview**           | • Auto-generated tables: number of categories and mean count per dataset<br>• Color-coded summary of category prevalence (all/multiple/single sources)       |
| **Geospatial Visualization**       | • *Currently disabled*: code for an interactive map of German Data Integration Centers (**leaflet** + **geodata**) exists but is commented out and not part of the running app |

---

## Input Formats

Each uploaded file is assigned a type on the **Data Upload** tab:

| **Type**     | **Files**                                                                                                                                                  |
|--------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **CSV/JSON** | Value distributions: CSV files (with `Category`/`Count` columns, or mapped columns), JSON histograms (`{"Histogram": [{"Category": {"@value": …}, "Count": {"@value": …}}]}`) and FHIR `MeasureReport`s such as DQ-SRs |
| **Census**   | Age × gender `MeasureReport`s in composite or separate style                                                                                               |
| **FHIR**     | FHIR bundles (JSON) with individual resources, e.g. `Patient`                                                                                              |

### Census report styles

Census reports (e.g. the MII *Summary Report Age Gender* measure) come in two styles:

* **Composite**: one stratifier whose strata combine an age group and a gender (`component`s), so every stratum is one age × gender cell.
* **Separate**: two stratifiers, one for gender and one for age group. They only hold the totals per gender and per age group, not the age × gender counts.

The app never estimates age × gender counts from a separate report. The **Census Data** tab shows a separate report as two panels (age groups, gender), and **Compare** uses its age totals: with census bins only when *Combine male and female* is ticked, with report bins always.

### Matching sources to bins

In **Compare**, every source is mapped onto the selected bins. With bins from the *categories of a report*, a category matches a bin

1. by name (case-insensitive),
2. by numeric range, when the category's number or range lies within the bin's range (e.g. `20` or `18-25` fall into `18-64`), or
3. without a trailing detail such as the gender, when the bin has none (e.g. `0-4 · male` counts towards `0-4`).

FHIR bundles are mapped through the resource type and attribute chosen in the **FHIR in bins** tab. Records outside all bins are not counted; the info box reports how many.

---

## System Architecture

The application is a single-file Shiny app (`app.R`) with a clear separation of user interface and server logic:

1. **UI Layer**
   Defined via `fluidPage()` and `navbarPage()`, grouping functionality into:

   * Data Upload
   * Census Data
   * FHIR in bins
   * Compare
   * Combined Data
   * Statistics

2. **Server Layer**

   * Reactivity: `reactive()`, `eventReactive()` ensure immediate UI updates
   * Observers: `observe()`, `observeEvent()` handle user-driven events

3. **Loaders and Helpers**

   * `loadCsvData()`, `loadJsonData()`, `loadFhirFile()`, `loadCensusData()` parse the input formats
   * `readMeasureReportDistributions()` reads any `MeasureReport` as category/count distributions
   * `parse_range_label()` and `map_to_report_bins()` map values and categories onto bins

4. **Plotting Components**

   * `ggplot2` charts; the **Compare** overlay is made interactive with `ggiraph`
   * A **leaflet** map render function exists but is currently commented out (see Geospatial Visualization above)

5. **Data Integration Pipeline**
   The reactive `allData()` unifies the CSV/JSON distributions for the Combined Data and Statistics tabs; `vizData()` maps all selected sources onto the bins for the Compare tab.

---

## Requirements

* **R** ≥ 4.2
* **OS**: Linux, macOS, Windows
* **Packages** (installed automatically via `ensure_pkg()`):

  ```r
  install.packages(c(
    "shiny", "shinythemes", "shinyjqui",
    "jsonlite", "readr", "fhircrackr", "httr",
    "dplyr", "tidyr", "ggplot2", "ggiraph", "leaflet",
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

   * **Data Upload**: Upload files and assign each a type (CSV/JSON, Census, FHIR); map CSV columns and choose the stratifier of multi-stratifier `MeasureReport`s
   * **Census Data**: Investigate a single census report: age × gender distribution (composite) or age and gender totals (separate), summary and raw tables, download as JSON or PNG
   * **FHIR in bins**: Bin the values of a FHIR attribute and export the bins as a FHIR `MeasureReport`; the bins can be reused in Compare
   * **Compare**: Select sites, aggregate them, choose the bins (census, categories of a report, or FHIR bins) and compare them side by side or overlaid; download as CSV, JSON or `MeasureReport`
   * **Combined Data**: Combine categories, download JSON
   * **Statistics**: View summaries & category presence

### Example files

`input_examples/` contains files for every input type:

| **Path**                                   | **Type**  | **Content**                                                    |
|--------------------------------------------|-----------|----------------------------------------------------------------|
| `age.json`                                 | CSV/JSON  | JSON histogram of age groups                                   |
| `csv/patients.csv`, `csv/people-100.csv`   | CSV/JSON  | Record-level CSV files (map a category column on upload)       |
| `fhir/hapi_batch*.json`                    | FHIR      | FHIR bundles fetched from a public HAPI test server            |
| `MeasureReport-…-composite-zensus-2022.json`, `sex_gender_zensus/2022.json` | Census | 2022 census, composite style                 |

---

## Future Directions

* **Extended FHIR Support**: Add Observations, Conditions
* **MeasureReport `measure`**: Exported `MeasureReport`s do not yet reference a `Measure`, which FHIR R4 requires
* **Automated Testing**: Full `testthat` suite integration

---

## License & Citation

* **License**: [MIT](LICENSE)
* **Citation**: Will be added once the paper is published.

---

> **Note**: A live FHIR-server connection was implemented and works, but is currently not exposed in the UI — no immediate use case was found for it. FHIR data is currently provided via uploading pre-fetched bundle JSON files instead.


