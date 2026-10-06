# use this if the ragg renderer doesn't work, it defaults to the standard renderer than
options(shiny.useragg = FALSE)

# 1. Install and load required packages
ensure_pkg <- function(pkgs) {
  missing <- setdiff(pkgs, installed.packages()[, "Package"])
  if (length(missing)) install.packages(missing, dependencies = TRUE)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

required_pkgs <- c(
  "shiny", "shinythemes", "shinyjqui",
  "jsonlite", "readr",
  "fhircrackr", "httr",
  "dplyr", "tidyr",
  "ggplot2", "ggiraph", "leaflet",
  "DT"
)

ensure_pkg(required_pkgs)

# 2. Helper: sanitize dynamic input IDs
make_safe_id <- function(x) {
  id <- gsub("[^[:alnum:]_]", "_", x)
  id <- gsub("_+", "_", id)
  gsub("^_|_$", "", id)
}

# Helper: parse a range label into a (low, high) pair, c(NA, NA) if it is none
# Supported formats:  "0-17"  "18-34"  "65+"  "under 18"  "80 and over"  "42"
parse_range_label <- function(lbl) {
  lbl <- trimws(lbl)
  if (is.na(lbl)) return(c(NA, NA))
  # Pattern: "65+" or "65 and over" or "65 and older" → [65, Inf)
  if (grepl("^(\\d+)\\s*\\+$", lbl) ||
      grepl("^(\\d+)\\s+and\\s+(over|older|above)", lbl, ignore.case = TRUE) ||
      grepl("^(\\d+)\\s+or\\s+(over|older|above)", lbl, ignore.case = TRUE)) {
    lo <- as.numeric(sub("^(\\d+).*", "\\1", lbl))
    return(c(lo, Inf))
  }
  # Pattern: "under 18" or "less than 18" → [0, 17]
  if (grepl("^under\\s+(\\d+)$", lbl, ignore.case = TRUE) ||
      grepl("^less\\s+than\\s+(\\d+)$", lbl, ignore.case = TRUE)) {
    hi <- as.numeric(sub("\\D*(\\d+)$", "\\1", lbl)) - 1
    return(c(0, hi))
  }
  # Pattern: "18-34" or "18 to 34" or "18–34"
  m <- regmatches(lbl, regexpr("^(\\d+)\\s*[-–to]+\\s*(\\d+)$", lbl))
  if (length(m) == 1) {
    nums <- as.numeric(regmatches(m, gregexpr("\\d+", m))[[1]])
    return(c(nums[1], nums[2]))
  }
  # Single number label – exact match
  n <- suppressWarnings(as.numeric(lbl))
  if (!is.na(n)) return(c(n, n))
  c(NA, NA)
}

# Helper: read a FHIR MeasureReport (e.g. a DQ-SR) as Category/Count
# distributions, one per stratifier, plus one over the groups when several
# groups carry no stratifier. Only the FHIR structure is used (R4 and R5).
# Returns a named list of data frames; the names describe group and stratifier.
readMeasureReportDistributions <- function(mr) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # Text of a CodeableConcept (or of the first one in a list of them)
  concept_text <- function(cc) {
    if (is.null(cc) || length(cc) == 0) return(NA_character_)
    if (is.null(names(cc))) cc <- cc[[1]]
    as.character(cc$text %||% cc$coding[[1]]$display %||% cc$coding[[1]]$code %||% NA_character_)
  }
  # Value of a stratum or stratum component: R4 value, or R5 value[x]
  value_text <- function(x) {
    if (!is.null(x$valueBoolean)) return(tolower(as.character(x$valueBoolean)))
    if (!is.null(x$valueQuantity$value)) return(as.character(x$valueQuantity$value))
    if (!is.null(x$valueRange)) {
      lo <- x$valueRange$low$value
      hi <- x$valueRange$high$value
      return(if (is.null(hi)) paste0(lo %||% 0, "+") else paste0(lo %||% 0, "-", hi))
    }
    concept_text(x$value %||% x$valueCodeableConcept)
  }
  stratum_label <- function(s) {
    if (!is.null(s$component)) {
      paste(vapply(s$component, value_text, character(1)), collapse = " · ")
    } else {
      value_text(s)
    }
  }
  # Initial population count, else the first population, else the measure score
  count_of <- function(x) {
    pops <- x$population %||% list()
    ip   <- Filter(function(p) identical(p$code$coding[[1]]$code, "initial-population"), pops)
    if (length(ip) == 0) ip <- pops
    count <- if (length(ip) > 0) ip[[1]]$count else NULL
    as.numeric(count %||% x$measureScore$value %||% x$measureScoreQuantity$value %||%
                 x$measureScoreDecimal %||% NA_real_)
  }
  to_df <- function(category, count) {
    category[is.na(category) | category == ""] <- "unknown"
    df <- data.frame(Category = category, Count = count, stringsAsFactors = FALSE)
    df[!is.na(df$Count), ]
  }

  groups   <- mr$group %||% list()
  g_labels <- vapply(seq_along(groups), function(i) {
    lbl <- concept_text(groups[[i]]$code)
    if (is.na(lbl)) paste("Group", i) else lbl
  }, character(1))

  dists <- list()
  for (gi in seq_along(groups)) {
    strats <- groups[[gi]]$stratifier %||% list()
    for (si in seq_along(strats)) {
      s      <- strats[[si]]
      strata <- s$stratum %||% list()
      if (length(strata) == 0) next
      s_label <- concept_text(s$code)
      if (is.na(s_label) && !is.null(strata[[1]]$component)) {
        s_label <- paste(vapply(strata[[1]]$component, function(c) concept_text(c$code), character(1)),
                         collapse = " × ")
      }
      if (is.na(s_label)) s_label <- paste("Stratifier", si)
      label <- if (length(groups) > 1) paste0(g_labels[gi], ": ", s_label) else s_label
      dists[[label]] <- to_df(vapply(strata, stratum_label, character(1)),
                              vapply(strata, count_of, numeric(1)))
    }
  }

  # Groups without strata, e.g. one group per data element
  plain <- vapply(groups, function(g) length(g$stratifier %||% list()) == 0, logical(1))
  if (sum(plain) > 1) {
    dists[["Groups"]] <- to_df(g_labels[plain], vapply(groups[plain], count_of, numeric(1)))
  }

  if (length(dists) == 0) return(list())
  names(dists) <- make.unique(names(dists), sep = " ")
  Filter(function(d) nrow(d) > 0, dists)
}

# Get all unique categories across all datasets for consistent x-axis
getAllCategories <- function(data_list) {
  all_cats <- unique(unlist(lapply(data_list, function(x) x$data$Category)))
  # Sort categories: put "unknown" last, others alphabetically
  known_cats <- sort(all_cats[all_cats != "unknown"])
  if ("unknown" %in% all_cats) {
    c(known_cats, "unknown")
  } else {
    known_cats
  }
}

# 3. UI definition
ui <- fluidPage(
  theme = shinytheme("spacelab"),

  # Custom CSS & JS
  tags$head(
    tags$style(HTML("
    .plot_box {
      min-width: 300px;
      padding: 15px;
      border: 1px solid #B0B0B0;
      border-radius: 8px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      background-color: transparent;
      position: absolute;
    }

    #plot_area {
      position: relative;
      height: 800px;
      border: 1px solid #DDD;
      overflow: auto;
      padding: 10px;
    }
  ")),
    tags$script(HTML("
      // Function to arrange plots side by side with dynamic width
      function arrangeSideBySide() {
        var plots = $('.plot_box');
        var containerWidth = $('#plot_area').width() - 20;
        var plotHeight = 350;

        plots.each(function(index) {
          var $plot = $(this);

          // Get number of categories from the plot's filter dropdown
          var plotIndex = $plot.find('select[id^=\"filter_\"]').attr('id');
          if (plotIndex) {
            var filterSelect = $('#' + plotIndex);
            var categoryCount = filterSelect.find('option').length;

            // Calculate width: base 250px + 40px per category, max 800px
            var plotWidth = Math.min(800, Math.max(250, 250 + (categoryCount * 40)));
            $plot.css('width', plotWidth + 'px');
          } else {
            // Fallback to default width
            var plotWidth = 300;
            $plot.css('width', plotWidth + 'px');
          }

          // Calculate position
          var currentRowWidth = 0;
          var currentRow = 0;
          var plotsInCurrentRow = [];

          // Group plots by rows
          plots.slice(0, index + 1).each(function(i) {
            var thisWidth = parseInt($(this).css('width')) + 20; // Add margin
            if (currentRowWidth + thisWidth > containerWidth && plotsInCurrentRow.length > 0) {
              currentRow++;
              currentRowWidth = thisWidth;
              plotsInCurrentRow = [i];
            } else {
              currentRowWidth += thisWidth;
              plotsInCurrentRow.push(i);
            }
          });

          // Position this plot
          var leftOffset = 0;
          for (var i = 0; i < plotsInCurrentRow.length; i++) {
            if (plotsInCurrentRow[i] === index) break;
            leftOffset += parseInt(plots.eq(plotsInCurrentRow[i]).css('width')) + 20;
          }

          $plot.css({
            top: (currentRow * plotHeight) + 'px',
            left: leftOffset + 'px'
          });
        });
      }

    // Initialize side-by-side layout when plots are added
    $(document).on('DOMNodeInserted', '#plot_area', function() {
      setTimeout(arrangeSideBySide, 100);
    });

    // Re-arrange on window resize
    $(window).resize(function() {
      setTimeout(arrangeSideBySide, 100);
    });

    // Stack all plots
    $(document).on('click','#stackPlots',function(){
      $('.plot_box').css({top:'0px',left:'0px'});
    });

    // Arrange plots side by side
    $(document).on('click','#arrangeSideBySide',function(){
      arrangeSideBySide();
    });

    // Stack selected plots
    $(document).on('click','#stackSelectedPlots',function(){
      var sel = $('#selectedPlotsToStack').val()||[];
      sel.slice(0,2).forEach(function(name){
        $('.plot_box[data-plot-name=\"'+name+'\"]')
          .css({top:'0px',left:'0px'});
      });
    });

    // Initialize side-by-side layout on page load
    $(document).ready(function() {
      setTimeout(arrangeSideBySide, 500);
    });
  "))
  ),

  navbarPage("Medical Data Dashboard",

             # -- Data Upload Tab --
             tabPanel("Data Upload",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Upload Files"),
                          fileInput("newFiles", "Add Files",
                                    accept = c(".csv", ".json"), multiple = TRUE),
                          hr(),
                          uiOutput("fileListUI"),
                          actionButton("removeSelected", "Remove Selected",
                                       class = "btn btn-danger",
                                       style = "margin-top: 10px; width: 100%;")
                        ),
                        mainPanel(
                          h4("Uploaded Datasets"),
                          span("Upload your files and assign each a type. Files will be used in the corresponding tabs.",
                               "\"CSV/JSON\" covers value distributions: CSV files and FHIR MeasureReports such as DQ-SRs;",
                               "\"Census\" is for age × gender MeasureReports."),
                          hr(),
                          tableOutput("dataList"),
                          uiOutput("mappingUI")
                        )
                      )
             ),
             # -- Census Data Tab --
             tabPanel("Census Data",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Data Selection"),
                          uiOutput("censusFileSelector"),
                          h4("Visualization Options"),
                          selectInput("census_chart_type", "Chart Type:",
                                      choices = c("Grouped Bar Chart" = "grouped",
                                                  "Stacked Bar Chart" = "stacked"),
                                      selected = "grouped"),
                          checkboxInput("census_show_values", "Show Values on Bars", FALSE),
                          hr(),
                          downloadButton("downloadCensusData", "Download Census Data (JSON)"),
                          br(), br(),
                          downloadButton("downloadCensusPlot", "Download Plot (PNG)"),
                        ),
                        mainPanel(
                          h4("Census Population by Age Group and Gender"),
                          plotOutput("censusPlot", height = "600px"),
                          hr(),
                          h4("Census Data Summary"),
                          tableOutput("censusSummaryTable"),
                          hr(),
                          h4("Raw Census Data"),
                          DT::dataTableOutput("censusDataTable")
                        )
                      )
             ),
             # -- FHIR in bins Tab --
             tabPanel("FHIR in bins",
                sidebarLayout(
                  sidebarPanel(
                    h4("Select FHIR Files"),
                    uiOutput("fhirFileSelectorBinning"),
                    hr(),
                    h4("Category Selection"),
                    uiOutput("fhirResourceTypeUIBinning"),
                    hr(),
                    uiOutput("fhirMappingUIBinning"),
                    conditionalPanel(
                      condition = "input.fhir_category_col_binning != null && input.fhir_category_col_binning != ''",
                      h4("Create the bins"),
                      radioButtons(
                        "value_types", "What is the type of the values",
                        c("Numeric" = "num", "Boolean" = "bool", "Text" = "text"), "text"),
                      conditionalPanel(
                        condition = "input.value_types != null && input.value_types != 'bool'",
                        sliderInput("fhir_n_bins", "Number of bins:",
                                    min = 1, max = 50, value = 5, step = 1)
                      ),
                      hr(),
                      radioButtons("bins_display_mode", "Display as:",
                                   choices = c("Percentages" = "percent",
                                               "Absolute Counts" = "absolute"),
                                   selected = "percent")
                    ),
                    uiOutput("fhirValuesUIBinning")
                  ),
                  mainPanel(
                    h4("Visualisation of the fhir data in bins"),
                    plotOutput("plotBins", height = "400px"),
                    br(),
                    actionButton("downloadBinsReport", "Download Report",
                                 class = "btn btn-primary",
                                 icon = icon("download"))
                  )
                ),
             ),
             # -- Compare Tab --
             tabPanel("Compare",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Data Sources"),
                          uiOutput("vizSourceSelector"),
                          hr(),
                          h4("Aggregate Sources"),
                          p("Sum the counts of several sources into one; the aggregate is added to the sources above.",
                            style = "color:#666; font-size:12px;"),
                          textInput("viz_group_name", "Name:", placeholder = "e.g. All sites"),
                          uiOutput("vizGroupMemberSelector"),
                          actionButton("viz_add_group", "Add aggregate", icon = icon("layer-group"),
                                       class = "btn-sm"),
                          uiOutput("vizGroupList"),
                          hr(),
                          h4("Binning"),
                          radioButtons("viz_bin_type", "Use bins from:",
                                       choices = c("Census (Age × Gender)"   = "census",
                                                   "Categories of a report" = "report",
                                                   "FHIR in Bins"           = "fhir_bins"),
                                       selected = "census"),
                          conditionalPanel(
                            condition = "input.viz_bin_type == 'census'",
                            checkboxInput("viz_combine_gender", "Combine male and female", FALSE)
                          ),
                          conditionalPanel(
                            condition = "input.viz_bin_type == 'report'",
                            uiOutput("vizBinReportSelector"),
                            p("Categories of the other sources are matched by name or by numeric range.",
                              "FHIR bundles use the attribute chosen in the \"FHIR in bins\" tab.",
                              style = "color:#666; font-size:12px;")
                          ),
                          hr(),
                          h4("Display"),
                          radioButtons("viz_display_mode", "Display as:",
                                       choices = c("Percentages"     = "percent",
                                                  "Absolute Counts" = "absolute"),
                                       selected = "percent"),
                          radioButtons("x_axis_display_mode", "X-Axis mode:",
                                       choices = c("As selected bin source"     = "uniform",
                                                   "Minimal" = "individual"),
                                       selected = "uniform"),
                          radioButtons("viz_layout_mode", "Layout:",
                                       choices = c("Individual plots" = "individual",
                                                   "Overlay"          = "overlay"),
                                       selected = "individual"),
                          conditionalPanel(
                            condition = "input.viz_layout_mode == 'overlay'",
                            radioButtons("viz_overlay_style", "Overlay style:",
                                         choices = c("Lines"                   = "lines",
                                                     "Difference to reference" = "diff",
                                                     "Grouped bars"            = "dodge",
                                                     "Transparent bars"        = "bars"),
                                         selected = "lines"),
                            conditionalPanel(
                              condition = "input.viz_overlay_style == 'diff'",
                              uiOutput("vizReferenceSelector")
                            ),
                            conditionalPanel(
                              condition = "input.viz_overlay_style == 'bars'",
                              sliderInput("viz_overlay_alpha", "Transparency:",
                                          min = 0.1, max = 1, value = 0.5, step = 0.05)
                            )
                          ),
                          hr(),
                          h4("Download"),
                          p("Counts and percentages of the shown sources, including aggregates, in the selected bins.",
                            style = "color:#666; font-size:12px;"),
                          downloadButton("vizDownloadCsv",  "CSV",  class = "btn-sm"),
                          downloadButton("vizDownloadJson", "JSON", class = "btn-sm"),
                          downloadButton("vizDownloadMeasureReport", "MeasureReport", class = "btn-sm"),
                          p("The MeasureReport holds one group per shown source, stratified by the bins.",
                            style = "color:#666; font-size:12px; margin-top:6px;")
                        ),
                        mainPanel(
                          uiOutput("vizInfoBox"),
                          uiOutput("vizPlotsUI")
                        )
                      )
             ),

             # -- Combined Data Tab --
             tabPanel("Combined Data",
                      fluidRow(
                        column(8,
                               h4("Combined Data Plot"),
                               plotOutput("combinedPlot"),
                               hr(),
                               h4("Intersection Plot"),
                               plotOutput("intersectionPlot")
                        ),
                        column(4,
                               div(style = "padding:15px; border:1px solid #DDD; border-radius:8px; background-color:#FFF;",
                                   h4("Combine Data"),
                                   checkboxGroupInput("combineFiles",
                                                      "Select Files to Combine:", choices = NULL),
                                   uiOutput("valueSelectors"),
                                   actionButton("combineData", "Combine Data"),
                                   downloadButton("downloadCombined", "Download Combined Data (JSON)"),
                                   br(), br(),
                                   h4("Intersection Settings"),
                                   p("Only categories present in ALL selected files will be kept."),
                                   selectInput("intersectionValues",
                                               "Common Categories:", choices = NULL, multiple = TRUE),
                                   actionButton("combineIntersection", "Combine Intersection Data"),
                                   downloadButton("downloadIntersection", "Download Intersection Data (JSON)")
                               )
                        )
                      )
             ),

             # -- Statistics Tab --
             tabPanel("Statistics",
                      fluidRow(
                        column(12,
                               h4("Dataset Statistics"),
                               tableOutput("statTable"),
                               hr(),
                               h4("Category Summary"),
                               tags$ul(
                                 tags$li(strong("Green:"), " present in ALL files"),
                                 tags$li(strong("Yellow:"), " present in ≥2 files"),
                                 tags$li(strong("Red:"), " present in only 1 file")
                               ),
                               uiOutput("categorySummary")
                        )
                      )
             )

  ) # navbarPage
) # fluidPage  # 4. Server logic
server <- function(input, output, session) {

  # 4.0 Manage file list
  uploadedFiles <- reactiveVal(list())
  lastCensusPlot <- reactiveVal(NULL)

  # 4.1 Load JSON data
  # A MeasureReport yields the distribution chosen in the upload tab (default: first)
  loadJsonData <- function(path, filename = basename(path)) {
    tryCatch({
      raw <- fromJSON(path, simplifyVector = FALSE)
      if (identical(raw$resourceType, "MeasureReport")) {
        dists  <- readMeasureReportDistributions(raw)
        if (length(dists) == 0) {
          warning(paste("MeasureReport", filename, "holds no stratifier or group counts"))
          return(data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE))
        }
        choice <- input[[paste0("map_mr_", make_safe_id(filename))]]
        return(dists[[if (!is.null(choice) && choice %in% names(dists)) choice else 1]])
      }

      jd <- fromJSON(path)

      # Check if the expected structure exists
      if (!is.null(jd$Histogram$Category$`@value`) &&
          !is.null(jd$Histogram$Count$`@value`)) {

        data.frame(
          Category = jd$Histogram$Category$`@value`,
          Count    = as.numeric(jd$Histogram$Count$`@value`),
          stringsAsFactors = FALSE
        )
      } else {
        # JSON doesn't have expected structure
        warning(paste("JSON file", path, "doesn't have expected Histogram structure"))
        data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      # If JSON parsing fails or structure is wrong
      warning(paste("Error loading JSON file:", e$message))
      data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE)
    })
  }

  # 4.2 Load CSV data with optional column mapping
  # The mapping input is keyed by file name, so it survives reordering of the file list
  loadCsvData <- function(path, filename) {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

    # If the CSV already has Category and Count columns, use them directly
    if (all(c("Category", "Count") %in% colnames(df))) {
      df <- data.frame(Category = trimws(as.character(df$Category)),
                       Count    = suppressWarnings(as.numeric(df$Count)),
                       stringsAsFactors = FALSE)
      return(df[!is.na(df$Count), ])
    }

    # Otherwise, we need column mapping
    category_col <- input[[paste0("map_cat_", make_safe_id(filename))]]

    # If no category column is selected yet, return empty data frame
    if (is.null(category_col) || category_col == "") {
      return(data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE))
    }

    # Check if the selected column exists in the current data frame
    if (!category_col %in% colnames(df)) {
      return(data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE))
    }

    # With a count column the CSV already holds a distribution: sum it per
    # category; without one, every row is a single record
    count_col <- input[[paste0("map_cnt_", make_safe_id(filename))]]
    weights   <- if (!is.null(count_col) && count_col %in% colnames(df)) {
      suppressWarnings(as.numeric(df[[count_col]]))
    } else {
      rep(1, nrow(df))
    }

    tryCatch({
      category <- trimws(as.character(df[[category_col]]))
      category[is.na(category) | category == ""] <- "unknown"
      result <- data.frame(Category = category, w = weights, stringsAsFactors = FALSE) %>%
        filter(!is.na(w)) %>%
        group_by(Category) %>%
        summarise(Count = sum(w), .groups = "drop") %>%
        as.data.frame(stringsAsFactors = FALSE)

      result[result$Count > 0, ]
    }, error = function(e) {
      # If there's any error, return empty data frame
      data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE)
    })
  }

  # 4.2.1 Load a "CSV/JSON" file as a Category/Count distribution (NULL on failure)
  loadCsvJsonFile <- function(f) {
    tryCatch({
      switch(tolower(tools::file_ext(f$name)),
             "json" = loadJsonData(f$path, f$name),
             "csv"  = loadCsvData(f$path, f$name),
             NULL
      )
    }, error = function(e) {
      warning(paste("Error processing file", f$name, ":", e$message))
      NULL
    })
  }

  # 4.3 load FHIR files

  loadFhirFile <- function(path, filename) {
    tryCatch({
      fhir_data <- fromJSON(path, simplifyVector = FALSE)

      # Helper function to safely flatten any FHIR resource
      flatten_fhir_resource <- function(resource, prefix = "") {
        result <- list()

        if (is.list(resource)) {
          for (name in names(resource)) {
            value <- resource[[name]]
            current_key <- if (prefix == "") name else paste(prefix, name, sep = ".")

            if (is.null(value)) {
              result[[current_key]] <- NA_character_
            } else if (is.list(value) && !is.null(names(value))) {
              # Named list - recurse
              nested_result <- flatten_fhir_resource(value, current_key)
              result <- c(result, nested_result)
            } else if (is.list(value)) {
              # Unnamed list (array) - convert to delimited string
              if (length(value) > 0) {
                array_strings <- sapply(value, function(item) {
                  if (is.list(item)) {
                    if (!is.null(item$value)) {
                      return(as.character(item$value))
                    } else if (!is.null(item$display)) {
                      return(as.character(item$display))
                    } else if (!is.null(item$code)) {
                      return(as.character(item$code))
                    } else if (!is.null(item$system)) {
                      return(paste0(item$system, ":", item$code %||% ""))
                    } else {
                      non_null_values <- item[!sapply(item, is.null)]
                      if (length(non_null_values) > 0) {
                        key_value_pairs <- paste(names(non_null_values),
                                                 sapply(non_null_values, as.character),
                                                 sep = ":", collapse = ",")
                        return(paste0("{", key_value_pairs, "}"))
                      } else {
                        return("")
                      }
                    }
                  } else {
                    return(as.character(item))
                  }
                })
                result[[current_key]] <- paste(array_strings[array_strings != ""], collapse = "; ")
              } else {
                result[[current_key]] <- NA_character_
              }
            } else if (length(value) > 1) {
              result[[current_key]] <- paste(as.character(value), collapse = "; ")
            } else {
              result[[current_key]] <- as.character(value)
            }
          }
        } else {
          key <- if (prefix == "") "value" else prefix
          result[[key]] <- as.character(resource)
        }

        return(result)
      }

      # Handle both single resources and bundles
      if (is.list(fhir_data) && !is.null(fhir_data$resourceType)) {
        if (fhir_data$resourceType == "Bundle" && !is.null(fhir_data$entry)) {
          # Extract resources and group by type
          resources_by_type <- list()

          for (i in seq_along(fhir_data$entry)) {
            entry <- fhir_data$entry[[i]]
            if (is.list(entry) && !is.null(entry$resource) &&
                is.list(entry$resource) && !is.null(entry$resource$resourceType)) {

              resource <- entry$resource
              resource_type <- tolower(as.character(resource$resourceType))

              # Initialize list for this resource type if needed
              if (is.null(resources_by_type[[resource_type]])) {
                resources_by_type[[resource_type]] <- list()
              }

              # Add resource to appropriate type group
              resources_by_type[[resource_type]][[length(resources_by_type[[resource_type]]) + 1]] <- resource
            }
          }

          # Create separate data frame for each resource type
          result_list <- list()

          for (resource_type in names(resources_by_type)) {
            resources <- resources_by_type[[resource_type]]
            df_list <- list()

            for (i in seq_along(resources)) {
              resource <- resources[[i]]

              # Flatten the resource
              flattened <- flatten_fhir_resource(resource)

              # Add resource type prefix to all column names
              if (length(flattened) > 0) {
                prefixed_flattened <- list()
                for (col_name in names(flattened)) {
                  prefixed_name <- paste(resource_type, col_name, sep = ".")
                  prefixed_flattened[[prefixed_name]] <- flattened[[col_name]]
                }
                flattened <- prefixed_flattened
              }

              # Create data frame for this resource
              if (length(flattened) > 0) {
                flattened <- lapply(flattened, function(x) {
                  if (is.null(x) || length(x) == 0) {
                    return(NA_character_)
                  } else {
                    return(as.character(x))
                  }
                })

                df_list[[i]] <- data.frame(flattened, stringsAsFactors = FALSE, check.names = FALSE)
              }
            }

            # Combine all resources of this type
            if (length(df_list) > 0) {
              df_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, df_list)

              if (length(df_list) > 0) {
                # Standardize columns
                all_cols <- unique(unlist(lapply(df_list, names)))
                df_list <- lapply(df_list, function(df) {
                  missing_cols <- setdiff(all_cols, names(df))
                  for (col in missing_cols) {
                    df[[col]] <- NA_character_
                  }
                  return(df[, all_cols, drop = FALSE])
                })

                result_df <- do.call(rbind, df_list)
                rownames(result_df) <- NULL

                # Store with resource type key
                result_list[[paste0(filename, "_", resource_type)]] <- result_df
              }
            }
          }

          return(result_list)
        }
      }

      # Return empty list if no valid data found
      return(list())

    }, error = function(e) {
      warning(paste("Error loading FHIR file", filename, ":", e$message))
      return(list())
    })
  }

  # Helper function for null coalescing
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }

  # 4.3.1 Dynamic UI: mapping CSV columns
  output$mappingUI <- renderUI({
    csv_files <- Filter(function(f) f$type == "csv_json" && tolower(tools::file_ext(f$name)) == "csv",
                        uploadedFiles())
    uiList <- lapply(csv_files, function(f) {
      df0 <- tryCatch(read.csv(f$path, stringsAsFactors = FALSE, check.names = FALSE, nrows = 1),
                      error = function(e) NULL)
      # Files that already carry Category and Count columns need no mapping
      if (!is.null(df0) && !all(c("Category", "Count") %in% colnames(df0))) {
        # Sort column names - group by resource type prefix, then alphabetically
        col_names <- colnames(df0)

        # Separate columns with resource type prefixes from those without
        prefixed_cols <- col_names[grepl("\\.", col_names)]
        non_prefixed_cols <- col_names[!grepl("\\.", col_names)]

        if (length(prefixed_cols) > 0) {
          # Group prefixed columns by resource type
          resource_groups <- split(prefixed_cols, sapply(prefixed_cols, function(x) {
            strsplit(x, "\\.")[[1]][1]
          }))

          # Sort resource types alphabetically, then sort columns within each group
          sorted_prefixed <- unlist(lapply(sort(names(resource_groups)), function(res_type) {
            cols <- resource_groups[[res_type]]
            # Put basic fields first (resourceType, id, meta.*), then sort the rest
            basic_pattern <- paste0("^", res_type, "\\.(resourceType|id|meta\\.)")
            basic_cols <- cols[grepl(basic_pattern, cols)]
            other_cols <- cols[!grepl(basic_pattern, cols)]
            c(sort(basic_cols), sort(other_cols))
          }))

          # Combine: non-prefixed first (sorted), then prefixed (grouped and sorted)
          sorted_cols <- c(sort(non_prefixed_cols), sorted_prefixed)
        } else {
          # No prefixed columns, just sort normally
          sorted_cols <- sort(col_names)
        }

        safe_name <- make_safe_id(f$name)
        cat_id    <- paste0("map_cat_", safe_name)
        cnt_id    <- paste0("map_cnt_", safe_name)
        # Keep the user's choice when the file list re-renders
        cat_sel   <- isolate(input[[cat_id]]) %||% sorted_cols[1]
        cnt_sel   <- isolate(input[[cnt_id]]) %||% ""

        div(style = "padding:8px; border:1px solid #DDD; border-radius:4px; margin-bottom:8px;",
            h5(strong(f$name)),
            selectInput(cat_id, "Category column:", choices = sorted_cols, selected = cat_sel),
            selectInput(cnt_id, "Count column:",
                        choices  = c("None (count rows)" = "", sorted_cols),
                        selected = cnt_sel)
        )
      }
    })
    uiList <- Filter(Negate(is.null), uiList)

    # MeasureReports with several stratifiers: choose the distribution to use
    json_files <- Filter(function(f) f$type == "csv_json" && tolower(tools::file_ext(f$name)) == "json",
                         uploadedFiles())
    mrList <- lapply(json_files, function(f) {
      raw <- tryCatch(fromJSON(f$path, simplifyVector = FALSE), error = function(e) NULL)
      if (!identical(raw$resourceType, "MeasureReport")) return(NULL)
      dists <- names(readMeasureReportDistributions(raw))
      if (length(dists) < 2) return(NULL)
      mr_id <- paste0("map_mr_", make_safe_id(f$name))
      div(style = "padding:8px; border:1px solid #DDD; border-radius:4px; margin-bottom:8px;",
          h5(strong(f$name)),
          selectInput(mr_id, "Distribution:", choices = dists,
                      selected = isolate(input[[mr_id]]) %||% dists[1]))
    })
    mrList <- Filter(Negate(is.null), mrList)

    if (length(uiList) == 0 && length(mrList) == 0) return(NULL)
    tagList(
      if (length(uiList) > 0) tagList(
        h4("CSV Column Mapping"),
        span("Choose which column holds the categories. If the CSV already contains a distribution, also pick its count column."),
        br(), br(),
        uiList
      ),
      if (length(mrList) > 0) tagList(
        h4("MeasureReport Distribution"),
        span("These reports hold several stratifiers. Choose the one whose strata are used as categories."),
        br(), br(),
        mrList
      )
    )
  })

  # 4.3.2
  output$censusFileSelector <- renderUI({
    files <- uploadedFiles()
    census_files <- Filter(function(f) f$type == "census", files)
    if (length(census_files) == 0) {
      p("No census files uploaded yet. Please upload in the Data Upload tab.",
        style = "color:#999; font-size:12px;")
    } else {
      choices <- setNames(sapply(census_files, `[[`, "path"), sapply(census_files, `[[`, "name"))
      kept    <- intersect(isolate(input$selected_census_file), choices)
      selectInput("selected_census_file", "Census File:", choices = choices,
                  selected = if (length(kept) > 0) kept[1] else choices[1])
    }
  })

  # A separate census report holds age totals (Gender NA) and gender totals
  # (Age NA) instead of age x gender counts
  is_separate_census <- function(df) any(is.na(df$Age) | is.na(df$Gender))

  # The totals of a separate census report as one table: Stratifier, Category, Count
  separate_census_totals <- function(df) {
    ages <- df[!is.na(df$Age), ]
    ages <- ages[order(as.numeric(sub("[-+].*", "", ages$Age))), ]
    rbind(data.frame(Stratifier = "Age group", Category = ages$Age, Count = ages$Count,
                     stringsAsFactors = FALSE),
          data.frame(Stratifier = "Gender", Category = df$Gender[is.na(df$Age)],
                     Count = df$Count[is.na(df$Age)], stringsAsFactors = FALSE))
  }

  loadCensusData <- function(path) {
    tryCatch({
      census_json <- fromJSON(path, simplifyVector = FALSE)

      if (is.null(census_json$group) || length(census_json$group) == 0) {
        warning("No group data found in census file")
        return(NULL)
      }

      first_group <- if (is.list(census_json$group[[1]])) {
        census_json$group[[1]]
      } else {
        census_json$group
      }

      if (is.null(first_group$stratifier) || length(first_group$stratifier) == 0) {
        warning("No stratifier found in census file")
        return(NULL)
      }

      stratifiers <- first_group$stratifier

      # ── Detect format ────────────────────────────────────────────────────────────
      # Composite: stratifier entries have $stratum[[1]]$component
      # Separate:  stratifier entries have $code with LOINC codes for age/gender
      first_stratum <- stratifiers[[1]]$stratum[[1]]
      is_composite  <- !is.null(first_stratum$component)

      if (is_composite) {
        # ── Composite format (existing logic) ──────────────────────────────────────
        stratum_list <- stratifiers[[1]]$stratum

        census_data <- lapply(stratum_list, function(stratum) {
          components <- stratum$component
          if (is.null(components) || length(components) < 2) return(NULL)

          age    <- components[[1]]$value$text %||% NA
          gender <- components[[2]]$value$text %||% NA

          count <- 0
          if (!is.null(stratum$measureScore$value)) {
            count <- as.numeric(stratum$measureScore$value)
          } else if (!is.null(stratum$population)) {
            count <- as.numeric(stratum$population[[1]]$count %||% 0)
          }

          data.frame(Age = age, Gender = gender, Count = count,
                     stringsAsFactors = FALSE)
        })

        census_df <- do.call(rbind, Filter(Negate(is.null), census_data))

      } else {
        # ── Separate format: two stratifiers, one for gender, one for age ──────────
        # Without a cross-tabulation only the totals are known: age rows carry no
        # gender and gender rows no age group (see is_separate_census())
        # Identify which stratifier is gender and which is age by LOINC code
        get_loinc <- function(strat) {
          tryCatch({
            strat$code[[1]]$coding[[1]]$code
          }, error = function(e) NA_character_)
        }

        gender_strat <- NULL
        age_strat    <- NULL

        for (s in stratifiers) {
          loinc <- get_loinc(s)
          if (!is.na(loinc) && loinc == "99502-7") gender_strat <- s  # Recorded sex or gender
          if (!is.na(loinc) && loinc == "46251-5") age_strat    <- s  # Age group
        }

        # Fallback: if LOINC codes not found, use position (first=gender, second=age)
        if (length(stratifiers) < 2) {
          warning("A separate census report needs an age and a gender stratifier")
          return(NULL)
        }
        if (is.null(gender_strat)) gender_strat <- stratifiers[[1]]
        if (is.null(age_strat))    age_strat    <- stratifiers[[2]]

        totals <- function(strat, column) {
          lapply(strat$stratum, function(s) {
            value <- as.character(s$value$text %||% NA_character_)
            data.frame(Age    = if (column == "Age")    value else NA_character_,
                       Gender = if (column == "Gender") value else NA_character_,
                       Count  = as.numeric(s$population[[1]]$count %||% 0),
                       stringsAsFactors = FALSE)
          })
        }
        census_df <- do.call(rbind, c(totals(age_strat, "Age"), totals(gender_strat, "Gender")))
      }

      if (is.null(census_df) || nrow(census_df) == 0) {
        warning("No valid census data extracted")
        return(NULL)
      }

      census_df <- if (is_composite) {
        census_df[!is.na(census_df$Age) & !is.na(census_df$Gender), ]
      } else {
        census_df[!is.na(census_df$Age) | !is.na(census_df$Gender), ]
      }
      census_df$Count <- as.numeric(census_df$Count)
      return(census_df)

    }, error = function(e) {
      warning(paste("Error loading census JSON:", e$message))
      return(NULL)
    })
  }

  # 4.4a Fetch comprehensive FHIR data using _include and _revinclude

  # Replace the fhirRawData function with this corrected version:

  fhirRawData <- reactive({
    #if (input$data_source == "fhir") {
      if (input$fhir_input_type == "api") {
        # Existing API logic - wrap in eventReactive
        req(input$load_fhir)
        req(input$fhir_url, input$max_bundles)

        showNotification("Starting FHIR data load...", type = "default", id = "fhir_load")

        all_resources <- list()

        resource_types_to_fetch <- c("Patient", "Observation", "Condition", "MedicationRequest",
                                     "Procedure", "Encounter", "AllergyIntolerance", "Immunization")

        for (resource_type in resource_types_to_fetch) {
          tryCatch({
            req_resource <- fhir_url(url = input$fhir_url, resource = resource_type)

            bundles <- fhir_search(
              request = req_resource,
              verbose = 0,
              max_bundles = input$max_bundles
            )

            if (length(bundles) > 0) {
              desc <- fhir_table_description(
                resource = resource_type,
                sep = " || ",
                brackets = character(0),
                rm_empty_cols = FALSE,
                format = "compact"
              )

              df <- fhir_crack(bundles = bundles, design = desc, verbose = 0)

              if (!is.null(df) && nrow(df) > 0) {
                all_resources[[resource_type]] <- df
              }
            }
          }, error = function(e) {
            print(paste("Error fetching", resource_type, ":", e$message))
          })
        }

        removeNotification("fhir_load")

        if (length(all_resources) > 0) {
          showNotification(paste("Loaded", length(all_resources), "resource types"), type = "default")
        } else {
          showNotification("No data could be loaded", type = "error")
        }

        return(all_resources)

      } else if (input$fhir_input_type == "file") {
        # File upload logic
        req(input$fhirFiles)

        all_files_data <- list()
        fps <- input$fhirFiles$datapath
        fns <- input$fhirFiles$name

        for (i in seq_along(fps)) {
          file_data_list <- loadFhirFile(fps[i], fns[i])  # Now returns list of resource types
          if (!is.null(file_data_list) && length(file_data_list) > 0) {
            # Merge all resource types from this file into main list
            all_files_data <- c(all_files_data, file_data_list)
          }
        }

        return(all_files_data)
      }
    #}
    return(NULL)
  })

  # 4.4b UI for selecting resource type to visualize
  output$fhirResourceTypeUI <- renderUI({
    #if (input$data_source == "fhir") {
      fhir_data <- fhirRawData()

      if (!is.null(fhir_data)) {
        available_resources <- names(fhir_data)

        if (length(available_resources) > 0) {
          if (input$fhir_input_type == "api") {
            selectInput("fhir_resource_to_viz", "Resource Type to Visualize:",
                        choices = available_resources,
                        selected = available_resources[1])
          } else {
            # For file uploads, add resource type selector
            resource_types <- unique(sapply(available_resources, function(x) {
              parts <- strsplit(x, "_")[[1]]
              if (length(parts) > 1) parts[length(parts)] else x
            }))

            tagList(
              selectInput("fhir_resource_to_viz", "Resource Type to Visualize:",
                          choices = resource_types,
                          selected = resource_types[1]),
#              tags$div(
#                h5("Available Datasets:"),
#                tags$ul(lapply(available_resources, function(x) tags$li(x)))
              #)
            )
          }
        }
      }
    #}
  })

  # 4.4c Update the mapping UI to show columns from selected resource
  output$fhirMappingUI <- renderUI({
    if (input$data_source == "fhir") {
      fhir_data <- fhirRawData()
      if (!is.null(fhir_data)) {
        if (input$fhir_input_type == "api") {
          req(input$fhir_resource_to_viz)
          df <- fhir_data[[input$fhir_resource_to_viz]]
          if (!is.null(df) && nrow(df) > 0) {
            selectInput("fhir_category_col", "Category column:",
                        choices = colnames(df),
                        selected = colnames(df)[1])
          }
        } else {
          # For file uploads, filter columns by selected resource type
          req(input$fhir_resource_to_viz)

          # Get all datasets that match the selected resource type
          selected_resource_type <- input$fhir_resource_to_viz
          matching_datasets <- names(fhir_data)[grepl(paste0("_", selected_resource_type, "$"), names(fhir_data))]

          if (length(matching_datasets) > 0) {
            # Get columns from all matching datasets
            all_columns <- unique(unlist(lapply(matching_datasets, function(dataset_name) {
              colnames(fhir_data[[dataset_name]])
            })))

            # Filter to only columns that match the selected resource type
            resource_prefix <- paste0(tolower(selected_resource_type), ".")
            resource_columns <- all_columns[grepl(paste0("^", resource_prefix), all_columns)]

            if (length(resource_columns) > 0) {
              # Remove the resource type prefix from display names
              display_names <- gsub(paste0("^", resource_prefix), "", resource_columns)

              # Sort: basic fields first, then alphabetically
              basic_fields <- c("resourceType", "id")
              meta_fields <- display_names[grepl("^meta\\.", display_names)]
              other_fields <- display_names[!display_names %in% basic_fields & !grepl("^meta\\.", display_names)]

              sorted_display_names <- c(
                intersect(basic_fields, display_names),
                sort(meta_fields),
                sort(other_fields)
              )

              # Create named vector: display names as labels, full column names as values
              choices <- setNames(resource_columns[match(sorted_display_names, display_names)], sorted_display_names)

              selectInput("fhir_category_col", "Category column:",
                          choices = choices,
                          selected = choices[1])
            }
          }
        }
      }
    }
  })

  # 4.5.1 List all uploaded files/requested data
  allDataUploads <- reactive({
    req(input$dataFiles, input$fhirFiles, input$fhirApiRequest, input$censusFiles)

    json_files <- input$dataFiles
    fhir_bundles <- input$fhirFiles
    census_files <- input$censusFiles
    fhir_api <- input$fhirApiRequest

  })

  # 4.5.2 Aggregate uploaded/FHIR datasets

  allData <- reactive({
    # The data source selector was removed from the UI; uploaded files are the default
    data_source <- input$data_source %||% "file"
    files <- uploadedFiles()
    csv_json_files <- Filter(function(f) f$type == "csv_json", files)

    if (data_source == "file") {
      if (length(csv_json_files) == 0) return(list())

      results <- lapply(csv_json_files, function(f) {
        df <- loadCsvJsonFile(f)
        if (!is.null(df) && nrow(df) > 0) list(name = f$name, data = df) else NULL
      })

      return(results[!sapply(results, is.null)])

    } else if (data_source == "fhir") {
      fhir_data <- fhirRawData()
      if (!is.null(fhir_data) && length(fhir_data) > 0) {
        results <- list()

        if (input$fhir_input_type == "file") {
          req(input$fhir_category_col)
          req(input$fhir_resource_to_viz)

          selected_resource_type <- input$fhir_resource_to_viz
          matching_datasets <- names(fhir_data)[grepl(paste0("_", selected_resource_type, "$"), names(fhir_data))]

          for (dataset_key in matching_datasets) {
            df <- fhir_data[[dataset_key]]
            category_col <- input$fhir_category_col

            if (category_col %in% colnames(df)) {
              df[[category_col]] <- ifelse(is.na(df[[category_col]]) | df[[category_col]] == "",
                                           "unknown", as.character(df[[category_col]]))

              result_df <- df %>%
                count(Category = .data[[category_col]], name = "Count") %>%
                as.data.frame(stringsAsFactors = FALSE)

              if (nrow(result_df) > 0) {
                results[[length(results) + 1]] <- list(name = dataset_key, data = result_df)
              }
            }
          }
        } else {
          if (!is.null(input$fhir_resource_to_viz) && !is.null(input$fhir_category_col)) {
            resource_key <- input$fhir_resource_to_viz
            if (resource_key %in% names(fhir_data)) {
              df <- fhir_data[[resource_key]]
              category_col <- input$fhir_category_col

              if (category_col %in% colnames(df)) {
                df[[category_col]] <- ifelse(is.na(df[[category_col]]), "unknown", df[[category_col]])

                result_df <- df %>%
                  count(Category = .data[[category_col]], name = "Count") %>%
                  as.data.frame(stringsAsFactors = FALSE)

                if (nrow(result_df) > 0) {
                  resource_name <- paste0("FHIR-", input$fhir_resource_to_viz, ":", input$fhir_url)
                  results[[length(results) + 1]] <- list(name = resource_name, data = result_df)
                }
              }
            }
          }
        }
        return(results)
      }
    }

    return(list())
  })

  # 4.6 Global maximum for shared y-axis
  globalMax <- reactive({
    req(allData())
    max(unlist(lapply(allData(), function(x) x$data$Count)),
        na.rm = TRUE)
  })

  # 4.7 Render dataset list & basic stats
  output$dataList <- renderTable({
    do.call(rbind, lapply(allData(), function(x)
      data.frame(Dataset = x$name,
                 Rows    = nrow(x$data),
                 stringsAsFactors = FALSE)))
  })
  output$statTable <- renderTable({
    do.call(rbind, lapply(allData(), function(x)
      data.frame(Dataset = x$name,
                 Count   = nrow(x$data),
                 Mean    = mean(x$data$Count, na.rm = TRUE),
                 stringsAsFactors = FALSE)))
  })

  # 4.8 Update UI choices for combine/stack tabs
  observe({
    req(allData())
    names <- sapply(allData(), `[[`, "name")
    updateCheckboxGroupInput(session, "combineFiles",
                             choices = names, selected = names)
    updateSelectInput(session, "selectedPlotsToStack",
                      choices = names)
  })

  # 4.9 Dynamic selectors for each chosen file
  observe({
    req(allData())
    dl <- allData()

    # Make sure we have data before proceeding
    if (length(dl) == 0) return(NULL)

    # Use local() to create a closure for each iteration
    for (i in seq_along(dl)) {
      local({
        idx <- i
        f <- dl[[idx]]

        # Make sure the data frame exists and has data
        if (is.null(f$data) || nrow(f$data) == 0) return(NULL)

        ui_name   <- paste0("plotUI_", idx)
        plot_name <- paste0("plot_", idx)

        output[[ui_name]] <- renderUI({
          enabled <- input[[paste0("cb_", idx)]]
          if (isTRUE(enabled)) {
            plotOutput(plot_name, height = "300px")
          }
        })

        output[[plot_name]] <- renderPlot({
          enabled <- input[[paste0("cb_", idx)]]
          chart     <- input[[paste0("pt_", idx)]]
          filterCat <- input[[paste0("filter_", idx)]]
          alpha     <- input[[paste0("op_", idx)]]
          data0     <- f$data  # Now this is captured in the local scope

          # Get all categories across all datasets for consistent x-axis
          all_categories <- getAllCategories(dl)

          # Ensure data0 has all categories (add missing ones with Count = 0)
          missing_cats <- setdiff(all_categories, data0$Category)
          if (length(missing_cats) > 0) {
            missing_data <- data.frame(
              Category = missing_cats,
              Count = 0,
              stringsAsFactors = FALSE
            )
            data0 <- rbind(data0, missing_data)
          }

          # Apply filter if selected
          df0 <- if (!is.null(filterCat) && length(filterCat) > 0) {
            data0[data0$Category %in% filterCat, ]
          } else {
            data0
          }

          # Ensure categories are in consistent order
          df0$Category <- factor(df0$Category, levels = all_categories)

          p_base <- ggplot(df0, aes(x = Category, y = Count, fill = Category)) +
            theme_minimal(base_size = 14) +
            scale_y_continuous(limits = c(0, globalMax())) +
            scale_x_discrete(drop = FALSE)  # Show all categories even if Count = 0

          p <- switch(chart,
                      "Histogram" = p_base + geom_bar(stat = "identity", alpha = alpha),
                      "Pie Chart" = ggplot(df0, aes(x = "", y = Count, fill = Category)) +
                        geom_bar(stat = "identity", alpha = alpha, width = 1) +
                        coord_polar("y", start = 0),
                      "Line Chart" = ggplot(df0, aes(x = Category, y = Count, group = 1)) +
                        geom_line(size = 1.2, alpha = alpha) +
                        geom_point(size = 3, alpha = alpha) +
                        scale_x_discrete(drop = FALSE)
          )

          p + labs(title = f$name, x = "Category", y = "Count") +
            theme(panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background  = element_rect(fill = "transparent", colour = NA),
                  panel.grid       = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate labels if needed
                  legend.position = "none"
            )
        }, bg = "transparent")
      })
    }
  })

  # 4.10 Combine data across files (robust)
  # Category pickers for every file chosen in "Select Files to Combine"
  output$valueSelectors <- renderUI({
    req(input$combineFiles)
    dl <- allData()
    tagList(lapply(Filter(function(d) d$name %in% input$combineFiles, dl), function(d) {
      id   <- paste0("values_", make_safe_id(d$name))
      cats <- unique(d$data$Category)
      selectizeInput(id, paste("Categories of", d$name),
                     choices  = cats,
                     selected = intersect(isolate(input[[id]]) %||% cats, cats),
                     multiple = TRUE)
    }))
  })

  combinedData <- reactiveVal(NULL)
  observeEvent(input$combineData, {
    req(input$combineFiles)
    dl <- allData()

    cmb <- do.call(rbind, lapply(input$combineFiles, function(fn) {
      idx     <- which(sapply(dl, `[[`, "name") == fn)
      df0     <- dl[[idx]]$data
      safe_fn <- make_safe_id(fn)
      sel     <- input[[paste0("values_", safe_fn)]]
      if (is.null(sel)) return(NULL)
      df1 <- df0[df0$Category %in% sel, , drop = FALSE]
      df1$Source <- fn
      df1$Count  <- as.numeric(df1$Count)
      df1        <- df1[!is.na(df1$Count), ]
      if (nrow(df1) == 0) return(NULL)
      df1
    }))

    if (is.null(cmb) || nrow(cmb) == 0) {
      showNotification("No valid data selected for combination.", type = "error")
      return(NULL)
    }

    combinedData(cmb)
  })

  output$combinedPlot <- renderPlot({
    req(combinedData())
    ggplot(combinedData(), aes(x = Category, y = Count, fill = Source)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(limits = c(0, globalMax())) +
      theme_minimal(base_size = 14) +
      labs(title = "Combined Data", x = "Category", y = "Count")
  })

  output$downloadCombined <- downloadHandler(
    filename = function() paste0("combined_data_", Sys.Date(), ".json"),
    content  = function(file) jsonlite::write_json(combinedData(), file)
  )

  # 4.11 Intersection across all selected files
  intersectionData <- reactiveVal(NULL)
  observe({
    req(input$combineFiles)
    dl     <- allData()[sapply(allData(), `[[`, "name") %in% input$combineFiles]
    common <- Reduce(intersect, lapply(dl, function(x) x$data$Category))
    updateSelectInput(session, "intersectionValues",
                      choices = common, selected = common)
  })

  observeEvent(input$combineIntersection, {
    req(input$combineFiles)
    cats <- input$intersectionValues
    if (is.null(cats) || length(cats) == 0) {
      showNotification("No categories selected for intersection.", type = "error")
      return(NULL)
    }

    dl <- allData()
    inter <- do.call(rbind, lapply(input$combineFiles, function(fn) {
      idx  <- which(sapply(dl, `[[`, "name") == fn)
      df0  <- dl[[idx]]$data
      df1  <- df0[df0$Category %in% cats, , drop = FALSE]
      df1$Source <- fn
      df1$Count  <- as.numeric(df1$Count)
      df1        <- df1[!is.na(df1$Count), ]
      if (nrow(df1) == 0) return(NULL)
      df1
    }))

    if (is.null(inter) || nrow(inter) == 0) {
      showNotification("No intersection data found.", type = "error")
      return(NULL)
    }

    intersectionData(inter)
  })

  output$intersectionPlot <- renderPlot({
    req(intersectionData())
    ggplot(intersectionData(), aes(x = Category, y = Count, fill = Source)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(limits = c(0, globalMax())) +
      theme_minimal(base_size = 14) +
      labs(title = "Intersection Data", x = "Category", y = "Count")
  })

  output$downloadIntersection <- downloadHandler(
    filename = function() paste0("intersection_data_", Sys.Date(), ".json"),
    content  = function(file) jsonlite::write_json(intersectionData(), file)
  )

  # 4.12 Map of German integration centers
  #output$map <- renderLeaflet({
  #  centers <- data.frame(
  #    name = c("Greifswald","Dresden","Leipzig","Aachen","Hannover","Hamburg","Berlin"),
  #    lat  = c(54.093,51.050,51.339,50.775,52.374,53.550,52.520),
  #    lng  = c(13.387,13.738,12.374,6.083,9.738,9.993,13.405),
  #    stringsAsFactors = FALSE
  #  )
  #  germany <- geodata::gadm("Germany", level = 0, path = tempdir())
  #  leaflet() %>%
  #    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  #    addPolygons(data = germany, color = "#333333", weight = 1, fill = FALSE) %>%
  #    setView(lng = 10.5, lat = 51.0, zoom = 6) %>%
  #    addCircleMarkers(data = centers, lat = ~lat, lng = ~lng,
  #                     label = ~name, radius = 6, fill = TRUE, fillOpacity = 0.9)
  #})


  #### Census data reactive
  # The selected census report; it also provides the census bins in the Compare tab
  censusData <- reactive({
    req(input$selected_census_file)
    f <- Find(function(f) f$path == input$selected_census_file, uploadedFiles())
    req(f)
    census_df <- loadCensusData(f$path)
    if (is.null(census_df)) {
      showNotification(paste("Failed to load census data from", f$name), type = "error")
      return(NULL)
    }
    census_df$Source <- tools::file_path_sans_ext(f$name)
    showNotification(paste("Loaded", nrow(census_df), "census records"), type = "message")
    census_df
  })

  output$censusPlot <- renderPlot({
    req(censusData())

    census_df  <- censusData()

    chart_type  <- input$census_chart_type
    show_values <- input$census_show_values

    # A separate report has no age x gender counts: one panel per stratifier
    if (is_separate_census(census_df)) {
      plot_df <- separate_census_totals(census_df) %>%
        group_by(Stratifier) %>%
        mutate(Percent = round(Count / sum(Count, na.rm = TRUE) * 100, 2)) %>%
        ungroup()
      plot_df$Category <- factor(plot_df$Category, levels = unique(plot_df$Category))

      p <- ggplot(plot_df, aes(x = Category, y = Percent, fill = Stratifier)) +
        geom_bar(stat = "identity", colour = "white", linewidth = 0.2) +
        facet_grid(~Stratifier, scales = "free_x", space = "free_x") +
        theme_minimal(base_size = 14) +
        labs(
          title    = "Population by Age Group and by Gender",
          subtitle = paste(census_df$Source[1],
                           "\u00b7 separate report: age and gender totals, no age \u00d7 gender counts"),
          x        = NULL,
          y        = "Population (%)"
        ) +
        theme(
          axis.text.x     = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.title      = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle   = element_text(hjust = 0.5)
        ) +
        scale_fill_brewer(palette = "Set2")
      if (show_values) {
        p <- p + geom_text(aes(label = Count), vjust = -0.4, size = 2.8)
      }
      lastCensusPlot(p)
      return(p)
    }

    plot_df <- census_df[, c("Age", "Gender", "Count")]
    plot_df$Percent <- round(plot_df$Count / sum(plot_df$Count, na.rm = TRUE) * 100, 2)

    age_order <- unique(plot_df$Age[order(as.numeric(sub("[-+].*", "", plot_df$Age)))])
    plot_df$Age <- factor(plot_df$Age, levels = age_order)

    p <- ggplot(plot_df, aes(x = Age, y = Percent, fill = Gender)) +
      theme_minimal(base_size = 14) +
      labs(
        title    = "Population by Age Group and Gender",
        subtitle = census_df$Source[1],
        x        = "Age Group",
        y        = "Population (%)",
        fill     = "Gender"
      ) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title      = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle   = element_text(hjust = 0.5)
      ) +
      scale_fill_brewer(palette = "Set2")

    # ── Geoms based on chart type ────────────────────────────────────────────────
    if (chart_type == "stacked") {
      p <- p + geom_bar(stat = "identity", position = "stack", alpha = 1,
                        colour = "white", linewidth = 0.2)
      if (show_values) {
        p <- p + geom_text(aes(label = paste0(Percent, "%")),
                           position = position_stack(vjust = 0.5), size = 2.8)
      }
    } else {
      p <- p + geom_bar(stat = "identity", position = position_dodge(width = 0.9),
                        alpha = 1, colour = "white", linewidth = 0.2)
      if (show_values) {
        p <- p + geom_text(aes(label = Count), position = position_dodge(width = 0.9),
                           vjust = -0.4, size = 2.8)
      }
    }

    lastCensusPlot(p)
    return(p)
  })

  # Input files table

  output$inputFilesTable <- DT::renderDataTable({
    req()
  })


  # Census summary table
  output$censusSummaryTable <- renderTable({
    req(censusData())

    df <- censusData()

    if (is_separate_census(df)) {
      return(separate_census_totals(df) %>%
               group_by(Stratifier) %>%
               summarise(Total_Population = sum(Count, na.rm = TRUE),
                         Categories = n(),
                         Average_per_Category = round(mean(Count, na.rm = TRUE), 0),
                         .groups = "drop") %>%
               as.data.frame())
    }

    # Create summary statistics
    summary_df <- df %>%
      group_by(Gender) %>%
      summarise(
        Total_Population = sum(Count, na.rm = TRUE),
        Age_Groups = n_distinct(Age),
        Average_per_Group = round(mean(Count, na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      as.data.frame()

    return(summary_df)
  })

  # Census data table
  output$censusDataTable <- DT::renderDataTable({
    req(censusData())

    df <- censusData()

    if (is_separate_census(df)) {
      df <- cbind(separate_census_totals(df), Source = df$Source[1])
    } else {
      # Sort age groups numerically
      age_order <- unique(df$Age[order(as.numeric(sub("[-+].*", "", df$Age)))])
      df$Age <- factor(df$Age, levels = age_order)
      df <- df[order(df$Age), ]
      df$Age <- as.character(df$Age)  # convert back so DT renders it cleanly
    }

    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list()  # ← remove default ordering so our pre-sort is respected
      ),
      rownames = FALSE
    )
  })

  # Download census data
  output$downloadCensusData <- downloadHandler(
    filename = function() {
      paste0("census_data_", Sys.Date(), ".json")
    },
    content = function(file) {
      req(censusData())
      jsonlite::write_json(censusData(), file, pretty = TRUE)
    }
  )

  # Download census plot
  output$downloadCensusPlot <- downloadHandler(
    filename = function() {
      paste0("census_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(lastCensusPlot())
      ggsave(file, plot = lastCensusPlot(), width = 12, height = 8, dpi = 300)
    }
  )

  # ── HELPER: bin a numeric age into whatever age-group labels exist in census ──
  # Reads the census age labels (e.g. "0-17", "18-34", "35-49", "50-64", "65+")
  # and maps a numeric age to the correct label.
  # Returns NA if no label can be matched.
  bin_age_to_census_groups <- function(age_numeric, census_age_labels) {
    # Build lookup table once
    bounds <- lapply(census_age_labels, parse_range_label)

    # Assign each age
    sapply(age_numeric, function(a) {
      if (is.na(a)) return(NA_character_)
      for (k in seq_along(census_age_labels)) {
        lo <- bounds[[k]][1]; hi <- bounds[[k]][2]
        if (!is.na(lo) && a >= lo && a <= hi) return(census_age_labels[k])
      }
      NA_character_          # age falls outside all defined groups
    })
  }

  # ── HELPER: normalise FHIR gender to census Gender labels ────────────────────
  # census_gender_labels: the unique Gender values found in censusData()
  # fhir_gender: character vector of raw FHIR gender values
  map_fhir_gender <- function(fhir_gender, census_gender_labels) {
    fhir_lower   <- tolower(trimws(as.character(fhir_gender)))
    census_lower <- tolower(trimws(census_gender_labels))

    unique_fhir <- unique(fhir_lower)
    # Remove NA values from the unique set — handle them at the end
    unique_fhir <- unique_fhir[!is.na(unique_fhir)]

    mapping <- setNames(rep(NA_character_, length(unique_fhir)), unique_fhir)

    for (fg in unique_fhir) {
      exact <- census_lower == fg
      exact[is.na(exact)] <- FALSE          # ← guard: NA → FALSE
      if (any(exact)) { mapping[fg] <- census_gender_labels[which(exact)[1]]; next }

      sub_match <- startsWith(census_lower, fg) | startsWith(fg, census_lower)
      sub_match[is.na(sub_match)] <- FALSE  # ← same guard
      if (any(sub_match)) { mapping[fg] <- census_gender_labels[which(sub_match)[1]]; next }

      if (!is.na(fg) && fg %in% c("male", "m"))             { m <- census_lower %in% c("male","männlich","m","man");    if (any(m)) mapping[fg] <- census_gender_labels[which(m)[1]] }
      if (!is.na(fg) && fg %in% c("female", "f", "w"))      { m <- census_lower %in% c("female","weiblich","f","w","woman"); if (any(m)) mapping[fg] <- census_gender_labels[which(m)[1]] }
      if (!is.na(fg) && fg %in% c("other", "diverse", "d")) { m <- census_lower %in% c("other","diverse","d","divers"); if (any(m)) mapping[fg] <- census_gender_labels[which(m)[1]] }
      if (!is.na(fg) && fg %in% c("unknown", ""))           { m <- census_lower %in% c("unknown","unbekannt","u");      if (any(m)) mapping[fg] <- census_gender_labels[which(m)[1]] }
    }

    # Apply mapping — NA fhir_gender values map to NA_character_ naturally
    mapped <- mapping[fhir_lower]
    ifelse(is.na(mapped), NA_character_, mapped)
  }

  # 4.13 Draggable mini‐plots
  output$plotsUI <- renderUI({
    req(allData())
    dl <- allData()
    tagList(lapply(seq_along(dl), function(i) {
      f      <- dl[[i]]
      safe_i <- i
      jqui_draggable(
        div(class = "plot_box", `data-plot-name` = f$name,
            div(style = "display:flex; justify-content:space-between;",
                h4(f$name), checkboxInput(paste0("cb_", safe_i), NULL, TRUE)
            ),
            selectizeInput(paste0("filter_", safe_i), "Filter Categories:",
                           choices = unique(f$data$Category), multiple = TRUE),
            selectInput(paste0("pt_", safe_i), "Chart Type:",
                        c("Histogram", "Pie Chart", "Line Chart")),
            uiOutput(paste0("plotUI_", safe_i)),
            sliderInput(paste0("op_", safe_i), "Transparency:",
                        min = 0.1, max = 1, value = 1, step = 0.1),
            downloadButton(paste0("download_", safe_i), "Export JSON",
                           class = "btn btn-sm btn-outline-secondary",
                           style = "width: 100%; margin-top: 10px;")
        )
      )
    }))
  })

  # 4.14 Category summary table
  output$categorySummary <- renderUI({
    req(allData())
    dl    <- allData()
    names <- sapply(dl, `[[`, "name")
    catMap <- list()
    for (f in dl) for (c in unique(f$data$Category)) {
      catMap[[c]] <- union(catMap[[c]], f$name)
    }

    # Build HTML table
    html <- '<table style="width:100%; border-collapse:collapse;" border="1">'
    html <- paste0(html, '<tr style="background:#f2f2f2;"><th>Category</th>',
                   paste0('<th>', names, '</th>', collapse = ''), '<th>Count</th></tr>')

    for (c in names(catMap)) {
      pres  <- length(catMap[[c]])
      color <- if      (pres == length(names)) "#d4edda"
      else if (pres >= 2)               "#fff3cd"
      else                              "#f8d7da"
      row  <- paste0(
        '<tr><td>', c, '</td>',
        paste0(ifelse(names %in% catMap[[c]],
                      paste0('<td style="background:', color, ';"></td>'),
                      '<td></td>'),
               collapse = ''),
        '<td style="text-align:center;">', pres, '</td></tr>'
      )
      html <- paste0(html, row)
    }

    HTML(paste0(html, '</table>'))
  })

  # 4.15 Individual plot download handlers
  observe({
    req(allData())
    dl <- allData()

    for (i in seq_along(dl)) {
      local({
        idx <- i
        f <- dl[[idx]]

        output[[paste0("download_", idx)]] <- downloadHandler(
          filename = function() {
            # Create safe filename from dataset name
            safe_name <- make_safe_id(f$name)
            category_col <- input$fhir_category_col
            safe_category <- make_safe_id(category_col)
            paste0(safe_name, "_", safe_category, "_", Sys.Date(), ".json")
          },
          content = function(file) {
            # Get the current filtered data (same logic as plot)
            filterCat <- input[[paste0("filter_", idx)]]

            export_data <- if (!is.null(filterCat) && length(filterCat) > 0) {
              f$data[f$data$Category %in% filterCat, ]
            } else {
              f$data
            }

            # Add metadata to the export
            export_object <- list(
              metadata = list(
                dataset_name = f$name,
                category_column = input$fhir_category_col,
                export_date = Sys.time(),
                total_rows = nrow(export_data),
                filtered = !is.null(filterCat) && length(filterCat) > 0,
                filter_categories = if (!is.null(filterCat)) filterCat else NULL
              ),
              data = export_data
            )

            jsonlite::write_json(export_object, file, pretty = TRUE, auto_unbox = TRUE)
          }
        )
      })
    }
  })

  # 4.16 handle file additions
  observeEvent(input$newFiles, {
    req(input$newFiles)
    current <- uploadedFiles()

    new_entries <- lapply(seq_len(nrow(input$newFiles)), function(i) {
      list(
        name = input$newFiles$name[i],
        path = input$newFiles$datapath[i],
        type = "csv_json"  # default type
      )
    })

    # Avoid duplicates by name
    existing_names <- sapply(current, `[[`, "name")
    new_entries <- Filter(function(e) !e$name %in% existing_names, new_entries)

    uploadedFiles(c(current, new_entries))
  })

  # 4.17 handlie file removal
  observeEvent(input$removeSelected, {
    current <- uploadedFiles()

    # Collect which checkboxes are checked
    to_remove <- which(sapply(seq_along(current), function(i) {
      isTRUE(input[[paste0("file_select_", i)]])
    }))

    if (length(to_remove) > 0) {
      uploadedFiles(current[-to_remove])
    }
  })

  # 4.19 Render file list UI
  output$fileListUI <- renderUI({
    files <- uploadedFiles()
    if (length(files) == 0) {
      return(p("No files uploaded yet.", style = "color: #999;"))
    }

    tagList(
      h4("Uploaded Files"),
      lapply(seq_along(files), function(i) {
        f <- files[[i]]
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;
                   padding:8px; border:1px solid #DDD; border-radius:4px;",
            checkboxInput(paste0("file_select_", i), label = NULL, value = FALSE),
            div(style = "flex:1; font-size:13px; word-break:break-all;", f$name),
            selectInput(paste0("file_type_", i), label = NULL,
                        choices = c("CSV/JSON" = "csv_json",
                                    "Census"   = "census",
                                    "FHIR"     = "fhir"),
                        selected = f$type,
                        width = "130px")
        )
      })
    )
  })

  # 4.20 Sync type changes
  observe({
    files <- uploadedFiles()
    if (length(files) == 0) return()

    updated <- lapply(seq_along(files), function(i) {
      type_val <- input[[paste0("file_type_", i)]]
      if (!is.null(type_val)) files[[i]]$type <- type_val
      files[[i]]
    })

    uploadedFiles(updated)
  })

  # 4.18 FHIR data binning and aggregation
  output$fhirFileSelectorBinning <- renderUI({
    files <- uploadedFiles()
    fhir_files <- Filter(function(f) f$type == "fhir", files)
    if (length(fhir_files) == 0) {
      p("No FHIR files uploaded yet. Please upload in the Data Upload tab.",
        style = "color:#999; font-size:12px;")
    } else {
      checkboxGroupInput("selected_fhir_files_binning", "Select FHIR Files:",
                         choices = setNames(
                           sapply(fhir_files, `[[`, "path"),
                           sapply(fhir_files, `[[`, "name")
                         ))
    }
  })

  fhirDataBinning <- reactive({
    req(input$selected_fhir_files_binning)

    selected_paths <- input$selected_fhir_files_binning
    files <- uploadedFiles()
    fhir_files <- Filter(function(f) f$type == "fhir" && f$path %in% selected_paths, files)

    if (length(fhir_files) == 0) return(list())

    all_files_data <- list()
    for (f in fhir_files) {
      file_data_list <- loadFhirFile(f$path, f$name)
      if (!is.null(file_data_list) && length(file_data_list) > 0) {
        all_files_data <- c(all_files_data, file_data_list)
      }
    }

    return(all_files_data)
  })

  output$fhirMappingUIBinning <- renderUI({
    fhir_data <- fhirDataBinning()
    req(input$fhir_resource_to_viz_binning)

    selected_resource_type <- input$fhir_resource_to_viz_binning
    matching_datasets <- names(fhir_data)[grepl(paste0("_", selected_resource_type, "$"), names(fhir_data))]

    if (length(matching_datasets) > 0) {
      all_columns <- unique(unlist(lapply(matching_datasets, function(dataset_name) {
        colnames(fhir_data[[dataset_name]])
      })))

      resource_prefix <- paste0(tolower(selected_resource_type), ".")
      resource_columns <- all_columns[grepl(paste0("^", resource_prefix), all_columns)]

      if (length(resource_columns) > 0) {
        selectInput("fhir_category_col_binning", "Category column:",
                    choices = resource_columns,
                    selected = resource_columns[1])
      }

    }
  })

  output$fhirValuesUIBinning <- renderUI({
    fhir_data <- fhirDataBinning()
    req(input$fhir_category_col_binning, input$fhir_resource_to_viz_binning)

    selected_resource_type <- input$fhir_resource_to_viz_binning
    selected_attribute <- input$fhir_category_col_binning

    n_bins <- input$fhir_n_bins %||% 5 #get bins from input or default to 5 if input isn't loaded yet
    value_type <- input$value_types %||% FALSE

    # Get unique values from the selected column
    matching_datasets <- names(fhir_data)[grepl(paste0("_", selected_resource_type, "$"), names(fhir_data))]

    uniqueValues <- sort(unique(unlist(lapply(matching_datasets, function(dataset_name) {
      df <- fhir_data[[dataset_name]]
      if (selected_attribute %in% colnames(df)) {
        df[[selected_attribute]]
      }
      else cat("FAILED \n")
    }))))

    if (value_type == "bool"){
      n_bins = 2
    }

    bin_inputs <- lapply(1:n_bins, function(i){
      if(value_type == "num"){
        numericInput(
          paste0("bin_", i), paste("Bin", i, "max:"), i)
      } else {
        selectInput(
          inputId = paste0("bin_", i),
          label = paste("Bin", i, "values:"),
          choices = uniqueValues,
          multiple = FALSE
        )
      }
    })

    tagList(
      h4("Create the bins"),
      bin_inputs
    )
  })

  output$plotBins <- renderPlot({
    fhir_data <- fhirDataBinning()
    req(input$fhir_category_col_binning, input$fhir_resource_to_viz_binning,
        input$fhir_n_bins, input$value_types, input$selected_fhir_files_binning)

    selected_resource_type <- input$fhir_resource_to_viz_binning
    selected_attribute     <- input$fhir_category_col_binning
    n_bins                 <- input$fhir_n_bins
    value_type             <- input$value_types
    display_mode           <- input$bins_display_mode

    files     <- uploadedFiles()
    fhir_files <- Filter(function(f) f$type == "fhir" &&
                           f$path %in% input$selected_fhir_files_binning, files)
    file_name_map <- setNames(
      sapply(fhir_files, `[[`, "name"),
      sapply(fhir_files, `[[`, "path")
    )

    # Get all data from matching datasets, tagged by source file
    matching_datasets <- names(fhir_data)[grepl(paste0("_", selected_resource_type, "$"),
                                                names(fhir_data))]

    all_data <- do.call(rbind, lapply(matching_datasets, function(dataset_name) {
      df <- fhir_data[[dataset_name]]
      if (selected_attribute %in% colnames(df)) {
        # Extract source file path from dataset name
        # dataset_name format is "filename_resourcetype"
        source_name <- dataset_name
        for (path in names(file_name_map)) {
          fname <- tools::file_path_sans_ext(file_name_map[path])
          if (grepl(fname, dataset_name, fixed = TRUE)) {
            source_name <- file_name_map[path]
            break
          }
        }
        data.frame(
          value  = df[[selected_attribute]],
          source = source_name,
          stringsAsFactors = FALSE
        )
      }
    }))

    if (is.null(all_data) || nrow(all_data) == 0) return(NULL)

    # Assign each value to a bin
    all_data$bin <- NA_character_
    for (i in 1:n_bins) {
      if (value_type == "text") {
        bin_values <- input[[paste0("bin_", i)]]
        if (!is.null(bin_values) && length(bin_values) > 0) {
          all_data$bin[all_data$value %in% bin_values] <- paste("Bin", i)
        }
      } else if (value_type == "num") {
        bin_max <- input[[paste0("bin_", i)]]
        bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
        all_data$bin[all_data$value <= bin_max & all_data$value > bin_min] <- paste("Bin", i)
      } else if (value_type == "bool") {
        if (i == 1) all_data$bin[all_data$value == FALSE] <- paste("Bin", i)
        if (i == 2) all_data$bin[all_data$value == TRUE]  <- paste("Bin", i)
      }
    }

    all_data <- all_data[!is.na(all_data$bin), ]
    if (nrow(all_data) == 0) return(NULL)

    # Count per bin per source
    # Count per bin per source
    bin_counts <- all_data %>%
      count(source, bin, name = "Count") %>%
      as.data.frame()

    # Calculate percentages within each source
    bin_counts <- bin_counts %>%
      group_by(source) %>%
      mutate(Percent = round(Count / sum(Count) * 100, 2)) %>%
      ungroup()

    # Truncate labels to 15 characters
    truncate_label <- function(x, max_chars = 15) {
      ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
    }

    # Build bin labels from selected values
    bin_labels <- setNames(sapply(1:n_bins, function(i) {
      if (value_type == "text") {
        vals <- input[[paste0("bin_", i)]]
        if (!is.null(vals) && length(vals) > 0) truncate_label(vals) else paste("Bin", i)
      } else if (value_type == "num") {
        bin_max <- input[[paste0("bin_", i)]]
        bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
        if (is.infinite(bin_min)) paste0("≤ ", bin_max) else paste0(bin_min, " – ", bin_max)
      } else if (value_type == "bool") {
        if (i == 1) "False" else "True"
      }
    }), paste0("Bin ", 1:n_bins))

    # Apply labels
    bin_counts$bin_label <- bin_labels[bin_counts$bin]
    bin_counts$bin_label <- factor(bin_counts$bin_label, levels = bin_labels)

    y_var   <- if (display_mode == "percent") "Percent" else "Count"
    y_label <- if (display_mode == "percent") "Percentage (%)" else "Count"

    ggplot(bin_counts, aes(x = bin_label, y = .data[[y_var]], fill = bin_label)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Distribution of", selected_attribute, "across bins"),
        x     = "Bin",
        y     = y_label,
        fill  = "Bin"
      ) +
      scale_fill_brewer(palette = "Set2") +
      facet_wrap(~source, ncol = length(unique(bin_counts$source))) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  output$fhirResourceTypeUIBinning <- renderUI({
    fhir_data <- fhirDataBinning()
    if (is.null(fhir_data) || length(fhir_data) == 0) return(NULL)

    available_resources <- names(fhir_data)
    if (length(available_resources) > 0) {
      resource_types <- unique(sapply(available_resources, function(x) {
        parts <- strsplit(x, "_")[[1]]
        if (length(parts) > 1) parts[length(parts)] else x
      }))

      selectInput("fhir_resource_to_viz_binning", "Resource Type:",
                  choices = resource_types,
                  selected = resource_types[1])
    }
  })

  # User-defined aggregates: named list, aggregate name -> paths of member files
  vizGroups <- reactiveVal(list())

  output$vizGroupMemberSelector <- renderUI({
    files <- uploadedFiles()
    if (length(files) < 2) {
      return(p("Upload at least two files to aggregate.", style = "color:#999; font-size:12px;"))
    }
    selectizeInput("viz_group_members", "Sources to aggregate:",
                   choices  = setNames(vapply(files, `[[`, character(1), "path"),
                                       vapply(files, `[[`, character(1), "name")),
                   multiple = TRUE,
                   options  = list(placeholder = "Select two or more"))
  })

  observeEvent(input$viz_add_group, {
    name    <- trimws(input$viz_group_name %||% "")
    members <- input$viz_group_members
    taken   <- c(vapply(uploadedFiles(), `[[`, character(1), "name"), names(vizGroups()))

    if (!nzchar(name)) {
      showNotification("Please enter a name for the aggregate.", type = "warning")
      return()
    }
    if (name %in% taken) {
      showNotification(paste0("A source named '", name, "' already exists."), type = "warning")
      return()
    }
    if (length(members) < 2) {
      showNotification("Select at least two sources to aggregate.", type = "warning")
      return()
    }

    groups <- vizGroups()
    groups[[name]] <- members
    vizGroups(groups)
    updateTextInput(session, "viz_group_name", value = "")
    updateSelectizeInput(session, "viz_group_members", selected = character(0))
  })

  observeEvent(input$viz_remove_group, {
    groups <- vizGroups()
    groups[[input$viz_remove_group]] <- NULL
    vizGroups(groups)
  })

  output$vizGroupList <- renderUI({
    groups <- vizGroups()
    if (length(groups) == 0) return(NULL)
    files      <- uploadedFiles()
    file_names <- setNames(vapply(files, `[[`, character(1), "name"),
                           vapply(files, `[[`, character(1), "path"))

    div(style = "margin-top:10px;",
        lapply(names(groups), function(g) {
          members <- file_names[intersect(groups[[g]], names(file_names))]
          div(style = "font-size:12px; margin-bottom:6px;",
              tags$a(href = "#", title = "Remove aggregate", style = "color:#c00; margin-right:4px;",
                     onclick = sprintf("Shiny.setInputValue('viz_remove_group', %s, {priority: 'event'}); return false;",
                                       jsonlite::toJSON(g, auto_unbox = TRUE)),
                     icon("xmark")),
              tags$b(g), ": ",
              if (length(members) > 0) paste(members, collapse = ", ")
              else tags$span("no member files left", style = "color:#999;"))
        }))
  })

  # Choices offered by the previous render of the source selector, so that a
  # re-render keeps the user's selection and only selects newly added choices
  vizKnownChoices <- character(0)

  output$vizSourceSelector <- renderUI({
    files  <- uploadedFiles()
    groups <- vizGroups()
    if (length(files) == 0) {
      return(p("No files uploaded yet.", style = "color:#999; font-size:12px;"))
    }

    choices <- setNames(vapply(files, `[[`, character(1), "path"),
                        vapply(files, `[[`, character(1), "name"))
    group_ids <- character(0)
    if (length(groups) > 0) {
      group_ids <- setNames(paste0("group:", names(groups)), paste(names(groups), "(aggregate)"))
      choices   <- c(choices, group_ids)
    }

    new_choices <- setdiff(choices, vizKnownChoices)
    selected    <- union(intersect(isolate(input$viz_selected_sources), choices), new_choices)
    # A newly added aggregate replaces its members in the selection
    for (id in intersect(new_choices, group_ids)) {
      selected <- setdiff(selected, groups[[sub("^group:", "", id)]])
    }
    vizKnownChoices <<- unname(choices)

    checkboxGroupInput("viz_selected_sources", "Select Sources:",
                       choices  = choices,
                       selected = selected)
  })

  # ── Bins from the categories of a report ────────────────────────────────────
  output$vizBinReportSelector <- renderUI({
    reports <- Filter(function(f) f$type %in% c("csv_json", "census"), uploadedFiles())
    if (length(reports) == 0) {
      return(p("Upload a CSV/JSON or census report first.", style = "color:#999; font-size:12px;"))
    }
    choices <- setNames(vapply(reports, `[[`, character(1), "path"),
                        vapply(reports, `[[`, character(1), "name"))
    kept <- intersect(isolate(input$viz_bin_report), choices)
    selectInput("viz_bin_report", "Report providing the bins:", choices = choices,
                selected = if (length(kept) > 0) kept[1] else choices[1])
  })

  # Category/Count distribution of a CSV/JSON or census file, NULL if it has none
  loadSourceDistribution <- function(f) {
    if (f$type == "csv_json") return(loadCsvJsonFile(f))
    if (f$type == "census") {
      df <- loadCensusData(f$path)
      if (is.null(df)) return(NULL)
      # A separate report contributes its age totals; its gender totals cover
      # the same people and cannot be added to the same distribution
      if (is_separate_census(df)) {
        df <- df[!is.na(df$Age), ]
        return(data.frame(Category = df$Age, Count = df$Count, stringsAsFactors = FALSE))
      }
      return(data.frame(Category = paste(df$Age, df$Gender, sep = " · "), Count = df$Count,
                        stringsAsFactors = FALSE))
    }
    NULL
  }

  # The bin labels: the categories of the selected report, in its order
  vizReportBins <- reactive({
    f <- Find(function(f) f$path == (input$viz_bin_report %||% ""), uploadedFiles())
    if (is.null(f)) return(NULL)
    dist <- loadSourceDistribution(f)
    if (is.null(dist) || nrow(dist) == 0) NULL else unique(dist$Category)
  })

  # Map values or categories onto the report bins (NA where none fits). A value
  # matches a bin by name, or when its numeric range lies within the bin's range;
  # a part after " · " (e.g. gender) must match too unless the bin has none.
  map_to_report_bins <- function(values, bins) {
    split_key <- function(x) {
      k   <- tolower(gsub("\\s*[·|;,]\\s*", " · ", trimws(as.character(x))))
      pos <- regexpr(" · ", k, fixed = TRUE)
      list(head = ifelse(pos > 0, substr(k, 1, pos - 1), k),
           tail = ifelse(pos > 0, substring(k, pos + 3), ""))
    }
    b       <- split_key(bins)
    b_range <- lapply(b$head, parse_range_label)

    uv  <- unique(as.character(values))
    v   <- split_key(uv)
    hit <- vapply(seq_along(uv), function(i) {
      if (is.na(uv[i])) return(NA_character_)
      # Bins with the same detail first, then coarser bins without one
      cand <- c(which(b$tail == v$tail[i]), which(b$tail == "" & v$tail[i] != ""))
      exact <- cand[b$head[cand] == v$head[i]]
      if (length(exact) > 0) return(bins[exact[1]])
      r <- parse_range_label(v$head[i])
      if (anyNA(r)) return(NA_character_)
      within <- cand[vapply(b_range[cand], function(br) !anyNA(br) && r[1] >= br[1] && r[2] <= br[2],
                            logical(1))]
      if (length(within) > 0) bins[within[1]] else NA_character_
    }, character(1))
    hit[match(as.character(values), uv)]
  }

  # Values of the attribute chosen in "FHIR in bins" from one FHIR file, one row
  # per value; NULL if the file has no such resources
  fhirAttributeValues <- function(f) {
    file_data_list <- loadFhirFile(f$path, f$name)
    if (is.null(file_data_list) || length(file_data_list) == 0) return(NULL)

    matching_datasets <- names(file_data_list)[grepl(
      paste0("_", input$fhir_resource_to_viz_binning, "$"), names(file_data_list))]
    if (length(matching_datasets) == 0) return(NULL)

    do.call(rbind, lapply(matching_datasets, function(dn) {
      d <- file_data_list[[dn]]
      if (input$fhir_category_col_binning %in% colnames(d)) {
        data.frame(value = d[[input$fhir_category_col_binning]], n = 1, stringsAsFactors = FALSE)
      }
    }))
  }

  # Sum the bin counts of several vizData() entries into one source
  aggregateVizSources <- function(name, parts, display_mode, bin_type) {
    if (length(parts) == 0) return(NULL)

    part_levels <- lapply(parts, function(p) levels(p$data$x_label))
    all_levels  <- unique(unlist(part_levels))
    # Minimal x-axis mode gives every source its own census label set: re-sort by age
    if (bin_type == "census" && !all(vapply(part_levels, identical, logical(1), part_levels[[1]]))) {
      all_levels <- all_levels[order(as.numeric(sub("[-+].*", "", sub(" · .*", "", all_levels))))]
    }
    if (bin_type == "report") all_levels <- intersect(vizReportBins(), all_levels)

    df <- do.call(rbind, lapply(parts, function(p) {
      data.frame(x_label = as.character(p$data$x_label), Count = p$data$Count,
                 stringsAsFactors = FALSE)
    })) %>%
      group_by(x_label) %>%
      summarise(Count = sum(Count), .groups = "drop") %>%
      as.data.frame()

    df$x_label <- factor(df$x_label, levels = all_levels)
    df <- df[order(df$x_label), ]
    rownames(df) <- NULL
    df$y_val <- if (display_mode == "percent") round(df$Count / sum(df$Count) * 100, 2) else df$Count

    list(name    = name,
         data    = df,
         total   = sum(vapply(parts, function(p) p$total %||% NA_real_, numeric(1))),
         members = vapply(parts, `[[`, character(1), "name"))
  }

  vizData <- reactive({
    req(input$viz_selected_sources)

    files        <- uploadedFiles()
    groups       <- vizGroups()
    sel_ids      <- input$viz_selected_sources
    sel_groups   <- intersect(sub("^group:", "", sel_ids[startsWith(sel_ids, "group:")]), names(groups))
    # Files needed for the selected sources, including members of selected aggregates
    needed_paths <- union(sel_ids, unlist(groups[sel_groups]))
    selected     <- Filter(function(f) f$path %in% needed_paths, files)
    bin_type     <- input$viz_bin_type
    display_mode <- input$viz_display_mode
    combine_gender <- isTRUE(input$viz_combine_gender) && bin_type == "census"
    fhir_bins_ready <- !is.null(input$fhir_resource_to_viz_binning) &&
      !is.null(input$fhir_category_col_binning) && nzchar(input$fhir_category_col_binning) &&
      !is.null(input$fhir_n_bins) && !is.null(input$value_types)
    fhir_attr_ready <- !is.null(input$fhir_resource_to_viz_binning) &&
      !is.null(input$fhir_category_col_binning) && nzchar(input$fhir_category_col_binning)

    # Every branch returns list(name, data, total) where total is the number of
    # records before binning, NULL when nothing fits the bins, or list(error)
    results <- lapply(selected, function(f) {
      total <- NA_real_

      if (bin_type == "census") {
        # Use census Age x Gender bins
        # Load census reference directly — independent of Census tab selector
        all_files  <- uploadedFiles()
        census_files <- Filter(function(f) f$type == "census", all_files)

        census_ref <- if (!is.null(input$selected_census_file)) {
          loadCensusData(input$selected_census_file)
        } else if (length(census_files) > 0) {
          loadCensusData(census_files[[1]]$path)
        } else {
          NULL
        }

        if (is.null(census_ref)) return(NULL)

        census_age_labels    <- unique(na.omit(census_ref$Age))
        census_gender_labels <- unique(na.omit(census_ref$Gender))

        tryCatch({
          if (f$type == "census") {
            df <- loadCensusData(f$path)
            if (is.null(df)) return(NULL)
            # A separate report only has age totals for the age groups
            if (is_separate_census(df)) {
              if (!combine_gender) return(NULL)
              df <- df[!is.na(df$Age), ]
              df$Gender <- "all"
            }
            df$Source <- f$name
            total     <- sum(df$Count)

            # Calculate percent within this source
            df <- df %>%
              mutate(x_label = paste(Age, Gender, sep = " · ")) %>%
              group_by(x_label) %>%
              summarise(Count = sum(Count), .groups = "drop")

          } else if (f$type == "fhir") {
            raw     <- jsonlite::fromJSON(f$path, simplifyVector = FALSE)
            entries <- if (!is.null(raw$resourceType) && raw$resourceType == "Bundle") raw$entry else raw
            if (is.null(entries) || length(entries) == 0) return(NULL)

            patients <- Filter(function(e) {
              res <- if (!is.null(e$resource)) e$resource else e
              !is.null(res$resourceType) && res$resourceType == "Patient"
            }, entries)
            if (length(patients) == 0) return(NULL)
            total <- length(patients)

            records <- lapply(patients, function(e) {
              p <- if (!is.null(e$resource)) e$resource else e
              list(birthDate = as.character(p$birthDate %||% NA_character_),
                   gender    = as.character(p$gender    %||% NA_character_))
            })

            df <- data.frame(
              birthDate = sapply(records, `[[`, "birthDate"),
              gender    = sapply(records, `[[`, "gender"),
              stringsAsFactors = FALSE
            )

            today <- Sys.Date()
            df$age_numeric <- sapply(df$birthDate, function(bd) {
              if (is.null(bd) || is.na(bd) || !nzchar(trimws(bd))) return(NA_real_)
              bd <- sub("T.*$", "", trimws(bd))
              bd_padded <- if (nchar(bd) == 4) paste0(bd, "-01-01")
              else if (nchar(bd) == 7) paste0(bd, "-01")
              else bd
              dob <- tryCatch(as.Date(bd_padded), error = function(e) NA)
              if (is.na(dob) || dob >= today || dob < as.Date("1900-01-01")) return(NA_real_)
              year_diff       <- as.numeric(format(today, "%Y")) - as.numeric(format(dob, "%Y"))
              birthday_passed <- format(today, "%m-%d") >= format(dob, "%m-%d")
              as.numeric(year_diff - ifelse(birthday_passed, 0L, 1L))
            })

            df$Age    <- bin_age_to_census_groups(df$age_numeric, census_age_labels)
            df$Gender <- map_fhir_gender(df$gender, census_gender_labels)
            df        <- df[!is.na(df$Age) & !is.na(df$Gender), ]
            if (nrow(df) == 0) return(NULL)

            df <- df %>%
              dplyr::count(Age, Gender, name = "Count") %>%
              mutate(x_label = paste(Age, Gender, sep = " · "))

          } else if (f$type == "csv_json") {
            # A CSV/JSON distribution keeps its own counts; its categories are
            # "<age>" or "<age> <sep> <gender>", where age is a census age group
            # or a plain number of years (binned into the census age groups)
            dist <- loadCsvJsonFile(f)
            if (is.null(dist) || nrow(dist) == 0) return(NULL)
            total <- sum(dist$Count)

            parts   <- strsplit(gsub("\\s*[·|;,]\\s*", " · ", trimws(dist$Category)), " · ", fixed = TRUE)
            age     <- vapply(parts, function(p) p[1], character(1))
            gender  <- vapply(parts, function(p) if (length(p) > 1) p[2] else NA_character_, character(1))
            age_num <- suppressWarnings(as.numeric(age))
            age     <- ifelse(age %in% census_age_labels, age,
                              bin_age_to_census_groups(age_num, census_age_labels))

            df <- data.frame(Age    = age,
                             Gender = map_fhir_gender(gender, census_gender_labels),
                             Count  = dist$Count,
                             stringsAsFactors = FALSE)
            # Without gender the categories only fit when genders are combined
            df <- df[!is.na(df$Age) & (combine_gender | !is.na(df$Gender)), ]
            if (nrow(df) == 0) return(NULL)

            df <- df %>%
              mutate(x_label = if (combine_gender) Age else paste(Age, Gender, sep = " · ")) %>%
              group_by(x_label) %>%
              summarise(Count = sum(Count), .groups = "drop")

          } else {
            return(NULL)
          }

          # Optionally sum genders per age group, before percentages are computed
          if (combine_gender) {
            df <- df %>%
              mutate(x_label = sub(" · .*", "", x_label)) %>%
              group_by(x_label) %>%
              summarise(Count = sum(Count), .groups = "drop")
          }

          # Sort x_label by age numerically
          age_order <- unique(df$x_label[order(as.numeric(sub("[-+].*", "",
                                                              sub(" · .*", "", df$x_label))))])
          df$x_label <- factor(df$x_label, levels = age_order)

          if (display_mode == "percent") {
            df <- df %>% mutate(y_val = round(Count / sum(Count) * 100, 2))
          } else {
            df <- df %>% mutate(y_val = Count)
          }

          # Full label set for uniform x-axis
          all_x_labels <-  if (bin_type == "census") {
            age_order <- census_age_labels[order(as.numeric(sub("[-+].*", "", census_age_labels)))]
            genders   <- sort(census_gender_labels)
            if (combine_gender) age_order
            else as.vector(t(outer(age_order, genders, paste, sep = " · ")))
          } else {
            # All bin labels from FHIR bins
            req(input$fhir_n_bins, input$value_types)
            n_bins     <- input$fhir_n_bins
            value_type <- input$value_types
            truncate_label <- function(x, max_chars = 15) {
              ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
            }
            sapply(1:n_bins, function(i) {
              if (value_type == "text") {
                vals <- input[[paste0("bin_", i)]]
                if (!is.null(vals) && length(vals) > 0) truncate_label(vals) else paste("Bin", i)
              } else if (value_type == "num") {
                bin_max <- input[[paste0("bin_", i)]]
                bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
                if (is.infinite(bin_min)) paste0("≤ ", bin_max) else paste0(bin_min, " – ", bin_max)
              } else {
                if (i == 1) "False" else "True"
              }
            })
          }

          # Expand to full label set for uniform mode
          if (input$x_axis_display_mode == "uniform") {
            full_df <- data.frame(x_label = factor(all_x_labels, levels = all_x_labels),
                                  stringsAsFactors = FALSE)
            df <- full_df %>%
              left_join(df %>% mutate(x_label = as.character(x_label)),
                        by = "x_label") %>%
              mutate(
                Count = ifelse(is.na(Count), 0, Count),
                y_val = ifelse(is.na(y_val), 0, y_val)
              )
            df$x_label <- factor(df$x_label, levels = all_x_labels)
          }

          list(name = f$name, data = df, total = total)

        }, error = function(e) {
          warning(paste("vizData error for", f$name, ":", e$message))
          list(error = e$message)
        })

      } else if (bin_type == "report") {
        # Use the categories of the selected report as bins
        bins <- vizReportBins()
        if (is.null(bins)) return(NULL)
        if (f$type == "fhir" && !fhir_attr_ready) return(NULL)

        tryCatch({
          # One row per value or category; n is how often it occurs
          all_vals <- if (f$type == "fhir") {
            fhirAttributeValues(f)
          } else {
            dist <- loadSourceDistribution(f)
            if (is.null(dist)) NULL
            else data.frame(value = dist$Category, n = dist$Count, stringsAsFactors = FALSE)
          }
          if (is.null(all_vals) || nrow(all_vals) == 0) return(NULL)
          total <- sum(all_vals$n)

          all_vals$bin <- map_to_report_bins(all_vals$value, bins)
          all_vals     <- all_vals[!is.na(all_vals$bin), ]
          if (nrow(all_vals) == 0) return(NULL)

          df <- all_vals %>%
            count(x_label = bin, wt = n, name = "Count") %>%
            as.data.frame(stringsAsFactors = FALSE)
          df$y_val <- if (display_mode == "percent") round(df$Count / sum(df$Count) * 100, 2) else df$Count

          # Uniform mode shows every bin, minimal mode only the filled ones
          labels <- if (input$x_axis_display_mode == "uniform") bins else bins[bins %in% df$x_label]
          df <- data.frame(x_label = labels, stringsAsFactors = FALSE) %>%
            left_join(df, by = "x_label") %>%
            mutate(Count = ifelse(is.na(Count), 0, Count),
                   y_val = ifelse(is.na(y_val), 0, y_val))
          df$x_label <- factor(df$x_label, levels = labels)

          list(name = f$name, data = df, total = total)

        }, error = function(e) {
          warning(paste("vizData report bins error for", f$name, ":", e$message))
          list(error = e$message)
        })

      } else if (bin_type == "fhir_bins") {
        # Use FHIR in bins configuration
        if (!fhir_bins_ready || !f$type %in% c("fhir", "csv_json")) return(NULL)

        tryCatch({
          selected_resource_type <- input$fhir_resource_to_viz_binning
          selected_attribute     <- input$fhir_category_col_binning
          n_bins                 <- input$fhir_n_bins
          value_type             <- input$value_types

          # One row per value; n is how often the value occurs
          all_vals <- if (f$type == "fhir") {
            fhirAttributeValues(f)
          } else {
            # A CSV/JSON distribution: its categories are binned like the FHIR values
            dist <- loadCsvJsonFile(f)
            if (is.null(dist) || nrow(dist) == 0) return(NULL)
            value <- switch(value_type,
                            num  = suppressWarnings(as.numeric(dist$Category)),
                            bool = as.logical(dist$Category),
                            dist$Category)
            # Categories that do not parse as the bin type cannot be binned
            data.frame(value = value, n = dist$Count, stringsAsFactors = FALSE)[!is.na(value), ]
          }
          if (is.null(all_vals) || nrow(all_vals) == 0) return(NULL)
          total <- sum(all_vals$n)

          # Build bin labels
          truncate_label <- function(x, max_chars = 15) {
            ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
          }

          bin_labels <- setNames(sapply(1:n_bins, function(i) {
            if (value_type == "text") {
              vals <- input[[paste0("bin_", i)]]
              if (!is.null(vals) && length(vals) > 0) truncate_label(vals) else paste("Bin", i)
            } else if (value_type == "num") {
              bin_max <- input[[paste0("bin_", i)]]
              bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
              if (is.infinite(bin_min)) paste0("≤ ", bin_max) else paste0(bin_min, " – ", bin_max)
            } else {
              if (i == 1) "False" else "True"
            }
          }), paste0("Bin ", 1:n_bins))

          # Assign bins
          all_vals$bin <- NA_character_
          for (i in 1:n_bins) {
            if (value_type == "text") {
              bv <- input[[paste0("bin_", i)]]
              if (!is.null(bv)) all_vals$bin[all_vals$value %in% bv] <- paste("Bin", i)
            } else if (value_type == "num") {
              bmax <- input[[paste0("bin_", i)]]
              bmin <- input[[paste0("bin_", i - 1)]] %||% -Inf
              all_vals$bin[all_vals$value <= bmax & all_vals$value > bmin] <- paste("Bin", i)
            } else {
              if (i == 1) all_vals$bin[all_vals$value == FALSE] <- paste("Bin", i)
              if (i == 2) all_vals$bin[all_vals$value == TRUE]  <- paste("Bin", i)
            }
          }

          all_vals <- all_vals[!is.na(all_vals$bin), ]
          if (nrow(all_vals) == 0) return(NULL)

          df <- all_vals %>%
            count(bin, wt = n, name = "Count") %>%
            mutate(x_label = bin_labels[bin],
                   x_label = factor(x_label, levels = bin_labels))

          if (display_mode == "percent") {
            df <- df %>% mutate(y_val = round(Count / sum(Count) * 100, 2))
          } else {
            df <- df %>% mutate(y_val = Count)
          }

          # Full label set for uniform x-axis
          all_x_labels <-  if (bin_type == "census") {
            age_order <- unique(census_ref$Age[order(as.numeric(sub("[-+].*", "", census_ref$Age)))])
            genders   <- sort(unique(census_ref$Gender))
            as.vector(t(outer(age_order, genders, paste, sep = " · ")))
          } else {
            # All bin labels from FHIR bins
            req(input$fhir_n_bins, input$value_types)
            n_bins     <- input$fhir_n_bins
            value_type <- input$value_types
            truncate_label <- function(x, max_chars = 15) {
              ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
            }
            sapply(1:n_bins, function(i) {
              if (value_type == "text") {
                vals <- input[[paste0("bin_", i)]]
                if (!is.null(vals) && length(vals) > 0) truncate_label(vals) else paste("Bin", i)
              } else if (value_type == "num") {
                bin_max <- input[[paste0("bin_", i)]]
                bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
                if (is.infinite(bin_min)) paste0("≤ ", bin_max) else paste0(bin_min, " – ", bin_max)
              } else {
                if (i == 1) "False" else "True"
              }
            })
          }

          # Expand to full label set for uniform mode
          if (input$x_axis_display_mode == "uniform") {
            full_df <- data.frame(x_label = factor(all_x_labels, levels = all_x_labels),
                                  stringsAsFactors = FALSE)
            df <- full_df %>%
              left_join(df %>% mutate(x_label = as.character(x_label)),
                        by = "x_label") %>%
              mutate(
                Count = ifelse(is.na(Count), 0, Count),
                y_val = ifelse(is.na(y_val), 0, y_val)
              )
            df$x_label <- factor(df$x_label, levels = all_x_labels)
          }

          list(name = f$name, data = df, total = total)

        }, error = function(e) {
          warning(paste("vizData fhir_bins error for", f$name, ":", e$message))
          list(error = e$message)
        })
      }
    })
    names(results) <- vapply(selected, `[[`, character(1), "path")

    # Why a file produced nothing: shown to the user in the info box
    exclusion_reason <- function(f) {
      res <- results[[f$path]]
      if (!is.null(res$error)) return(paste("the file could not be read:", res$error))
      if (bin_type == "census") {
        if (!f$type %in% c("census", "fhir", "csv_json"))
          return("this file type cannot be mapped to census bins")
        if (!any(vapply(files, function(x) x$type == "census", logical(1))))
          return("no census report is uploaded to provide the bins")
        if (f$type == "census" && !combine_gender && isTRUE(is_separate_census(loadCensusData(f$path))))
          return("a separate report has age and gender totals but no age \u00d7 gender counts (tick \"Combine male and female\" to compare its age groups)")
        if (f$type == "csv_json" && !combine_gender)
          return("none of its categories match the census age and gender groups (categories without gender need \"Combine male and female\")")
        "none of its values match the census age and gender groups"
      } else if (bin_type == "report") {
        if (is.null(vizReportBins()))
          return("no report with categories is selected to provide the bins")
        if (!f$type %in% c("census", "fhir", "csv_json"))
          return("this file type cannot be mapped to report categories")
        if (f$type == "fhir" && !fhir_attr_ready)
          return("choose a resource type and attribute in the \"FHIR in bins\" tab to map its values")
        "none of its categories match the categories of the selected report"
      } else {
        if (!fhir_bins_ready) return("no bins are defined yet in the \"FHIR in bins\" tab")
        if (!f$type %in% c("fhir", "csv_json")) return("census reports cannot be mapped to FHIR bins")
        "none of its values fall into the bins defined in the \"FHIR in bins\" tab"
      }
    }
    file_by_path <- setNames(selected, vapply(selected, `[[`, character(1), "path"))
    ok <- vapply(results, function(r) !is.null(r) && is.null(r$error), logical(1))
    excluded <- lapply(file_by_path[!ok], function(f) list(name = f$name, reason = exclusion_reason(f)))
    results  <- results[ok]

    # Keep the order of the source selector; aggregates replace their member ids
    out <- lapply(sel_ids, function(id) {
      if (startsWith(id, "group:")) {
        g <- sub("^group:", "", id)
        aggregateVizSources(g, results[intersect(groups[[g]], names(results))],
                            display_mode, bin_type)
      } else {
        results[[id]]
      }
    })
    names(out) <- sel_ids
    # Aggregates whose members were all excluded are reported, too
    for (id in sel_ids[startsWith(sel_ids, "group:") & vapply(out, is.null, logical(1))]) {
      excluded[[id]] <- list(name   = sub("^group:", "", id),
                             reason = "none of its member sources could be mapped to the bins")
    }
    # Files pulled in only as aggregate members are named with their aggregate
    for (path in setdiff(names(excluded), sel_ids)) {
      in_groups <- sel_groups[vapply(sel_groups, function(g) path %in% groups[[g]], logical(1))]
      excluded[[path]]$name <- paste0(excluded[[path]]$name, " (member of ",
                                      paste(in_groups, collapse = ", "), ")")
    }

    out <- unname(Filter(Negate(is.null), out))
    attr(out, "excluded") <- unname(excluded)
    out
  })

  # Sources that are left out entirely, and sources with records outside the bins
  output$vizInfoBox <- renderUI({
    req(input$viz_selected_sources)
    dl       <- vizData()
    excluded <- attr(dl, "excluded")

    outside <- vizOutsideBins(dl)
    partial <- Filter(Negate(is.null), lapply(seq_along(dl), function(i) {
      d <- dl[[i]]
      if (is.na(outside[i]) || outside[i] < 0.5) return(NULL)
      sprintf("%s: %s of %s records (%.1f %%) lie outside the bins and are not counted.",
              d$name, format(outside[i], big.mark = ","), format(d$total, big.mark = ","),
              outside[i] / d$total * 100)
    }))

    tagList(
      if (length(excluded) > 0)
        div(class = "alert alert-warning", role = "alert",
            icon("triangle-exclamation"),
            strong(" Not shown: these sources cannot be mapped to the selected bins."),
            tags$ul(style = "margin:6px 0 0 0;",
                    lapply(excluded, function(x) tags$li(strong(x$name), ": ", x$reason)))),
      if (length(partial) > 0)
        div(class = "alert alert-info", role = "alert",
            icon("circle-info"),
            strong(" Partly mapped:"),
            tags$ul(style = "margin:6px 0 0 0;", lapply(partial, tags$li)))
    )
  })

  # The shown sources in the selected bins as one long table
  vizExportTable <- reactive({
    dl <- vizData()
    req(length(dl) > 0)
    do.call(rbind, lapply(dl, function(d) {
      count <- d$data$Count
      data.frame(
        source       = d$name,
        aggregate_of = if (is.null(d$members)) "" else paste(d$members, collapse = "; "),
        bin          = as.character(d$data$x_label),
        count        = count,
        percent      = if (sum(count) > 0) round(count / sum(count) * 100, 2) else 0,
        stringsAsFactors = FALSE
      )
    }))
  })

  # Records of each shown source that lie outside the bins
  vizOutsideBins <- function(dl) {
    setNames(vapply(dl, function(d) {
      out <- (d$total %||% NA_real_) - sum(d$data$Count, na.rm = TRUE)
      if (is.na(out)) NA_real_ else max(out, 0)
    }, numeric(1)), vapply(dl, `[[`, character(1), "name"))
  }

  vizBinningLabel <- function() {
    if (input$viz_bin_type == "census") {
      if (isTRUE(input$viz_combine_gender)) "census age groups" else "census age groups x gender"
    } else if (input$viz_bin_type == "report") {
      f <- Find(function(f) f$path == (input$viz_bin_report %||% ""), uploadedFiles())
      paste0("categories of ", if (is.null(f)) "a report" else f$name)
    } else {
      paste0("FHIR bins: ", input$fhir_resource_to_viz_binning, ".", input$fhir_category_col_binning)
    }
  }

  output$vizDownloadCsv <- downloadHandler(
    filename = function() paste0("compare_bins_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(vizExportTable(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$vizDownloadJson <- downloadHandler(
    filename = function() paste0("compare_bins_", Sys.Date(), ".json"),
    content  = function(file) {
      tbl     <- vizExportTable()
      outside <- vizOutsideBins(vizData())
      sources <- lapply(split(tbl, factor(tbl$source, levels = unique(tbl$source))), function(d) {
        list(
          name                 = d$source[1],
          aggregate_of         = if (nzchar(d$aggregate_of[1])) strsplit(d$aggregate_of[1], "; ", fixed = TRUE)[[1]] else NULL,
          count_in_bins        = sum(d$count),
          records_outside_bins = unname(outside[d$source[1]]),
          bins         = lapply(seq_len(nrow(d)), function(i)
                           list(bin = d$bin[i], count = d$count[i], percent = d$percent[i]))
        )
      })
      jsonlite::write_json(
        list(created  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
             binning  = vizBinningLabel(),
             excluded = lapply(attr(vizData(), "excluded"), function(x) list(name = x$name, reason = x$reason)),
             sources  = unname(sources)),
        file, auto_unbox = TRUE, pretty = TRUE, null = "null"
      )
    }
  )

  # The shown sources as one FHIR MeasureReport: a group per source (aggregates
  # included), each stratified by the selected bins
  output$vizDownloadMeasureReport <- downloadHandler(
    filename = function() paste0("compare_measurereport_", Sys.Date(), ".json"),
    content  = function(file) {
      tbl <- vizExportTable()
      initial_population <- function(count) {
        list(list(code  = list(coding = list(list(
                    system = "http://terminology.hl7.org/CodeSystem/measure-population",
                    code   = "initial-population"))),
                  count = count))
      }
      groups <- lapply(split(tbl, factor(tbl$source, levels = unique(tbl$source))), function(d) {
        label <- d$source[1]
        if (nzchar(d$aggregate_of[1])) label <- paste0(label, " (aggregate of ", d$aggregate_of[1], ")")
        list(
          code       = list(text = label),
          population = initial_population(sum(d$count)),
          stratifier = list(list(
            code    = list(list(text = vizBinningLabel())),
            stratum = lapply(seq_len(nrow(d)), function(i) {
              list(value = list(text = d$bin[i]), population = initial_population(d$count[i]))
            })
          ))
        )
      })
      report <- list(
        resourceType = "MeasureReport",
        status       = "complete",
        type         = "summary",
        date         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC"),
        period       = list(start = as.character(Sys.Date()), end = as.character(Sys.Date())),
        group        = unname(groups)
      )
      jsonlite::write_json(report, file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
    }
  )

  output$vizPlotsUI <- renderUI({
    req(input$viz_selected_sources)
    dl <- vizData()
    if (length(dl) == 0) return(p("No data to display.", style = "color:#999;"))

    if (input$viz_layout_mode == "overlay") {
      girafeOutput("vizOverlayPlot", width = "100%", height = "auto")
    } else {
      tagList(lapply(seq_along(dl), function(i) {
        plotOutput(paste0("vizPlot_", i), height = "350px")
      }))
    }
  })

  observe({
    req(length(vizData()) > 0)
    dl           <- vizData()
    display_mode <- isolate(input$viz_display_mode)
    y_label      <- if (isolate(input$viz_display_mode) == "percent") "Percentage (%)" else "Count"

    # Shared y max
    y_max <- max(unlist(lapply(dl, function(x) x$data$y_val)), na.rm = TRUE)

    for (i in seq_along(dl)) {
      local({
        idx  <- i
        d    <- dl[[idx]]

        # Without " · <gender>" in the labels (genders combined, FHIR bins) there
        # is a single series: one colour, no legend
        has_gender <- all(grepl(" · ", levels(d$data$x_label)))

        output[[paste0("vizPlot_", idx)]] <- renderPlot({
          ggplot(d$data, aes(x = x_label, y = y_val,
                             fill = if (has_gender) sub(".* · ", "", as.character(x_label)) else "All")) +
            geom_bar(stat = "identity", alpha = 1, colour = "white", linewidth = 0.2) +
            scale_y_continuous(limits = c(0, y_max * 1.05)) +
            scale_x_discrete(drop = FALSE) +  # ← keep empty bins
            {if (has_gender) scale_fill_brewer(palette = "Set2")
             else scale_fill_manual(values = c(All = "#2a78d6"), guide = "none")} +
            theme_minimal(base_size = 14) +
            labs(
              title    = d$name,
              subtitle = if (!is.null(d$members)) paste("Aggregate of:", paste(d$members, collapse = ", ")),
              x     = NULL,
              y     = y_label,
              fill  = "Gender"
            ) +
            theme(
              axis.text.x     = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              plot.title      = element_text(face = "bold")
            )
        })
      })
    }
  })

  # Categorical colours in fixed order; a source keeps its colour as long as
  # its position among the uploaded files does not change
  viz_source_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                          "#e87ba4", "#008300", "#4a3aa7", "#e34948")

  vizSourceColours <- function(source_names) {
    all_names <- c(sapply(uploadedFiles(), `[[`, "name"), names(vizGroups()))
    idx <- match(source_names, all_names)
    if (length(source_names) > length(viz_source_palette) ||
        any(is.na(idx)) || max(idx, 0) > length(viz_source_palette)) {
      idx <- seq_along(source_names)
    }
    cols <- if (length(source_names) <= length(viz_source_palette)) {
      viz_source_palette[idx]
    } else {
      scales::hue_pal()(length(source_names))
    }
    setNames(cols, source_names)
  }

  output$vizReferenceSelector <- renderUI({
    req(vizData())
    src_names <- sapply(vizData(), `[[`, "name")
    if (length(src_names) == 0) return(NULL)
    default <- src_names[grepl("Deutschland", src_names)][1]
    if (is.na(default)) default <- src_names[1]
    selectInput("viz_reference_source", "Reference source:",
                choices = src_names, selected = default)
  })

  output$vizOverlayPlot <- renderGirafe({
    req(length(vizData()) > 0)
    dl         <- vizData()
    style      <- input$viz_overlay_style %||% "lines"
    is_percent <- input$viz_display_mode == "percent"
    y_label    <- if (is_percent) "Percentage (%)" else "Count"

    # Combine all sources; keep the x order of every source (minimal mode may
    # give each source a different label set)
    all_levels <- unique(unlist(lapply(dl, function(d) levels(d$data$x_label))))
    combined <- do.call(rbind, lapply(dl, function(d) {
      data.frame(
        x_label = as.character(d$data$x_label),
        y_val   = d$data$y_val,
        source  = d$name,
        stringsAsFactors = FALSE
      )
    }))
    combined$source <- factor(combined$source, levels = sapply(dl, `[[`, "name"))
    strip_ext <- function(x) sub("\\.(json|csv)$", "", x, ignore.case = TRUE)

    # Census bins look like "<age> · <gender>": split them so age becomes the
    # x-axis and gender a panel, instead of interleaving genders along one axis
    split_gender <- all(grepl(" · ", all_levels))
    if (split_gender) {
      age_levels       <- unique(sub(" · .*", "", all_levels))
      combined$x       <- factor(sub(" · .*", "", combined$x_label), levels = age_levels)
      combined$Gender  <- sub(".* · ", "", combined$x_label)
    } else {
      combined$x <- factor(combined$x_label, levels = all_levels)
    }

    if (style == "diff") {
      req(input$viz_reference_source)
      ref_name <- input$viz_reference_source
      req(ref_name %in% combined$source)
      ref <- combined[combined$source == ref_name, c("x_label", "y_val")]
      names(ref)[2] <- "ref_val"
      combined <- combined[combined$source != ref_name, ]
      req(nrow(combined) > 0)
      combined <- merge(combined, ref, by = "x_label", all.x = TRUE)
      combined$ref_val[is.na(combined$ref_val)] <- 0
      combined$y_val <- combined$y_val - combined$ref_val
      combined <- combined[order(combined$source, combined$x), ]
      y_label <- if (is_percent) "Difference (percentage points)" else "Difference (count)"
    }

    colours <- vizSourceColours(levels(droplevels(combined$source)))
    labels  <- setNames(strip_ext(names(colours)), names(colours))

    value_fmt <- if (style == "diff") {
      function(v) paste0(ifelse(v > 0, "+", ""), formatC(v, format = "f", digits = if (is_percent) 2 else 0, big.mark = ","),
                         if (is_percent) " pp" else "")
    } else {
      function(v) paste0(formatC(v, format = "f", digits = if (is_percent) 2 else 0, big.mark = ","),
                         if (is_percent) " %" else "")
    }
    combined$tooltip <- paste0("<b>", htmltools::htmlEscape(strip_ext(as.character(combined$source))), "</b><br>",
                               htmltools::htmlEscape(combined$x_label), ": ", value_fmt(combined$y_val))

    p <- ggplot(combined, aes(x = x, y = y_val, fill = source, group = source,
                              data_id = source, tooltip = tooltip))

    if (style %in% c("lines", "diff")) {
      if (style == "diff") {
        p <- p + geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.6)
      }
      p <- p +
        geom_line_interactive(aes(colour = source, tooltip = strip_ext(as.character(source))),
                              linewidth = 0.9, show.legend = FALSE) +
        geom_point_interactive(size = 2.2, shape = 21, colour = "white", stroke = 0.6) +
        scale_colour_manual(values = colours, guide = "none")
    } else if (style == "dodge") {
      p <- p + geom_col_interactive(position = position_dodge(width = 0.85), width = 0.8,
                        colour = "white", linewidth = 0.2)
    } else {
      p <- p + geom_col_interactive(position = "identity", alpha = input$viz_overlay_alpha,
                        colour = "white", linewidth = 0.2)
    }

    if (style != "diff") {
      p <- p + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
    }
    if (split_gender) {
      p <- p + facet_wrap(~Gender, ncol = 1)
    }

    p <- p +
      scale_x_discrete(drop = FALSE) +
      # data_id on the legend keys/labels lets the legend drive the highlight
      scale_fill_manual_interactive(
        values  = colours,
        data_id = function(breaks) as.character(breaks),
        labels  = function(breaks) lapply(breaks, function(b)
          label_interactive(labels[[b]], data_id = b))
      ) +
      theme_minimal(base_size = 14) +
      labs(
        title    = switch(style,
                          lines = "Overlay Comparison",
                          diff  = paste("Difference to", strip_ext(input$viz_reference_source)),
                          dodge = "Overlay Comparison",
                          bars  = "Overlay Comparison"),
        subtitle = if (style == "diff" && !is_percent)
                     "Absolute counts depend on population size; switch to Percentages to compare distributions"
                   else NULL,
        x        = if (input$viz_bin_type == "census") "Age group" else NULL,
        y        = y_label,
        fill     = "Source"
      ) +
      guides(fill = guide_legend_interactive(ncol = 2, override.aes = list(size = 3.5))) +
      theme(
        axis.text.x        = element_text(angle = 45, hjust = 1),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text         = element_text(face = "bold", hjust = 0),
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold")
      )

    girafe(
      ggobj     = p,
      width_svg = 11,
      height_svg = if (split_gender) 8 else 6,
      options   = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = girafe_css(css = "", line = "stroke-width:3px;"), reactive = TRUE),
        opts_hover_inv(css = "opacity:0.12;"),
        opts_hover_key(css = girafe_css(css = "cursor:pointer;", text = "font-weight:bold;cursor:pointer;"),
                       reactive = TRUE),
        opts_tooltip(css = "background:#fff;color:#222;border:1px solid #ccc;border-radius:4px;padding:6px 8px;font-size:12px;",
                     use_fill = FALSE, opacity = 0.95),
        opts_selection(type = "none"),
        opts_selection_key(type = "none"),
        opts_toolbar(saveaspng = TRUE)
      )
    )
  })

  # Hovering a source in the legend highlights its line/bars: ggiraph reports the
  # hovered legend key, and the data highlight is set to the same source
  observeEvent(input$vizOverlayPlot_key_hovered, {
    session$sendCustomMessage("vizOverlayPlot_hovered_set",
                              as.character(input$vizOverlayPlot_key_hovered %||% character(0)))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Show format selection modal on button click
  observeEvent(input$downloadBinsReport, {
    req(input$fhir_category_col_binning, input$fhir_resource_to_viz_binning)

    showModal(modalDialog(
      title = "Download Bin Report",
      radioButtons("bins_report_format", "Report Format:",
                   choices = c("Separate" = "separate",
                               "Composite" = "composite"),
                   selected = "separate"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("downloadBinsReportFile", "Download",
                       class = "btn btn-primary")
      )
    ))
  })

  output$downloadBinsReportFile <- downloadHandler(
    filename = function() {
      attr <- make_safe_id(input$fhir_category_col_binning)
      paste0("bin_report_", attr, "_", Sys.Date(), ".json")
    },
    content = function(file) {
      fhir_data    <- fhirDataBinning()
      n_bins       <- input$fhir_n_bins
      value_type   <- input$value_types
      selected_resource_type <- input$fhir_resource_to_viz_binning
      selected_attribute     <- input$fhir_category_col_binning
      format       <- input$bins_report_format

      # Build truncate helper
      truncate_label <- function(x, max_chars = 15) {
        ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), "..."), x)
      }

      # Build bin labels
      bin_labels <- setNames(sapply(1:n_bins, function(i) {
        if (value_type == "text") {
          vals <- input[[paste0("bin_", i)]]
          if (!is.null(vals) && length(vals) > 0) truncate_label(vals) else paste("Bin", i)
        } else if (value_type == "num") {
          bin_max <- input[[paste0("bin_", i)]]
          bin_min <- input[[paste0("bin_", i - 1)]] %||% -Inf
          if (is.infinite(bin_min)) paste0("\u2264 ", bin_max) else paste0(bin_min, " \u2013 ", bin_max)
        } else {
          if (i == 1) "False" else "True"
        }
      }), paste0("Bin ", 1:n_bins))

      # Collect all data
      matching_datasets <- names(fhir_data)[grepl(
        paste0("_", selected_resource_type, "$"), names(fhir_data))]

      all_vals <- do.call(rbind, lapply(matching_datasets, function(dn) {
        d <- fhir_data[[dn]]
        if (selected_attribute %in% colnames(d)) {
          data.frame(value = d[[selected_attribute]], stringsAsFactors = FALSE)
        }
      }))

      # Assign bins
      all_vals$bin <- NA_character_
      for (i in 1:n_bins) {
        if (value_type == "text") {
          bv <- input[[paste0("bin_", i)]]
          if (!is.null(bv)) all_vals$bin[all_vals$value %in% bv] <- paste("Bin", i)
        } else if (value_type == "num") {
          bmax <- input[[paste0("bin_", i)]]
          bmin <- input[[paste0("bin_", i - 1)]] %||% -Inf
          all_vals$bin[all_vals$value <= bmax & all_vals$value > bmin] <- paste("Bin", i)
        } else {
          if (i == 1) all_vals$bin[all_vals$value == FALSE] <- paste("Bin", i)
          if (i == 2) all_vals$bin[all_vals$value == TRUE]  <- paste("Bin", i)
        }
      }

      all_vals <- all_vals[!is.na(all_vals$bin), ]

      # Count per bin
      bin_counts <- all_vals %>%
        count(bin, name = "Count") %>%
        mutate(label = bin_labels[bin]) %>%
        as.data.frame()

      total_count <- sum(bin_counts$Count)

      if (format == "separate") {
        # ── Separate format: one stratifier with one stratum per bin ──────────────
        report <- list(
          resourceType = "MeasureReport",
          status       = "complete",
          type         = "summary",
          date         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00"),
          period       = list(
            start = as.character(Sys.Date()),
            end   = as.character(Sys.Date())
          ),
          group = list(list(
            population = list(list(
              code  = list(coding = list(list(
                system = "http://terminology.hl7.org/CodeSystem/measure-population",
                code   = "initial-population"
              ))),
              count = total_count
            )),
            stratifier = list(list(
              code   = list(list(text = selected_attribute)),
              stratum = lapply(seq_len(nrow(bin_counts)), function(i) {
                list(
                  value      = list(text = bin_counts$label[i]),
                  population = list(list(
                    code  = list(coding = list(list(
                      system = "http://terminology.hl7.org/CodeSystem/measure-population",
                      code   = "initial-population"
                    ))),
                    count = bin_counts$Count[i]
                  ))
                )
              })
            ))
          ))
        )

      } else {
        # ── Composite format: one stratifier with component per bin ───────────────
        report <- list(
          resourceType = "MeasureReport",
          status       = "complete",
          type         = "summary",
          date         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00"),
          period       = list(
            start = as.character(Sys.Date()),
            end   = as.character(Sys.Date())
          ),
          group = list(list(
            population = list(list(
              code  = list(coding = list(list(
                system = "http://terminology.hl7.org/CodeSystem/measure-population",
                code   = "initial-population"
              ))),
              count = total_count
            )),
            stratifier = list(list(
              code   = list(list(text = selected_attribute)),
              stratum = lapply(seq_len(nrow(bin_counts)), function(i) {
                list(
                  component = list(
                    list(
                      code  = list(text = selected_attribute),
                      value = list(text = bin_counts$label[i])
                    )
                  ),
                  measureScore = list(value = bin_counts$Count[i]),
                  population   = list(list(
                    code  = list(coding = list(list(
                      system = "http://terminology.hl7.org/CodeSystem/measure-population",
                      code   = "initial-population"
                    ))),
                    count = bin_counts$Count[i]
                  ))
                )
              })
            ))
          ))
        )
      }

      jsonlite::write_json(report, file, pretty = TRUE, auto_unbox = TRUE)
      removeModal()
    }
  )
}
# end server

# 5. Launch the application
shinyApp(ui = ui, server = server)