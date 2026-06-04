# -------------------------------------------------------------------------------
# Title:        Interactive Medical Data App
# Authors:      Sarah Braun, Christian Draeger, Lea Michaelis,
#               Sherry Freiesleben, Dagmar Waltemath,
#               Matthias Löbe, Judith Wodke
# Date:         2025-01-08
# Contact:      sarah.braun@med.uni-greifswald.de
# Description:  Shiny dashboard for uploading, combining, visualizing,
#               and summarizing CSV/JSON/FHIR datasets.
# -------------------------------------------------------------------------------

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
  "ggplot2", "leaflet",
  "DT"
)

ensure_pkg(required_pkgs)

# 2. Helper: sanitize dynamic input IDs
make_safe_id <- function(x) {
  id <- gsub("[^[:alnum:]_]", "_", x)
  id <- gsub("_+", "_", id)
  gsub("^_|_$", "", id)
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
                          span("Upload your files and assign each a type. Files will be used in the corresponding tabs."),
                          hr(),
                          tableOutput("dataList")
                        )
                      )
             ),
             # -- Census Data Tab --
             tabPanel("Census Data",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Data Selection"),
                          uiOutput("censusFileSelector"),
                          uiOutput("fhirFileSelector"),
                          h4("Visualization Options"),
                          selectInput("census_chart_type", "Chart Type:",
                                      choices = c("Grouped Bar Chart" = "grouped",
                                                  "Stacked Bar Chart" = "stacked"),
                                      selected = "grouped"),
                          checkboxInput("census_show_values", "Show Values on Bars", FALSE),
                          checkboxInput("census_log_scale", "Log Scale (Y-axis)", FALSE),
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
                          DT::dataTableOutput("censusDataTable"),
                          hr(),
                          h4("FHIR Patient Data Summary"),
                          tableOutput("fhirSummaryTable"),
                          hr(),
                          h4("Raw FHIR Patient Data"),
                          DT::dataTableOutput("fhirDataTable")
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
                                   choices = c("Absolute Counts" = "absolute",
                                               "Percentages" = "percent"),
                                   selected = "absolute")
                    ),
                    uiOutput("fhirValuesUIBinning")
                  ),
                  mainPanel(
                    h4("Visualisation of the fhir data in bins incoming"),
                    plotOutput("plotBins", height = "400px")
                  )
                ),
             ),
             # -- Visualization Tab --
             tabPanel("Visualization",
                      fluidRow(
                        column(12,
                               div(style = "margin-bottom: 15px;",
                                   actionButton("arrangeSideBySide", "Arrange Side by Side",
                                                class = "btn btn-primary"),
                                   actionButton("stackPlots", "Stack All Plots",
                                                class = "btn btn-secondary"),
                                   br(), br(),
                                   selectInput("selectedPlotsToStack",
                                               "Select Two Plots to Stack:", choices = NULL, multiple = TRUE,
                                               width = "300px"),
                                   actionButton("stackSelectedPlots", "Stack Selected Plots",
                                                class = "btn btn-info")
                               ),
                               div(
                                 id = "plot_area",
                                 style = "position:relative; height:800px; border:1px solid #DDD; overflow:auto; padding:10px;",
                                 uiOutput("plotsUI")
                               )
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
  loadJsonData <- function(path) {
    tryCatch({
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
  loadCsvData <- function(path, idx) {
    df <- read.csv(path, stringsAsFactors = FALSE)

    # If the CSV already has Category and Count columns, use them directly
    if (all(c("Category", "Count") %in% colnames(df))) {
      df$Count <- as.numeric(df$Count)
      return(df[!is.na(df$Count), ])
    }

    # Otherwise, we need column mapping
    category_col <- input[[paste0("map_cat_", idx)]]

    # If no category column is selected yet, return empty data frame
    if (is.null(category_col) || category_col == "") {
      return(data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE))
    }

    # Check if the selected column exists in the current data frame
    if (!category_col %in% colnames(df)) {
      return(data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE))
    }

    # Count occurrences of each category
    tryCatch({
      result <- df %>%
        count(Category = .data[[category_col]], name = "Count") %>%
        as.data.frame(stringsAsFactors = FALSE)

      result$Count <- as.numeric(result$Count)
      result[!is.na(result$Count), ]
    }, error = function(e) {
      # If there's any error, return empty data frame
      data.frame(Category = character(), Count = numeric(), stringsAsFactors = FALSE)
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
    req(input$dataFiles)
    fps <- input$dataFiles$datapath
    fns <- input$dataFiles$name
    uiList <- lapply(seq_along(fps), function(i) {
      if (tools::file_ext(fns[i]) == "csv") {
        df0 <- read.csv(fps[i], stringsAsFactors = FALSE)

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

        #if (!all(c("Category","Count") %in% colnames(df0))) {
        tagList(
          h4(paste("Map columns for", fns[i])),
          selectInput(paste0("map_cat_", i),
                      "Category column:", choices = sorted_cols),
          #selectInput(paste0("map_cnt_", i),
          #            "Count column:",    choices = sorted_cols)
        )
        #}
      }
    })
    do.call(tagList, uiList)
  })

  # 4.3.2
  output$censusFileSelector <- renderUI({
    files <- uploadedFiles()
    census_files <- Filter(function(f) f$type == "census", files)
    if (length(census_files) == 0) {
      p("No census files uploaded yet. Please upload in the Data Upload tab.",
        style = "color:#999; font-size:12px;")
    } else {
      selectInput("selected_census_file", "Census File:",
                  choices = setNames(
                    sapply(census_files, `[[`, "path"),
                    sapply(census_files, `[[`, "name")
                  ))
    }
  })

  output$fhirFileSelector <- renderUI({
    files <- uploadedFiles()
    fhir_files <- Filter(function(f) f$type == "fhir", files)
    if (length(fhir_files) == 0) {
      p("No FHIR files uploaded yet. Please upload in the Data Upload tab.",
        style = "color:#999; font-size:12px;")
    } else {
      selectInput("selected_fhir_file", "FHIR File:",
                  choices = setNames(
                    sapply(fhir_files, `[[`, "path"),
                    sapply(fhir_files, `[[`, "name")
                  ))
    }
  })

  loadCensusData <- function(path) {
    tryCatch({
      census_json <- fromJSON(path, simplifyVector = FALSE)

      # Navigate to stratum list
      stratum_list <- NULL

      if (!is.null(census_json$group)) {
        if (is.list(census_json$group) && length(census_json$group) > 0) {
          first_group <- if(is.list(census_json$group[[1]])) {
            census_json$group[[1]]
          } else {
            census_json$group
          }

          if (!is.null(first_group$stratifier)) {
            if (is.list(first_group$stratifier) && length(first_group$stratifier) > 0) {
              first_stratifier <- if(is.list(first_group$stratifier[[1]])) {
                first_group$stratifier[[1]]
              } else {
                first_group$stratifier
              }
              stratum_list <- first_stratifier$stratum
            }
          }
        }
      }

      if (is.null(stratum_list) || length(stratum_list) == 0) {
        warning("No stratum data found in census file")
        return(NULL)
      }

      # Extract age and gender from each stratum
      census_data <- lapply(stratum_list, function(stratum) {
        components <- stratum$component

        if (is.null(components) || length(components) < 2) {
          return(NULL)
        }

        # Extract age (component 1) and gender (component 2)
        age <- if (!is.null(components[[1]]$value$text)) {
          components[[1]]$value$text
        } else {
          NA
        }

        gender <- if (!is.null(components[[2]]$value$text)) {
          components[[2]]$value$text
        } else {
          NA
        }

        # Extract count from measureScore or population
        count <- 0
        if (!is.null(stratum$measureScore$value)) {
          count <- as.numeric(stratum$measureScore$value)
        } else if (!is.null(stratum$population)) {
          pop_list <- stratum$population
          if (is.list(pop_list) && length(pop_list) > 0) {
            count <- as.numeric(pop_list[[1]]$count %||% 0)
          }
        }

        data.frame(
          Age = age,
          Gender = gender,
          Count = count,
          stringsAsFactors = FALSE
        )
      })

      # Combine all data frames
      census_df <- do.call(rbind, Filter(Negate(is.null), census_data))

      if (is.null(census_df) || nrow(census_df) == 0) {
        warning("No valid census data extracted")
        return(NULL)
      }

      # Clean data
      census_df <- census_df[!is.na(census_df$Age) & !is.na(census_df$Gender), ]
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
    req(input$data_source)
    files <- uploadedFiles()
    csv_json_files <- Filter(function(f) f$type == "csv_json", files)

    if (input$data_source == "file") {
      if (length(csv_json_files) == 0) return(list())

      results <- lapply(seq_along(csv_json_files), function(i) {
        f   <- csv_json_files[[i]]
        ext <- tools::file_ext(f$name)

        df <- tryCatch({
          switch(ext,
                 "json" = loadJsonData(f$path),
                 "csv"  = loadCsvData(f$path, i),
                 NULL
          )
        }, error = function(e) {
          warning(paste("Error processing file", f$name, ":", e$message))
          NULL
        })

        if (!is.null(df) && nrow(df) > 0) {
          list(name = f$name, data = df)
        } else {
          NULL
        }
      })

      results[!sapply(results, is.null)]

    } else if (input$data_source == "fhir") {
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
  censusData <- reactive({
    req(input$selected_census_file)
    census_df <- loadCensusData(input$selected_census_file)
    if (is.null(census_df)) {
      showNotification("Failed to load census data.", type = "error")
      return(NULL)
    }
    census_df$Source <- "Census"
    showNotification(paste("Loaded", nrow(census_df), "census records"), type = "message")
    return(census_df)
  })

  fhirPatientData <- reactive({
    req(censusData())

    census_df            <- censusData()
    census_age_labels    <- unique(census_df$Age)
    census_gender_labels <- unique(census_df$Gender)

    tryCatch({
      req(input$selected_fhir_file)
      raw <- jsonlite::fromJSON(input$selected_fhir_file, simplifyVector = FALSE)

      entries <- NULL
      if (!is.null(raw$resourceType) && raw$resourceType == "Bundle") {
        entries <- raw$entry
      } else if (is.list(raw)) {
        entries <- raw
      }

      if (is.null(entries) || length(entries) == 0) {
        showNotification("No entries found in FHIR bundle.", type = "warning")
        return(NULL)
      }

      patients <- Filter(function(e) {
        res <- if (!is.null(e$resource)) e$resource else e
        !is.null(res$resourceType) && res$resourceType == "Patient"
      }, entries)

      if (length(patients) == 0) {
        showNotification("No Patient resources found in FHIR bundle.", type = "warning")
        return(NULL)
      }

      records <- lapply(patients, function(e) {
        p      <- if (!is.null(e$resource)) e$resource else e
        bd     <- p$birthDate %||% NA_character_
        gender <- p$gender    %||% NA_character_
        list(birthDate = as.character(bd), gender = as.character(gender))
      })

      df <- data.frame(
        birthDate = sapply(records, `[[`, "birthDate"),
        gender    = sapply(records, `[[`, "gender"),
        stringsAsFactors = FALSE
      )

      today <- Sys.Date()

      df$age_numeric <- sapply(df$birthDate, function(bd) {
        if (is.null(bd) || is.na(bd) || !nzchar(trimws(bd))) return(NA_real_)

        bd <- trimws(bd)
        bd <- sub("T.*$", "", bd)

        bd_padded <- if (nchar(bd) == 4)      paste0(bd, "-01-01")
        else if (nchar(bd) == 7) paste0(bd, "-01")
        else                     bd

        dob <- tryCatch(as.Date(bd_padded), error = function(e) NA)

        if (is.null(dob) || length(dob) == 0 || is.na(dob)) return(NA_real_)
        if (dob >= today || dob < as.Date("1900-01-01"))     return(NA_real_)

        year_diff       <- as.numeric(format(today, "%Y")) - as.numeric(format(dob, "%Y"))
        birthday_passed <- format(today, "%m-%d") >= format(dob, "%m-%d")
        as.numeric(year_diff - ifelse(birthday_passed, 0L, 1L))
      })

      df$Age    <- bin_age_to_census_groups(df$age_numeric, census_age_labels)
      df$Gender <- map_fhir_gender(df$gender, census_gender_labels)
      df        <- df[!is.na(df$Age) & !is.na(df$Gender), ]

      if (nrow(df) == 0) {
        showNotification(
          "FHIR patients could not be matched to census Age/Gender groups.",
          type = "warning"
        )
        return(NULL)
      }

      result <- df %>%
        dplyr::count(Age, Gender, name = "Count") %>%
        as.data.frame(stringsAsFactors = FALSE)

      result$Source <- "FHIR"

      showNotification(
        paste0("FHIR: ", nrow(df), " patients matched across ",
               nrow(result), " Age×Gender groups"),
        type = "message"
      )

      return(result)

    }, error = function(e) {
      showNotification(paste("Error parsing FHIR bundle:", e$message), type = "error")
      return(NULL)
    })
  })

  # Census plot
  output$censusPlot <- renderPlot({
    req(censusData())

    census_df  <- censusData()
    fhir_df    <- fhirPatientData()     # NULL if no FHIR file uploaded yet — that's fine

    chart_type  <- input$census_chart_type
    show_values <- input$census_show_values

    # ── Combine census + FHIR (if available) ────────────────────────────────────
    if (!is.null(fhir_df)) {
      # Keep only Age groups and Genders present in census so axes stay consistent
      fhir_df <- fhir_df[fhir_df$Age    %in% unique(census_df$Age) &
                           fhir_df$Gender %in% unique(census_df$Gender), ]

      plot_df <- rbind(
        census_df[, c("Age", "Gender", "Count", "Source")],
        fhir_df  [, c("Age", "Gender", "Count", "Source")]
      )

      # Interaction label used for fill: e.g. "female · Census", "male · FHIR"
      plot_df$fill_group <- paste(plot_df$Gender, "\u00b7", plot_df$Source)

      # Colour palette: one hue per gender (from Set2), light shade = Census, dark = FHIR
      genders      <- sort(unique(census_df$Gender))
      base_colours <- RColorBrewer::brewer.pal(max(3, length(genders)), "Set2")[seq_along(genders)]

      # Census uses Set2 (soft greens/blues/oranges), FHIR uses Dark2 (bold versions)
      census_colours <- RColorBrewer::brewer.pal(max(3, length(genders)), "Set2")[seq_along(genders)]
      fhir_colours   <- RColorBrewer::brewer.pal(max(3, length(genders)), "Dark2")[seq_along(genders)]

      fill_vals <- c()
      for (k in seq_along(genders)) {
        census_lbl <- paste(genders[k], "\u00b7 Census")
        fhir_lbl   <- paste(genders[k], "\u00b7 FHIR")
        fill_vals[census_lbl] <- census_colours[k]
        fill_vals[fhir_lbl]   <- fhir_colours[k]
      }

      overlay_active <- TRUE
    } else {
      # No FHIR data — plot census only exactly as before
      plot_df <- census_df[, c("Age", "Gender", "Count", "Source")]
      plot_df$fill_group <- plot_df$Gender

      fill_vals      <- NULL    # fall back to scale_fill_brewer below
      overlay_active <- FALSE
    }

    plot_df <- plot_df %>%
      group_by(Source) %>%
      mutate(Percent = round(Count / sum(Count, na.rm = TRUE) * 100, 2)) %>%
      ungroup()

    age_order <- unique(plot_df$Age[order(as.numeric(sub("[-+].*", "", plot_df$Age)))])
    plot_df$Age <- factor(plot_df$Age, levels = age_order)
    # ── Base ggplot ──────────────────────────────────────────────────────────────
    p <- ggplot(plot_df, aes(x = Age, y = Percent, fill = fill_group)) +
      theme_minimal(base_size = 14) +
      labs(
        title = if (overlay_active) "Population by Age Group and Gender  (Census vs FHIR)"
        else                "Population by Age Group and Gender",
        x     = "Age Group",
        y = "Population (%)",
        fill  = if (overlay_active) "Gender \u00b7 Source" else "Gender"
      ) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title   = element_text(hjust = 0.5, face = "bold", size = 16)
      )

    # Apply manual colours when FHIR overlay is active
    if (overlay_active) {
      p <- p + scale_fill_manual(values = fill_vals)
    } else {
      p <- p + scale_fill_brewer(palette = "Set2")
    }

    # ── Geoms based on chart type ────────────────────────────────────────────────
    dodge_width <- if (overlay_active) 0.85 else 0.9   # slightly tighter when doubled

    if (chart_type == "grouped" || chart_type == "dodged") {
      p <- p + geom_bar(stat = "identity",
                        position = position_dodge(width = dodge_width),
                        alpha = 1,
                        colour = "white", linewidth = 0.2)
      if (show_values) {
        p <- p + geom_text(aes(label = Count),
                           position = position_dodge(width = dodge_width),
                           vjust = -0.4, size = 2.8)
      }

    } else if (chart_type == "stacked") {
      p <- ggplot(plot_df, aes(x = Age, y = Percent, fill = fill_group)) +
        theme_minimal(base_size = 14) +
        labs(
          title = if (overlay_active) "Population by Age Group and Gender  (Census vs FHIR)"
          else "Population by Age Group and Gender",
          x     = "Age Group",
          y     = "Population (%)",
          fill  = if (overlay_active) "Gender · Source" else "Gender"
        ) +
        theme(
          axis.text.x  = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title   = element_text(hjust = 0.5, face = "bold", size = 16)
        ) +
        geom_bar(stat = "identity", position = "stack", alpha = 1,
                 colour = "white", linewidth = 0.2) +
        {if (overlay_active) facet_wrap(~Source, ncol = 2) else NULL} +
        {if (overlay_active) scale_fill_manual(values = fill_vals)
          else scale_fill_brewer(palette = "Set2")}

      if (show_values) {
        p <- p + geom_text(aes(label = paste0(Percent, "%")),
                           position = position_stack(vjust = 0.5), size = 2.8)
      }
    }

    if (isTRUE(input$census_log_scale)) {
      p <- p + scale_y_continuous(
        trans = "log10",
        labels = scales::comma
      )
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

    # Create summary statistics
    summary_df <- df %>%
      group_by(Gender) %>%
      summarise(
        Total_Population = sum(Count, na.rm = TRUE),
        Age_Groups = n_distinct(Age),
        Average_per_Group = round(mean(Count, na.rm = TRUE), 0)
      ) %>%
      as.data.frame()

    return(summary_df)
  })

  # Census data table
  output$censusDataTable <- DT::renderDataTable({
    req(censusData())

    df <- censusData()

    # Sort age groups numerically
    age_order <- unique(df$Age[order(as.numeric(sub("[-+].*", "", df$Age)))])
    df$Age <- factor(df$Age, levels = age_order)
    df <- df[order(df$Age), ]
    df$Age <- as.character(df$Age)  # convert back so DT renders it cleanly

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

  output$fhirSummaryTable <- renderTable({
    req(fhirPatientData())

    df <- fhirPatientData()

    df %>%
      group_by(Gender) %>%
      summarise(
        Total_Patients = sum(Count, na.rm = TRUE),
        Age_Groups = n_distinct(Age),
        Average_per_Group = round(mean(Count, na.rm = TRUE), 0)
      ) %>%
      as.data.frame()
  })

  output$fhirDataTable <- DT::renderDataTable({
    req(fhirPatientData())

    df <- fhirPatientData()

    # Sort age groups numerically
    age_order <- unique(df$Age[order(as.numeric(sub("[-+].*", "", df$Age)))])
    df$Age <- factor(df$Age, levels = age_order)
    df <- df[order(df$Age), ]
    df$Age <- as.character(df$Age)

    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list()
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
    # Parse each label into a (low, high) pair
    # Supported formats:  "0-17"  "18-34"  "65+"  "under 18"  "80 and over"
    parse_label <- function(lbl) {
      lbl <- trimws(lbl)
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
        print("bin: ", c(0, hi))
        return(c(0, hi))
      }
      # Pattern: "18-34" or "18 to 34" or "18–34"
      m <- regmatches(lbl, regexpr("^(\\d+)\\s*[-–to]+\\s*(\\d+)$", lbl))
      if (length(m) == 1) {
        nums <- as.numeric(regmatches(m, gregexpr("\\d+", m))[[1]])
        return(c(nums[1], nums[2]))
      }
      # Single number label – exact match
      if (grepl("^\\d+$", lbl)) {
        n <- as.numeric(lbl)
        return(c(n, n))
      }
      return(c(NA, NA))
    }

    # Build lookup table once
    bounds <- lapply(census_age_labels, parse_label)

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
}
# end server

# 5. Launch the application
shinyApp(ui = ui, server = server)