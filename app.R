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
  "geodata", "terra"
)

ensure_pkg(required_pkgs)

# 2. Helper: sanitize dynamic input IDs
make_safe_id <- function(x) {
  id <- gsub("[^[:alnum:]_]", "_", x)
  id <- gsub("_+", "_", id)
  gsub("^_|_$", "", id)
}

# 3. UI definition
ui <- fluidPage(
  theme = shinytheme("spacelab"),

  # Custom CSS & JS
  tags$head(
    tags$style(HTML("
    .plot_box {
      width: 300px;
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
    // Function to arrange plots side by side
    function arrangeSideBySide() {
      var plots = $('.plot_box');
      var containerWidth = $('#plot_area').width() - 20; // Account for padding
      var plotWidth = 320; // 300px + padding + border
      var plotHeight = 350; // Approximate height including padding
      var plotsPerRow = Math.floor(containerWidth / plotWidth);

      plots.each(function(index) {
        var row = Math.floor(index / plotsPerRow);
        var col = index % plotsPerRow;
        var left = col * plotWidth;
        var top = row * plotHeight;

        $(this).css({
          top: top + 'px',
          left: left + 'px'
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
                          radioButtons("data_source", "Data Source:",
                                       choices = c("File (CSV/JSON)" = "file",
                                                   "FHIR (HAPI Test Server)" = "fhir")
                          ),
                          conditionalPanel(
                            "input.data_source=='file'",
                            h4("Upload Files"),
                            fileInput("dataFiles", "Select CSV or JSON Files",
                                      accept = c(".csv", ".json"), multiple = TRUE),
                            uiOutput("mappingUI")
                          ),
                          conditionalPanel(
                            "input.data_source=='fhir'",
                            h4("FHIR Settings"),
                            radioButtons("fhir_input_type", "FHIR Input Type:",
                                         choices = c("API Request" = "api", "File Upload" = "file"),
                                         selected = "api"),
                            conditionalPanel(
                              "input.fhir_input_type=='api'",
                              textInput("fhir_url", "Server URL:",
                                        value = "http://hapi.fhir.org/baseR4"),
                              numericInput("max_bundles", "Max Bundles:",
                                           value = 10, min = 1, step = 1),
                              actionButton("load_fhir", "Load FHIR Data")
                            ),
                            conditionalPanel(
                              "input.fhir_input_type=='file'",
                              fileInput("fhirFiles", "Select FHIR JSON Files",
                                        accept = c(".json"), multiple = TRUE)
                            ),
                            uiOutput("fhirResourceTypeUI"),
                            uiOutput("fhirMappingUI")
                          )
                        ),
                        mainPanel(
                          h4("Uploaded / Loaded Datasets"),
                          tableOutput("dataList")
                        )
                      )
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

  # 4.4a Fetch comprehensive FHIR data using _include and _revinclude

  # Replace the fhirRawData function with this corrected version:

  fhirRawData <- reactive({
    if (input$data_source == "fhir") {
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
    }
    return(NULL)
  })

  # 4.4b UI for selecting resource type to visualize
  output$fhirResourceTypeUI <- renderUI({
    if (input$data_source == "fhir") {
      fhir_data <- fhirRawData()
      if (!is.null(fhir_data)) {
        available_resources <- names(fhir_data)

        if (length(available_resources) > 0) {
          if (input$fhir_input_type == "api") {
            selectInput("fhir_resource_to_viz", "Resource Type to Visualize:",
                        choices = available_resources,
                        selected = available_resources[1])
          } else {
            # For file uploads, show info about available resource types
            resource_types <- unique(sapply(available_resources, function(x) {
              parts <- strsplit(x, "_")[[1]]
              if (length(parts) > 1) parts[length(parts)] else x
            }))
            tags$div(
              h5("Available Resource Types:"),
              tags$ul(lapply(resource_types, function(x) tags$li(x)))
            )
          }
        }
      }
    }
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
          # For file uploads, get all unique columns across all datasets
          all_columns <- unique(unlist(lapply(fhir_data, colnames)))
          if (length(all_columns) > 0) {
            # Sort columns by resource type prefix
            prefixed_cols <- all_columns[grepl("\\.", all_columns)]
            non_prefixed_cols <- all_columns[!grepl("\\.", all_columns)]

            if (length(prefixed_cols) > 0) {
              resource_groups <- split(prefixed_cols, sapply(prefixed_cols, function(x) {
                strsplit(x, "\\.")[[1]][1]
              }))

              sorted_prefixed <- unlist(lapply(sort(names(resource_groups)), function(res_type) {
                cols <- resource_groups[[res_type]]
                basic_pattern <- paste0("^", res_type, "\\.(resourceType|id|meta\\.)")
                basic_cols <- cols[grepl(basic_pattern, cols)]
                other_cols <- cols[!grepl(basic_pattern, cols)]
                c(sort(basic_cols), sort(other_cols))
              }))

              sorted_cols <- c(sort(non_prefixed_cols), sorted_prefixed)
            } else {
              sorted_cols <- sort(all_columns)
            }

            selectInput("fhir_category_col", "Category column:",
                        choices = sorted_cols,
                        selected = sorted_cols[1])
          }
        }
      }
    }
  })



  # Also update the fhirMappingUI to reset when resource type changes:
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
          # For file uploads, get all unique columns across all resources
          all_columns <- unique(unlist(lapply(fhir_data, colnames)))
          if (length(all_columns) > 0) {
            selectInput("fhir_category_col", "Category column:",
                        choices = all_columns,
                        selected = all_columns[1])
          }
        }
      }
    }
  })

  # 4.5 Aggregate uploaded/FHIR datasets
  allData <- reactive({
    if (input$data_source == "file") {
      # Existing file logic unchanged
      req(input$dataFiles)
      fps <- input$dataFiles$datapath
      fns <- input$dataFiles$name

      results <- lapply(seq_along(fps), function(i) {
        ext <- tools::file_ext(fns[i])

        df <- tryCatch({
          switch(ext,
                 "json" = loadJsonData(fps[i]),
                 "csv"  = loadCsvData(fps[i], i),
                 NULL
          )
        }, error = function(e) {
          warning(paste("Error processing file", fns[i], ":", e$message))
          NULL
        })

        if (!is.null(df) && nrow(df) > 0) {
          list(name = fns[i], data = df)
        } else {
          NULL
        }
      })

      results[!sapply(results, is.null)]

    } else if (input$data_source == "fhir") {
      # FHIR data source handling
      fhir_data <- fhirRawData()
      if (!is.null(fhir_data) && length(fhir_data) > 0) {
        results <- list()

        if (input$fhir_input_type == "file") {
          # For file uploads, each dataset creates one entry
          req(input$fhir_category_col)

          for (dataset_key in names(fhir_data)) {
            df <- fhir_data[[dataset_key]]
            category_col <- input$fhir_category_col

            # ONLY process datasets that actually have the selected column
            if (category_col %in% colnames(df)) {
              cat("Processing dataset:", dataset_key, "\n")
              cat("  Rows in dataset:", nrow(df), "\n")
              cat("  Looking for column:", category_col, "\n")

              df[[category_col]] <- ifelse(is.na(df[[category_col]]) | df[[category_col]] == "", "unknown", as.character(df[[category_col]]))

              result_df <- df %>%
                count(Category = .data[[category_col]], name = "Count") %>%
                as.data.frame(stringsAsFactors = FALSE)

              if (nrow(result_df) > 0) {
                cat("  Created result with", nrow(result_df), "categories\n")
                results[[length(results) + 1]] <- list(name = dataset_key, data = result_df)
              }
            } else {
              cat("Skipping dataset:", dataset_key, "(column not found)\n")
            }
          }
        } else {
          # API logic - single combined dataset
          if (!is.null(input$fhir_resource_to_viz) &&
              !is.null(input$fhir_category_col)) {

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
          # Get current input values
          chart     <- input[[paste0("pt_", idx)]]
          filterCat <- input[[paste0("filter_", idx)]]
          alpha     <- input[[paste0("op_", idx)]]

          # Get the data
          data0 <- f$data

          # Apply filter if selected
          df0 <- if (!is.null(filterCat) && length(filterCat) > 0) {
            data0[data0$Category %in% filterCat, ]
          } else {
            data0
          }

          # Make sure we have data to plot
          if (nrow(df0) == 0) {
            plot.new()
            text(0.5, 0.5, "No data to display", cex = 1.5)
            return()
          }

          # Create the plot based on chart type
          p_base <- ggplot(df0, aes(x = Category, y = Count, fill = Category)) +
            theme_minimal(base_size = 14) +
            scale_y_continuous(limits = c(0, globalMax()))

          p <- switch(chart,
                      "Histogram" = p_base + geom_bar(stat = "identity", alpha = alpha),
                      "Pie Chart" = ggplot(df0, aes(x = "", y = Count, fill = Category)) +
                        geom_bar(stat = "identity", alpha = alpha, width = 1) +
                        coord_polar("y", start = 0),
                      "Line Chart" = ggplot(df0, aes(x = Category, y = Count, group = 1)) +
                        geom_line(size = 1.2, alpha = alpha) +
                        geom_point(size = 3, alpha = alpha)
          )

          p + labs(title = f$name, x = "Category", y = "Count") +
            theme(panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background  = element_rect(fill = "transparent", colour = NA),
                  panel.grid       = element_blank())
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
            # ADD THIS DOWNLOAD BUTTON:
            downloadButton(paste0("download_", safe_i), "Export JSON",
                           class = "btn btn-sm btn-outline-secondary",
                           style = "width: 100%; margin-top: 10px;")
        )
      )
    }))
  })

  observe({
    req(allData())
    dl <- allData()

    # Use local() to create proper closures for each iteration
    for (i in seq_along(dl)) {
      local({
        idx <- i
        f <- dl[[idx]]

        ui_name   <- paste0("plotUI_", idx)
        plot_name <- paste0("plot_", idx)

        output[[ui_name]] <- renderUI({
          enabled <- input[[paste0("cb_", idx)]]
          if (isTRUE(enabled)) plotOutput(plot_name, height = "300px")
        })

        output[[plot_name]] <- renderPlot({
          enabled <- input[[paste0("cb_", idx)]]
          chart     <- input[[paste0("pt_", idx)]]
          filterCat <- input[[paste0("filter_", idx)]]
          alpha     <- input[[paste0("op_", idx)]]
          data0     <- f$data  # Now this is captured in the local scope

          df0 <- if (!is.null(filterCat) && length(filterCat) > 0) {
            data0[data0$Category %in% filterCat, ]
          } else data0

          p_base <- ggplot(df0, aes(x = Category, y = Count, fill = Category)) +
            theme_minimal(base_size = 14) +
            scale_y_continuous(limits = c(0, globalMax()))

          p <- switch(chart,
                      "Histogram" = p_base + geom_bar(stat = "identity", alpha = alpha),
                      "Pie Chart" = ggplot(df0, aes(x = "", y = Count, fill = Category)) +
                        geom_bar(stat = "identity", alpha = alpha, width = 1) +
                        coord_polar("y", start = 0),
                      "Line Chart" = ggplot(df0, aes(x = Category, y = Count, group = 1)) +
                        geom_line(size = 1.2, alpha = alpha) +
                        geom_point(size = 3, alpha = alpha)
          )

          p + labs(title = f$name, x = "Category", y = "Count") +
            theme(panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background  = element_rect(fill = "transparent", colour = NA),
                  panel.grid       = element_blank())
        }, bg = "transparent")
      })
    }
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

}
# end server

# 5. Launch the application
shinyApp(ui = ui, server = server)