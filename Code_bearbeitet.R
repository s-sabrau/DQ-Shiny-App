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
    ")),
    tags$script(HTML("
      // Stack all plots
      $(document).on('click','#stackPlots',function(){
        $('.plot_box').css({top:'0px',left:'0px'});
      });
      // Stack selected plots
      $(document).on('click','#stackSelectedPlots',function(){
        var sel = $('#selectedPlotsToStack').val()||[];
        sel.slice(0,2).forEach(function(name){
          $('.plot_box[data-plot-name=\"'+name+'\"]')
            .css({top:'0px',left:'0px'});
        });
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
            textInput("fhir_url", "Server URL:",
                      value = "http://hapi.fhir.org/baseR4"),
            numericInput("max_bundles", "Max Bundles:",
                         value = 10, min = 1, step = 1),
            actionButton("load_fhir", "Load FHIR Data"),
            uiOutput("fhirMappingUI")  # Add this line
          ),
          #br(),
          #h4("Data Integration Centers (Germany)"),
          #leafletOutput("map", height = "400px")
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
          actionButton("stackPlots", "Stack All Plots"),
          br(), br(),
          selectInput("selectedPlotsToStack",
            "Select Two Plots to Stack:", choices = NULL, multiple = TRUE),
          actionButton("stackSelectedPlots", "Stack Selected Plots"),
          br(), br(),
          div(
            id = "plot_area",
            style = "position:relative; height:800px; border:1px solid #DDD; overflow:hidden;",
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

  )  # navbarPage

)  # fluidPage

# 4. Server logic
server <- function(input, output, session) {

  # 4.1 Load JSON data
  loadJsonData <- function(path) {
    jd <- fromJSON(path)
    data.frame(
      Category = jd$Histogram$Category$`@value`,
      Count    = as.numeric(jd$Histogram$Count$`@value`),
      stringsAsFactors = FALSE
    )
  }

  # 4.2 Load CSV data with optional column mapping
  loadCsvData <- function(path, idx) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    if (!all(c("Category","Count") %in% colnames(df))) {
      req(input[[paste0("map_cat_", idx)]])
      category_col <- input[[paste0("map_cat_", idx)]]

      # Count occurrences of each category
      df <- df %>%
        count(Category = .data[[category_col]], name = "Count") %>%
        as.data.frame(stringsAsFactors = FALSE)
    }
    df$Count <- as.numeric(df$Count)
    df[!is.na(df$Count), ]
  }

  # 4.3.1 Dynamic UI: mapping CSV columns
  output$mappingUI <- renderUI({
    req(input$dataFiles)
    fps <- input$dataFiles$datapath
    fns <- input$dataFiles$name
    uiList <- lapply(seq_along(fps), function(i) {
      if (tools::file_ext(fns[i]) == "csv") {
        df0 <- read.csv(fps[i], stringsAsFactors = FALSE)
        #if (!all(c("Category","Count") %in% colnames(df0))) {
        tagList(
          h4(paste("Map columns for", fns[i])),
          selectInput(paste0("map_cat_", i),
                      "Category column:", choices = colnames(df0)),
          #selectInput(paste0("map_cnt_", i),
          #            "Count column:",    choices = colnames(df0))
        )
        #}
      }
    })
    do.call(tagList, uiList)
  })

  # 4.3.2 Dynamic UI: mapping FHIR categories
  output$fhirMappingUI <- renderUI({
    req(input$data_source == "fhir")
    if (!is.null(fhirRawData())) {  # Changed from fhirColumns()
      selectInput("fhir_category_col", "Category column:",
                  choices = colnames(fhirRawData()),
                  selected = colnames(fhirRawData())[1])  # Auto-select first column
    }
  })

  # 4.4a Fetch comprehensive FHIR data using _include and _revinclude
  fhirRawData <- eventReactive(input$load_fhir, {
    req(input$fhir_url, input$max_bundles)

    # Build comprehensive search request
    req_fhir <- fhir_url(url = input$fhir_url, resource = "Patient")
    search_request <- paste0(req_fhir, "?_include=*&_revinclude=*")

    # Execute search
    bundles <- fhir_search(request = search_request,
                           verbose = 0,
                           max_bundles = input$max_bundles)

    # Extract all resource types from bundles
    all_resources <- list()

    # Get unique resource types
    resource_types <- unique(unlist(lapply(bundles, function(bundle) {
      if (!is.null(bundle$entry)) {
        sapply(bundle$entry, function(entry) {
          entry$resource$resourceType
        })
      }
    })))

    # Process each resource type
    for (rt in resource_types) {
      desc <- fhir_table_description(
        resource = rt,
        sep = " || ",
        brackets = character(0),
        rm_empty_cols = FALSE,
        format = "compact"
      )

      df <- fhir_crack(bundles = bundles, design = desc, verbose = 0)

      if (!is.null(df) && nrow(df) > 0) {
        all_resources[[rt]] <- df
      }
    }

    all_resources
  })

  # 4.4b UI for selecting resource type to visualize
  output$fhirResourceTypeUI <- renderUI({
    req(fhirRawData())
    available_resources <- names(fhirRawData())

    if (length(available_resources) > 0) {
      selectInput("fhir_resource_to_viz", "Resource Type to Visualize:",
                  choices = available_resources,
                  selected = available_resources[1])
    }
  })

  # 4.4c Update the mapping UI to show columns from selected resource
  output$fhirMappingUI <- renderUI({
    req(input$data_source == "fhir")
    req(fhirRawData())
    req(input$fhir_resource_to_viz)

    df <- fhirRawData()[[input$fhir_resource_to_viz]]
    if (!is.null(df) && nrow(df) > 0) {
      selectInput("fhir_category_col", "Category column:",
                  choices = colnames(df),
                  selected = colnames(df)[1])
    }
  })

  # 4.4d Process FHIR data based on selected resource and category
  fhirSummary <- reactive({
    req(fhirRawData())
    req(input$fhir_resource_to_viz)
    req(input$fhir_category_col)

    df <- fhirRawData()[[input$fhir_resource_to_viz]]
    req(df)

    category_col <- input$fhir_category_col

    # Handle NA values
    df[[category_col]] <- ifelse(is.na(df[[category_col]]), "unknown", df[[category_col]])

    # Count occurrences
    df %>%
      count(Category = .data[[category_col]], name = "Count") %>%
      as.data.frame(stringsAsFactors = FALSE)
  })

  # 4.5 Aggregate uploaded/FHIR datasets
  allData <- reactive({
    if (input$data_source == "file") {
      req(input$dataFiles)
      fps <- input$dataFiles$datapath
      fns <- input$dataFiles$name
      lapply(seq_along(fps), function(i) {
        ext <- tools::file_ext(fns[i])
        df  <- switch(ext,
          "json" = loadJsonData(fps[i]),
          "csv"  = loadCsvData(fps[i], i),
          NULL
        )
        list(name = fns[i], data = df)
      })
    } else {
      df <- fhirSummary(); req(df)
      list(list(name = paste0("FHIR:", input$fhir_url), data = df))
    }
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
    req(input$combineFiles)
    dl        <- allData()
    sel       <- input$combineFiles
    names_vec <- sapply(dl, `[[`, "name")
    uis <- lapply(sel, function(fn) {
      idx     <- which(names_vec == fn)
      df0     <- dl[[idx]]$data
      safe_fn <- make_safe_id(fn)
      selectInput(paste0("values_", safe_fn),
                  paste("Values for", fn),
                  choices  = unique(df0$Category),
                  selected = unique(df0$Category),
                  multiple = TRUE)
    })
    output$valueSelectors <- renderUI(do.call(tagList, uis))
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
                      min = 0.1, max = 1, value = 1, step = 0.1)
        )
      )
    }))
  })

  observe({
    req(allData())
    dl <- allData()
    for (i in seq_along(dl)) {
      ui_name   <- paste0("plotUI_", i)
      plot_name <- paste0("plot_", i)
      enabled   <- input[[paste0("cb_", i)]]
      chart     <- input[[paste0("pt_", i)]]
      filterCat <- input[[paste0("filter_", i)]]
      alpha     <- input[[paste0("op_", i)]]
      data0     <- dl[[i]]$data

      output[[ui_name]] <- renderUI({
        if (isTRUE(enabled)) plotOutput(plot_name, height = "300px")
      })

      output[[plot_name]] <- renderPlot({
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

        p + labs(title = dl[[i]]$name, x = "Category", y = "Count") +
            theme(panel.background = element_rect(fill = "transparent", colour = NA),
                  plot.background  = element_rect(fill = "transparent", colour = NA),
                  panel.grid       = element_blank())
      }, bg = "transparent")
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

}  # end server

# 5. Launch the application
shinyApp(ui = ui, server = server)
