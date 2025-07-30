#' 02_data_selection UI Function
#'
#' @description A shiny Module for data upload and column mapping.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList fileInput selectInput actionButton radioButtons conditionalPanel updateSelectInput updateRadioButtons
#' @importFrom bslib page_sidebar sidebar card card_header card_body accordion accordion_panel value_box navset_card_tab nav_panel
#' @importFrom DT DTOutput
#' @noRd
#'
mod_02_data_selection_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # Sidebar for controls
    sidebar = sidebar(
      title = "Data Controls",
      width = 350,
      
      # File upload accordion
      accordion(
        id = ns("upload_accordion"),
        open = "upload_panel",
        
        accordion_panel(
          title = "1. Upload Data",
          value = "upload_panel",
          icon = icon("upload"),
          
          fileInput(
            ns("file_upload"),
            "Choose File:",
            accept = c(".csv", ".xlsx", ".xls"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          
          div(
            class = "text-muted small mb-3",
            "Supported: CSV, Excel (.xlsx, .xls)"
          ),
          
          # Upload status
          uiOutput(ns("upload_status_sidebar")),
          
          # Action buttons
          div(
            class = "d-grid gap-2 mt-3",
            actionButton(
              ns("load_example_data"),
              "Load Example Data",
              class = "btn-success btn-sm"
            ),
            actionButton(
              ns("show_sample_data"),
              "View Data Format Guide",
              class = "btn-outline-info btn-sm"
            )
          )
        ),
        
        accordion_panel(
          title = "2. Map Columns",
          value = "mapping_panel",
          icon = icon("arrows-alt-h"),
          
          # Data type selection
          div(
            class = "mb-4",
            h6("Data Type", class = "text-primary fw-bold mb-3"),
            radioButtons(
              ns("data_type"),
              NULL,
              choices = list(
                "Raw Trial Data" = "raw",
                "Structured Data" = "structured"
              ),
              selected = "raw",
              inline = TRUE
            )
          ),
          
          # Required columns section
          div(
            h6("Required Columns", class = "text-success fw-bold mb-3"),
            
            selectInput(
              ns("patient_id_col"),
              "Patient ID:",
              choices = NULL,
              selected = ""
            ),
            
            selectInput(
              ns("date_col"),
              "Date Column:",
              choices = NULL,
              selected = ""
            ),
            
            conditionalPanel(
              condition = "input.data_type == 'raw'",
              ns = ns,
              selectInput(
                ns("visit_col"),
                "Visit Type:",
                choices = NULL,
                selected = ""
              )
            ),
            
            selectInput(
              ns("response_col"),
              "Response Type:",
              choices = NULL,
              selected = ""
            )
          ),
          
          # Optional columns section
          div(
            class = "mt-4",
            h6("Optional Columns", class = "text-info fw-bold mb-3"),
            
            selectInput(
              ns("treatment_type_col"),
              "Treatment Type:",
              choices = NULL,
              selected = ""
            ),

            selectInput(
              ns("response_value_col"),
              "Response Value:",
              choices = NULL,
              selected = ""
            )            
          ),
          
          # Validate button
          div(
            class = "d-grid mt-4",
            actionButton(
              ns("validate_mapping"),
              "Validate Mapping",
              class = "btn-success"
            )
          )
        ),
        
        accordion_panel(
          title = "3. Data Summary",
          value = "summary_panel", 
          icon = icon("chart-bar"),
          
          uiOutput(ns("data_summary"))
        )
      )
    ),
    
    # Main content area
    div(
      class = "h-100",
      
      # Tabbed data preview
      navset_card_tab(
        id = ns("data_tabs"),
        
        nav_panel(
          title = "Raw Data",
          icon = icon("table"),
          div(
            class = "p-3",
            uiOutput(ns("raw_data_info")),
            DT::DTOutput(ns("raw_data_table"), height = "60vh")
          )
        ),
        
        nav_panel(
          title = "Processed Data", 
          icon = icon("filter"),
          div(
            class = "p-3",
            uiOutput(ns("processed_data_info")),
            DT::DTOutput(ns("processed_data_table"), height = "60vh")
          )
        )
      )
    )
  )
}

#' 02_data_selection Server Functions
#'
#' @noRd
#' 
mod_02_data_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      raw_data = NULL,
      processed_data = NULL,
      column_names = NULL,
      file_info = NULL,
      mapping_valid = FALSE
    )
    
    # Load example data
    observeEvent(input$load_example_data, {
      tryCatch({
        # Load the built-in example data
        data("example_data_raw", package = "dashResponse", envir = environment())
        
        values$raw_data <- example_data_raw
        values$column_names <- names(example_data_raw)
        values$file_info <- list(
          name = "example_data_raw.rda",
          size = object.size(example_data_raw)
        )
        values$mapping_valid <- FALSE
        
        # Update column choices for both data types
        column_choices <- create_column_choices(values$column_names, include_empty = TRUE)
        optional_choices <- create_column_choices(values$column_names, include_empty = TRUE, empty_label = "Not specified")
        
        updateSelectInput(session, "patient_id_col", choices = column_choices)
        updateSelectInput(session, "date_col", choices = column_choices)
        updateSelectInput(session, "response_col", choices = column_choices)
        updateSelectInput(session, "visit_col", choices = column_choices)
        updateSelectInput(session, "treatment_type_col", choices = optional_choices)
        updateSelectInput(session, "discontinuation_col", choices = optional_choices)
        updateSelectInput(session, "death_cause_col", choices = optional_choices)
        updateSelectInput(session, "response_value_col", choices = optional_choices)
        
        # Set suggested mappings for the example data
        updateSelectInput(session, "patient_id_col", selected = "patient_id")
        updateSelectInput(session, "date_col", selected = "visit_date")
        updateSelectInput(session, "response_col", selected = "tumor_response")
        updateSelectInput(session, "visit_col", selected = "visit_type")
        updateSelectInput(session, "treatment_type_col", selected = "dose_cohort")
        updateSelectInput(session, "discontinuation_col", selected = "discontinuation_reason")
        updateSelectInput(session, "death_cause_col", selected = "death_cause")
        updateSelectInput(session, "response_value_col", selected = "size_at_assessment")
        
        # Set data type to raw
        updateRadioButtons(session, "data_type", selected = "raw")
        
        # Open mapping accordion panel
        accordion_panel_open("upload_accordion", "mapping_panel")
        
        showNotification("Example data loaded with suggested column mappings!", type = "message", duration = 4)
        
      }, error = function(e) {
        showNotification(paste("Error loading example data:", e$message), type = "error")
      })
    })
    
    # File upload handling
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      ext <- tools::file_ext(input$file_upload$datapath)
      
      tryCatch({
        if (ext == "csv") {
          values$raw_data <- readr::read_csv(input$file_upload$datapath, show_col_types = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          values$raw_data <- readxl::read_excel(input$file_upload$datapath)
        }
        
        values$column_names <- names(values$raw_data)
        values$file_info <- input$file_upload
        values$mapping_valid <- FALSE
        
        # Update column choices for both data types
        column_choices <- create_column_choices(values$column_names, include_empty = TRUE)
        optional_choices <- create_column_choices(values$column_names, include_empty = TRUE, empty_label = "Not specified")
        
        updateSelectInput(session, "patient_id_col", choices = column_choices)
        updateSelectInput(session, "date_col", choices = column_choices)
        updateSelectInput(session, "response_col", choices = column_choices)
        updateSelectInput(session, "visit_col", choices = column_choices)
        updateSelectInput(session, "treatment_type_col", choices = optional_choices)
        updateSelectInput(session, "discontinuation_col", choices = optional_choices)
        updateSelectInput(session, "death_cause_col", choices = optional_choices)
        updateSelectInput(session, "response_value_col", choices = optional_choices)

        # Open mapping accordion panel
        accordion_panel_open("upload_accordion", "mapping_panel")
        
        showNotification("File uploaded successfully! Please map columns.", type = "success", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error uploading file:", e$message), type = "error")
        values$raw_data <- NULL
      })
    })
    
    # Sidebar upload status
    output$upload_status_sidebar <- renderUI({
      if (is.null(values$raw_data)) {
        div(
          class = "alert alert-secondary py-2",
          icon("info-circle"), " No data loaded"
        )
      } else {
        div(
          class = "alert alert-success py-2",
          icon("check-circle"), 
          strong(" Data loaded:"), br(),
          span(values$file_info$name, class = "small"), br(),
          span(paste(nrow(values$raw_data), "rows,", 
                    ncol(values$raw_data), "columns"), class = "small")
        )
      }
    })
    
    # Show sample data modal
    observeEvent(input$show_sample_data, {
      showModal(modalDialog(
        title = "Sample Data Format",
        size = "l",
        includeMarkdown(app_sys("app", "www", "sample_data_format.Rmd")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Validate column mapping
    observeEvent(input$validate_mapping, {
      
      # Debug: Check if data exists
      if (is.null(values$raw_data)) {
        showNotification("No data loaded. Please load data first.", type = "error")
        return()
      }
      
      if (nrow(values$raw_data) == 0) {
        showNotification("Data is empty. Please load valid data.", type = "error")
        return()
      }
      
      # Check required fields based on data type
      if (input$data_type == "raw") {
        required_inputs <- list(
          patient_id_col = input$patient_id_col,
          date_col = input$date_col,
          response_col = input$response_col,
          visit_col = input$visit_col
        )
      } else {
        required_inputs <- list(
          patient_id_col = input$patient_id_col,
          date_col = input$date_col,
          response_col = input$response_col
        )
      }
      
      # Debug: Print all inputs
      cat("=== DEBUGGING COLUMN MAPPING ===\n")
      cat("Data type:", input$data_type, "\n")
      cat("Data dimensions:", nrow(values$raw_data), "x", ncol(values$raw_data), "\n")
      cat("Available columns:", paste(names(values$raw_data), collapse = ", "), "\n")
      
      for (name in names(required_inputs)) {
        value <- required_inputs[[name]]
        cat(name, ":", ifelse(is.null(value), "NULL", 
                             ifelse(length(value) == 0, "EMPTY", 
                                   ifelse(value == "", "BLANK", value))), "\n")
      }
      
      # Check for empty/null values
      empty_fields <- sapply(required_inputs, function(x) {
        is.null(x) || length(x) == 0 || x == ""
      })
      
      if (any(empty_fields)) {
        empty_names <- names(empty_fields)[empty_fields]
        showNotification(paste("Please select values for:", paste(empty_names, collapse = ", ")), 
                         type = "warning")
        return()
      }
      
      # Check if selected columns exist in data
      selected_cols <- unlist(required_inputs)
      missing_cols <- setdiff(selected_cols, names(values$raw_data))
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Selected columns not found in data:", paste(missing_cols, collapse = ", ")), 
                         type = "error")
        return()
      }
      
      # Process data using business logic functions
      tryCatch({
        if (input$data_type == "raw") {
          cat("Calling process_raw_trial_data with parameters:\n")
          
          values$processed_data <- process_raw_trial_data(
            data = values$raw_data,
            patient_col = input$patient_id_col,
            date_col = input$date_col,
            visit_col = input$visit_col,
            response_col = input$response_col,
            treatment_col = if (!is.null(input$treatment_type_col) && input$treatment_type_col != "") input$treatment_type_col else NULL,
            discontinuation_col = if (!is.null(input$discontinuation_col) && input$discontinuation_col != "") input$discontinuation_col else NULL,
            death_cause_col = if (!is.null(input$death_cause_col) && input$death_cause_col != "") input$death_cause_col else NULL,
            response_value_col = if (!is.null(input$response_value_col) && input$response_value_col != "") input$response_value_col else NULL
          )
        } else {
          values$processed_data <- process_structured_data(
            data = values$raw_data,
            patient_col = input$patient_id_col,
            treatment_start_col = input$date_col,
            assessment_date_col = input$date_col,
            response_col = input$response_col
          )
        }
        
        values$mapping_valid <- TRUE
        showNotification("Column mapping successful!", type = "message", duration = 3)
        
      }, error = function(e) {
        cat("ERROR DETAILS:\n")
        cat("Message:", e$message, "\n")
        cat("Call:", deparse(e$call), "\n")
        showNotification(paste("Error in column mapping:", e$message), type = "error")
        values$mapping_valid <- FALSE
      })
    })
    
    # Data summary in sidebar
    output$data_summary <- renderUI({
      if (!values$mapping_valid || is.null(values$processed_data)) {
        div(
          class = "text-muted text-center py-3",
          icon("info-circle"), br(),
          "Complete column mapping to see summary"
        )
      } else {
        # Generate summary using business logic function
        summary_stats <- generate_data_summary(values$processed_data)
        
        div(
          value_box(
            title = "Total Patients",
            value = summary_stats$n_patients,
            showcase = icon("users"),
            theme = "primary",
            height = "80px"
          ),
          
          value_box(
            title = "Total Records",
            value = summary_stats$n_records,
            showcase = icon("table"),
            theme = "info", 
            height = "80px"
          ),
          
          value_box(
            title = "Date Range",
            value = if (!is.null(summary_stats$date_range)) {
              paste(format(summary_stats$date_range$min_date, "%Y-%m"),
                    "to",
                    format(summary_stats$date_range$max_date, "%Y-%m"))
            } else {
              "No valid dates"
            },
            showcase = icon("calendar"),
            theme = "success",
            height = "80px"
          )
        )
      }
    })
    
    # Raw data info and table
    output$raw_data_info <- renderUI({
      if (is.null(values$raw_data)) {
        div(
          class = "text-center text-muted mb-4",
          icon("upload", class = "fa-2x mb-2"), br(),
          h5("No Data Loaded"),
          p("Upload a file or load example data to begin")
        )
      } else {
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            h5("Raw Data Preview", class = "mb-0"),
            tags$small(paste("Showing uploaded data with", nrow(values$raw_data), "rows and", 
                       ncol(values$raw_data), "columns"), class = "text-muted")
          ),
          span(class = "badge bg-primary", "Original Data")
        )
      }
    })
    
    output$raw_data_table <- DT::renderDT({
      req(values$raw_data)
      
      DT::datatable(
        values$raw_data,
        options = list(
          scrollX = TRUE,
          scrollY = "50vh",
          pageLength = 25,
          dom = 'ftip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        class = "table table-striped table-hover table-sm",
        rownames = FALSE
      )
    })
    
    # Processed data info and table
    output$processed_data_info <- renderUI({
      if (!values$mapping_valid || is.null(values$processed_data)) {
        div(
          class = "text-center text-muted mb-4",
          icon("filter", class = "fa-2x mb-2"), br(),
          h5("No Processed Data"),
          p("Complete column mapping and validation to see processed data")
        )
      } else {
        summary_stats <- generate_data_summary(values$processed_data)
        
        div(
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            div(
              h5("Processed Data Preview", class = "mb-0"),
              tags$small(paste("Showing", summary_stats$n_records, "records for", 
                         summary_stats$n_patients, "patients"), class = "text-muted")
            ),
            span(class = "badge bg-success", "Ready for Analysis")
          ),
          
          # Quick summary badges
          div(
            class = "d-flex gap-2 mb-3 flex-wrap",
            if (!is.null(summary_stats$date_range)) {
              span(class = "badge bg-info", 
                   paste("Date Range:", format(summary_stats$date_range$min_date, "%Y-%m"), 
                         "to", format(summary_stats$date_range$max_date, "%Y-%m")))
            },
            if (!is.null(summary_stats$response_distribution) && nrow(summary_stats$response_distribution) > 0) {
              span(class = "badge bg-warning text-dark",
                   paste("Responses:", paste(unique(summary_stats$response_distribution$response[1:min(3, nrow(summary_stats$response_distribution))]), collapse = ", ")))
            },
            if (!is.null(summary_stats$treatment_types) && nrow(summary_stats$treatment_types) > 0) {
              span(class = "badge bg-secondary",
                   paste("Treatments:", nrow(summary_stats$treatment_types), "types"))
            }
          )
        )
      }
    })
    
    output$processed_data_table <- DT::renderDT({
      req(values$processed_data)
      
      DT::datatable(
        values$processed_data,
        options = list(
          scrollX = TRUE,
          scrollY = "50vh", 
          pageLength = 25,
          dom = 'ftip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        class = "table table-striped table-hover table-sm",
        rownames = FALSE
      )
    })
    
    # Return processed data for other modules
    return(
      list(
        data = reactive(values$processed_data),
        is_ready = reactive(values$mapping_valid),
        summary = reactive({
          if (values$mapping_valid && !is.null(values$processed_data)) {
            list(
              n_patients = length(unique(values$processed_data$patient_id)),
              n_records = nrow(values$processed_data),
              columns = names(values$processed_data)
            )
          } else {
            NULL
          }
        }),
        # Add mapping_info for code export
        mapping_info = reactive({
          if (values$mapping_valid) {
            list(
              data_type = input$data_type,
              patient_col = input$patient_id_col,
              date_col = input$date_col,
              visit_col = if (input$data_type == "raw") input$visit_col else NULL,
              response_col = input$response_col,
              treatment_col = if (!is.null(input$treatment_type_col) && input$treatment_type_col != "" && input$treatment_type_col != "Not specified") input$treatment_type_col else NULL,
              response_value_col = if (!is.null(input$response_value_col) && input$response_value_col != "" && input$response_value_col != "Not specified") input$response_value_col else NULL,
              discontinuation_col = if (!is.null(input$discontinuation_col) && input$discontinuation_col != "" && input$discontinuation_col != "Not specified") input$discontinuation_col else NULL,
              death_cause_col = if (!is.null(input$death_cause_col) && input$death_cause_col != "" && input$death_cause_col != "Not specified") input$death_cause_col else NULL
            )
          } else {
            NULL
          }
        })
      )
    )
  })
}