#' 04_code_export UI Function
#'
#' @description A shiny Module for exporting reproducible code.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList selectInput checkboxGroupInput checkboxInput downloadButton verbatimTextOutput
#' @importFrom bslib card card_header card_body
#' @noRd
mod_04_code_export_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        h5("Export Reproducible Code", class = "mb-0"),
        div(
          selectInput(
            ns("export_format"),
            "Format:",
            choices = list(
              "R Markdown (.Rmd)" = "rmd",
              "Quarto (.qmd)" = "qmd", 
              "R Script (.R)" = "r"
            ),
            selected = "rmd",
            width = "150px"
          ),
          downloadButton(
            ns("download_code"),
            "Download",
            class = "btn-success btn-sm ms-2"
          )
        )
      )
    ),
    card_body(
      div(
        style = "height: 70vh; overflow-y: auto;",
        
        # Code preview
        div(
          class = "mb-3",
          h6("Preview:", class = "text-muted"),
          verbatimTextOutput(ns("code_preview"), placeholder = TRUE)
        ),
        
        # Export options
        div(
          class = "row",
          div(
            class = "col-md-6",
            h6("Include Sections:", class = "text-primary"),
            checkboxGroupInput(
              ns("export_sections"),
              "Select Sections:",
              choices = list(
                "Data Processing" = "data_prep",
                "Response Timeline" = "timeline",
                "Waterfall Plot" = "waterfall", 
                "Swimmer Plot" = "swimmer",
                "Summary Statistics" = "summary"
              ),
              selected = c("data_prep", "timeline", "waterfall")
            )
          ),
          div(
            class = "col-md-6",
            h6("Export Options:", class = "text-info"),
            checkboxInput(ns("include_libraries"), "Include Library Calls", TRUE),
            checkboxInput(ns("include_comments"), "Include Comments", TRUE),
            checkboxInput(ns("include_session_info"), "Include Session Info", TRUE),
            checkboxInput(ns("static_versions"), "Create Static Versions (ggplot2)", FALSE)
          )
        )
      )
    )
  )
}

#' 04_code_export Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param data_module Reactive data from mod_02_data_selection
#' @param plot_module Reactive plot configurations from mod_03_plotting
#'
#' @noRd
mod_04_code_export_server <- function(id, data_module, plot_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Generate complete reproducible code
    generated_code <- reactive({
      req(data_module$is_ready())
      
      # Get data processing info
      mapping_info <- data_module$mapping_info()
      
      # Get plot configurations (if available)
      plot_configs <- if (!is.null(plot_module)) plot_module$configs() else NULL
      
      # Generate code sections
      code_sections <- list()
      
      # Header section
      if (input$export_format == "rmd") {
        code_sections$header <- generate_rmd_header()
      } else if (input$export_format == "qmd") {
        code_sections$header <- generate_qmd_header()
      } else {
        code_sections$header <- generate_r_header()
      }
      
      # Libraries section
      if (input$include_libraries) {
        code_sections$libraries <- generate_libraries_code(input$export_format)
      }
      
      # Data processing section
      if ("data_prep" %in% input$export_sections) {
        code_sections$data_prep <- generate_data_processing_code(
          mapping_info, 
          include_comments = input$include_comments,
          format = input$export_format
        )
      }
      
      # Plot sections
      if ("timeline" %in% input$export_sections && !is.null(plot_configs$timeline)) {
        code_sections$timeline <- generate_timeline_code(
          plot_configs$timeline,
          static_version = input$static_versions,
          format = input$export_format
        )
      }
      
      if ("waterfall" %in% input$export_sections && !is.null(plot_configs$waterfall)) {
        code_sections$waterfall <- generate_waterfall_code(
          plot_configs$waterfall,
          static_version = input$static_versions,
          format = input$export_format
        )
      }
      
      # Session info
      if (input$include_session_info) {
        code_sections$session_info <- generate_session_info_code(input$export_format)
      }
      
      # Combine all sections
      paste(code_sections, collapse = "\n\n")
    })
    
    # Code preview
    output$code_preview <- renderText({
      if (data_module$is_ready()) {
        generated_code()
      } else {
        "Please complete data selection and processing first."
      }
    })
    
    # Download handler
    output$download_code <- downloadHandler(
      filename = function() {
        paste0("clinical_trial_analysis_", Sys.Date(), ".", input$export_format)
      },
      content = function(file) {
        writeLines(generated_code(), file)
      }
    )
  })
}

# Helper functions for code generation
generate_data_processing_code <- function(mapping_info, include_comments = TRUE, format = "rmd") {
  
  comments <- if (include_comments) {
    "# This code reproduces the data processing steps from your dashResponse analysis\n"
  } else ""
  
  code_chunk_start <- if (format %in% c("rmd", "qmd")) "```{r data-processing}\n" else ""
  code_chunk_end <- if (format %in% c("rmd", "qmd")) "\n```" else ""
  
  # Build the processing function call based on data type
  if (mapping_info$data_type == "raw") {
    processing_call <- sprintf(
      "processed_data <- process_raw_trial_data(\n  data = raw_data,\n  patient_col = \"%s\",\n  date_col = \"%s\",\n  visit_col = \"%s\",\n  response_col = \"%s\"",
      mapping_info$patient_col,
      mapping_info$date_col,
      mapping_info$visit_col,
      mapping_info$response_col
    )
    
    # Add optional parameters
    if (!is.null(mapping_info$treatment_col)) {
      processing_call <- paste0(processing_call, sprintf(",\n  treatment_col = \"%s\"", mapping_info$treatment_col))
    }
    if (!is.null(mapping_info$response_value_col)) {
      processing_call <- paste0(processing_call, sprintf(",\n  response_value_col = \"%s\"", mapping_info$response_value_col))
    }
    
    processing_call <- paste0(processing_call, "\n)")
    
  } else {
    processing_call <- sprintf(
      "processed_data <- process_structured_data(\n  data = raw_data,\n  patient_col = \"%s\",\n  treatment_start_col = \"%s\",\n  assessment_date_col = \"%s\",\n  response_col = \"%s\"\n)",
      mapping_info$patient_col,
      mapping_info$date_col,
      mapping_info$date_col,
      mapping_info$response_col
    )
  }
  
  paste0(
    code_chunk_start,
    comments,
    "# Load your raw data (replace this with your actual data loading code)\n",
    "# raw_data <- read.csv(\"your_data_file.csv\")\n\n",
    processing_call,
    code_chunk_end
  )
}