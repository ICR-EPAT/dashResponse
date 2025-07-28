#' Data Processing Business Logic Functions
#'
#' Main functions for processing clinical trial data
#'
#' @noRd
#'
#' Process Raw Clinical Trial Data
#'
#' @param data Raw clinical trial data frame
#' @param patient_col Name of patient ID columns
#' @param date_col Name of date column  
#' @param visit_col Name of visit/event type column
#' @param response_col Name of response column
#' @param treatment_col Name of treatment type column (optional)
#' @param discontinuation_col Name of discontinuation reason column (optional)
#' @param death_cause_col Name of death cause column (optional)
#' @param response_value_col Name of response value column (optional)
#' @return Processed data frame with derived treatment dates and responses
#' @export
process_raw_trial_data <- function(data, patient_col, date_col, visit_col, response_col,
                                  treatment_col = NULL, discontinuation_col = NULL, response_value_col = NULL,
                                  death_cause_col = NULL) {
  
  # Input validation - check for NULL or missing data
  if (is.null(data) || nrow(data) == 0) {
    stop("Input data is empty or NULL")
  }
  
  # Validate column names exist
  required_cols <- c(patient_col, date_col, visit_col, response_col)
  validation <- validate_required_columns(data, required_cols)
  if (!validation$valid) {
    stop(validation$message)
  }
  
  # Check for any data in required columns
  if (all(is.na(data[[patient_col]])) || all(data[[patient_col]] == "")) {
    stop("Patient ID column contains no valid data")
  }
  
  if (all(is.na(data[[date_col]]))) {
    stop("Date column contains no valid data")
  }
  
  if (all(is.na(data[[visit_col]])) || all(data[[visit_col]] == "")) {
    stop("Visit type column contains no valid data")
  }
  
  # Clean and standardize the data
  # use bang bang operator to ensure columns are dynamically assigned
  processed_data <- data |>
    dplyr::mutate(
      # Parse dates flexibly
      !!date_col := parse_flexible_dates(.data[[date_col]]),
      # Clean response values
      !!response_col := clean_response_values(.data[[response_col]]),
      # Standardize visit types
      !!visit_col := stringr::str_trim(.data[[visit_col]])
    )
  
  # Extract treatment start dates (C1D1 visits)
  treatment_starts <- processed_data |>
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], stringr::regex("C1D1|Cycle 1 Day 1", ignore_case = TRUE)),
      !is.na(.data[[date_col]])
    ) |>
    dplyr::group_by(.data[[patient_col]]) |>
    dplyr::summarise(
      treatment_start = min(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Check if any treatment start dates were found
  if (nrow(treatment_starts) == 0) {
    warning("No treatment start dates (C1D1 or Cycle 1 Day 1) found in the data")
    # Create empty treatment_starts with proper structure
    treatment_starts <- data.frame(
      !!patient_col := character(0),
      treatment_start = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  }
  
  # Extract treatment end dates
  treatment_ends <- processed_data |>
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], stringr::regex("End of treatment", ignore_case = TRUE)),
      !is.na(.data[[date_col]])
    ) |>
    dplyr::group_by(.data[[patient_col]]) |>
    dplyr::summarise(
      treatment_end = max(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Check if any treatment end dates were found
  if (nrow(treatment_ends) == 0) {
    warning("No treatment end dates found in the data")
    # Create empty treatment_ends with proper structure
    treatment_ends <- data.frame(
      !!patient_col := character(0),
      treatment_end = as.Date(character(0)),
      stringsAsFactors = FALSE
    )
  }
  
  # Extract response assessments only
  response_data <- processed_data |>
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], stringr::regex("Response|Baseline", ignore_case = TRUE))
    )
  
  # Check if any response data was found
  if (nrow(response_data) == 0) {
    stop("No valid response assessment data found. Check that visit types contain 'Response' or 'Baseline' and response values are not missing.")
  }
  
  # Join treatment dates with response data
  final_data <- response_data |>
    dplyr::left_join(treatment_starts, by = patient_col) |>
    dplyr::left_join(treatment_ends, by = patient_col) |>
    dplyr::select(
      patient_id = !!patient_col,
      treatment_start,
      treatment_end,
      assessment_date = !!date_col,
      response = !!response_col,
      visit = !!visit_col,
      dplyr::everything()
    )
  
  # Add optional columns if specified
  if (!is.null(treatment_col) && treatment_col %in% names(data)) {
    final_data <- final_data |>
      dplyr::mutate(treatment_type = .data[[treatment_col]])
  }
  
  if (!is.null(discontinuation_col) && discontinuation_col %in% names(data)) {
    final_data <- final_data |>
      dplyr::mutate(discontinuation_reason = .data[[discontinuation_col]])
  }
  
  if (!is.null(death_cause_col) && death_cause_col %in% names(data)) {
    final_data <- final_data |>
      dplyr::mutate(death_cause = .data[[death_cause_col]])
  }

  if (!is.null(response_value_col) && response_value_col %in% names(data)) {
    # Check if response_value_col has any valid data
    if (all(is.na(data[[response_value_col]]))) {
      warning("Response value column contains no valid data - skipping baseline calculations")
    } else {
      final_data <- final_data |>
        dplyr::mutate(response_value = .data[[response_value_col]])
      
      # Calculate changes from baseline
      final_data <- final_data |>
        dplyr::group_by(patient_id) |>
        dplyr::mutate(
          baseline_value = dplyr::first(.data$response_value[
            stringr::str_detect(.data$visit, stringr::regex("Baseline", ignore_case = TRUE))
          ]),
          absolute_change = ifelse(!is.na(.data$response_value) & !is.na(.data$baseline_value),
                                   .data$response_value - .data$baseline_value, 
                                   NA_real_),
          percent_change = ifelse(!is.na(.data$baseline_value) & .data$baseline_value != 0 & !is.na(.data$response_value),
                                  (.data$response_value - .data$baseline_value) / .data$baseline_value * 100,
                                  NA_real_)
        ) |>
        dplyr::ungroup()
    }
  }
  
  # Final validation
  if (nrow(final_data) == 0) {
    stop("No valid data remains after processing")
  }
  
  return(final_data)
}

#' Process Already Structured Clinical Data
#'
#' @param data Structured clinical data frame
#' @param patient_col Name of patient ID column
#' @param treatment_start_col Name of treatment start column
#' @param assessment_date_col Name of assessment date column
#' @param response_col Name of response column
#' @param treatment_end_col Name of treatment end column (optional)
#' @param treatment_type_col Name of treatment type column (optional)
#' @return Standardized data frame
#' @export
process_structured_data <- function(data, patient_col, treatment_start_col, assessment_date_col, 
                                   response_col, treatment_end_col = NULL, treatment_type_col = NULL) {
  
  # Validate inputs
  required_cols <- c(patient_col, treatment_start_col, assessment_date_col, response_col)
  validation <- validate_required_columns(data, required_cols)
  if (!validation$valid) {
    stop(validation$message)
  }
  
  # Process the structured data
  processed_data <- data |>
    dplyr::mutate(
      # Parse dates
      !!treatment_start_col := parse_flexible_dates(.data[[treatment_start_col]]),
      !!assessment_date_col := parse_flexible_dates(.data[[assessment_date_col]]),
      # Clean responses
      !!response_col := clean_response_values(.data[[response_col]])
    ) |>
    dplyr::select(
      patient_id = !!patient_col,
      treatment_start = !!treatment_start_col,
      assessment_date = !!assessment_date_col,
      response = !!response_col,
      dplyr::everything()
    )
  
  # Add optional columns
  if (!is.null(treatment_end_col) && treatment_end_col %in% names(data)) {
    processed_data <- processed_data |>
      dplyr::mutate(
        treatment_end = parse_flexible_dates(.data[[treatment_end_col]])
      )
  }
  
  if (!is.null(treatment_type_col) && treatment_type_col %in% names(data)) {
    processed_data <- processed_data |>
      dplyr::mutate(treatment_type = .data[[treatment_type_col]])
  }
  
  return(processed_data)
}

#' Calculate Best Overall Response
#'
#' @param data Processed clinical data
#' @return Data frame with best overall response per patient
#' @export
calculate_best_response <- function(data) {
  
  # Response hierarchy (best to worst)
  response_hierarchy <- c("CR", "PR", "SD", "uPD", "PD", "NE")
  
  best_responses <- data |>
    dplyr::filter(!is.na(response), response != "") |>
    dplyr::group_by(patient_id) |>
    dplyr::summarise(
      best_response = {
        responses <- unique(response)
        # Find the best response according to hierarchy
        best_idx <- which(response_hierarchy %in% responses)
        if (length(best_idx) > 0) {
          response_hierarchy[min(best_idx)]
        } else {
          "Unknown"
        }
      },
      n_assessments = dplyr::n(),
      .groups = "drop"
    )
  
  return(best_responses)
}

#' Generate Data Summary Statistics
#'
#' @param data Processed clinical data
#' @return List with summary statistics
#' @export
generate_data_summary <- function(data) {
  
  summary_stats <- list(
    n_patients = length(unique(data$patient_id)),
    n_records = nrow(data),
    date_range = NULL,
    response_distribution = NULL,
    treatment_types = NULL
  )
  
  # Calculate date range
  if ("assessment_date" %in% names(data)) {
    valid_dates <- data$assessment_date[!is.na(data$assessment_date)]
    if (length(valid_dates) > 0) {
      summary_stats$date_range <- list(
        min_date = min(valid_dates),
        max_date = max(valid_dates)
      )
    }
  }
  
  # Response distribution
  if ("response" %in% names(data)) {
    summary_stats$response_distribution <- data |>
      dplyr::filter(!is.na(response), response != "") |>
      dplyr::count(response, sort = TRUE)
  }
  
  # Treatment types
  if ("treatment_type" %in% names(data)) {
    summary_stats$treatment_types <- data |>
      dplyr::filter(!is.na(treatment_type)) |>
      dplyr::distinct(patient_id, treatment_type) |>
      dplyr::count(treatment_type, sort = TRUE)
  }
  
  return(summary_stats)
}