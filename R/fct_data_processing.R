#' Data Processing Business Logic Functions
#'
#' Main functions for processing clinical trial data
#'
#' @noRd
#'
#' Process Raw Clinical Trial Data
#'
#' @param data Raw clinical trial data frame
#' @param patient_col Name of patient ID column
#' @param date_col Name of date column  
#' @param visit_col Name of visit/event type column
#' @param response_col Name of response column
#' @param treatment_col Name of treatment type column (optional)
#' @param discontinuation_col Name of discontinuation reason column (optional)
#' @param death_cause_col Name of death cause column (optional)
#' @return Processed data frame with derived treatment dates and responses
#' @export
process_raw_trial_data <- function(data, patient_col, date_col, visit_col, response_col,
                                  treatment_col = NULL, discontinuation_col = NULL, 
                                  death_cause_col = NULL) {
  
  # Validate inputs
  required_cols <- c(patient_col, date_col, visit_col, response_col)
  validation <- validate_required_columns(data, required_cols)
  if (!validation$valid) {
    stop(validation$message)
  }
  
  # Clean and standardize the data
  processed_data <- data %>%
    dplyr::mutate(
      # Parse dates flexibly
      !!date_col := parse_flexible_dates(.data[[date_col]]),
      # Clean response values
      !!response_col := clean_response_values(.data[[response_col]]),
      # Standardize visit types
      !!visit_col := stringr::str_trim(.data[[visit_col]])
    )
  
  # Extract treatment start dates (C1D1 or Baseline visits)
  treatment_starts <- processed_data %>%
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], "C1D1|Baseline", ignore.case = TRUE),
      !is.na(.data[[date_col]])
    ) %>%
    dplyr::group_by(.data[[patient_col]]) %>%
    dplyr::summarise(
      treatment_start = min(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Extract treatment end dates
  treatment_ends <- processed_data %>%
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], "End of treatment|Study discontinuation", ignore.case = TRUE),
      !is.na(.data[[date_col]])
    ) %>%
    dplyr::group_by(.data[[patient_col]]) %>%
    dplyr::summarise(
      treatment_end = max(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Extract response assessments only
  response_data <- processed_data %>%
    dplyr::filter(
      stringr::str_detect(.data[[visit_col]], "Response", ignore.case = TRUE),
      !is.na(.data[[response_col]]),
      .data[[response_col]] != "NA"
    )
  
  # Join treatment dates with response data
  final_data <- response_data %>%
    dplyr::left_join(treatment_starts, by = patient_col) %>%
    dplyr::left_join(treatment_ends, by = patient_col) %>%
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
    final_data <- final_data %>%
      dplyr::mutate(treatment_type = .data[[treatment_col]])
  }
  
  if (!is.null(discontinuation_col) && discontinuation_col %in% names(data)) {
    final_data <- final_data %>%
      dplyr::mutate(discontinuation_reason = .data[[discontinuation_col]])
  }
  
  if (!is.null(death_cause_col) && death_cause_col %in% names(data)) {
    final_data <- final_data %>%
      dplyr::mutate(death_cause = .data[[death_cause_col]])
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
  processed_data <- data %>%
    dplyr::mutate(
      # Parse dates
      !!treatment_start_col := parse_flexible_dates(.data[[treatment_start_col]]),
      !!assessment_date_col := parse_flexible_dates(.data[[assessment_date_col]]),
      # Clean responses
      !!response_col := clean_response_values(.data[[response_col]])
    ) %>%
    dplyr::select(
      patient_id = !!patient_col,
      treatment_start = !!treatment_start_col,
      assessment_date = !!assessment_date_col,
      response = !!response_col,
      dplyr::everything()
    )
  
  # Add optional columns
  if (!is.null(treatment_end_col) && treatment_end_col %in% names(data)) {
    processed_data <- processed_data %>%
      dplyr::mutate(
        treatment_end = parse_flexible_dates(.data[[treatment_end_col]])
      )
  }
  
  if (!is.null(treatment_type_col) && treatment_type_col %in% names(data)) {
    processed_data <- processed_data %>%
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
  
  best_responses <- data %>%
    dplyr::filter(!is.na(response), response != "") %>%
    dplyr::group_by(patient_id) %>%
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
    summary_stats$response_distribution <- data %>%
      dplyr::filter(!is.na(response), response != "") %>%
      dplyr::count(response, sort = TRUE)
  }
  
  # Treatment types
  if ("treatment_type" %in% names(data)) {
    summary_stats$treatment_types <- data %>%
      dplyr::filter(!is.na(treatment_type)) %>%
      dplyr::distinct(patient_id, treatment_type) %>%
      dplyr::count(treatment_type, sort = TRUE)
  }
  
  return(summary_stats)
}