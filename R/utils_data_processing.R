#' Data Processing Utility Functions
#'
#' Collection of utility functions for data processing and validation
#'
#' @noRd
#'
#' Parse Date Columns with Multiple Formats
#'
#' @param date_vector Character vector of dates in various formats
#' @return Date vector with parsed dates
#' @noRd
parse_flexible_dates <- function(date_vector) {
  # Common date formats in clinical data
  formats <- c(
    "%d/%m/%Y",    # 24/03/2022
    "%Y-%m-%d",    # 2022-03-24
    "%m/%d/%Y",    # 03/24/2022
    "%d-%m-%Y",    # 24-03-2022
    "%Y/%m/%d"     # 2022/03/24
  )
  
  parsed_dates <- as.Date(rep(NA, length(date_vector)))
  
  for (fmt in formats) {
    missing_idx <- is.na(parsed_dates)
    if (sum(missing_idx) == 0) break
    
    parsed_dates[missing_idx] <- tryCatch({
      as.Date(date_vector[missing_idx], format = fmt)
    }, error = function(e) rep(NA, sum(missing_idx)))
  }
  
  return(parsed_dates)
}

#' Clean Response Values
#'
#' @param response_vector Character vector of response values
#' @return Cleaned response vector with standardized categories
#' @noRd
clean_response_values <- function(response_vector) {
  # Remove NA/empty values for cleaning
  clean_responses <- stringr::str_trim(response_vector)
  clean_responses <- stringr::str_to_upper(clean_responses)
  
  # Standardize common response patterns
  clean_responses <- stringr::str_replace_all(clean_responses, 
    c(
      ".*COMPLETE RESPONSE.*" = "CR",
      ".*PARTIAL RESPONSE.*" = "PR",
      ".*STABLE DISEASE.*" = "SD",
      ".*PROGRESSIVE DISEASE.*" = "PD",
      "CPD.*" = "PD",
      "UPD.*" = "uPD",
      ".*NOT EVALUABLE.*" = "NE"
    )
  )
  
  return(clean_responses)
}

#' Validate Required Columns
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @return List with 'valid' (logical) and 'message' (character)
#' @noRd
validate_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  return(list(valid = TRUE, message = "All required columns present"))
}

#' Check Data Quality
#'
#' @param data Data frame to check
#' @param patient_col Name of patient ID column
#' @param date_col Name of date column
#' @return List with quality metrics and warnings
#' @noRd
check_data_quality <- function(data, patient_col, date_col) {
  quality_report <- list(
    n_patients = length(unique(data[[patient_col]])),
    n_records = nrow(data),
    missing_patients = sum(is.na(data[[patient_col]])),
    missing_dates = sum(is.na(data[[date_col]])),
    warnings = character(0)
  )
  
  # Check for missing patient IDs
  if (quality_report$missing_patients > 0) {
    quality_report$warnings <- c(quality_report$warnings,
      paste(quality_report$missing_patients, "records with missing patient IDs"))
  }
  
  # Check for missing dates
  if (quality_report$missing_dates > 0) {
    quality_report$warnings <- c(quality_report$warnings,
      paste(quality_report$missing_dates, "records with missing dates"))
  }
  
  # Check for duplicate patient-date combinations
  if (length(date_col) == 1) {
    duplicates <- data %>%
      dplyr::group_by(!!rlang::sym(patient_col), !!rlang::sym(date_col)) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1)
    
    if (nrow(duplicates) > 0) {
      quality_report$warnings <- c(quality_report$warnings,
        paste(nrow(duplicates), "duplicate patient-date combinations found"))
    }
  }
  
  return(quality_report)
}

#' Create Column Choices for Select Inputs
#'
#' @param column_names Character vector of column names
#' @param include_empty Logical, whether to include empty option
#' @param empty_label Character, label for empty option
#' @return Named character vector for select input choices
#' @noRd
create_column_choices <- function(column_names, include_empty = TRUE, empty_label = "Select column...") {
  if (include_empty) {
    choices <- c(setNames("", empty_label), setNames(column_names, column_names))
  } else {
    choices <- setNames(column_names, column_names)
  }
  return(choices)
}