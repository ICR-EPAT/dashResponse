library(testthat)
library(dplyr)

# Test data setup
setup_test_data <- function() {
  # Raw trial data for testing
  raw_data <- data.frame(
    patient_id = rep(c("P001", "P002", "P003"), each = 5),
    visit_date = as.Date(c(
      # P001
      "2023-01-01", "2023-01-15", "2023-02-15", "2023-03-15", "2023-04-15",
      # P002  
      "2023-01-05", "2023-01-20", "2023-02-20", "2023-03-20", "2023-04-20",
      # P003
      "2023-01-10", "2023-01-25", "2023-02-25", "2023-03-25", "2023-04-25"
    )),
    visit_type = rep(c("C1D1", "Baseline", "Response Assessment Week 6", 
                      "Response Assessment Week 10", "End of treatment"), 3),
    response = rep(c(NA, NA, "SD", "PR", NA), 3),
    treatment_arm = rep(c("Drug A", "Drug B", "Drug A"), each = 5),
    tumor_size = c(
      # P001: baseline 50,
      NA, 50, 55, 80, NA,
      # P002: baseline 60,
      NA, 60, 65, 70, NA,
      # P003: baseline 40,
      NA, 40, 45, 55, NA
    ),
    stringsAsFactors = FALSE
  )
  
  # Structured data for testing
  structured_data <- data.frame(
    patient_id = c("P001", "P001", "P002", "P002", "P003", "P003"),
    treatment_start = as.Date(rep(c("2023-01-01", "2023-01-05", "2023-01-10"), each = 2)),
    assessment_date = as.Date(c("2023-01-15", "2023-02-15", "2023-01-20", "2023-02-20", 
                               "2023-01-25", "2023-02-25")),
    response = c("PR", "PR", "SD", "SD", "PD", "PD"),
    treatment_end = as.Date(rep(c("2023-04-15", "2023-04-20", "2023-04-25"), each = 2)),
    treatment_type = rep(c("Drug A", "Drug B", "Drug A"), each = 2),
    stringsAsFactors = FALSE
  )
  
  list(raw = raw_data, structured = structured_data)
}

test_that("process_raw_trial_data works with basic input", {
  test_data <- setup_test_data()
  
  result <- process_raw_trial_data(
    data = test_data$raw,
    patient_col = "patient_id",
    date_col = "visit_date",
    visit_col = "visit_type",
    response_col = "response"
  )
  
  # Check basic structure
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(all(c("patient_id", "treatment_start", "assessment_date", "response") %in% names(result)))
  
  # Check that only response assessments are included
  expect_true(all(grepl("Response|baseline", result$visit, ignore.case = TRUE)))
  
  # Check that treatment start dates are extracted correctly
  expect_true(all(!is.na(result$treatment_start)))
  expect_equal(length(unique(result$patient_id)), 3)
})

test_that("process_raw_trial_data handles optional columns", {
  test_data <- setup_test_data()
  
  result <- process_raw_trial_data(
    data = test_data$raw,
    patient_col = "patient_id",
    date_col = "visit_date", 
    visit_col = "visit_type",
    response_col = "response",
    treatment_col = "treatment_arm",
    response_value_col = "tumor_size"
  )
  
  expect_true("treatment_type" %in% names(result))
  expect_true("tumor_size" %in% names(result))
  expect_true(all(result$treatment_type %in% c("Drug A", "Drug B")))
})

test_that("process_raw_trial_data handles response values and baseline calculation", {
  # Create test data with baseline visits
  test_data <- setup_test_data()
  
  result <- process_raw_trial_data(
    data = test_data$raw,
    patient_col = "patient_id",
    date_col = "visit_date", 
    visit_col = "visit_type",
    response_col = "response",
    response_value_col = "tumor_size"
  )
  
  # Check response value columns are added
  expect_true("response_value" %in% names(result))
  expect_true("baseline_value" %in% names(result))
  expect_true("absolute_change" %in% names(result))
  expect_true("percent_change" %in% names(result))
  
  # Check baseline values are correctly identified
  baselines <- result %>% 
    filter(!is.na(baseline_value)) %>% 
    distinct(patient_id, baseline_value)
  
  expect_equal(nrow(baselines), 3)
  expect_true(50 %in% baselines$baseline_value) # P001 baseline
  expect_true(60 %in% baselines$baseline_value) # P002 baseline  
  expect_true(40 %in% baselines$baseline_value) # P003 baseline
})

test_that("process_structured_data works correctly", {
  test_data <- setup_test_data()
  
  result <- process_structured_data(
    data = test_data$structured,
    patient_col = "patient_id",
    treatment_start_col = "treatment_start",
    assessment_date_col = "assessment_date",
    response_col = "response"
  )
  
  # Check basic structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 6)
  expect_true(all(c("patient_id", "treatment_start", "assessment_date", "response") %in% names(result)))
  
  # Check data types
  expect_true(inherits(result$treatment_start, "Date"))
  expect_true(inherits(result$assessment_date, "Date"))
})

test_that("process_structured_data handles optional columns", {
  test_data <- setup_test_data()
  
  result <- process_structured_data(
    data = test_data$structured,
    patient_col = "patient_id",
    treatment_start_col = "treatment_start",
    assessment_date_col = "assessment_date",
    response_col = "response",
    treatment_end_col = "treatment_end",
    treatment_type_col = "treatment_type"
  )
  
  expect_true("treatment_end" %in% names(result))
  expect_true("treatment_type" %in% names(result))
  expect_true(inherits(result$treatment_end, "Date"))
})

test_that("calculate_best_response works correctly", {
  test_data <- data.frame(
    patient_id = c("P001", "P001", "P002", "P002", "P003", "P003"),
    response = c("PR", "SD", "CR", "PR", "PD", "PD")
  )
  
  result <- calculate_best_response(test_data)
  
  expect_equal(nrow(result), 3)
  expect_true(all(c("patient_id", "best_response", "n_assessments") %in% names(result)))
  
  # Check best response hierarchy
  expect_equal(result$best_response[result$patient_id == "P001"], "PR")
  expect_equal(result$best_response[result$patient_id == "P002"], "CR") 
  expect_equal(result$best_response[result$patient_id == "P003"], "PD")
})

test_that("generate_data_summary produces correct statistics", {
  test_data <- data.frame(
    patient_id = c("P001", "P001", "P002", "P002"),
    assessment_date = as.Date(c("2023-01-01", "2023-02-01", "2023-01-15", "2023-02-15")),
    response = c("PR", "SD", "CR", "PR"),
    treatment_type = c("Drug A", "Drug A", "Drug B", "Drug B")
  )
  
  result <- generate_data_summary(test_data)
  
  expect_equal(result$n_patients, 2)
  expect_equal(result$n_records, 4)
  expect_true(!is.null(result$date_range))
  expect_true(!is.null(result$response_distribution))
  expect_true(!is.null(result$treatment_types))
  
  # Check date range
  expect_equal(result$date_range$min_date, as.Date("2023-01-01"))
  expect_equal(result$date_range$max_date, as.Date("2023-02-15"))
})

test_that("functions handle missing data gracefully", {
  # Test with missing columns
  incomplete_data <- data.frame(
    patient_id = c("P001", "P002"),
    visit_date = as.Date(c("2023-01-01", "2023-01-02"))
    # Missing visit_col and response_col
  )
  
  expect_error(
    process_raw_trial_data(
      data = incomplete_data,
      patient_col = "patient_id",
      date_col = "visit_date", 
      visit_col = "missing_col",
      response_col = "response"
    )
  )
})

test_that("functions handle edge cases", {
  # Test with empty data
  empty_data <- data.frame(
    patient_id = character(0),
    visit_date = as.Date(character(0)),
    visit_type = character(0),
    response = character(0)
  )
  
  expect_error(
    process_raw_trial_data(
      data = empty_data,
      patient_col = "patient_id",
      date_col = "visit_date", 
      visit_col = "visit_type",
      response_col = "response"
    )
  )
  
  # Test calculate_best_response with no valid responses
  no_response_data <- data.frame(
    patient_id = c("P001", "P002"),
    response = c(NA, "")
  )
  
  result <- calculate_best_response(no_response_data)
  expect_equal(nrow(result), 0)
})

test_that("date parsing works with different formats", {
  mixed_date_data <- data.frame(
    patient_id = c("P001", "P001", "P001"),
    visit_date = c("2022/12/20", "2023-01-01", "01/15/2023"),
    visit_type = c("C1D1", "Response Assessment", "End of treatment"),
    response = c(NA, "PR", NA),
    stringsAsFactors = FALSE
  )
  
  # This test assumes parse_flexible_dates function exists and works
  result <- process_raw_trial_data(
    data = mixed_date_data,
    patient_col = "patient_id",
    date_col = "visit_date",
    visit_col = "visit_type",
    response_col = "response"
  )
  
  expect_true(inherits(result$assessment_date, "Date"))
  expect_true(inherits(result$treatment_start, "Date"))
})
