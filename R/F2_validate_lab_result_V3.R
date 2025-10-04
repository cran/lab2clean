# Specify global variables
utils::globalVariables(c("group", "validation_result", "time_diff", "time_interval", "value_diff", "total_records_1000",
                         "lower_percentile", "upper_percentile", "flag", "lab_date", ".", "i.validation_result", "logic_check_eligible",
                         "reportable_interval", "logic_rules"))

#' Validate Quantitative Laboratory Result Values
#'
#' This function is designed to validate quantitative laboratory result values.
#' It modifies the provided `lab_data` dataframe in-place, adding one new column.
#'
#' @param lab_data A data frame containing laboratory data.
#' @param result_value The column in `lab_data` with quantitative result values for validation.
#' @param result_unit The column in `lab_data` with result units in a UCUM-valid format.
#' @param loinc_code The column in `lab_data` indicating the LOINC code of the laboratory test.
#' @param patient_id The column in `lab_data` indicating the identifier of the tested patient.
#' @param lab_datetime The column in `lab_data` with the date or datetime of the laboratory test.
#' @param report A report is written in the console. Defaults to "TRUE".
#'
#' @import data.table
#' @importFrom stats ave quantile
#' @importFrom utils data globalVariables
#'
#' @return A modified `lab_data` data frame with additional columns:
#'   * `flag`: specifies the flag detected in the result records that violated one or more of the validation checks
#'
#' @details
#' The function employs the following validation methodology:
#'   1. Reportable limits check: Identifies implausible values outside reportable limits.
#'   2. Logic rules check: Identifies values that contradict some predefined logic rules.
#'   3. Delta limits check: Flags values with excessive change from prior results for the same test and patient.
#'
#' Internal Datasets:
#' The function uses two internal datasets included with the package:
#'   1. `reportable_interval`: Contains information on reportable intervals.
#'   2. `logic_rules`: Contains logic rules for validation.
#'
#' @author Ahmed Zayed <ahmed.zayed@kuleuvne.be>, Arne Janssens <arne.janssens@kuleuven.be>
#'
#' @note
#' This function is a component of a broader laboratory data cleaning pipeline and should be evaluated accordingly.
#' The package's framework includes functions for cleaning result values, validating quantitative results,
#' standardizing unit formats, performing unit conversion, and assisting in LOINC code mapping.
#'
#' Concerning performance, the function's speed might be influenced by the size of `lab_data`. Consider:
#'   * Limiting the number of records processed.
#'   * Optimize the function for larger datasets.
#'   * Implement pre-processing steps to divide the dataset chronologically.
#'
#'
#' @seealso
#' Function 1 for result value cleaning,
#'
#' @export
validate_lab_result <- function(lab_data, result_value, result_unit, loinc_code, patient_id, lab_datetime, report = TRUE){
  start.time <- Sys.time()

  # Define ANSI escape codes for text decorations and colors
  bold <- "\033[1m"
  reset <- "\033[0m"
  Warning <- "\u26A0"
  success <- "\u2714"
  red <- "\033[31m"
  green <- "\033[32m"
  blue <- "\033[34m"
  clock <- "\u23F0"

  # Add flag column
  lab_data$flag <- NA

  # Convert columns to correct type
  lab_data[[result_value]] <- as.numeric(lab_data[[result_value]])

  # Convert lab_data to data.table
  lab_data <- data.table::as.data.table(lab_data)

  # Preprocessing cleaning steps
  if (report){
    cat(paste0(bold, "Preprocessing Step for Duplicate Records", reset, "\n"))
    cat("===============================================================================================\n")
  }
  # Flag duplicates based on patient_id, lab_datetime, and loinc_code
  duplicate_tests <- duplicated(lab_data, by = c(patient_id, lab_datetime, loinc_code)) |
    duplicated(lab_data, by = c(patient_id, lab_datetime, loinc_code), fromLast = TRUE)
  lab_data$flag[duplicate_tests] <- "duplicate"
  if (report){
    cat(paste0(red, Warning, reset," ", blue, sum(duplicate_tests), reset, " duplicate records were flagged.\nThese are multiple records of the same test for the same patient at the same result timestamp.\n"))
  }
  rm(duplicate_tests)

  #### Check1: Reportable Limits ####
  if (report){
    cat(paste0(bold, "Check 1: Reportable Limits Check", reset, "\n"))
    cat("===============================================================================================\n")
  }
  data("reportable_interval", package = "lab2clean", envir = environment())

  # Join lab_data with reportable_interval based on loinc_code and result_unit
  # Temporarily rename the columns
  names(lab_data)[names(lab_data) %in% c(loinc_code, result_unit)] <- c("interval_loinc_code", "UCUM_unit")
  # Perform the merge
  lab_data <- merge(lab_data, reportable_interval, by = c("interval_loinc_code", "UCUM_unit"), all.x = TRUE)
  # Revert the column names back to their original values
  names(lab_data)[names(lab_data) %in% c("interval_loinc_code", "UCUM_unit")] <- c(loinc_code, result_unit)

  # Flag rows where result_value is out of the reportable range
  lab_data$flag[lab_data[[result_value]] < lab_data$low_reportable_limit] <- "low_unreportable"
  lab_data$flag[lab_data[[result_value]] > lab_data$high_reportable_limit] <- "high_unreportable"

  if (report){
    low_unreportable_records <- sum(grepl("low_unreportable", lab_data$flag))
    cat(paste0(red, Warning, reset," ", blue, low_unreportable_records, reset, " extremely low result records were flagged (low_unreportable).\n"))
    high_unreportable_records <- sum(grepl("high_unreportable", lab_data$flag))
    cat(paste0(red, Warning, reset," ", blue, high_unreportable_records, reset, " extremely high records were flagged (high_unreportable).\n"))
  }
  # Drop the columns that were merged from reportable_interval
  lab_data$low_reportable_limit <- NULL
  lab_data$high_reportable_limit <- NULL

  #### Check2: Logic Rules ####
  if (report){
    cat(paste0(bold, "Check 2: Logic Consistency Checks", reset, "\n"))
    cat("===============================================================================================\n")
  }
  data("logic_rules", package = "lab2clean", envir = environment())
  logic_check_loinc <- logic_rules$rule_part[logic_rules$rule_part_type == "loinc_code"]

  # Filter out entries flagged as duplicates or with missing flags
  filtered_data <- lab_data[!grepl("duplicate", lab_data$flag) | is.na(lab_data$flag), ]

  # Identify rows to be checked based on loinc_code and duplication criteria
  to_be_checked <- filtered_data[[loinc_code]] %in% logic_check_loinc &
    (duplicated(filtered_data, by = c(patient_id, lab_datetime, result_unit)) |
       duplicated(filtered_data, by = c(patient_id, lab_datetime, result_unit), fromLast = TRUE))

  # Add a new column to indicate eligibility for logic check
  filtered_data[, logic_check_eligible := data.table::fcase(
    to_be_checked, "1",
    default = "0"
  )]
  rm(to_be_checked, logic_check_loinc)

  # Split the data into two datasets based on logic check eligibility
  eligible_data <- filtered_data[logic_check_eligible == "1"]
  eligible_data[, logic_check_eligible := NULL]
  ineligible_data <- filtered_data[logic_check_eligible == "0"]
  ineligible_data[, logic_check_eligible := NULL]
  ineligible_data <- rbind(ineligible_data, lab_data[grepl("duplicate", lab_data$flag)], fill = TRUE)
  rm(filtered_data)

  # Split the data to be checked using the new grouping variable
  eligible_data[, group := do.call(paste, c(.SD, sep = "|")), .SDcols = c(patient_id, lab_datetime, result_unit)]
  split_data <- split(eligible_data, by = "group", keep.by = FALSE)

  # Nested function to evaluate a single rule on a subset of the data
  eval_rule <- function(lab_subset, rule){
    # Extract values based on the loinc codes mentioned in the rule
    extract_value <- function(rule_part, rule_part_type, dt) {
      if (rule_part_type == "loinc_code") {
        value <- dt[[result_value]][dt[[loinc_code]] == rule_part]
        if (length(value) == 0) return(NA)
        return(value)
      } else if (rule_part_type == "operator") {
        return(rule_part)
      }
      return(NA)
    }
    # Create the expression to evaluate, based on the rule parts provided in the rule
    eval_str <- sapply(1:nrow(rule), function(i) {
      extract_value(rule$rule_part[i], rule$rule_part_type[i], lab_subset)
    }, USE.NAMES = FALSE)
    eval_str <- paste(eval_str, collapse = "")
    # Evaluate the created expression
    validation_result <- eval(parse(text = eval_str))
    return(list(validation_result = validation_result, eval_str = as.character(eval_str)))
  }
  # Evaluate each rule for each group
  unique_rules <- max(logic_rules$rule_id)
  flag_results <- eligible_data[, {
    # This will be a list of data.tables
    results_list <- lapply(1:unique_rules, function(i) {
      rule_subset <- logic_rules[logic_rules$rule_id == i, ]
      result <- eval_rule(.SD, rule_subset)
      # Convert each result to a data.table
      data.table::as.data.table(list(rule_id = i, validation_result = result$validation_result, eval_str = result$eval_str))
    })
    # Combine all the data.tables in the list into one data.table
    results_dt <- data.table::rbindlist(results_list, fill = TRUE)
    # Return the results data.table
    results_dt
  }, by = group]

  rm(split_data)

  # Map the flagged result to their loinc codes (to avoid flagging tests with the same units in the group)
  flag_results <- flag_results[validation_result == FALSE]
  setDT(flag_results)
  rules_loinc_codes <- logic_rules[logic_rules$rule_part_type == "loinc_code", c("rule_id", "rule_part")]
  names(rules_loinc_codes)[2] <- "loinc_code"
  flag_results <- merge(flag_results, rules_loinc_codes, by = "rule_id", all.x = TRUE, allow.cartesian=TRUE)
  flag_results$group <- paste0(flag_results$group, "|", flag_results$loinc_code)

  # Map the flag results back to the original data
  eligible_data[, group := do.call(paste, c(.SD, sep = "|")), .SDcols = c("group", loinc_code)]
  eligible_data[flag_results, validation_result := i.validation_result, on = "group"]
  eligible_data[, group := NULL]
  rm(flag_results)

  # Update rows where not all values in results_temp are NA and there's at least one FALSE
  eligible_data[!is.na(validation_result) & validation_result == FALSE,
            flag := ifelse(is.na(flag),
                           "logic_flag",
                           paste0(flag, ", logic_flag"))]
  if (report){
    logic_flag_records <- sum(grepl("logic_flag", eligible_data$flag))
    cat(paste0(red, Warning, reset," ", blue, logic_flag_records, reset, " result records were flagged for violating relational logic rules (logic_flag).\n"))
  }
  eligible_data[, validation_result := NULL]
  lab_data <- data.table::rbindlist(list(ineligible_data, eligible_data))
  rm(ineligible_data, eligible_data)

  #### Check3: Delta Check ####
  if (report){
    cat(paste0(bold, "Check 3: Delta Change Limits Checks", reset, "\n"))
    cat("===============================================================================================\n")
  }
  # Convert columns to correct type
  lab_data[, lab_date := as.Date(lab_data[[lab_datetime]])]
  data.table::setorderv(lab_data, c(patient_id, loinc_code, lab_datetime))

  # Compute time differences
  lab_data[, time_diff := lapply(.SD, function(x) x - data.table::shift(x, type="lag")), .SDcols = "lab_date",
           by = c(patient_id, loinc_code, result_unit)]
  lab_data[, time_diff := as.numeric(time_diff)]
  lab_data[, time_interval := data.table::fcase(
    time_diff > 90, "outside_timerange",
    time_diff <= 7, "time_7d",
    time_diff > 7 & time_diff <= 90, "time_8_90d",
    default = NA
  )]

  # Analogue for difference in value
  lab_data[, value_diff := lapply(.SD, function(x) x - data.table::shift(x, type="lag")), .SDcols = result_value,
           by = c(patient_id, loinc_code, result_unit)]

  # Before computing percentiles determine the number of records per group
  lab_data[, total_records_1000 := .N >= 1000, by = c(loinc_code, result_unit, "time_interval")]

  # Compute Quantiles/Percentiles

  # Set value_diff to NA if outise time range and if !total_records_1000
  lab_data[time_interval == "outside_timerange" | !total_records_1000, value_diff := NA]

  # Split into two groups: < 7 days and between 7 and 90 days
  test_7d <- lab_data[time_interval == "time_7d" & !is.na(value_diff)]
  test_8_90d <- lab_data[time_interval == "time_8_90d" & !is.na(value_diff)]
  test_outside <- lab_data[is.na(value_diff)]

  test_7d[, lower_percentile := ave(value_diff, loinc_code, FUN = function(x) quantile(x, .0005))]
  test_7d[, upper_percentile := ave(value_diff, loinc_code, FUN = function(x) quantile(x, .9995))]
  test_7d[value_diff < lower_percentile | value_diff > upper_percentile,
          flag := ifelse(is.na(flag),
                         "delta_flag_7d",
                         paste0(flag, ", delta_flag_7d"))]
  if (report){
    delta_flag_records1 <- sum(grepl("delta_flag_7d", test_7d$flag))
    cat(paste0(red, Warning, reset," ", blue, delta_flag_records1, reset, " records were flagged for having extreme change values from previous results within 7 days (delta_flag_7d).\n"))
  }
  test_8_90d[, lower_percentile := ave(value_diff, loinc_code, FUN = function(x) quantile(x, .0005))]
  test_8_90d[, upper_percentile := ave(value_diff, loinc_code, FUN = function(x) quantile(x, .9995))]
  test_8_90d[value_diff < lower_percentile | value_diff > upper_percentile,
             flag := ifelse(is.na(flag),
                            "delta_flag_8_90d",
                            paste0(flag, ", delta_flag_8_90d"))]
  if (report){
    delta_flag_records2 <- sum(grepl("delta_flag_8_90d", test_8_90d$flag))
    cat(paste0(red, Warning, reset," ", blue, delta_flag_records2, reset, " records were flagged for having extreme change values from previous results within 8-90 days (delta_flag_8_90d).\n"))
  }
  test_outside[, c("lower_percentile", "upper_percentile") := .(NA, NA)]

  lab_data <- data.table::rbindlist(list(test_7d, test_8_90d, test_outside))
  lab_data[, c("lab_date", "time_diff", "time_interval", "value_diff", "total_records_1000", "lower_percentile", "upper_percentile") := NULL]
  data.table::setorderv(lab_data, c(patient_id, loinc_code, lab_datetime))
  if (report){
    cat("===============================================================================================\n")
    plausibility_check <- round((sum(is.na(lab_data$flag))/length(lab_data$flag) * 100), 3)
    cat(paste0(green, success, reset," ",blue, plausibility_check, "%", reset, " of the lab data records were validated with no flag detected.\n"))
  }
  end.time <- Sys.time()
  time.taken <- as.numeric(difftime(end.time, start.time, units = "secs"))
  # Break into minutes and seconds
  mins <- floor(time.taken / 60)
  secs <- round(time.taken %% 60, 1)
  cat(paste0(clock, " ", bold, "Time taken is ", blue, mins, " min, ", secs, " sec\n", reset))

  return(lab_data)
}
