## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(fansi)   # safer ANSI -> HTML

knit_hooks$set(output = function(x, options) {
  # Only touch true console text in HTML docs
  if (!knitr::is_html_output()) return(x)

  # Skip anything that is already HTML or is emitted as 'asis'
  if (isTRUE(options$results == "asis") ||
      grepl("^\\s*<", x) || grepl("<table|<div", x, ignore.case = TRUE)) {
    return(x)
  }

  # Convert ANSI safely and keep line breaks only for console text
  x <- fansi::sgr_to_html(x)        # handles \033[…m sequences robustly
  x <- gsub("\n", "<br>\n", x)      # line breaks for console output only
  x
})

# keep your table setup
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)

library(kableExtra)
knit_print.data.frame <- function(x, ...) {
  knitr::asis_output(
    kableExtra::kbl(x, ...) |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive")
      )
  )
}
library(printr)

## ----install package----------------------------------------------------------
#install.packages("lab2clean")

## ----read library-------------------------------------------------------------
library(lab2clean)

## ----Function_1_dummy---------------------------------------------------------
data("Function_1_dummy", package = "lab2clean")
head(Function_1_dummy,6)

## ----function with report, results='markup'-----------------------------------
cleaned_results <- clean_lab_result(Function_1_dummy, raw_result = "raw_result", report = TRUE, n_records = "frequency")

## ----function with report 1---------------------------------------------------
cleaned_results <- clean_lab_result(Function_1_dummy, raw_result = "raw_result", report = FALSE)
cleaned_results

## ----locale, warning=FALSE, message=FALSE-------------------------------------
Function_1_dummy_subset <- Function_1_dummy[c(71,72),, drop = FALSE]
cleaned_results <- clean_lab_result(Function_1_dummy_subset, raw_result = "raw_result", report = FALSE, locale = "US")
cleaned_results
cleaned_results <- clean_lab_result(Function_1_dummy_subset, raw_result = "raw_result", report = FALSE, locale = "DE")
cleaned_results

## ----common words, warning=FALSE, message=FALSE-------------------------------
data("common_words", package = "lab2clean")
common_words

## ----Function_2_dummy dataset, warning=FALSE, message=FALSE-------------------
data("Function_2_dummy", package = "lab2clean")
head(Function_2_dummy, 6)

## ----apply validate_lab_result, warning=FALSE, message=FALSE------------------
validate_results <- validate_lab_result(Function_2_dummy, 
                                        result_value="result_value",
                                        result_unit="result_unit",
                                        loinc_code="loinc_code",
                                        patient_id = "patient_id" , 
                                        lab_datetime="lab_datetime1")


## ----flag column creation, warning=FALSE, message=FALSE-----------------------
head(validate_results, 6)
levels(factor(validate_results$flag))

## ----flag explain by subseting patients, warning=FALSE, message=FALSE---------
subset_patients <- validate_results[validate_results$patient_id %in% c("14236258", "10000003", "14499007"), ]
subset_patients

## ----reportable_interval, warning=FALSE, message=FALSE------------------------
data("reportable_interval", package = "lab2clean")
reportable_interval_subset <- reportable_interval[reportable_interval$interval_loinc_code == "2160-0", ]
reportable_interval_subset

## ----logic_rules, warning=FALSE, message=FALSE--------------------------------
data("logic_rules", package = "lab2clean")
logic_rules <- logic_rules[logic_rules$rule_id == 3, ]
logic_rules

## ----Function_3_dummy dataset, warning=FALSE, message=FALSE-------------------
data("Function_3_dummy", package = "lab2clean")
head(Function_3_dummy, 6)

## ----apply standardize_lab_unit, warning=FALSE, message=FALSE-----------------
standardized_units <- standardize_lab_unit(Function_3_dummy, raw_unit = "unit_raw", n_records = "n_records")

## ----standardized_units_head, warning=FALSE, message=FALSE--------------------
head(standardized_units, 10)

## ----Function_4_dummy---------------------------------------------------------
data("Function_4_dummy", package = "lab2clean")
head(Function_4_dummy,6)

## ----apply harmonize_lab_unit, warning=FALSE, message=FALSE-------------------
harmonized_units <- harmonize_lab_unit(Function_4_dummy,
                                       loinc_code="loinc_code",
                                       result_value="result_value",
                                       result_unit="result_unit")

## ----harmonized_units_head, warning=FALSE, message=FALSE----------------------
head(harmonized_units, 6)

## ----harmonized_units new_loinc_code, warning=FALSE, message=FALSE------------
harmonized_units[which(harmonized_units$loinc_code != harmonized_units$new_loinc_code), ]

## ----harmonized_units cleaning_comments, warning=FALSE, message=FALSE---------
levels(factor(harmonized_units$cleaning_comments))

## ----preferred_unit_system, warning=FALSE, message=FALSE----------------------
Function_4_dummy_subset <- Function_4_dummy[c(27, 15, 38, 45),, drop = FALSE]
harmonized_units <- harmonize_lab_unit(Function_4_dummy_subset,
                                       loinc_code="loinc_code",
                                       result_value="result_value",
                                       result_unit="result_unit",
                                       report = FALSE,
                                       preferred_unit_system = "SI")
harmonized_units
harmonized_units <- harmonize_lab_unit(Function_4_dummy_subset,
                                       loinc_code="loinc_code",
                                       result_value="result_value",
                                       result_unit="result_unit", 
                                       report = FALSE,
                                       preferred_unit_system = "conventional")
harmonized_units

