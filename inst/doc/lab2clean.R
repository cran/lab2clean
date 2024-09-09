## ----setup, include=FALSE-----------------------------------------------------
library(knitr)

# Define the custom output hook
# Simplified custom output hook
knit_hooks$set(output = function(x, options) {
  if (knitr::is_html_output()) {
    # Convert newlines to <br> for HTML output
    x <- gsub("\n", "<br>", x)
    # Handle ANSI colors and formatting
    x <- gsub("\033\\[1m", "<strong>", x)  # Bold
    x <- gsub("\033\\[31m", "<span style='color:red;'>", x)  # Red
    x <- gsub("\033\\[32m", "<span style='color:green;'>", x)  # Green
    x <- gsub("\033\\[34m", "<span style='color:blue;'>", x)  # Blue
    # Ensure all open tags are closed when encountering reset
    x <- gsub("\033\\[0m", "</span></strong>", x)
    # Handle ANSI colors and formatting
    x <- gsub("\u26A0", "&#9888;", x)  # Warning symbol
    x <- gsub("\u2714", "&#10004;", x)  # Success checkmark
    x <- gsub("\u23F0", "&#9200;", x)  # Clock
  } else if (knitr::is_latex_output()) {
    x <- gsub("\033\\[1m", "\\textbf{", x)  # Bold
    x <- gsub("\033\\[31m", "\\textcolor{red}{", x)  # Red
    x <- gsub("\033\\[32m", "\\textcolor{green}{", x)  # Green
    x <- gsub("\033\\[34m", "\\textcolor{blue}{", x)  # Blue
    x <- gsub("\033\\[0m", "}", x)  # Reset/Close tags
    x <- gsub("\u26A0", "\\ding{102}", x)  # Warning
    x <- gsub("\u2714", "\\checkmark", x)  # Success
    x <- gsub("\u23F0", "\\clock", x)  # Clock (you might need to define this)
  }
  return(x)
})
#clean_ansi <- function(text) {
#  gsub("\033\\[[0-9;]*m", "", text)
#}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
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

