# Specify global variables
utils::globalVariables(c("first_number", "second_number", "common_words"))

#' Clean and Standardize Laboratory Result Values
#'
#' This function is designed to clean and standardize laboratory result values.
#' It creates two new columns "clean_result" and "scale_type" without altering
#' the original result values. The function is part of a comprehensive R package
#' designed for cleaning laboratory datasets.
#'
#' @param lab_data A data frame containing laboratory data.
#' @param raw_result The column in `lab_data` that contains raw result values to be cleaned.
#' @param locale A string representing the locale for the laboratory data. Defaults to "NO".
#' @param report A report is written in the console. Defaults to "TRUE".
#' @param n_records In case you are loading a grouped list of distinct results, then you can assign the n_records to the column that contains the frequency of each distinct result. Defaults to NA.
#'
#' @importFrom utils data globalVariables
#'
#' @return A modified `lab_data` data frame with additional columns:
#'   * `clean_result`: Cleaned and standardized result values.
#'   * `scale_type`: The scale type of result values (Quantitative, Ordinal, Nominal).
#'   * `cleaning_comments`: Comments about the cleaning process for each record.
#'
#' @details
#' The function undergoes the following methodology:
#'   1. Clear Typos: Removes typographical errors and extraneous characters.
#'   2. Handle Extra Variables: Identifies and separates extra variables from result values.
#'   3. Detect and Assign Scale Types: Identifies and assigns the scale type using regular expressions.
#'   4. Number Formatting: Standardizes number formats based on predefined rules and locale.
#'   5. Mining Text Results: Identifies common words and patterns in text results.
#'
#' Internal Datasets:
#' The function uses an internal dataset; `common_words_languages.csv` which contains common words
#' in various languages used for pattern identification in text result values.
#'
#' @author Ahmed Zayed <ahmed.zayed@kuleuven.be>
#'
#' @note
#' This function is part of a larger data cleaning pipeline and should be evaluated in that context.
#' The package framework includes functions for cleaning result values and validating quantitative results for each test identifier.
#'
#' Performance of the function can be affected by the size of `lab_data`. Considerations for data size
#' or pre-processing may be needed.
#'
#' @seealso
#' Function 2 for result validation,
#'
#' @export
clean_lab_result <- function(lab_data, raw_result, locale = "NO", report = TRUE, n_records = NA){
  #add two new columns
  start.time <- Sys.time()
  lab_data$clean_result <- lab_data[[raw_result]]
  lab_data$scale_type <- NA
  lab_data$cleaning_comments <- NA
  total_n_records <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]]),
                            NA
                            )

#### Define regex patterns to be detected ####
  data("common_words", package = "lab2clean", envir = environment())

  #common_words <- read.csv(system.file("extdata", "common_words_languages_ASCII-TRANSLIT.csv", package="lab2clean"))
  positive_pattern <- paste0(common_words$Positive, collapse = "|")
  negative_pattern <- paste0(common_words$Negative, collapse = "|")
  notdetected_pattern <- paste0(common_words$Not_detected, collapse = "|")
  normal_pattern <- paste0(common_words$Normal, collapse = "|")
  high_pattern <- paste0(common_words$High, collapse = "|")
  low_pattern <- paste0(common_words$Low, collapse = "|")
  sample_pattern <-  paste0(common_words$Sample, collapse = "|")
  specimen_pattern <-  paste0(common_words$Specimen, collapse = "|")
  flags_pattern <- "high|H|HH|low|L|normal|N|P|negative|Neg|positive|Pos"
  unit_pattern <- "[A-Za-z]{0,3}/[A-Za-z]{1,3}" #this is not a pattern to detect all possible units, but only some common units with only alphabets

  # Numeric Patterns
  Qn1_pattern <- "-?([.,]?[0-9]+)+"
  number_pattern <- "([.,]?[0-9]+)+"
  exponent_pattern <- "( ?[Xx*]{1} ?10(([Ee^][+-]?)|([+-]{1}))[0-9]{1,2})|([Ee][+-]?[0-9]{1,2})"
  Qn2_pattern <- paste0("[<>]{1}(=)? *(",number_pattern,")")
  Qn3_pattern <- "[0-9]{1,4}\\s*[-]\\s*[0-9]{1,4}"
  Qn4_pattern <- "[<>]?\\s*1[:/][0-9]{1,6}"
  Qn_patterns <- paste0("(",Qn1_pattern,")|(",Qn2_pattern,")|(",Qn3_pattern,")|(",Qn4_pattern,")")

  # Define ANSI escape codes for text decorations and colors
  bold <- "\033[1m"
  reset <- "\033[0m"
  Warning <- "\u26A0"
  success <- "\u2714"
  red <- "\033[31m"
  green <- "\033[32m"
  blue <- "\033[34m"
  clock <- "\u23F0"

#### Function Block 1: Clear typos to transform invalid result entries into Null ####
  # This block performs initial data cleaning by removing typographical errors and extraneous characters from the result values.
  # The operation is distributed across multiple parts of the code to ensure the safe removal of certain characters.
  # Remove some invaluable punct characters, with escaping metacharacters
  lab_data$clean_result <- iconv(lab_data$clean_result, to ="ASCII//TRANSLIT")
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = "[!#$&'();?@_`{|}~\"\\[\\]]",
                                replacement = "", perl = TRUE
                                )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = "^=|=$",
                                replacement = ""
                                )
  # Remove leading and trailing space characters
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = "^\\s+|\\s+$",
                                replacement = ""
                                )

#### Function Block 2: Handle extra variables adjacent to the result ####
  # This block identifies and separates any additional variables that might be stored in the result values such as interpretative flags and units.
  # It does this by detecting patterns in the result values using regular expressions and removing them.
  if (report){
    cat(paste0(bold, "Step 1: Handling records with extra variables stored with the result value removing interpretative flags, or units", reset, "\n"))
    cat("==========================================================================================\n")
  }
  # Preprocessing flags of different languages
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("(\\b|\\d)", "(", positive_pattern, ")", "(\\b|\\d)"),
                                replacement = "\\1positive\\3",
                                ignore.case = TRUE, perl = TRUE
  )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("(\\b|\\d)", "(", negative_pattern, ")", "(\\b|\\d)"),
                                replacement = "\\1negative\\3",
                                ignore.case = TRUE, perl = TRUE
  )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("(\\b|\\d)", "(", normal_pattern, ")", "(\\b|\\d)"),
                                replacement = "\\1normal\\3",
                                ignore.case = TRUE, perl = TRUE
  )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("(\\b|\\d)", "(", high_pattern, ")", "(\\b|\\d)"),
                                replacement = "\\1high\\3",
                                ignore.case = TRUE, perl = TRUE
  )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("(\\b|\\d)", "(", low_pattern, ")", "(\\b|\\d)"),
                                replacement = "\\1low\\3",
                                ignore.case = TRUE, perl = TRUE
  )
  # Detect if any flags or units are present adjacent to a number
  adjacent_pattern1 <- paste0("^(",Qn_patterns,")\\s*(",flags_pattern,"|-)$")
  adjacent_pattern2 <- paste0("^(",flags_pattern,")\\s*(",Qn_patterns,")$")
  lab_data$cleaning_comments[
    grepl(
      lab_data$clean_result, pattern = adjacent_pattern1, ignore.case = TRUE
    )
  ] <- "flag"
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = adjacent_pattern1, ignore.case = TRUE,
                                replacement = "\\1"
  )
  lab_data$cleaning_comments[
    grepl(
      lab_data$clean_result, pattern = adjacent_pattern2, ignore.case = TRUE
    )
  ] <- "flag"
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = adjacent_pattern2, ignore.case = TRUE,
                                replacement = "\\2"
  )
  # negative & positive signs >> removed
  lab_data$cleaning_comments[
    grepl(
      lab_data$clean_result, pattern = paste0("^(-\\s+)(",Qn_patterns,")$")
    )
  ] <- "flag"
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(-\\s+)(",Qn_patterns,")$"),
                                replacement = "\\2"
                                )
  lab_data$cleaning_comments[
    grepl(
      lab_data$clean_result, pattern = paste0("^([+]\\s*)(",Qn_patterns,")$")
    )
  ] <- "flag"
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^([+]\\s*)(",Qn_patterns,")$"),
                                replacement = "\\2"
  )
  # reporting numbers of records detected with flags
  if (report){
    flag_records <- sum(grepl("flag", lab_data$cleaning_comments))
    flag_records_n <- ifelse(!is.na(n_records),
                             sum(lab_data[[n_records]][grepl("flag", lab_data$cleaning_comments)]),
                             NA
    )
    flag_records_percent <- ifelse(!is.na(n_records),
                                   round((flag_records_n / total_n_records * 100), 3),
                                   NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, flag_records, reset, "  records with interpretative flags (e.g. positive, negative, H, L) -> flags removed with cleaning comment added ", blue, "flag", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, flag_records, reset, " distinct results (",blue, flag_records_percent, "%", reset," of the total result records) with interpretative flags (e.g. positive, negative, H, L) -> flags removed with cleaning comment added ", blue, "flag", reset, ").\n"))
    }
  }
  # Handling unit (Percents & Exponents)
  percent_flag <- grepl(
    lab_data$clean_result, pattern = paste0("^(",number_pattern,") *%$")
    )
  lab_data$cleaning_comments[percent_flag] <- ifelse(is.na(lab_data$cleaning_comments[percent_flag]),
                                                     "Percent",
                                                     paste0(lab_data$cleaning_comments[percent_flag],", Percent")
                                                     )
  rm(percent_flag)
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(",number_pattern,") *%$"),
                                replacement = "\\1"
                                )
  exponent_flag <- grepl(
    lab_data$clean_result, pattern = paste0("^(",number_pattern,")(",exponent_pattern,")$")
  )
  lab_data$cleaning_comments[exponent_flag] <- ifelse(is.na(lab_data$cleaning_comments[exponent_flag]),
                                                     "Exponents",
                                                     paste0(lab_data$cleaning_comments[exponent_flag],", Exponents")
  )
  rm(exponent_flag)
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(",number_pattern,")(",exponent_pattern,")$"),
                                replacement = "\\1"
                                )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = "[\\*\\^]",
                                replacement = "", perl = TRUE
                                )
  unit_flag <- grepl(
    lab_data$clean_result, pattern = paste0("^(",number_pattern,")(",unit_pattern,")$")
  )
  lab_data$cleaning_comments[unit_flag] <- ifelse(is.na(lab_data$cleaning_comments[unit_flag]),
                                                      "Units",
                                                      paste0(lab_data$cleaning_comments[unit_flag],", Units")
  )
  rm(unit_flag)
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(",number_pattern,")(",unit_pattern,")$"),
                                replacement = "\\1"
  )
  # reporting numbers of records detected with units
  if (report){
    units_records <- sum(grepl("Percent|Exponents|Units", lab_data$cleaning_comments))
    units_records_n <- ifelse(!is.na(n_records),
                              sum(lab_data[[n_records]][grepl("Percent|Exponents|Units", lab_data$cleaning_comments)]),
                              NA
    )
    units_records_percent <- ifelse(!is.na(n_records),
                                    round((units_records_n / total_n_records * 100), 3),
                                    NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, units_records, reset, " records with unit (%, exponents, or other units) -> units removed with cleaning comment added (", blue, "Percent, Exponent, or Units", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, units_records, reset, " distinct results (",blue, units_records_percent, "%", reset," of the total result records) with unit (%, exponents, or other units) -> units removed with cleaning comment added ", blue, "Percent, Exponent, or Units", reset, ").\n"))
    }
  }

#### Function Block 3: Detect and assign scale types - Part 1 ####
  # This block identifies the scale type of each result value using regular expressions and assigns it to the scale_type column.
  # It specifically handles Ordinal scale types (Ord.2 and Ord.1) and Special Quantitative scale types (Qn.2, Qn.3 and Qn.4).
  if (report){
    cat(paste0(bold, "Step 2: classify and standardize different scale types - part 1",reset,"\n"))
    cat("==========================================================================================\n")
  }
  # Ord.2: This scale type describes the amount of the substance analyzed in 4 grades ("1+","2+","3+","4+") or ("+","++","+++","++++")
  positive_grades_pattern <- "[1-4]{1}[+]|[+]{1,4}"
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(",positive_grades_pattern,") *(positive)$"), ignore.case = TRUE,
                                replacement = "\\1"
                                )
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = paste0("^(positive) *(",positive_grades_pattern,")$"), ignore.case = TRUE,
                                replacement = "\\2"
                                )
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = "^[1-4]{1}[+]$|^[+]{1,4}$"
      )
  ] <- "Ord.2"
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = "^-$"
    )
  ] <- "Ord.1"
  # removing text
  lab_data$clean_result <- gsub(lab_data$clean_result,
                                pattern = "[A-Za-z]",
                                replacement = ""
                                )
  # Qn.3: This scale type describes Numeric Range results when two numbers are separated by a hyphen
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = paste0("^",Qn3_pattern,"$")
    )
  ] <- "Qn.3"

  # Qn.2: This scale type describes inequality results when a single number is preceded by relational operators (<, >, <= or >=),
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = paste0("^",Qn2_pattern,"$")
    )
  ] <- "Qn.2"

  # Qn.4: This scale type describes Titer results when "1" is divided by a second integer number. Both are separated by ":" or "/",
  #       and Can be preceded by relational operators
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = paste0("^",Qn4_pattern,"$")
    )
  ] <- "Qn.4"

####  Function Block 4: Standardization of types Ord.2, Qn.3, Qn.2, Qn.4
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Ord.2", "Ord.1") & !is.na(lab_data$scale_type),
                                  gsub("^[+]{4}$", "4+",
                                       gsub("^[+]{3}$", "3+",
                                            gsub("^[+]{2}$", "2+",
                                                 gsub("^[+]{1}$", "1+",
                                                      gsub("^-$", "Neg", lab_data$clean_result))))),
                                  lab_data$clean_result
                                  )
  lab_data$cleaning_comments[
    lab_data$scale_type == "Ord.2"
  ] <- NA
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Qn.2", "Qn.3","Qn.4") & !is.na(lab_data$scale_type),
                                  gsub("/", ":",
                                       gsub(" ", "", lab_data$clean_result)),
                                  lab_data$clean_result
                                  )
  # In Qn.3, check if the second number is higher than the first number
  # Extract the first and second numbers from the 'clean_result' column
  lab_data[["first_number"]] <- ifelse(lab_data$scale_type == "Qn.3" & !is.na(lab_data$scale_type),
                                  gsub("^([0-9]+)-[0-9]+$", "\\1",
                                       lab_data$clean_result), NA)
  lab_data[["second_number"]] <- ifelse(lab_data$scale_type == "Qn.3"  & !is.na(lab_data$scale_type),
                                   gsub("^[0-9]+-([0-9]+)$", "\\1",
                                        lab_data$clean_result), NA)
  # Remove scale type if the second number is lower than the first number
  lab_data$scale_type <- ifelse(lab_data$scale_type == "Qn.3"  & !is.na(lab_data$scale_type) &
                                  lab_data$second_number < lab_data$first_number,
                                NA,
                                lab_data$scale_type)
  # Remove temporary columns 'first_number' and 'second_number'
  lab_data <- subset(lab_data, select = -c(first_number, second_number))

  # reporting numbers of records assigned to scale types Ord.2, Qn.2, Qn.3, Qn.4
  if (report){
    ord2_records <- sum(grepl("Ord.2", lab_data$scale_type))
    ord2_records_n <- ifelse(!is.na(n_records),
                             sum(lab_data[[n_records]][grepl("Ord.2", lab_data$scale_type)]),
                             NA)
    ord2_records_percent <- ifelse(!is.na(n_records),
                                   round((ord2_records_n / total_n_records * 100), 3),
                                   NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, ord2_records, reset," result records of scale type ", blue, "'Ord.2'", reset, ", which describes grades of positivity (e.g. 2+, 3+).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, ord2_records, reset," distinct results (", blue, ord2_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Ord.2'", reset, ", which describes grades of positivity (e.g. 2+, 3+).\n"))
    }
    Qn2_records <- sum(grepl("Qn.2", lab_data$scale_type))
    Qn2_records_n <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]][grepl("Qn.2", lab_data$scale_type)]),
                            NA)
    Qn2_records_percent <- ifelse(!is.na(n_records),
                                  round((Qn2_records_n / total_n_records * 100), 3),
                                  NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn2_records, reset, " result records of scale type ", blue, "'Qn.2'", reset, ", which describes inequality results (e.g. >120, <1).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn2_records, reset, " distinct results (", blue, Qn2_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Qn.2'", reset, ", which describes inequality results (e.g. >120, <1).\n"))
    }
    Qn3_records <- sum(grepl("Qn.3", lab_data$scale_type))
    Qn3_records_n <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]][grepl("Qn.3", lab_data$scale_type)]),
                            NA)
    Qn3_records_percent <- ifelse(!is.na(n_records),
                                  round((Qn3_records_n / total_n_records * 100), 3),
                                  NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn3_records, reset, " result records of scale type ", blue, "'Qn.3'", reset, ", which describes numeric range results (e.g. 2-4).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn3_records, reset, " distinct results (", blue, Qn3_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Qn.3'", reset, ", which describes numeric range results (e.g. 2-4).\n"))
    }
    Qn4_records <- sum(grepl("Qn.4", lab_data$scale_type))
    Qn4_records_n <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]][grepl("Qn.4", lab_data$scale_type)]),
                            NA)
    Qn4_records_percent <- ifelse(!is.na(n_records),
                                  round((Qn4_records_n / total_n_records * 100), 3),
                                  NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn4_records, reset, " result records of scale type ", blue, "'Qn.4'", reset, ", which describes titer results (e.g. 1/40).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn4_records, reset, " distinct results (", blue, Qn4_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Qn.4'", reset, ", which describes titer results (e.g. 1/40).\n"))
    }
  }

#### Function Block 5: Standardize Qn.1 numeric results according to locale ####
  # In our R package, we chose these conditions for standard number formatting:
  #  - Only Arabic numerals (0-9) allowed >> detected by regex pattern
  #  - No digit grouping (either thousands or hundreds separator) >> removed based on locale cases
  #  - The character used as the decimal separator is the period ".", not the comma "," >> Corrected based on locale cases
  #  - No leading zeros except followed by a decimal separator (e.g. 0.1) >>
  #  - Maximum three fractional digits after the decimal point with no ending zeros (i.e. 1.35 not 1.350, and 1 not 1.0).
  #    rounded if more than three and left as is if less than three.
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = paste0("^",Qn1_pattern,"$")
      )
    ] <- "Qn.1"
  # reporting numbers of records assigned to scale type Qn.1
  if (report){
    Qn1_records <- sum(grepl("Qn.1", lab_data$scale_type))
    Qn1_records_n <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]][grepl("Qn.1", lab_data$scale_type)]),
                            NA)
    Qn1_records_percent <- ifelse(!is.na(n_records),
                                  round((Qn1_records_n / total_n_records * 100), 3),
                                  NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn1_records, reset, " result records of scale type ", blue, "'Qn.1'", reset, ", which describes numeric results (e.g. 56, 5.6, 5600).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Qn1_records, reset, " distinct results (", blue, Qn1_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Qn.1'", reset, ", which describes numeric results (e.g. 56, 5.6, 5600).\n"))
    }
  }

#position of comma or period from right or left
  # if more than one , or . of the same type >> then it's thousand separator >> Remove
  # from the end if in position 2 or 3 >> then it's surely a decimal separator >> "," to "."
  # from the start if in position 5 or more >> then it's surely a decimal separator >> "," to "."
  # if other cases, not sure enough, create a cleaning comment
  subset_indices <- which(lab_data$scale_type %in% c("Qn.1", "Qn.2") &
                            !grepl("Exponents", lab_data$cleaning_comments) &
                            !grepl("^[<>=]{0,2}0[.,]", lab_data$clean_result) &
                            grepl("^[<>=]{0,2}[0-9]{1,3}[.,][0-9]{3}$", lab_data$clean_result)
  )
  lab_data$cleaning_comments[subset_indices] <- ifelse(
    is.na(lab_data$cleaning_comments[subset_indices]),
    "locale_check",
    paste0(lab_data$cleaning_comments[subset_indices],", locale_check")
  )
  rm(subset_indices)
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Qn.1", "Qn.2") &
      !grepl("locale_check", lab_data$cleaning_comments),
    ifelse(
      grepl(",", lab_data$clean_result) & grepl("\\.", lab_data$clean_result),
      gsub("^([<>=]{0,2}[0-9]{1,3})[.]([0-9]{3})[,]([0-9]+)$", paste0("\\1","\\2",".","\\3"),
           gsub("^([<>=]{0,2}[0-9]{1,3})[,]([0-9]{3}[.][0-9]+)$", paste0("\\1","\\2"),
                lab_data$clean_result)
      ),
      ifelse(
        grepl(",", lab_data$clean_result),
        gsub(",", ".", lab_data$clean_result, fixed = TRUE),
        lab_data$clean_result
      )
    ),
    lab_data$clean_result
  )
  lab_data$clean_result <- ifelse(locale == "US" &
                                    lab_data$scale_type %in% c("Qn.1", "Qn.2") & !is.na(lab_data$scale_type) &
                                    grepl("locale_check", lab_data$cleaning_comments),
                                  gsub(",", "", fixed = TRUE,
                                       lab_data$clean_result
                                       ),
                                  lab_data$clean_result
                                  )
  lab_data$clean_result <- ifelse(locale == "DE" &
                                    lab_data$scale_type %in% c("Qn.1", "Qn.2") & !is.na(lab_data$scale_type) &
                                    grepl("locale_check", lab_data$cleaning_comments),
                                  gsub(",", ".", fixed = TRUE,
                                       gsub(".", "", fixed = TRUE,
                                             lab_data$clean_result)),
                                  lab_data$clean_result
                                  )
  lab_data$cleaning_comments[locale == "US" | locale == "DE" &
                               grepl("locale_check", lab_data$cleaning_comments)
                             ] <- gsub("locale_check", "",
                                       gsub(", locale_check", "", lab_data$cleaning_comments))
  suppressWarnings({
    lab_data$clean_result <- ifelse(!is.na(lab_data$clean_result) &
                                      lab_data$scale_type == "Qn.1" & !is.na(lab_data$scale_type) &
                                      (!grepl("locale_check", lab_data$cleaning_comments) | is.na(lab_data$cleaning_comments)),
                                    round(as.numeric(lab_data$clean_result, na.rm = TRUE), 3),
                                    lab_data$clean_result
                                    )
  })
  lab_data$scale_type[
    is.na(lab_data$clean_result)
  ] <- NA
  # reporting numbers of Qn.1 records that requires locale check
  if (report){
    locale_check_records <- sum(grepl("locale_check", lab_data$cleaning_comments))
    locale_check_records_n <- ifelse(!is.na(n_records),
                                     sum(lab_data[[n_records]][grepl("locale_check", lab_data$cleaning_comments)]),
                                     NA)
    locale_check_records_percent <- ifelse(!is.na(n_records),
                                           round((locale_check_records_n / total_n_records * 100), 3),
                                           NA)
    if (is.na(n_records) & locale == "NO") {
      cat(paste0(red, Warning, reset," ",blue, locale_check_records, reset, " records with numeric result values that cannot be determined without predefined locale setting (US or DE) -> cleaning comment added ", blue, "locale_check", reset, ").\n"))
    }
    if (!is.na(n_records) & locale == "NO") {
      cat(paste0(red, Warning, reset," ",blue, locale_check_records, reset, " distinct results (",blue, locale_check_records_percent, "%", reset," of the total result records) with numeric result values that cannot be determined without predefined locale setting (US or DE) -> cleaning comment added ", blue, "locale_check", reset, ").\n"))
    }
  }
  # Remove any leading zeros except followed by a decimal separator (e.g. 0.1)
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Qn.1", "Qn.2") & !is.na(lab_data$scale_type) &
                                    (!grepl("locale_check", lab_data$cleaning_comments)),
                                  gsub(lab_data$clean_result,
                                       pattern = "^([<>=-]{0,2})(0+)([0-9]+)",
                                       replacement = paste0("\\1", "\\3")
                                       ),
                                   lab_data$clean_result
                                   )
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Qn.1", "Qn.2") & !is.na(lab_data$scale_type) &
                                    !grepl("locale_check", lab_data$cleaning_comments),
                                  gsub(lab_data$clean_result,
                                       pattern = "^([<>=-]{0,2}) ?([.][0-9]+)",
                                       replacement = paste0("\\1", "0", "\\2")
                                       ),
                                  lab_data$clean_result
                                  )

  lab_data$clean_result <- ifelse(lab_data$scale_type == "Qn.3" & !is.na(lab_data$scale_type),
                                  gsub(lab_data$clean_result,
                                       pattern = "^0+([1-9]{1-3}-)0+([1-9]{1-3})",
                                       replacement = paste0("\\1", "\\2")
                                  ),
                                  lab_data$clean_result
                                  )
  # Remove any trailing zeros
  lab_data$clean_result <- ifelse(lab_data$scale_type %in% c("Qn.1", "Qn.2") & !is.na(lab_data$scale_type) &
                                    !grepl("locale_check", lab_data$cleaning_comments),
                                  gsub(lab_data$clean_result,
                                       pattern = "(-?[0-9]+[.][0-9]*[1-9])(0+)$",
                                       replacement = "\\1"
                                       ),
                                  lab_data$clean_result
                                  )

#### Function Block 6: Mining text results and assigning them one of the relevant scale type (Ordinal, or Nominal) ####
  # ord.1: This scale type describe the detection of the substance analyzed either as positive or negative.
  #        there can be a gray zone in-between both values to determine intermediate, or "Gray Zone" results
  # detecting positive and negative words
  subset_data <- subset(lab_data, is.na(lab_data$scale_type))
  subset_data$clean_result <- subset_data[[raw_result]]
  subset_data$clean_result <- iconv(subset_data$clean_result, to ="ASCII//TRANSLIT")
  # remove space regex and some invaluable punct characters, with escaping metacharacters
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                pattern = "[!#$&\\'()*;?@_`{|}~\"\\[\\]:/<=>+%\\^.,]",
                                replacement = "", perl = TRUE
                                )
  # leading and trailing space characters
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                pattern = "^\\s+|\\s+$",
                                replacement = ""
                                )
  # Preprocessing text of different languages
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                   pattern = positive_pattern, ignore.case = TRUE,
                                   replacement = "positive"
                                   )
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                   pattern = negative_pattern, ignore.case = TRUE,
                                   replacement = "negative"
                                   )
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                   pattern = sample_pattern, ignore.case = TRUE,
                                   replacement = "sample"
                                   )
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                   pattern = specimen_pattern, ignore.case = TRUE,
                                   replacement = "specimen"
                                   )

  # Detection of Blood Group Types (Nom.1)
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                pattern = "^(A|B|AB|O)(\\sRh)? *(negative)$", ignore.case = TRUE,
                                replacement = paste0("\\1", "-")
                                )
  subset_data$clean_result <- gsub(subset_data$clean_result,
                                pattern = "^(A|B|AB|O)(\\sRh)? *(positive)$", ignore.case = TRUE,
                                replacement = paste0("\\1", "+")
                                )
  # Detection & Standardization of Positive and Negative results (Ord.1)
  subset_data$clean_result[
    grepl("negative", subset_data$clean_result, ignore.case = TRUE) &
      grepl("positive", subset_data$clean_result, ignore.case = TRUE)
  ] <- "Neg & Pos"
  subset_data$clean_result[
    grepl("negative", subset_data$clean_result, ignore.case = TRUE
          )
  ] <- "Neg"
  subset_data$clean_result[
    grepl("positive", subset_data$clean_result, ignore.case = TRUE
          )
  ] <- "Pos"
  subset_data$clean_result[
    grepl(
      subset_data$clean_result, pattern = normal_pattern, ignore.case = TRUE
    )
  ] <- "Normal"
  subset_data$clean_result[
    grepl(
      subset_data$clean_result, pattern = notdetected_pattern, ignore.case = TRUE
      )
  ] <- "Neg"
  lab_data$clean_result[
    is.na(lab_data$scale_type)
  ] <- subset_data$clean_result
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = "^neg$|^pos$|^normal$", ignore.case = TRUE
    )
  ] <- "Ord.1"
  lab_data$clean_result <- ifelse(lab_data$scale_type == "Ord.1" & !is.na(lab_data$scale_type),
                                  paste0(toupper(substr(lab_data$clean_result, 1, 1)),
                                         tolower(substr(lab_data$clean_result, 2, nchar(lab_data$clean_result)))),
                                  lab_data$clean_result
                                  )
  # reporting numbers of records assigned to scale type Ord.1
  if (report){
    Ord1_records <- sum(grepl("Ord.1", lab_data$scale_type))
    Ord1_records_n <- ifelse(!is.na(n_records),
                             sum(lab_data[[n_records]][grepl("Ord.1", lab_data$scale_type)]),
                             NA)
    Ord1_records_percent <- ifelse(!is.na(n_records),
                                   round((Ord1_records_n / total_n_records * 100), 3),
                                   NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Ord1_records, reset, " result records of scale type ", blue, "'Ord.1'", reset, ", which describes positive or negative results (Neg, Pos, or Normal).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Ord1_records, reset, " distinct results (", blue, Ord1_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Ord.1'", reset, ", which describes positive or negative results (Neg, Pos, or Normal).\n"))
    }
  }
  # Detection & Standardization of Blood Group Types (Nom.1)
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = "^(A|B|AB|O)(\\sRh)? *(pos|neg|\\+|-)?$", ignore.case = TRUE
      )
    ] <- "Nom.1"
  lab_data$clean_result <- ifelse(lab_data$scale_type == "Nom.1" & !is.na(lab_data$scale_type),
                                  toupper(lab_data$clean_result),
                                  lab_data$clean_result
                                  )
  lab_data$scale_type[
    grepl(
      lab_data$clean_result, pattern = "^(ccee)(\\sK)? *(\\+|-)?$", ignore.case = TRUE
    )
  ] <- "Nom.1"
  lab_data$clean_result <- ifelse(lab_data$scale_type == "Nom.1" & !is.na(lab_data$scale_type),
                                  gsub("\\s", "",
                                       gsub("Rh", "", ignore.case = TRUE,
                                            gsub("neg", "-", ignore.case = TRUE,
                                                 gsub("pos", "+", lab_data$clean_result, ignore.case = TRUE)))),
                                  lab_data$clean_result
                                  )
  # reporting numbers of records assigned to scale type Ord.1
  if (report){
    Nom1_records <- sum(grepl("Nom.1", lab_data$scale_type))
    Nom1_records_n <- ifelse(!is.na(n_records),
                             sum(lab_data[[n_records]][grepl("Nom.1", lab_data$scale_type)]),
                             NA)
    Nom1_records_percent <- ifelse(!is.na(n_records),
                                   round((Nom1_records_n / total_n_records * 100), 3),
                                   NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Nom1_records, reset, " result records of scale type ", blue, "'Nom.1'", reset, ", which describes blood groups (e.g. A+, AB).\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, Nom1_records, reset, " distinct results (", blue, Nom1_records_percent, "%", reset," of the total result records) of scale type ", blue, "'Nom.1'", reset, ", which describes blood groups (e.g. A+, AB).\n"))
    }
  }

#### Handling other not standardized result values ####
  if (report){
    cat(paste0(bold, "Last Step: Classifying non-standard text records", reset, "\n"))
    cat("==========================================================================================\n")
  }
  lab_data$cleaning_comments <- ifelse(lab_data$clean_result == "Neg & Pos" & !is.na(lab_data$clean_result) &
                                         is.na(lab_data$scale_type),
                                       "multiple_results",
                                       lab_data$cleaning_comments
                                       )
  lab_data$cleaning_comments <- ifelse(is.na(lab_data$scale_type) & is.na(lab_data$cleaning_comments) &
                                         grepl(
                                           pattern = "sample|specimen",
                                           lab_data$clean_result, ignore.case = TRUE),
                                       "test_not_performed",
                                       lab_data$cleaning_comments
                                       )
  lab_data$cleaning_comments <- ifelse(lab_data$clean_result == "" | is.na(lab_data$clean_result),
                                       "No_result",
                                       lab_data$cleaning_comments)
  lab_data$cleaning_comments <- ifelse(is.na(lab_data$scale_type) & is.na(lab_data$cleaning_comments),
                                       "not_standardized",
                                       lab_data$cleaning_comments
                                       )

  # reporting number of records with non standardized result values
  if (report){
    pos_neg_records <- sum(grepl("multiple_results", lab_data$cleaning_comments))
    pos_neg_records_n <- ifelse(!is.na(n_records),
                                sum(lab_data[[n_records]][grepl("multiple_results", lab_data$cleaning_comments)]),
                                NA)
    pos_neg_records_percent <- ifelse(!is.na(n_records),
                                      round((pos_neg_records_n / total_n_records * 100), 3),
                                      NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, pos_neg_records, reset, " text result records with multiple result values (e.g. postive X & negative Y) -> cleaning comment added (", blue, "multiple_results", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, pos_neg_records, reset, " distinct results (",blue, pos_neg_records_percent, "%", reset," of the total result records) with multiple result values (e.g. postive X & negative Y) -> cleaning comment added (", blue, "multiple_results", reset, ").\n"))
    }
    TNP_records <- sum(grepl("test_not_performed", lab_data$cleaning_comments))
    TNP_records_n <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]][grepl("test_not_performed", lab_data$cleaning_comments)]),
                            NA)
    TNP_records_percent <- ifelse(!is.na(n_records),
                                  round((TNP_records_n / total_n_records * 100), 3),
                                  NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, TNP_records, reset, " text result records with words about sample or specimen (e.g. sample not found) -> cleaning comment added (", blue, "test_not_performed", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, TNP_records, reset, " distinct results (",blue, TNP_records_percent, "%", reset," of the total result records) with words about sample or specimen (e.g. sample not found) -> cleaning comment added (", blue, "test_not_performed", reset, ").\n"))
    }
    No_result_records <- sum(grepl("No_result", lab_data$cleaning_comments))
    No_result_records_n <- ifelse(!is.na(n_records),
                                  sum(lab_data[[n_records]][grepl("No_result", lab_data$cleaning_comments)]),
                                  NA)
    No_result_records_percent <- ifelse(!is.na(n_records),
                                        round((No_result_records_n / total_n_records * 100), 3),
                                        NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, No_result_records, reset, " result records with meaningless inputs (e.g. = , .) -> cleaning comment added (", blue, "No_result", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, No_result_records, reset, " distinct results (",blue, No_result_records_percent, "%", reset," of the total result records) with meaningless inputs (e.g. = , .) -> cleaning comment added (", blue, "No_result", reset, ").\n"))
    }
    not_standard_records <- sum(grepl("not_standardized", lab_data$cleaning_comments))
    not_standard_records_n <- ifelse(!is.na(n_records),
                                     sum(lab_data[[n_records]][grepl("not_standardized", lab_data$cleaning_comments)]),
                                     NA)
    not_standard_records_percent <- ifelse(!is.na(n_records),
                                           round((not_standard_records_n / total_n_records * 100), 3),
                                           NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_standard_records, reset, " text result records that could not be standardized or classified -> cleaning comment added (", blue, "not_standardized", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_standard_records, reset, " distinct results (",blue, not_standard_records_percent, "%", reset," of the total result records) that could not be standardized or classified -> cleaning comment added (", blue, "not_standardized", reset, ").\n"))
    }
  }

  lab_data$clean_result[
    is.na(lab_data$scale_type)
  ] <- NA
  if (report){
    cat("==========================================================================================\n")
  }
  standard_records <- sum(!is.na(lab_data$scale_type))
  standard_records_n <- ifelse(!is.na(n_records),
                                   sum(lab_data[[n_records]][!is.na(lab_data$scale_type)]),
                                   NA)
  standard_records_percent <- ifelse(!is.na(n_records),
                                         round((standard_records_n / total_n_records * 100), 3),
                                         NA)
  if (is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, standard_records, reset, " result records were cleaned, classified, and standardized.\n"))
  }
  if (!is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, standard_records, reset, " distinct results (",blue, standard_records_percent, "%", reset," of the total result records) were cleaned, classified, and standardized.\n"))
  }
  end.time <- Sys.time()
  time.taken <- as.numeric(difftime(end.time, start.time, units = "secs"))
    # Break into minutes and seconds
  mins <- floor(time.taken / 60)
  secs <- round(time.taken %% 60, 1)
  cat(paste0(clock, " ", bold, "Time taken is ", blue, mins, " min, ", secs, " sec\n", reset))

  return(lab_data)
}
