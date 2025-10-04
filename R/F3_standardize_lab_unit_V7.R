# Specify global variables
utils::globalVariables(c("RWD_units_to_UCUM_V2"))

#' Clean and Standardize Formats of Laboratory Units of Measurement
#'
#' This function is designed to clean and standardize formats of laboratory units of measurement.
#' It standardizes the units' format according to the Unified Code for Units of Measure (UCUM) https://ucum.org/ucum
#'
#' @param lab_data A data frame containing laboratory data.
#' @param raw_unit The column in `lab_data` that contains raw units to be cleaned.
#' @param report A report is written in the console. Defaults to "TRUE".
#' @param n_records In case you are loading a grouped list of distinct results, then you can assign the n_records to the column that contains the frequency of each distinct result. Defaults to NA.
#'
#' @importFrom utils data globalVariables
#'
#' @return A modified `lab_data` data frame with additional columns:
#'   * `ucum_code`: Cleaned and standardized units according to UCUM syntax.
#'   * `cleaning_comments`: Comments about the cleaning process for each unit.
#'
#' @details
#' The function undergoes the following methodology:
#'   1. Pre-processing unit srings.
#'   2. Lookup in commom units database.
#'   3. Check Syntax Integrity of units with no UCUM match.
#'   4. Parsing of units which passesd checks (tokenize and classify)
#'   5. Restructuring of parsed units (apply correction rules & final validation)
#'
#' Internal Datasets:
#' The function uses an internal dataset; `RWD_units_to_UCUM_V2` which contains 3739 synonyms of 1448 ucum units.
#'
#' @author Ahmed Zayed <ahmed.zayed@kuleuven.be>, Ilias Sarikakis <sarikakisilias@gmail.com>
#'
#' @note
#' This function is part of a larger data cleaning pipeline and should be evaluated in that context.
#' The package framework includes functions for cleaning result values and validating quantitative results for each test identifier.
#'
#' Performance of the function can be affected by the size of `lab_data`. Considerations for data size
#' or pre-processing may be needed.
#'
#' @seealso
#' Function 1 for result value cleaning,
#' Function 2 for result validation,
#' Function 3 for unit format cleaning,
#' Function 4 for unit conversion.
#'
#' @export
#### Function Body ####
#----------------------------------------------------------------------------------#
standardize_lab_unit <- function(lab_data, raw_unit, report = TRUE, n_records = NA){
  #add two new columns
  start.time <- Sys.time()
  lab_data$clean_unit <- lab_data[[raw_unit]]
  lab_data$cleaning_comments <- NA
  total_n_records <- ifelse(!is.na(n_records),
                            sum(lab_data[[n_records]]),
                            NA
                            )
  # Loading required files
  data("RWD_units_to_UCUM_V2", package = "lab2clean", envir = environment())
  data("parsed_units_df", package = "lab2clean", envir = environment())
  units_df <- parsed_units_df[!grepl("\\{", parsed_units_df$csCode_),]
  data("annotable_strings", package = "lab2clean", envir = environment())
  annotable_strings <- annotable_strings$annotation

  # Define ANSI escape codes for text decorations and colors
  bold <- "\033[1m"
  reset <- "\033[0m"
  Warning <- "\u26A0"
  success <- "\u2714"
  red <- "\033[31m"
  green <- "\033[32m"
  blue <- "\033[34m"
  clock <- "\u23F0"

  #### Function Block 1: Preprocessing ####
  # Dealing with special characters which are meaningful in laboratory units but not part of the ASCII 33-126 characters allowed by ucum
  # https://www.ibm.com/docs/en/sdse/6.4.0?topic=configuration-ascii-characters-from-33-126z
  if (report){
    cat(paste0(bold, "Step 1: Preprocessing unit srings", reset, "\n"))
    cat("==========================================================================================\n")
  }

  clean_units <- function(x) {
    # 1. normalise: whatever the input encoding is, give me UTF-8
    x <- iconv(x, from = "", to = "UTF-8")

    # 2. turn textual escapes like "<U+00B5>" into the real char
    # find every <U+XXXX> sequence and replace it with intToUtf8(hex)
    tag_pat <- gregexpr("<U\\+([0-9A-Fa-f]{4})>", x, perl = TRUE)
    regmatches(x, tag_pat) <- lapply(regmatches(x, tag_pat), function(hits) {
      vapply(hits, function(tag) intToUtf8(strtoi(substr(tag, 4, 7), 16L)), "")
    })

    # 3. ASCII replacements
    repl <- c("\u00B5" = "u",  # micro-sign
              "\u03BC" = "u",  # Greek mu
              "\u00D7" = "*",  # multiplication sign
              "\u00B9" = "^1", "\u00B2" = "^2", "\u00B3" = "^3",
              "\u2074" = "^4", "\u2075" = "^5", "\u2076" = "^6",
              "\u2077" = "^7", "\u2078" = "^8", "\u2079" = "^9")

    for (i in seq_along(repl))          # loop is fine, column is short
      x <- gsub(names(repl)[i], repl[i], x, perl = TRUE)

    # 4. the usual latin shortcuts
    x <- gsub("\\bmcg\\b",  "ug", x, ignore.case = TRUE)
    x <- gsub("\\bmcl\\b",  "uL", x, ignore.case = TRUE)
    x <- gsub("\\bmicro\\s*", "u", x, ignore.case = TRUE)

    x
  }
  lab_data$clean_unit <- clean_units(lab_data$clean_unit)

  # removing all other non-ASCII characters
  lab_data$clean_unit <- iconv(as.character(lab_data$clean_unit), to = "ASCII", sub = "")

  # remove leading and trailing space characters
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "^\\s+|\\s+$",
                              replacement = "", perl = T
  )
  # remove leading and trailing multiplication operators & trailing division operator
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "^x|^\\*|^\\.|^\\^|\\.$|\\*$|x$|\\/$",
                              replacement = ""
                              )
  # remove leading and trailing +/-
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "^\\+|^-|\\+$|-$",
                              replacement = "", perl = T
  )
  # remove leading and trailing space characters
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "^\\s+|\\s+$",
                              replacement = "", perl = T
                              )
  # Handle multiple similar characters in a row (operators, percent, and space)
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "\\.+",
                              replacement = "."
  )
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "\\/+",
                              replacement = "/"
  )
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "\\*+",
                              replacement = "*"
  )
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "\\%+",
                              replacement = "%"
  )
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "\\s+",
                              replacement = " "
  )
  # Handling different exponent expressions 10exp, 10E, 10e
  exponent_pattern <- "E|exp|\\^"
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = paste0("(10)", "(", exponent_pattern, ")", "([+-]?[0-9]{1,2})"),
                              replacement = "\\1*\\3",
                              ignore.case = TRUE
                              )
  # Standardizing mm3 formats to uL
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "mm(\\*|\\^)?3",
                              replacement = "uL",
                              ignore.case = TRUE
  )
  # Standardizing GFR units (as specially complex common unit)
  lab_data$clean_unit[
    grepl(
      "ml/m[in]{0,2}/1[,.]73", lab_data$clean_unit, ignore.case = TRUE
      )] <-
    "mL/min/{1.73_m2}"

  # Handle I.U. OR U.I.>> [IU]
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "I.U.",
                              replacement = "[IU]"
  )
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                              pattern = "U.I.",
                              replacement = "[IU]"
  )

  # remove space regex and some invaluable punct characters, with escaping metacharacters
  lab_data$clean_unit <- gsub(lab_data$clean_unit,
                                 pattern = "[!$;?@|~=]",
                                 replacement = "", perl = T
  )
  lab_data$cleaning_comments <- ifelse(is.na(lab_data$clean_unit) | lab_data$clean_unit == "",
                                       "No unit", lab_data$cleaning_comments)
  # reporting numbers of records detected with no units
  if (report){
    no_unit_records <- sum(grepl("No unit", lab_data$cleaning_comments))
    no_unit_records_n <- ifelse(!is.na(n_records),
                             sum(lab_data[[n_records]][grepl("No unit", lab_data$cleaning_comments)]),
                             NA
    )
    no_unit_records_percent <- ifelse(!is.na(n_records),
                                   round((no_unit_records_n / total_n_records * 100), 3),
                                   NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, no_unit_records, reset, " records with no units after pre-processing -> cleaning comment added ", blue, "No unit", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, no_unit_records, reset, " distinct unit strings (",blue, no_unit_records_percent, "%", reset," of the total result records) with no units after pre-processing -> cleaning comment added ", blue, "No unit", reset, ").\n"))
    }
  }
  #### Function Block 2: Merging with the mapping table  ####
  if (report){
    cat(paste0(bold, "Step 2: Lookup in commom units database", reset, "\n"))
    cat("==========================================================================================\n")
  }
  # case insensitive match
  lab_data$clean_unit_lower <- tolower(lab_data$clean_unit)
  lab_data <- merge(lab_data, RWD_units_to_UCUM_V2, by= "clean_unit_lower", all.x = T)
  lab_data$clean_unit_lower <- NULL
  lab_data$source_match <- NULL
  # reporting numbers of records matched with UCUM units
  if (report){
  lookup_ucum_records <- sum(!is.na(lab_data$ucum_code))
  lookup_ucum_records_n <- ifelse(!is.na(n_records),
                               sum(lab_data[[n_records]][!is.na(lab_data$ucum_code)]),
                               NA)
  lookup_ucum_records_percent <- ifelse(!is.na(n_records),
                                     round((lookup_ucum_records_n / total_n_records * 100), 3),
                                     NA)
  if (is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, lookup_ucum_records, reset, " unit records were matched to ucum codes.\n"))
  }
  if (!is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, lookup_ucum_records, reset, " distinct units (",blue, lookup_ucum_records_percent, "%", reset," of the total result records) were matched to ucum codes.\n"))
  }
  }
  # SPLIT original DATASET
  dataset_1 <- lab_data[!is.na(lab_data$ucum_code) | !is.na(lab_data$cleaning_comments),]
  dataset_2 <- lab_data[is.na(lab_data$ucum_code) & is.na(lab_data$cleaning_comments),]

  #### Function Block 3: UCUM Validation  ####
  if (report){
    cat(paste0(bold, "Step 3: Check Syntax Integrity of units with no UCUM match", reset, "\n"))
    cat("==========================================================================================\n")
  }
  # SPLIT DATASET 2
  #dataset_3 <- dataset_2[is.na(dataset_2$ucum_code) & is.na(dataset_2$cleaning_comments),]
  #dataset_2 <- dataset_2[!is.na(dataset_2$ucum_code) | !is.na(dataset_2$cleaning_comments),]
  subset_data <- dataset_2

  # 1st Check ~ multiple different operators in a row
  active_check <- grepl("\\.\\/|\\/\\.", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", multiple operators in a row"),
                                                        "not_valid - multiple operators in a row")
  # 2nd Check ~ exponent following a parenthesis eg. (16.kg)6
  active_check <- grepl("\\)\\d+", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", exponent following parenthesis"),
                                                        "not_valid - exponent following parenthesis")
  # 3rd Check ~ digits before parenthesis
  active_check <- grepl("\\d+\\(", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", digits before parenthesis"),
                                                        "not_valid - digits before parenthesis")
  # 4th Check ~ braces before parenthesis
  active_check <- grepl("\\}\\(", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", braces before parenthesis"),
                                                        "not_valid - braces before parenthesis")
  # 5th Check ~ characters after parenthesis
  active_check <- grepl("\\)[a-zA-Z]+", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", characters after parenthesis"),
                                                        "not_valid - characters after parenthesis")
  # 6th Check ~ "," exists inside "{...}"
  active_check <- grepl("\\{[^{}]*,[^{}]*\\}", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", comma inside braces"),
                                                        "not_valid - comma inside braces")
  # 7th Check ~ (+) or subtraction (-) operations are being performed
  active_check <- grepl("\\d+[+|-]+\\d+", subset_data$clean_unit)
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", +/- operations"),
                                                        "not_valid - +/- operations")

  # Identifies how many instances of a given pattern exist inside a given string
  string_count <- function(string, pattern){
    loc <- gregexpr(pattern = pattern, text = string, fixed = TRUE)[[1]]
    if(length(loc) > 1){
      return(length(loc))
    }
    else if(as.numeric(loc) != -1){
      return(length(loc))
    }
    else{
      return(0)
    }
  }
  # Identifying number of opening and closing braces
  subset_data$opening_braces <- mapply(string_count,
                                       string = subset_data$clean_unit,
                                       pattern = "{")

  subset_data$closing_braces <- mapply(string_count,
                                       string = subset_data$clean_unit,
                                       pattern = "}")

  # 8th Check ~ missing opening braces "{"
  active_check <- subset_data$opening_braces < subset_data$closing_braces
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", missing opening brace"),
                                                        "not_valid - missing opening brace")
  # 9th Check ~ missing closing braces "}"
  active_check <- subset_data$opening_braces > subset_data$closing_braces
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", missing closing brace"),
                                                        "not_valid - missing closing brace")

  # Checking if the parentheses of a string (if any) are balanced:
  # - All parentheses open and close
  # - No parentheses close before being opened
  # - No parentheses remain opened
  check_parentheses <- function(input_string) {

    # Initialize a counter to keep track of open parentheses
    open_parentheses_count <- 0

    # Iterate through each character in the input string
    for (char in strsplit(input_string, "")[[1]]) {

      # print(char)

      if (char == "(") {
        # Increment the counter if an open parenthesis is encountered
        open_parentheses_count <- open_parentheses_count + 1
      } else if (char == ")") {
        # Decrement the counter if a close parenthesis is encountered
        open_parentheses_count <- open_parentheses_count - 1

        # If the counter goes negative, there was a closing parenthesis without an opening one
        if (open_parentheses_count < 0) {
          return(FALSE)
        }
      }
    }

    # If the counter is not zero at the end, there were more open than close parentheses
    return(open_parentheses_count == 0)
  }
  subset_data$balanced_parentheses <- lapply(subset_data$clean_unit, check_parentheses)

  # 10th Check ~ unbalanced parentheses
  active_check <- subset_data$balanced_parentheses == FALSE
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", unbalanced parentheses"),
                                                        "not_valid - unbalanced parentheses")

  # Checking if a unit string contains nested braces or not,
  # as well as whether it contains unclosed braces
  check_nested_braces <- function(input_string) {
    stack <- 0
    max_depth <- 0

    for (char in strsplit(input_string, "")[[1]]) {
      if (char == "{") {
        stack <- stack + 1
        if (stack > max_depth) {
          max_depth <- stack
        }
      } else if (char == "}") {
        if (stack == 0) {
          return(FALSE)  # Closing braces without a corresponding opening braces
        } else {
          stack <- stack - 1
        }
      }
    }

    if (stack > 0) {
      return(FALSE)  # Unclosed braces
    } else if (max_depth > 1) {
      return(TRUE)   # Nested braces
    } else {
      return(FALSE)  # No nested braces
    }
  }
  subset_data$nested_braces <- lapply(subset_data$clean_unit, check_nested_braces)

  # 11th Check ~ nested braces
  active_check <- subset_data$nested_braces == TRUE
  subset_data$cleaning_comments[active_check] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[active_check]),
                                                        paste0(subset_data$cleaning_comments[active_check], ", nested braces"),
                                                        "not_valid - nested braces")
  # reporting numbers of records with not valid syntax
  if (report){
    not_valid_records <- sum(grepl("not_valid", subset_data$cleaning_comments))
    not_valid_records_n <- ifelse(!is.na(n_records),
                                sum(subset_data[[n_records]][grepl("not_valid", subset_data$cleaning_comments)]),
                                NA
    )
    not_valid_records_percent <- ifelse(!is.na(n_records),
                                      round((not_valid_records_n / total_n_records * 100), 3),
                                      NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " unit records with not valid syntax -> detailed cleaning comments added ", blue, "not_valid - reason", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " distinct unit strings (",blue, not_valid_records_percent, "%", reset," of the total result records) with not valid syntax -> detailed cleaning comments added ", blue, "not_valid - reason", reset, ").\n"))
    }
  }
  # Bringing dataset_2 columns to the same format as the rest
  dataset_2 <- subset_data
  dataset_2$opening_braces <- NULL
  dataset_2$closing_braces <- NULL
  dataset_2$balanced_parentheses <- NULL
  dataset_2$nested_braces <- NULL

  #### Function Block 4: Further cleaning of the non-mapped units - SPLIT STRUCTURE ####
  if (report){
    cat(paste0(bold, "Step 4: Parsing of units which passesd checks (tokenize and classify)", reset, "\n"))
    cat("==========================================================================================\n")
  }
  # SPLIT DATASET 2
  dataset_3 <- dataset_2[is.na(dataset_2$cleaning_comments),]
  dataset_2 <- dataset_2[!is.na(dataset_2$cleaning_comments),]

  subset_data <- dataset_3
  # Handling some common units or unit parts
  subset_data$clean_unit <- gsub("liter", "L", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("\\/cel", "\\/cell", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("gram", "g", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("gr", "g", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("pico", "p", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("nano", "n", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("1000000", "10*6", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("100000", "10*5", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("10000", "10*4", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("1000", "10*3", subset_data$clean_unit)
  subset_data$clean_unit <- gsub("mEq/l", "meq/L", subset_data$clean_unit)

  # Splitting the unit based on its core components
  separators_pattern <- "\\/|\\.|\\^|\\*|\\(|\\)"
  subset_data$split_unit <- strsplit(subset_data$clean_unit,
                                     paste0("(?<=\\d)(?=\\D)|(?=\\d)(?<=\\D)|
                                            (?<=\\s)|(?=\\s+)|(?<=\\%)|(?=\\%)|(?=", separators_pattern, ")"), perl = TRUE)
  # Replacing any other character other than operators, digits and whitespaces with "t"
  subset_data$split_structure <- lapply(subset_data$split_unit, function(x) gsub(pattern = "[^0-9[:space:]/.^*()]+",
                                                                                 replacement = "t",
                                                                                 x))
  # Replacing digits with "d"
  subset_data$split_structure <- lapply(subset_data$split_structure, function(x) {
    # Temporarily replace "10" with a unique placeholder
    x <- gsub("\\b10\\b", "TEMP_TEN", x, perl = TRUE)
    # Replace all other numbers with 'd'
    x <- gsub("\\b\\d+\\b", "d", x, perl = TRUE)
    # Restore "10" from placeholder
    gsub("TEMP_TEN", "10", x, perl = TRUE)
  })
  # Replacing whitespace with "s"
  subset_data$split_structure <- lapply(subset_data$split_structure, function(x) gsub(pattern = "\\s",
                                                                                      replacement = "s",
                                                                                      x))
  validate_unit <- function(split_unit, split_structure, annotation_list){
    # Looping through all split_structure elements
    element_position <- 0
    for(element in split_structure){
      element_position <- element_position + 1

      # Processing elements marked as "t"
      if(grepl("t", element)){
        unit <- split_unit[element_position]

        # Checking if unit exists in the given annotation list
        if(tolower(unit) %in% tolower(annotation_list)){
          split_structure[element_position] <- "{t}"
          next()
        }

        # Checking if the given unit exists inside units dataframe (case-sensitive)
        found_unit <- units_df[units_df$csCode_ == unit,]

        # If the given unit is not found, check if the unit exists in a case-insensitive format.
        # Else, if the given unit is found, the unit is flagged as "Valid"
        if(nrow(found_unit) < 1){
          found_unit <- units_df[units_df$ciCode_ == toupper(unit),]

          # If the given unit is not found again, we check whether the unit begins with a
          # valid SI prefix.

          # In this stage it is assumed that all given units begin with an SI prefix. We want
          # to split the raw unit from its SI prefix to check whether the raw unit
          # exists in the units dataframe.
          if(nrow(found_unit) < 1){

            # Extracting the first character of the unit, which is assumed to be an SI prefix
            unit_prefix <- substr(unit, start = 1, stop = 1)

            # Based on the first character of the unit, we check the possibility of the SI prefix
            # being 2 characters.
            if(unit_prefix %in% c("d", "D", "Y", "Z", "E", "P", "T", "G", "M")){
              unit_prefix2 <- substr(unit, start = 1, stop = 2)

              if(unit_prefix2 %in% c("da", "DA", "YA", "YO", "ZA", "ZO", "EX",
                                     "PT", "TR", "GA", "MA")){
                unit_prefix <- unit_prefix2
              }
            }

            # Extracting the raw unit (base unit).
            # Checking if the found SI prefix is 2 characters long and is a valid prefix
            if(unit_prefix %in% c("da", "DA", "YA", "YO", "ZA", "ZO", "EX",
                                  "PT", "TR", "GA", "MA")){
              raw_unit <- substr(unit, start = 3, stop = nchar(unit))

              # Checking if the found SI prefix is 1 character long and is a valid prefix
            }else if(unit_prefix %in% c("Y", "Z", "E", "P", "T", "G", "M", "k",
                                        "K", "h", "H", "d", "D", "c", "C", "m",
                                        "M", "u", "U", "n", "N", "p", "P", "f",
                                        "F", "a", "A", "z", "y")){
              raw_unit <- substr(unit, start = 2, stop = nchar(unit))

              # If the found SI prefix is not considered valid based on the above checks,
              # it is assumed that the given unit does not begin with an SI prefix.
              # Since the unit has not been previously found, it is not being considered as "Invalid"
            }else{
              # Invalid (unrecognized prefix)
              split_structure[element_position] <- "x"
              next()
            }

            # The raw unit is now being checked whether it exists inside the units dataframe (case-sensitive)
            found_unit <- units_df[units_df$csCode_ == raw_unit,]

            # If the raw unit is not found, check if the raw unit exists in a case-insensitive format.
            # Else, if the raw unit is found, the unit is flagged as "Valid"
            if(nrow(found_unit) < 1){
              found_unit <- units_df[units_df$ciCode_ == toupper(raw_unit),]

              # If the raw unit is yet again not found, then the unit remains unrecognizable, thus being
              # considered as "Invalid"

              #### !!!!! CHANGE 1: CHECK IF UNIT STARTS WITH SI PREFIX !!!!!! ####
              if(nrow(found_unit) < 1 || isTRUE(found_unit$starts_with_si)){
                # Invalid (unrecognized raw unit)
                split_structure[element_position] <- "x"
              }
              # Valid (case_insensitive raw unit)
              else{
                split_structure[element_position] <- "u"
              }
            }
            # Valid (case-sensitive raw unit)
            else{
              split_structure[element_position] <- "u"
            }
          }
          # Valid (case-insensitive)
          else{
            split_structure[element_position] <- "u"
          }
        }
        # Valid (case-sensitive)
        else{
          if(found_unit$csCode_ == "%"){
            split_structure[element_position] <- "%"
          }
          else{
            split_structure[element_position] <- "u"
          }
        }
      }
    }

    return(split_structure)
  }

  # Applying the "validate_unit" function which recognizes text "t" as either a unit "u",
  # an annotation "{t}" or something unknown "x"
  subset_data$new_split_structure <- mapply(validate_unit,
                                            split_unit = subset_data$split_unit,
                                            split_structure = subset_data$split_structure,
                                            MoreArgs = list(annotation_list = annotable_strings))

  # "x" <- cleaning_comments as "not_valid"
  subset_data$cleaning_comments <- ifelse(grepl("x", subset_data$new_split_structure),
                                          "not_valid - unrecognisable text", subset_data$cleaning_comments)
  # reporting numbers of records with unrecognisable text
  if (report){
    not_valid_records <- sum(grepl("unrecognisable text", subset_data$cleaning_comments))
    not_valid_records_n <- ifelse(!is.na(n_records),
                                  sum(subset_data[[n_records]][grepl("unrecognisable text", subset_data$cleaning_comments)]),
                                  NA
    )
    not_valid_records_percent <- ifelse(!is.na(n_records),
                                        round((not_valid_records_n / total_n_records * 100), 3),
                                        NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " unit records with unrecognisable text -> cleaning comment added ", blue, "not_valid - unrecognisable text", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " distinct unit strings (",blue, not_valid_records_percent, "%", reset," of the total result records) with unrecognisable text -> cleaning comment added ", blue, "not_valid - unrecognisable text", reset, ").\n"))
    }
  }

  #### Function Block 5: Further cleaning of the non-mapped units - Apply correction rules ####
  if (report){
    cat(paste0(bold, "Step 5: Restructuring of parsed units (apply correction rules & final validation)", reset, "\n"))
    cat("==========================================================================================\n")
  }
  # function for applying correction rules to the split_unit based on new_split_structure
  apply_correction_rules <- function(new_split_structure, split_unit) {
    # Ensure the inputs are character vectors
    new_split_structure <- unlist(new_split_structure)
    split_unit <- unlist(split_unit)

    i <- 1
    while (i <= length(new_split_structure)) {
      # Fixing annotable text
      if (new_split_structure[i] == "{t}" && i <= length(new_split_structure)) {
        # Rules 1 to 4 to merge subsequent annotation texts
        # Rule 1: "{t}", ("s" OR "."), "{t}" <- "{t_t}"
        if (new_split_structure[i + 1] %in% c("s", ".") &&
            new_split_structure[i + 2] == "{t}") {
          split_unit[i] <- paste0("{", split_unit[i], "_", split_unit[i + 2], "}")
          split_unit <- split_unit[-c(i + 1, i + 2)]
          new_split_structure <- new_split_structure[-c(i + 1, i + 2)]

          # Rule 2: "{t}", ("s", ".") OR (".", "s"), "{t}" <- "{t_t}"
        } else if (i + 3 <= length(new_split_structure) &&
                   new_split_structure[i + 1] %in% c("s", ".") &&
                   new_split_structure[i + 2] %in% c("s", ".") &&
                   new_split_structure[i + 3] == "{t}")  {
          split_unit[i] <- paste0("{", split_unit[i], "_", split_unit[i + 3], "}")
          split_unit <- split_unit[-c(i + 1, i + 2, i + 3)]
          new_split_structure <- new_split_structure[-c(i + 1, i + 2, i + 3)]

          # Rule 3: "{t}", "d", ("s" OR "."), "{t}" <- "{t_d_t}"
        } else if (i + 3 <= length(new_split_structure) &&
                   new_split_structure[i + 1] == "d" &&
                   new_split_structure[i + 2] %in% c("s", ".") &&
                   new_split_structure[i + 3] == "{t}")  {
          split_unit[i] <- paste0("{", split_unit[i], "_", split_unit[i + 1], "_", split_unit[i + 3], "}")
          split_unit <- split_unit[-c(i + 1, i + 2, i + 3)]
          new_split_structure <- new_split_structure[-c(i + 1, i + 2, i + 3)]

          # Rule 4: "{t}", ("s" OR "."), "d", "{t}" <- "{t_d_t}"
        } else if (i + 3 <= length(new_split_structure) &&
                   new_split_structure[i + 1] %in% c("s", ".") &&
                   new_split_structure[i + 2] == "d" &&
                   new_split_structure[i + 3] == "{t}") {
          split_unit[i] <- paste0("{", split_unit[i], "_", split_unit[i + 2], "_", split_unit[i + 3], "}")
          split_unit <- split_unit[-c(i + 1, i + 2, i + 3)]
          new_split_structure <- new_split_structure[-c(i + 1, i + 2, i + 3)]

        # Rule 5: adding curly brackets to annotable text
        } else {
          split_unit[i] <- paste0("{", split_unit[i], "}")
        }

        # Rule 6: Removing parentheses if only contain {t}
      } else if (new_split_structure[i] == "(" && i + 2 <= length(new_split_structure) &&
                 new_split_structure[i + 1] == "{t}" &&
                 new_split_structure[i + 2] == ")") {
        split_unit[i+1] <- paste0("{", split_unit[i+1], "}")
        split_unit <- split_unit[-c(i, i + 2)]
        new_split_structure <- new_split_structure[-c(i, i + 2)]

        # Fixing Space Characters
        # Rule 1: Remove space in between ("u" OR "d" OR "%") and "{t}"
      } else if ((new_split_structure[i] %in% c("u", "d", "%")) && i + 2 <= length(new_split_structure) &&
                 new_split_structure[i + 1] == "s" &&
                 new_split_structure[i + 2] == "{t}") {
        split_unit <- split_unit[-(i + 1)]
        new_split_structure <- new_split_structure[-(i + 1)]

        # Rule 2: Annotations OR Digits should always be followed by an operator (if not at the end): Remove space then add multiplication
      } else if ((new_split_structure[i] %in% c("{t}", "d")) && i + 1 <= length(new_split_structure)) {
        if (i + 2 <= length(new_split_structure) && new_split_structure[i + 1] == "s") {
          split_unit <- split_unit[-(i + 1)]
          new_split_structure <- new_split_structure[-(i + 1)]
          if (!new_split_structure[i] %in% c("/", ".")) {
            split_unit[i] <- paste0(split_unit[i], ".")
            new_split_structure[i] <- paste0(new_split_structure[i], ".")
          }
        } else if (!new_split_structure[i + 1] %in% c("/", ".")) {
          split_unit[i] <- paste0(split_unit[i], ".")
          #new_split_structure[i] <- paste0(new_split_structure[i], ".")
        }
        # Rule 3: Remove space between ("u" OR "d" OR "{t}") and operators ("/" OR ".")
      } else if ((new_split_structure[i] %in% c("/", ".")) && i + 2 <= length(new_split_structure) &&
                 new_split_structure[i + 1] == "s" &&
                 new_split_structure[i + 2] %in% c("u", "d", "{t}")) {
        split_unit <- split_unit[-(i + 1)]
        new_split_structure <- new_split_structure[-(i + 1)]
      } else if (new_split_structure[i] == "u" && i + 2 <= length(new_split_structure) && #spaces following digits or annotations are removed in rule 2
                 new_split_structure[i + 1] == "s" &&
                 new_split_structure[i + 2] %in% c("/", ".")) {
        split_unit <- split_unit[-(i + 1)]
        new_split_structure <- new_split_structure[-(i + 1)]

        # Rule 4: Remove space between "u" and "%"
      } else if (new_split_structure[i] == "u" && i + 2 <= length(new_split_structure) &&
                 new_split_structure[i + 1] == "s" &&
                 new_split_structure[i + 2] == "%") {
        split_unit <- split_unit[-(i + 1)]
        new_split_structure <- new_split_structure[-(i + 1)]

        # Other Fixing Rules for correcting exponents
      } else if (new_split_structure[i] == "u" && i + 2 <= length(new_split_structure) &&
                 new_split_structure[i + 1] == "\\*|^" &&
                 new_split_structure[i + 2] == "d") {
        split_unit <- split_unit[-(i + 1)]
        new_split_structure <- new_split_structure[-(i + 1)]
        # Rule for correcting digits like "1.000" to remove the period
      }
      i <- i + 1
    }
    #return(paste(split_unit, collapse = ""))
    return(list(split_unit = split_unit, new_split_structure = new_split_structure, clean_unit = paste(split_unit, collapse = "")))
  }
  # Apply the function to each row of the dataframe and unpack the results
  result <- mapply(apply_correction_rules, subset_data$new_split_structure, subset_data$split_unit, SIMPLIFY = FALSE)
  subset_data$split_unit <- lapply(result, `[[`, "split_unit")
  subset_data$new_split_structure <- lapply(result, `[[`, "new_split_structure")
  subset_data$clean_unit <- sapply(result, `[[`, "clean_unit")
  rm(result)

  # Validaing the output AFTER applying the correction rules
  # Exclude not valid structure that contains "s" >> flagged
  space_structure <- grepl("s", subset_data$new_split_structure)
  subset_data$cleaning_comments[space_structure] <- ifelse(grepl("not_valid", subset_data$cleaning_comments[space_structure]),
                                                           paste0(subset_data$cleaning_comments[space_structure], ", space characters"),
                                                           "not_valid - space characters")
  # reporting numbers of records with space characters (not corrected)
  if (report){
    not_valid_records <- sum(grepl("not_valid - space characters", subset_data$cleaning_comments))
    not_valid_records_n <- ifelse(!is.na(n_records),
                                  sum(subset_data[[n_records]][grepl("not_valid - space characters", subset_data$cleaning_comments)]),
                                  NA
    )
    not_valid_records_percent <- ifelse(!is.na(n_records),
                                        round((not_valid_records_n / total_n_records * 100), 3),
                                        NA)
    if (is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " unit records with space characters (not corrected) -> cleaning comment added ", blue, "not_valid - space characters", reset, ").\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(red, Warning, reset," ",blue, not_valid_records, reset, " distinct unit strings (",blue, not_valid_records_percent, "%", reset," of the total result records) with space characters (not corrected) -> cleaning comment added ", blue, "not_valid - space characters", reset, ").\n"))
    }
  }
  # reporting numbers of records corrected and validated to UCUM units
  if (report){
    parsed_ucum_records <- sum(is.na(subset_data$cleaning_comments))
    parsed_ucum_records_n <- ifelse(!is.na(n_records),
                                    sum(subset_data[[n_records]][is.na(subset_data$cleaning_comments)]),
                                    NA)
    parsed_ucum_records_percent <- ifelse(!is.na(n_records),
                                          round((parsed_ucum_records_n / total_n_records * 100), 3),
                                          NA)
    if (is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, parsed_ucum_records, reset, " unit records were transformed and validated to ucum codes.\n"))
    }
    if (!is.na(n_records)) {
      cat(paste0(green, success, reset," ",blue, parsed_ucum_records, reset, " distinct units (",blue, parsed_ucum_records_percent, "%", reset," of the total result records) were transformed and validated to ucum codes.\n"))
    }
  }
  # Fixing columns of subset_data
  subset_data$split_unit <- NULL
  subset_data$split_structure <- NULL
  subset_data$new_split_structure <- NULL

  # SPLIT DATASET 3
  dataset_3 <- subset_data
  dataset_4 <- dataset_3[is.na(dataset_3$cleaning_comments),]
  dataset_4$ucum_code <- ifelse(is.na(dataset_4$ucum_code), dataset_4$clean_unit, dataset_4$ucum_code)

  dataset_3 <- dataset_3[!is.na(dataset_3$cleaning_comments),]

  # rbind (1,2,3,4)
  lab_data <- do.call("rbind", list(dataset_1, dataset_2, dataset_3, dataset_4))
  lab_data$clean_unit <- NULL

  if (report){
    cat("==========================================================================================\n")
  }
  standard_records <- sum(is.na(lab_data$cleaning_comments))
  standard_records_n <- ifelse(!is.na(n_records),
                               sum(lab_data[[n_records]][is.na(lab_data$cleaning_comments)]),
                               NA)
  standard_records_percent <- ifelse(!is.na(n_records),
                                     round((standard_records_n / total_n_records * 100), 3),
                                     NA)
  if (is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, standard_records, reset, " unit records were standardized to UCUM.\n"))
  }
  if (!is.na(n_records)) {
    cat(paste0(green, success, reset," ",blue, standard_records, reset, " distinct results (",blue, standard_records_percent, "%", reset," of the total result records) were standardized to UCUM.\n"))
  }
  end.time <- Sys.time()
  time.taken <- as.numeric(difftime(end.time, start.time, units = "secs"))
  # Break into minutes and seconds
  mins <- floor(time.taken / 60)
  secs <- round(time.taken %% 60, 1)
  cat(paste0(clock, " ", bold, "Time taken is ", blue, mins, " min, ", secs, " sec\n", reset))

  return(lab_data)
}
