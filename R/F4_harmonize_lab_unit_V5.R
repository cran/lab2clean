# Specify global variables
utils::globalVariables(c("arbitrary_units", "cnv_units", "convert", "harmonized_unit", "loinc_reference_unit_v1",
                         "parsed_units_df", "si_prefixes"))

#' Harmonizing Laboratory Units of Measurement through Unit Conversion
#'
#' This function is designed to harmonize the units found in a laboratory data set
#' to either SI or Conventional units, converting the numeric result values in
#' the process and (optionally) updating LOINC codes when mass–molar conversion
#' are required.
#'
#' @param lab_data A data frame containing laboratory data.
#' @param loinc_code The column in `lab_data` indicating the LOINC code of the laboratory test.
#' @param result_value The column in `lab_data` with quantitative result values for conversion.
#' @param result_unit The column in `lab_data` with result units in a UCUM-valid format.
#' @param preferred_unit_system A string representing the preference of the user for the unit system used for standardization. Defaults to "SI", the other option is "Conventional".
#' @param report A report is written in the console. Defaults to "TRUE".
#'
#' @importFrom utils data globalVariables
#'
#' @return A modified `lab_data` data frame with additional columns (original row order preserved):
#'   * `harmonized_unit`: Harmonized units according to the preferred unit system.
#'   * `OMOP_concept_id`: The concept id of the harmonized unit according to the OMOP Common Data Model.
#'   * `new_value`: The result value after the conversion.
#'   * `new_loinc_code`: If the unit conversion led to a new loinc code (e.g. in mass-molar conversion).
#'   * `property_group_id`: the code of the LOINC group (parent group ID / Group ID).
#'   * `cleaning_comments`: Comments about the harmonization and conversion process for each lab result.
#'
#' @details
#' The function undergoes the following methodology:
#'   1. Extracting unit parameters (dimension & magnitude)
#'   2. Setting reference unit (LOINC-UCUM mapping)
#'   3. Check compatibility between reported unit and reference unit
#'   4. Executing regular conversion
#'   5. Executing mass<>molar conversion
#'   6. Checking LOINC codes
#'
#' Internal Datasets:
#' The function uses an internal dataset; `parsed_units_df` which contains 1450 parsed ucum units
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
#' Function 3 for unit format standardized to UCUM,
#'
#' @export
#### Function Body ####
#----------------------------------------------------------------------------------#
harmonize_lab_unit <- function(lab_data,
                               loinc_code,
                               result_value,
                               result_unit,
                               preferred_unit_system = "SI",
                               report = TRUE){
  start.time <- Sys.time()
  ## ---- 0  Safety checks, column capture & load data -------------------------------- ##
  if(!is.data.frame(lab_data))
    stop("`lab_data` must be a data.frame")
  preferred_unit_system <- tolower(preferred_unit_system)
  if(!preferred_unit_system %in% c("si", "conventional"))
    stop("`preferred_unit_system` must be \"SI\" or \"Conventional\"")
  lab_data[[result_value]] <- as.numeric(lab_data[[result_value]])

  # Helper to accept bare or quoted names
  col_name <- function(x) if(is.character(x)) x else deparse(substitute(x))
  loinc_code   <- col_name(loinc_code)
  result_value <- col_name(result_value)
  result_unit  <- col_name(result_unit)

  # Keeping track of the given lab_data order
  original_cols <- names(lab_data)
  lab_data$order <- 1:nrow(lab_data)

  # Loading required files
  data("parsed_units_df", package = "lab2clean", envir = environment())
  data("loinc_reference_unit_v1", package = "lab2clean", envir = environment())
  loinc_reference_unit_v1$molecular_weight[is.na(loinc_reference_unit_v1$molecular_weight)] <- 0

  # Define ANSI escape codes for text decorations and colors
  bold <- "\033[1m"
  reset <- "\033[0m"
  Warning <- "\u26A0"
  success <- "\u2714"
  red <- "\033[31m"
  green <- "\033[32m"
  blue <- "\033[34m"
  clock <- "\u23F0"

  # Function Block 1: Getting the unit parameters // extend the parsed_units_df with Parsing ####
  #---------------------------------------------------------------------------------------------#
  if (report){
    cat(paste0(bold, "Step 1: Extracting unit parameters (dimension & magnitude)", reset, "\n"))
  }
  # Checking whether the given unit exists inside the parsed_units_df table
  # and returning the found unit.
  # If the unit is not found, it gets parsed.
  # 1- look them up in the table
  # 2- get the list of units to be parsed
  # 3- pass them to the parseunit function
  # 4- get the parameters ready for all units
  units_in_data   <- unique(as.character(lab_data[[result_unit]]))
  units_in_data  <- units_in_data[!is.na(units_in_data) & nzchar(units_in_data)]
  missing_units <- units_in_data[!units_in_data %in% parsed_units_df$csCode_ &
                                 !units_in_data %in% parsed_units_df$ciCode_]
  # REVIEW parsed_units_df ####
  # 1- depend on csCode ONLY, merge afterwards is based on csCode only
  #    or change this column into parsed_unit and keep cs & ci Codes for parsing purposes
  # 2- add source: * ucum_defs (only source with ciCode)
  #                * extend with parsed resources (loinc examples, omop_standard, npu_concepts)
  #                * extend with real world units (Intego after cleaningl output of function 3)
  # 3- make sure no duplicates of csCode
  #    parsed_units_df <- parsed_units_df %>% group_by(csCode_) %>% filter(n() > 1) %>% ungroup()
  if(length(missing_units)){
    parsed_list <- lapply(missing_units, function(unit) {
      tryCatch(
        parseUnit(unit, units_df = parsed_units_df),
        error = function(e) {
          new_unit <- data.frame(csCode_ = unit,
                                 ciCode_ = unit,
                                 magnitude_ = NA,
                                 dim_ = NA,
                                 cnv_ = NA,
                                 cnvPfx_ = NA,
                                 isArbitrary_ = NA,
                                 moleExp_ = NA,
                                 source = NA,
                                 starts_with_si = NA,
                                 flag = e$message)
          return(new_unit)
        }
      )
    })

    parsed_units_df <- rbind(parsed_units_df, do.call(rbind, parsed_list))
  }

 # Function Block 2: Compare lab_data to mapping table to determine required conversions ####
  #------------------------------------------------------------------------------------------#
  if (report){
    cat(paste0(bold, "Step 2: Setting reference unit (LOINC-UCUM mapping)", reset, "\n"))
  }
  # Join mapping table by loinc code
  lab_data <- merge(lab_data, loinc_reference_unit_v1,
                    by.x = loinc_code, by.y = "loinc_code",
                    all.x = TRUE, sort = FALSE)
  # determining harmonized unit
  lab_data$harmonized_unit <- if(preferred_unit_system == "Conventional") lab_data$to_conventional else lab_data$to_SI
  lab_data$harmonized_unit <- ifelse(is.na(lab_data$mass_molar_loinc),
                                     lab_data$reference_unit,
                                     ifelse(lab_data$unit_system == preferred_unit_system & !is.na(lab_data$unit_system),
                                            lab_data$reference_unit,
                                            lab_data$mass_molar_unit))
  # updating OMOP concept id for the harmonized unit
  lab_data$OMOP_concept_id <- NULL
  omop_concept_id <- unique(loinc_reference_unit_v1[ , c("reference_unit", "OMOP_concept_id")])
  lab_data <- merge(lab_data, omop_concept_id,
                    by.x = "harmonized_unit", by.y = "reference_unit",
                    all.x = TRUE, sort = FALSE)

  # subsetting the df that needs to be converted & defining conversion type
  lab_data$convert <- !is.na(lab_data$harmonized_unit) & !is.na(lab_data[[result_value]]) & !is.na(lab_data[[result_unit]]) &
    lab_data[[result_unit]] != lab_data$harmonized_unit
  to_be_converted  <- subset(lab_data, convert == TRUE)
  no_conversion <- subset(lab_data, convert == FALSE)
  # flagging loinc codes not in the mapping table --> cleaning_comments == "not_found_loinc_code"
  no_conversion$cleaning_comments <- ifelse(is.na(no_conversion[[result_value]]),
                                            "not_harmonized: no_numeric_result",
                                            ifelse(is.na(no_conversion[[result_unit]]),
                                                   "not_harmonized: no_source_unit",
                                                   ifelse(is.na(no_conversion$harmonized_unit),
                                                          "not_harmonized: no_reference_unit_available",
                                                          "harmonized: source = reference unit")))
  no_conversion$new_value <- ifelse(is.na(no_conversion$harmonized_unit), NA, no_conversion[[result_value]])

  # Get one row per original-column combination
  unique_conversions <- unique(to_be_converted[, c(result_value, result_unit, "harmonized_unit", "molecular_weight")])

  # Function Block 3: Executing Conversion####
  #------------------------------------------#
  unique_conversions <- merge(unique_conversions, parsed_units_df,
                              by.x = result_unit, by.y = "csCode_",
                              all.x = TRUE, sort = FALSE)
  unique_conversions <- merge(unique_conversions, parsed_units_df,
                              by.x = "harmonized_unit", by.y = "csCode_",
                              all.x = TRUE, sort = FALSE)
  # Mark non-UCUM units
  unique_conversions$cleaning_comments <- ifelse(is.na(unique_conversions$dim_.x) ,
                                                 "not_harmonized: Non UCUM unit", NA)
  # Check Special Conversion -> Throw Error
  unique_conversions$cleaning_comments <- ifelse(!is.na(unique_conversions$cnv_.x) ,
                                                 "not_harmonized: special conversion",
                                                 unique_conversions$cleaning_comments)
  # Check Dimension and Magnitude
  if (report){
    cat(paste0(bold, "Step 3: Check compatibility between reported unit and reference unit", reset, "\n"))
  }
  same_dim <- mapply(
    function(x, y)
      identical(eval(parse(text = x)),
                eval(parse(text = y))),
    unique_conversions$dim_.x,
    unique_conversions$dim_.y
  )
  same_magnitude <- unique_conversions$magnitude_.x == unique_conversions$magnitude_.y
  idx_regular <- unique_conversions$moleExp_.x == unique_conversions$moleExp_.y
  idx_nocomment <- is.na(unique_conversions$cleaning_comments)
  unique_conversions$cleaning_comments[idx_regular & !same_dim & is.na(unique_conversions$cleaning_comments)] <- "not_harmonized: different dimensions"


  # Check Arbitrary units
  # (1) Check if only one unit is arbitrary
  one_arbitrary <- unique_conversions$isArbitrary_.x != unique_conversions$isArbitrary_.y
  unique_conversions$cleaning_comments[one_arbitrary & is.na(unique_conversions$cleaning_comments)] <- "not_harmonized: between arbitrary units and non-arbitrary units"
  # (2) Check if arbitrary identifiers inside square brackets are not the same (case-insensitive)
  both_arbitrary <- unique_conversions$isArbitrary_.x == TRUE & unique_conversions$isArbitrary_.y == TRUE
  arbitary_pattern <- "\\[[^]]+\\]"
  same_arbitrary <- rep(NA, nrow(unique_conversions))
  same_arbitrary[both_arbitrary & is.na(unique_conversions$cleaning_comments)] <-
    tolower(regmatches(unique_conversions[[result_unit]][both_arbitrary & is.na(unique_conversions$cleaning_comments)],
                       regexpr(arbitary_pattern, unique_conversions[[result_unit]][both_arbitrary & is.na(unique_conversions$cleaning_comments)]))) ==
    tolower(regmatches(unique_conversions$harmonized_unit[both_arbitrary & is.na(unique_conversions$cleaning_comments)],
                       regexpr(arbitary_pattern, unique_conversions$harmonized_unit[both_arbitrary & is.na(unique_conversions$cleaning_comments)])))
  ## flag rows where both are arbitrary *and* the units differ
  unique_conversions$cleaning_comments[both_arbitrary & !same_arbitrary & is.na(unique_conversions$cleaning_comments)] <- "not_harmonized: different arbitrary units"

  # Regular Conversion
  if (report){
    cat(paste0(bold, "Step 4: Executing regular conversion", reset, "\n"))
  }
  # (1) Check Dimension and Magnitude
  unique_conversions$new_value[idx_regular & same_dim & same_magnitude & is.na(unique_conversions$cleaning_comments)] <- unique_conversions[[result_value]][idx_regular & same_dim & same_magnitude & is.na(unique_conversions$cleaning_comments)]
  unique_conversions$cleaning_comments[idx_regular & same_dim & same_magnitude & is.na(unique_conversions$cleaning_comments)] <- "harmonized: different_unit_same_value"
  # (2) Execute conversion
  unique_conversions$new_value[idx_regular & same_dim & !same_magnitude & is.na(unique_conversions$cleaning_comments)] <-
    unique_conversions[[result_value]][idx_regular & same_dim & !same_magnitude & is.na(unique_conversions$cleaning_comments)] * unique_conversions$magnitude_.x[idx_regular & same_dim & !same_magnitude & is.na(unique_conversions$cleaning_comments)] /
    unique_conversions$magnitude_.y[idx_regular & same_dim & !same_magnitude & is.na(unique_conversions$cleaning_comments)]
  unique_conversions$cleaning_comments[idx_regular & same_dim & !same_magnitude & is.na(unique_conversions$cleaning_comments)] <- "harmonized: regular_conversion"

  # Mass-molar Conversion
  if (report){
    cat(paste0(bold, "Step 5: Executing mass<>molar conversion", reset, "\n"))
  }
  # (1) Checking whether the given units are Mole-To-Mass commensurable
  same_dim <- mapply(
    function(x, y) {
      from <- eval(parse(text = x))
      to   <- eval(parse(text = y))
      identical(from[-3], to[-3])      # ignore the third exponent (mol)
    },
    unique_conversions$dim_.x,
    unique_conversions$dim_.y
  )
  N_A <- 6.02214076e23  # Avogadro's number (exact in SI since 2019) instead of 602213669999999967040202
  # (2) Mass to Mole
  idx_mass_molar <- unique_conversions$moleExp_.x == 0 & unique_conversions$moleExp_.y == 1 & unique_conversions$molecular_weight != 0 & is.na(unique_conversions$cleaning_comments)
  unique_conversions$new_value[idx_mass_molar & same_dim] <-
    (unique_conversions[[result_value]][idx_mass_molar & same_dim] * unique_conversions$magnitude_.x[idx_mass_molar & same_dim] / unique_conversions$molecular_weight[idx_mass_molar & same_dim]) /
    (unique_conversions$magnitude_.y[idx_mass_molar & same_dim] / N_A)
  unique_conversions$cleaning_comments[idx_mass_molar & same_dim] <- "harmonized: mass_to_mole_conversion"
  # (3) Mole to Mass
  idx_mass_molar <- unique_conversions$moleExp_.x == 1 & unique_conversions$moleExp_.y == 0 & unique_conversions$molecular_weight != 0 & is.na(unique_conversions$cleaning_comments)
  unique_conversions$new_value[idx_mass_molar & same_dim] <-
    (unique_conversions[[result_value]][idx_mass_molar & same_dim] * (unique_conversions$magnitude_.x[idx_mass_molar & same_dim] / N_A) *
       unique_conversions$molecular_weight[idx_mass_molar & same_dim]) / unique_conversions$magnitude_.y[idx_mass_molar & same_dim]
  unique_conversions$cleaning_comments[idx_mass_molar & same_dim] <- "harmonized: mole_to_mass_conversion"
  # (4) Mass-Mole no conversion
  idx_mass_molar <- unique_conversions$moleExp_.x != unique_conversions$moleExp_.y & is.na(unique_conversions$cleaning_comments)
  unique_conversions$cleaning_comments[idx_mass_molar & !same_dim] <- "not_harmonized: different dimensions"
  unique_conversions$cleaning_comments[idx_mass_molar & same_dim & unique_conversions$molecular_weight == 0] <- "not_harmonized: no molecular weight available"
  # (5) Eq <> mol conversion (moleExp_.x == 1 & moleExp_.y == 1) Requiring charge/valence of substance ####

  # Merge the converted values back onto to_be_converted
  unique_conversions <- unique_conversions[ , c(result_value, result_unit, "harmonized_unit", "molecular_weight", "cleaning_comments", "new_value")]
  to_be_converted <- merge(
    x     = to_be_converted,
    y     = unique_conversions,
    by    = c(result_value, result_unit, "harmonized_unit", "molecular_weight"),
    all.x = TRUE,
    sort  = FALSE
  )
  # Bind back together & clean up
  lab_data <- rbind(to_be_converted, no_conversion)
  lab_data <- lab_data[order(lab_data$order), ]

  # Function Block 4: Checking LOINC codes on all records####
  #---------------------------------------------------------#
  if (report){
    cat(paste0(bold, "Step 6: Checking LOINC codes", reset, "\n"))
  }
  # Loinc code replacement criteria:
  # 1. unit_system != detected_unit_system --> "loinc_unitsystem_mismatch"
  # 2. unit_system != new_unit_system --> "mass_molar_conversion"
  lab_data$detected_unit_system <- ifelse(
    (lab_data$molecular_weight != 0 & grepl("mol", lab_data[[result_unit]]))
    | lab_data$molecular_weight == 0,
    "si", "conventional"
  )
  # Check detected_unit_system compatibility with loinc code property (unit_system)
  idx_mismatch <- !is.na(lab_data$unit_system) & (lab_data$unit_system != lab_data$detected_unit_system) & !grepl("Non UCUM unit", lab_data$cleaning_comments)
  lab_data$cleaning_comments[idx_mismatch] <- ifelse(
    is.na(lab_data$cleaning_comments[idx_mismatch]),
    "loinc_unit_mismatch",
    paste0(lab_data$cleaning_comments[idx_mismatch], ", loinc_unit_mismatch")
  )
  has_mass_mole <- grepl("mass_to_mole|mole_to_mass", lab_data$cleaning_comments)
  has_loinc_mismatch <- grepl("loinc_unit_mismatch", lab_data$cleaning_comments)
  lab_data$new_loinc_code <- NA
  # Case 1: Both conversion and mismatch → original LOINC
  idx_same_loinc <- has_mass_mole & has_loinc_mismatch
  lab_data$new_loinc_code[idx_same_loinc] <- lab_data[[loinc_code]][idx_same_loinc]

  # Case 2: Conversion OR mismatch (but not both) → mass_molar_loinc
  idx_new_loinc <- (has_mass_mole | has_loinc_mismatch) & !idx_same_loinc & !is.na(lab_data$new_value)
  lab_data$new_loinc_code[idx_new_loinc] <- lab_data$mass_molar_loinc[idx_new_loinc]

  # Case 3: Otherwise → if new_value is not NA, use original LOINC
  idx_keep_original <- !idx_new_loinc & !idx_same_loinc & !is.na(lab_data$new_value)
  lab_data$new_loinc_code[idx_keep_original] <- lab_data[[loinc_code]][idx_keep_original]

  # updating property group id for the new/mass-molar loinc
  lab_data$property_group_id <- NULL
  property_groups <- loinc_reference_unit_v1[ , c("loinc_code", "property_group_id")]
  lab_data <- merge(lab_data, property_groups,
                    by.x = "new_loinc_code", by.y = "loinc_code",
                    all.x = TRUE, sort = FALSE)

  # drop helper cols and rename
  cols_to_drop <- c("order", "convert", "molecular_weight","unit_system", "reference_unit", "mass_molar_unit", "mass_molar_loinc", "detected_unit_system")
  lab_data <- lab_data[ , !(names(lab_data) %in% cols_to_drop)]
  lab_data <- lab_data[ , c(original_cols, "new_loinc_code", "new_value", "harmonized_unit", "OMOP_concept_id", "property_group_id", "cleaning_comments")]

  if (report){
    cat("===============================================================================================\n")
    cat(paste0(bold, "Reporting Results:", reset, "\n"))
    harmonization_percent <- round((sum(!grepl("not_harmonized:", lab_data$cleaning_comments))/length(lab_data$cleaning_comments) * 100), 3)
    cat(paste0(green, success, reset," ",blue, harmonization_percent, "%", reset, " of the lab data records were harmonized to reference units.\n"))
    harmonized_detailed <- sum(grepl("harmonized: source = reference unit", lab_data$cleaning_comments))
    cat(paste0(green, success, reset," ", blue, harmonized_detailed, reset, " records had reported units same as reference units -> result value not converted.\n"))
    harmonized_detailed <- sum(grepl("harmonized: different_unit_same_value", lab_data$cleaning_comments))
    cat(paste0(green, success, reset," ", blue, harmonized_detailed, reset, " records had different reported units, but equivalent to reference units -> result value not converted.\n"))
    harmonized_detailed <- sum(grepl("harmonized: regular_conversion", lab_data$cleaning_comments))
    cat(paste0(green, success, reset," ", blue, harmonized_detailed, reset, " records harmonized to reference units by regular conversions.\n"))
    harmonized_detailed <- sum(grepl("harmonized: mass_to_mole_conversion", lab_data$cleaning_comments))
    cat(paste0(green, success, reset," ", blue, harmonized_detailed, reset, " records harmonized to reference units by mass to mole conversions.\n"))
    harmonized_detailed <- sum(grepl("harmonized: mole_to_mass_conversion", lab_data$cleaning_comments))
    cat(paste0(green, success, reset," ", blue, harmonized_detailed, reset, " records harmonized to reference units by mole to mass conversions.\n"))
    cat("===============================================================================================\n")
    no_harmonization_percent <- round((sum(grepl("not_harmonized:", lab_data$cleaning_comments))/length(lab_data$cleaning_comments) * 100), 3)
    cat(paste0(red, Warning, reset," ",blue, no_harmonization_percent, "%", reset, " of the lab data records could NOT harmonized to reference units.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: different dimensions", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no conversion between units of different dimensions.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: between arbitrary units and non-arbitrary units", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no conversion between arbitrary units and non-arbitrary units.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: different arbitrary units", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no conversion between different arbitrary units.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: no_reference_unit_available", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no reference unit available for the given loinc codes.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: no molecular weight available", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no molecular weight available for the given analytes (loinc codes).\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: Non UCUM unit", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: reported units are not ucum-valid.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: special conversion", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: reported units require special conversion.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: no_numeric_result", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: result values are not numeric.\n"))
    not_harmonized_detailed <- sum(grepl("not_harmonized: no_source_unit", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, not_harmonized_detailed, reset, " records not harmonized: no reported units.\n"))
    cat("===============================================================================================\n")
    updated_loinc <- sum(grepl("loinc_unit_mismatch", lab_data$cleaning_comments))
    cat(paste0(red, Warning, reset," ", blue, updated_loinc, reset, " records with updated loinc code to match the harmonized unit system.\n"))
  }
  end.time <- Sys.time()
  time.taken <- as.numeric(difftime(end.time, start.time, units = "secs"))
  # Break into minutes and seconds
  mins <- floor(time.taken / 60)
  secs <- round(time.taken %% 60, 1)
  cat(paste0(clock, " ", bold, "Time taken is ", blue, mins, " min, ", secs, " sec\n", reset))

  return(lab_data)
}
