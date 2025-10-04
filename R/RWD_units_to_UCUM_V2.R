#' Data for the RWD units mapped to standard UCUM-valid units
#'
#' A dataset containing RWD units mapped to standard UCUM-valid units.
#'
#' @format A data frame with N rows and 3 variables:
#' \describe{
#'   \item{clean_unit_lower}{Case-insensitive representation of invalid or inconsistent units as found in real-world data (RWD).}
#'   \item{ucum_code}{The equivalent UCUM-compliant format for the given RWD unit.}
#'   \item{source_match}{The source from which this mapping or match was derived.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name RWD_units_to_UCUM_V2
#' @usage data(RWD_units_to_UCUM_V2)
#' @format A data frame with 5120 rows and 3 variables.
NULL
