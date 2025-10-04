#' Data for the parsed UCUM units
#'
#' Intermediate dataset representing parsed UCUM units and the parameters necessary for machine readability and conversion.
#'
#' \describe{
#'   \item{csCode_}{Case-sensitive code.}
#'   \item{ciCode_}{Case-insensitive code.}
#'   \item{magnitude_}{Magnitude of the unit.}
#'   \item{dim_}{The dimensionality of the unit (e.g., mass/time).}
#'   \item{cnv_}{Special conversion involved (if any).}
#'   \item{cnvPfx_}{Prefix used in the special conversion.}
#'   \item{isArbitrary_}{Logical flag indicating if the unit is arbitrary.}
#'   \item{moleExp_}{Logical flag indicating if the unit includes molar expression.}
#' }

#'
#' @docType data
#' @keywords datasets
#' @name parsed_units_df
#' @usage data(parsed_units_df)
#' @format A data frame with 1439 rows and 8 variables.
NULL
