#' Data for the Reference Harmonized Units for LOINC Groups
#'
#' A dataset mapping each LOINC codes to the reference harmonized unit of their LOINC group.
#'
#' \describe{
#'   \item{loinc_code}{Contains 33,197 different LOINC codes.}
#'   \item{unit_system}{The unit system (SI or conventional) of the reference unit.}
#'   \item{reference_unit}{The harmonized reference unit.}
#'   \item{OMOP_concept_id}{The OMOP standardized concept ID for the harmonized unit, if applicable.}
#'   \item{mass_molar_unit}{The reference unit of another LOINC code from the same mass–molar group.}
#'   \item{molecular_weight}{The molecular weight of the analyte, if applicable.}
#'   \item{mass_molar_loinc}{The other LOINC code that shares the same mass–molar group.}
#'   \item{property_group_id}{The LOINC group ID that shares the same component, property, and time aspect.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name loinc_reference_unit_v1
#' @usage data(loinc_reference_unit_v1)
#' @format A data frame with 33197 rows and 8 variables.
NULL
