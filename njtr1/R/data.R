#' Insurance companies licensed to do business in New Jersey
#'
#' A tibble containing all insurance companies licensed to operate in New Jersey.
#' Can be joined with accident data.
#'
#' @format A tibble with 454 rows and 9 variables:
#' \describe{
#'   \item{ID_NO}{Unique ID of the insurance company}
#'   \item{NAME}{Name of company}
#'   \item{ADDRESS_1}{Address}
#'   \item{ADDRESS_2}{Address}
#'   \item{ADDRESS_3}{Address}
#'   \item{CITY}{City of insurance company}
#'   \item{STATE}{State of insurance company}
#'   \item{ZIP}{ZIP code of insurance company}
#'   \item{NOTES}{Notes}
#' }
#' @source \url{https://www.nj.gov/mvc/pdf/vehicles/insurance_codes.pdf}
"insurance"
