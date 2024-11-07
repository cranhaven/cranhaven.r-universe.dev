#' Polar bear litter size data set
#'
#' This data set corresponds to live-captured polar bears from late March 1992 to beginning of May
#' 2017 at Svalbard, Norway.
#'
#' @docType data
#'
#' @usage data(polar)
#'
#' @format A data frame with 231 rows and 7 columns.
#' \describe{
#'   \item{year}{Catch year}
#'   \item{days}{Number of the day of the catch year}
#'   \item{id}{Unique specimen id}
#'   \item{age}{Age of the specimen, estimated using premolar tooth}
#'   \item{agecat}{Categorized age of the specimen}
#'   \item{length}{Body straight length (cm)}
#'   \item{cubnumber}{Litter size}
#' }
#'
#' @keywords datasets
#'
#' @source Folio, Dorinda Marie et al. (2019), Data from: How many cubs can a mum nurse?
#' Maternal age and size influence litter size in polar bears, Dryad, Dataset.
#'
#' @references Folio D. M., Aars J., Gimenez O., Derocher A. E., Wiig O. and Cubaynes S. (2019)
#' How many cubs can a mum nurse? Maternal age and size influence litter size in
#' polar bears, Biology letters, 15.
#'
#' @examples
#' data(polar)
#' head(polar)
"polar"
