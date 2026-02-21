#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' re-export magrittr pipe operator
#'
#' @importFrom dplyr %>%
#' @name %>%
#' @rdname re-exports
#' @return Various outputs, please see documentation for source package
#' @return description Used to easily make pipe operator available
#' @export
NULL

# Allow use of dot in pipelines
globalVariables(".")

#' re-export tribble
#'
#' @importFrom dplyr tribble
#' @name tribble
#' @rdname re-exports
#' @return A tibble, made available for easy data entry manually
#' @export
NULL

#' re-export glue
#'
#' @importFrom glue glue
#' @name glue
#' @rdname re-exports
#' @return A string with glue substitution completed
#' @export
NULL

#' Vascr growth data
#'
#' A dataset containing the growth curves of hCMEC/D3 cell lines seeded at various densities. 
#' The variables are as follows:
#'
#' @format A tibble with 346370 rows and 9 variables:
#' \describe{
#'   \item{Time}{The time at which the measurement was taken (hours)}
#'   \item{Unit}{The unit the measurement was taken in}
#'   \item{Well}{The well in which the measurement was taken}
#'   \item{Value}{The value of the measurement taken}
#'   \item{Frequency}{The frequency at which data was collected}
#'   \item{Experiment}{Name of the experiment}
#'   \item{Instrument}{The instrument data was collected on}
#'   \item{SampleID}{The numerical ID of the sample}
#'   \item{Sample}{The name of the treatment applied to the dataset}
#' }
#' 
#' @source Hucklesby 2020
"growth.df"



#' Vascr growth data which is not resampled
#'
#' A dataset containing the growth curves of hCMEC/D3 cell lines seeded at various densities. 
#' This is a subset of 'growth.df', without the final stages of processing.
#'
#' @format A tibble with 346370 rows and 9 variables:
#' \describe{
#'   \item{Time}{The time at which the measurement was taken (hours)}
#'   \item{Unit}{The unit the measurement was taken in}
#'   \item{Well}{The well in which the measurement was taken}
#'   \item{Value}{The value of the measurement taken}
#'   \item{Frequency}{The frequency at which data was collected}
#'   \item{Experiment}{Name of the experiment}
#'   \item{Instrument}{The instrument data was collected on}
#'   \item{SampleID}{The numerical ID of the sample}
#'   \item{Sample}{The name of the treatment applied to the dataset}
#' }
#' 
#' @keywords internal
#' 
#' @source Hucklesby 2020
"growth_unresampled.df"

#' Vascr growth data which is not re sampled
#'
#' A dataset containing the growth curves of hCMEC/D3 cell lines seeded at various densities. 
#' This is a subset of 'growth.df', with only Rb data for faster processing.
#'
#' @format A tibble with 346370 rows and 9 variables:
#' \describe{
#'   \item{Time}{The time at which the measurement was taken (hours)}
#'   \item{Unit}{The unit the measurement was taken in}
#'   \item{Well}{The well in which the measurement was taken}
#'   \item{Value}{The value of the measurement taken}
#'   \item{Frequency}{The frequency at which data was collected}
#'   \item{Experiment}{Name of the experiment}
#'   \item{Instrument}{The instrument data was collected on}
#'   \item{SampleID}{The numerical ID of the sample}
#'   \item{Sample}{The name of the treatment applied to the dataset}
#' }
#' 
#' @keywords internal
#' 
#' @source Hucklesby 2020
"rbgrowth.df"

#' Small growth data set for testing
#'
#'
#' @format A tibble with 346370 rows and 9 variables:
#' \describe{
#'   \item{Time}{The time at which the measurement was taken (hours)}
#'   \item{Unit}{The unit the measurement was taken in}
#'   \item{Well}{The well in which the measurement was taken}
#'   \item{Value}{The value of the measurement taken}
#'   \item{Frequency}{The frequency at which data was collected}
#'   \item{Experiment}{Name of the experiment}
#'   \item{Instrument}{The instrument data was collected on}
#'   \item{SampleID}{The numerical ID of the sample}
#'   \item{Sample}{The name of the treatment applied to the dataset}
#' }
#' 
#' @keywords internal
#' 
#' @source Hucklesby 2020
"small_growth.df"


