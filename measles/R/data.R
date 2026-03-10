#' Short Creek Mixing Matrix
#'
#' A matrix containing spatial data for the Short Creek area (Hildale city,
#' Utah, Colorado City town, Arizona, and Centenial Park, Arizona).
#' The matrix provides an estimate of
#' the mixing rates between schools and the rest of the population in the area.
#'
#'
#' @format
#' A row-stochastic matrix (rows add up to one) with 15 rows and 15 columns
#' with the
#'
#' @source
#' The data was generated using the `multigroup.vaccine` R package:
#' Toth D (2025). _multigroup.vaccine: Multigroup Vaccine Model_. R
#' package version 0.1.0, commit 3047ebf568c9b2028336dc14af587a282de9e225,
#' <https://github.com/EpiForeSITE/multigroup-vaccine>. The source code
#' is available at <https://github.com/UofUEpiBio/measles>
#'
#'
"short_creek_matrix"

#' Short Creek Population Data by Age Group
#'
#' A dataset containing population information for the Short Creek area
#' (Hildale city, Utah, Colorado City town, Arizona, and Centennial Park,
#' Arizona) organized by age
#' groups.
#'
#' @format A data frame with 15 rows and 4 columns:
#' \describe{
#'   \item{age_labels}{character. Labels describing the age groups.}
#'   \item{agepops}{numeric. Population counts for each age group.}
#'   \item{agelims}{numeric. Age limit boundaries for each group.}
#'   \item{vacc_rate}{numeric. Vaccination rate for each age group.}
#' }
#'
#' @details
#' This dataset provides demographic information for the Short Creek area
#' (Hildale city, Utah, Colorado City town, Arizona, and Centenial Park,
#' Arizona), with population data
#' disaggregated by 15 age categories. This dataset matches the
#' [short_creek_matrix] matrix.
#'
#' This data uses real vaccination rates from publicly available school
#' records, and population age structure and composition from the latest US
#' census. Vaccination rates for the non-school-aged population were imputed
#' based on assumptions and do not reflect the actual vaccination information
#' for those age groups.
#'
#' @source
#' The data was generated using the `multigroup.vaccine` R package:
#' Toth D (2025). _multigroup.vaccine: Multigroup Vaccine Model_. R
#' package version 0.1.0, commit 3047ebf568c9b2028336dc14af587a282de9e225,
#' <https://github.com/EpiForeSITE/multigroup-vaccine>. The source code
#' is available at <https://github.com/UofUEpiBio/measles>
#'
#'
#' @examples
#' data(short_creek)
#' head(short_creek)
#'
"short_creek"
