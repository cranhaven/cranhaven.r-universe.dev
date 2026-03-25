#' Example simulation data
#'
#' A toy dataset included in the package to illustrate the use of
#' the twfeiv_decomp() function. This is artificial data and does not
#' represent real observations.
#'
#' @format A data frame with 60 rows and 6 variables:
#' \describe{
#'   \item{id}{Individual identifier (1–10)}
#'   \item{time}{Time period (2000–2005)}
#'   \item{instrument}{Binary instrumental variable}
#'   \item{treatment}{Treatment variable}
#'   \item{outcome}{Outcome variable}
#'   \item{control1}{Control variable 1}
#'   \item{control2}{Control variable 2}
#' }
#'
#' @examples
#' data(simulation_data)
#' head(simulation_data)
"simulation_data"