#' Simulated lognormal mixture data.
#'
#' A simulated dataset with 10000 observations from a lognormal mixutre
#' model with 2 componentes.
#'
#' @format ## `sim_data`
#' A list with two componentes:
#' - $data: A data frame with 10,000 rows and 3 columns:
#' \describe{
#'   \item{y}{observed survival time}
#'   \item{delta}{event indicator. 1 == event, 0 == censored.}
#'   \item{x}{binary covariate}
#' }
#' - $true_vals: A named vector with the true values used to generate the data.
"sim_data"