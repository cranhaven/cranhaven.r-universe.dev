#' Random walks for 10 assets as example data.
#'
#' The dataset contains the price data (not returns!), each starting at a value of 100. The dates are randomly recreated by choosing the latest date as Sys.Date() going backwards on a daily basis per row.
#'
#' @format An xts-object with 1000 rows and 10 variables:
#' \describe{
#'   \item{asset1}{Column with price data of a random walk called asset1.}
#'   \item{asset2}{Column with price data of a random walk called asset2.}
#'   ...
#' }
"assets"
