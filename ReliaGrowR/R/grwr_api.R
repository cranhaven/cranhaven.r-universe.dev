#' ReliaGrowR API
#'
#' This function provides an interface to the ReliaGrowR API.#' This function provides an interface to the ReliaGrowR API.
#' @import plumber
#' @examples
#' \dontrun{
#' grwr_api()#' grwr_api()
#' }
#' @export
grwr_api <- function() {

  # Run the API
  root <- plumber::pr("inst/plumber/plumber.R")
  root %>% plumber::pr_run()
}
