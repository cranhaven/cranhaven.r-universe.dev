# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' print the paygap object in the terminal
#'
#' @param x paygap object, as created by the function div_paygpa()
#' @param ... arguments passed on to the generic print function: print(x$data)
#'
#' @return text output
#' @export
#'
#' @examples
#' library(div)
#' div_fake_team() %>%
#'   div_paygap    %>%
#'   print
print.paygap <- function(x, ...) {
  cat("The pay-gap matrix is:\n")
  print(x$data, ...)
  cat(x$narr)
}
