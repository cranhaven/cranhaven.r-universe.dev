# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Title
#'
#' @param object paygap S3 object, as created by the function dif_paygap()
#' @param ... passed on to summary()
#'
#' @return a summary of the paygap object
#' @export
#'
#' @examples
#' library(div)
#' d <- div_fake_team()
#' pg <- div_paygap(d)
#' summary(pg)
summary.paygap <- function(object, ...) {
  summary(object$data$paygap, ...)
  cat(object$narr)
}
