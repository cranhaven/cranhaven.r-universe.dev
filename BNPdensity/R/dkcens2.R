#' Density of the chosen kernel
#'
#' Computes likelihood contribution for censored data.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(xleft, xright, c_code_filters, distr = NULL, mu = NULL,
#'          sigma = NULL) {
#'   res <- seq_along(xleft)
#'   res[c_code_filters[["1"]]] <- dk(
#'     x = xleft[c_code_filters[["1"]]],
#'     distr, mu, sigma
#'   )
#'   res[c_code_filters[["2"]]] <- pk(
#'     xright[c_code_filters[["2"]]],
#'     distr, mu, sigma
#'   )
#'   res[c_code_filters[["0"]]] <- 1 - pk(
#'     xleft[c_code_filters[["0"]]],
#'     distr, mu, sigma
#'   )
#'   res[c_code_filters[["3"]]] <- pk(
#'     xright[c_code_filters[["3"]]],
#'     distr, mu, sigma
#'   ) - pk(
#'     xleft[c_code_filters[["3"]]],
#'     distr, mu, sigma
#'   )
#'   return(res)
#' }
dkcens2 <-
  function(xleft, xright, c_code_filters, distr = NULL, mu = NULL,
           sigma = NULL) {
    res <- seq_along(xleft)
    res[c_code_filters[["1"]]] <- dk(
      x = xleft[c_code_filters[["1"]]],
      distr, mu, sigma
    )
    res[c_code_filters[["2"]]] <- pk(
      xright[c_code_filters[["2"]]],
      distr, mu, sigma
    )
    res[c_code_filters[["0"]]] <- 1 - pk(
      xleft[c_code_filters[["0"]]],
      distr, mu, sigma
    )
    res[c_code_filters[["3"]]] <- pk(
      xright[c_code_filters[["3"]]],
      distr, mu, sigma
    ) - pk(
      xleft[c_code_filters[["3"]]],
      distr, mu, sigma
    )
    return(res)
  }
