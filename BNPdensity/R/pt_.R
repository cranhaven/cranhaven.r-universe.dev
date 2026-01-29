#' Distribution function non-standard student-t
#'
#' Computes the cdf.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(x, df, mean, sd) {
#'   pt((x - mean) / sd, df, ncp = 0)
#' }
pt_ <-
  function(x, df, mean, sd) {
    pt((x - mean) / sd, df, ncp = 0)
  }
