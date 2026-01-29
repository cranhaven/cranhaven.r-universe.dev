#' Censoring data check
#'
#' Checks that a censored dataset is valid. This performs two checks: check that the dataset does not contain only NA, and check that the for interval censored data, the bounds are in the right order.
#'
#' For internal use
#'
#' @param xleft left bounds for the censored dataset. Can be a real number or NA
#' @param xright right bounds for the censored dataset
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(xleft, xright) {
#'   if (any(xright < xleft, na.rm = T)) {
#'     stop("in censored data, left bound not always smaller than right bound")
#'   }
#'   if (any(mapply(FUN = function(xileft, xiright) {
#'     is.na(xileft) & is.na(xiright)
#'   }, xleft, xright))) {
#'     stop("in censored data, there is an NA NA")
#'   }
#' }
cens_data_check <-
  function(xleft, xright) {
    if (any(xright < xleft, na.rm = TRUE)) {
      stop("in censored data, left bound not always smaller than right bound")
    }
    if (any(mapply(FUN = function(xileft, xiright) {
      is.na(xileft) & is.na(xiright)
    }, xleft, xright))) {
      stop("in censored data, there is an NA NA")
    }
  }
