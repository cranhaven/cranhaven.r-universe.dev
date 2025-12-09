#' Do values fall outside a specified 'plausible' range?
#'
#' A utility function for indicating if elements of a vector are implausible.
#'
#' Though the function \code{\link[dplyr]{between}} already exists, it is not vectorised over the bounds.
#'
#' @param x numeric vector
#' @param lower minimum plausible value
#' @param upper maximum plausible value
#' @param open logical. If \code{TRUE}, values exactly equal to \code{lower} or \code{upper} are also considered implausible
outside_range <- function(x, lower, upper, open = TRUE) {
  if (!open) {
    x < lower | x > upper
  } else x <= lower | x >= upper
}

#' Get the mode (most common value) of a vector
#'
#' @param v a vector
#' @param na.rm Logical. If \code{TRUE} (the default), find mode of non-\code{NA} values
get_mode <- function(v, na.rm = TRUE) {
  # Returns the most common value. If multiple: whichever appears first.
  if (na.rm) v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

globalVariables('.')
