# In makeJournalTables: Don't export.
#' @noRd
#' @title Is a vector an integer, whole, natural, even, or odd?
#' @name is.integer
#' @family statistics
#' @keywords statistics
#' @seealso {\code{\link[base]{is.numeric}}}

#' @description The following logical functions check if numeric vector x is an integer, whole number, natural number, even or odd.
#'
#' @details
#' The following functions expand \code{\link[base]{is.numeric}}: \itemize{
#' \item \code{is.integer} determines if x contains integer numbers (..., -2, -1, 0,1,2, ...).
#' \item \code{is.wholenumber} determines if a vector x contains whole numbers (0,1,2, ...).
#' \item \code{is.naturalnumber} determines if a vector x contains natural numbers (1,2, ...).
#' \item \code{is.even} and \code{is.odd} determines if a vector x contains even or odd numbers, respectively.
#' \item \code{fact}: My own factorial function. Returns the factorial for whole numbers only.
#' }
#'
#' @note
#'   When x is not integer, the \code{is.even} and \code{is.odd} functions return an `NA`, as it is neither odd or even.
#'
#' @param x a vector
#' @param t a number. Used in fact function.
#' @param tol tolerance detecting lower bound. Default:
#'
#' @examples
#' \dontrun{
#' # is integer?
#' is.integer(-2) # TRUE
#' is.integer(0)  # TRUE
#' is.integer(3)  # TRUE
#' is.integer(3.3) # FALSE
#' is.integer(pi) # FALSE
#' is.integer(c(-2:2, 3.5, pi))
#'
#' # is whole number?
#' is.wholenumber(-2) # FALSE
#' is.wholenumber(0)  # TRUE
#' is.wholenumber(3)  # TRUE
#' is.wholenumber(3.3) # FALSE
#'
#' # is even or odd?
#' is.even(2) # TRUE
#' is.even(3) # FALSE
#' is.even(3.4) # FALSE
#' is.odd(2) # FALSE
#' is.odd(3) # TRUE
#' is.odd(3.3) # FALSE
#' is.odd(-3.3) # FALSE
#' is.odd(-3) # TRUE
#' a <- c(-2:8, pi, 3.3)
#' d <- data.frame(x = a, is_odd = is.odd(a), is_even = is.even(a),
#'   truth = c(rep("alt", 11), NA, NA))
#' d$sum <- d$is_odd + d$is_even
#' d
#' }
#'
#' # #' @rdname is.integer
is.integer <- function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}


# #' @rdname is.integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  x > -1 &&  is.integer(x, tol)
}


# #' @rdname is.integer
is.naturalnumber <- function(x, tol = .Machine$double.eps^0.5) {
  x > 0 && is.integer(x, tol)
}


# #' @rdname is.integer
is.even <- function(x, tol = .Machine$double.eps^0.5) {
  ifelse(is.integer(x, tol), x %% 2 == 0, NA)
}

# #' @rdname is.integer
is.odd <- function(x, tol = .Machine$double.eps^0.5) {
  ifelse(is.integer(x, tol), x %% 2 != 0, NA)
}

# #' @rdname is.integer
fact <- function(t, tol = .Machine$double.eps^0.5) {
  if (is.wholenumber(t, tol)) {
    return(exp(prod(log(1:t))))
  } else {
    return(NA)  # does not exist if not a whole number
  }
}
