#' Count the number of times an iterable object is TRUE
#'
#' Returns the number of elements from an iterable object that
#' evaluate to \code{TRUE}.
#'
#' @export
#' @param obj an iterable object
#' @param ... further arguments passed to [iteror].
#' @return the number of \code{TRUE} elements
#'
#' @seealso reduce
#'
#' @examples
#' it <- iteror(c(TRUE, FALSE, TRUE))
#' quantify(it) # 2
#'
#' set.seed(42)
#' x <- sample(c(TRUE, FALSE), size=10, replace=TRUE)
#' quantify(x) # Equivalent to sum(x)
quantify <- function(obj, ...)
  UseMethod("quantify")

#' @exportS3Method
quantify.default <- function(obj, ...)
  quantify.iteror(iteror(obj, ...))

#' @exportS3Method
quantify.iteror <- function(obj, ...) {
  stop_unused(...)
  i <- 0
  repeat {
    next_elem <- obj(or = break)
    if (next_elem) {
      i <- i + 1
    }
  }
  i
}

