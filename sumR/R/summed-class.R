#' S3 Class for objects containing iterated summations
#' 
#' Contains the summations in the log scale. The value can either be an
#' approximation to an infinite series or a finite sum.
#' @name summed-objects
#' @section Elements in the list:
#' \describe{
#' \item{`sum`}{The resulting sum in the log scale.}
#' \item{`n`}{The performed number of iterations. This value represents
#' the number of series elements evaluations performed during the summation.}
#' \item{`method`}{The method used for the summation.}
#' \item{`maxReached`}{`TRUE` or `FALSE`. Indicates whether the
#' maximum iterations was reached.}
#' }
#' @seealso [infiniteSum()], [infiniteSum_batches()] and
#' [finiteSum()] for available methods.
NULL

#' @rdname summed-objects
#' @param x The `summed` object.
#' @param ... Currently unused.
#' @return For `print`: The invisible object.
#' @method print summed
#' @export
print.summed <- function(x, ...) {
  cat("Method ", x$method, " performed ",
      ifelse(x$maxReached, "the maximum of ", ""),
      x$n, " iterations and reached ",
      "the sum in the log scale: ", x$sum, "\n", sep = "")
  invisible(x)
}

#' @rdname summed-objects
#' @param x The `summed` object.
#' @param ... Currently unused.
#' @return For `as.numeric`/`as.double`: The approximated sum.
#' @method as.double summed
#' @export
as.double.summed <- function(x, ...) x$sum
