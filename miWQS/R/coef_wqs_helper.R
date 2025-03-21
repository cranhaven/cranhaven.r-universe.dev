#' Finding WQS Coefficients
#'
#' @name coef.wqs
#' @family wqs
#' @seealso \code{\link[stats]{coef}}
#' @keywords wqs
#'
#' @description An accessor function that returns the coefficients from the validation WQS model,
#' a \strong{wqs} object.
#'
#' @details
#' In a \strong{wqs} object, the \emph{fit} element, a \pkg{glm2} object, is extracted. See \code{\link{glm2}{glm2}}.
#'
#' @param object An object of class "wqs", usually as a result of \code{\link{estimate.wqs}}.
# Or object from class miWQS, which is an output using \code{pool.wqs}.
# previously W. Need argument names to match up with default methods (see stats::coef)
#' @inheritParams stats::coef
#'
#' @examples
#' # Use simulated dataset and set seed for reproducibility.
#' data(simdata87)
#' set.seed(23456)
#' Wa <- estimate.wqs(
#'   y = simdata87$y.scenario, X = simdata87$X.true[, 1:3],
#'   B = 10, family = "binomial"
#' )
#' coef(Wa)
#' @importFrom stats coef
#' @export

coef.wqs <- function(object, ...) {
  # Check if
  if (is(object, "wqs")) {
    coef(object$fit, ...) # generic function in stats.
  } else {
    stop("object is not from class `wqs`.")
  }
}

# @rdname coef.wqs
# @export coef.miWQS
# coef.miWQS <- function(object, ...){
#  if(class(object) == "miWQS"){
#    fit <- object$pooled.wqs.estimates$pooled.mean
#    names(fit) <- dimnames(object$pooled.wqs.estimates)[[1]]
#    return(fit)
#  }
# }
