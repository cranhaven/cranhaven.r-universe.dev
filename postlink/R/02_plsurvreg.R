#' Fit Parametric Survival Models with Linkage Error Adjustment
#'
#' \code{plsurvreg} fits parametric survival models (Accelerated Failure Time models)
#' to linked data. It serves as a wrapper for the internal engines, compatible with
#' \code{\link[survival]{survreg}} specifications.
#'
#' @param formula A formula object, with the response on the left of a ~ operator,
#'   and the terms on the right. The response must be a survival object as returned
#'   by the \code{\link[survival]{Surv}} function.
#' @param adjustment An object inheriting from class \code{"adjustment"}, or a
#'   \code{list} containing the necessary parameter specifications.
#' @param subset An optional vector specifying a subset of observations.
#' @param na.action A function for handling NAs.
#' @param dist Character string specifying the survival distribution
#'   (currently it must be "weibull" or "gamma").
#' @param model Logical; if \code{TRUE}, the model frame is returned.
#' @param x,y Logical; if \code{TRUE}, the model matrix (\code{x}) and response
#'   (\code{y}) are returned.
#' @param control A list of control parameters.
#' @param ... Additional arguments passed to the internal engine.
#'
#' @return
#' An object representing the fitted model. The specific class and structure of the
#' returned object depend directly on the `adjustment` method provided:
#' \itemize{
#'   \item If `adjustment` is of class `adjMixBayes`, returns an object of class \code{\link{survregMixBayes}}.
#' }
#' The returned object is a list containing the original `adjustment` object
#' under the \code{adjustment} element.
#'
#' @examples
#' \donttest{
#' library(survival)
#' set.seed(202)
#' n <- 200
#'
#' # Simulate Weibull AFT data
#' trt <- rbinom(n, 1, 0.5)
#' true_time <- rweibull(n, shape = 1.5, scale = exp(1 + 0.8 * trt))
#' cens_time <- rexp(n, 0.05)
#' true_obs_time <- pmin(true_time, cens_time)
#' true_status <- as.numeric(true_time <= cens_time)
#'
#' # Induce linkage mismatch errors by...
#' is_mismatch <- rbinom(n, 1, 0.2) # ~20% overall mismatch rate
#' obs_time <- true_obs_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' # Shuffle time and status together for mismatched records
#' shuffled <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled]
#' obs_status[mismatch_idx] <- obs_status[shuffled]
#'
#' linked_df <- data.frame(time = obs_time, status = obs_status, trt)
#'
#' # Specify the Bayesian Mixture Adjustment
#' adj <- adjMixBayes(linked.data = linked_df)
#'
#' # Fit the Adjusted Parametric Survival Model
#' fit <- plsurvreg(
#'   Surv(time, status) ~ trt,
#'   dist = "weibull",
#'   adjustment = adj,
#'   control = list(iterations = 2000, burnin.iterations = 500)
#' )
#' }
#' @seealso \code{\link{adjMixBayes}}, \code{\link{survregMixBayes}}
#' @export
plsurvreg <- function(formula,
                      adjustment,
                      subset,
                      na.action,
                      dist = "weibull",
                      model = TRUE,
                      x = FALSE,
                      y = FALSE,
                      control = list(),
                      ...) {

 cl <- match.call()

 # Data Retrieval
 if (missing(adjustment) || (!inherits(adjustment, "adjustment") && !is.list(adjustment))) {
  stop("'adjustment' must be a valid adjustment object or a list.", call. = FALSE)
 }

 data_linked <- NULL
 if (!is.null(adjustment$data_ref) && exists("data", envir = adjustment$data_ref)) {
  data_linked <- adjustment$data_ref$data
 } else if (is.list(adjustment) && !is.null(adjustment$data) && is.data.frame(adjustment$data)) {
  data_linked <- adjustment$data
 }

 # Model Frame Construction
 mf <- match.call(expand.dots = FALSE)
 m <- match(c("formula", "subset", "na.action"), names(mf), 0L)
 mf <- mf[c(1L, m)]
 mf$drop.unused.levels <- TRUE
 mf[[1L]] <- quote(stats::model.frame)

 if (!is.null(data_linked)) {
  mf$data <- data_linked
 }

 mf <- eval(mf, parent.frame())

 # Extract X and Y
 Y_obj <- stats::model.response(mf)
 if (!inherits(Y_obj, "Surv")) {
  stop("Response must be a 'Surv' object.", call. = FALSE)
 }

 mt <- attr(mf, "terms")
 X_mat <- stats::model.matrix(mt, mf)

 # Dispatch
 fit <- fitsurvreg(x = X_mat,
                   y = Y_obj,
                   dist = dist,
                   adjustment = adjustment,
                   control = control,
                   ...)

 # Post-Processing
 fit$adjustment <- adjustment
 fit$call <- cl
 if (model) fit$model <- mf
 if (x) fit$x <- X_mat
 if (y) fit$y <- Y_obj

 class(fit) <- c(class(fit), "plsurvreg", "plmodel")

 return(fit)
}
