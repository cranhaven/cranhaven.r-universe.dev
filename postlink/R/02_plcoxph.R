#' Fit Cox Proportional Hazards Models with Linkage Error Adjustment
#'
#' \code{plcoxph} fits Cox proportional hazards models to linked data, adjusting for
#' potential mismatch errors. It serves as a wrapper around the internal \code{fitcoxph}
#' function, for compatibility with the \code{\link[survival]{coxph}} syntax.
#'
#' @param formula A formula object, with the response on the left of a ~ operator,
#'   and the terms on the right. The response must be a survival object as returned
#'   by the \code{\link[survival]{Surv}} function.
#' @param adjustment An object inheriting from class \code{"adjustment"}, or a
#'   \code{list} containing the necessary parameter specifications.
#' @param subset An optional vector specifying a subset of observations.
#' @param na.action A function for handling NAs.
#' @param model Logical; if \code{TRUE}, the model frame is returned.
#' @param x,y Logical; if \code{TRUE}, the model matrix (\code{x}) and response
#'   (\code{y}) are returned. Defaults are \code{FALSE} and \code{FALSE}.
#' @param control A list of parameters for controlling the linkage error
#' adjustment process.
#' @param ... Additional arguments passed to the internal function.
#'
#' @return
#' An object representing the fitted model. The specific class and structure of the
#' returned object depend directly on the `adjustment` method provided:
#' \itemize{
#'   \item If `adjustment` is of class `adjELE`, returns an object of class \code{\link{coxphELE}}.
#'   \item If `adjustment` is of class `adjMixture`, returns an object of class \code{\link{coxphMixture}}.
#' }
#' In all cases, the returned object is a list containing the original `adjustment` object
#' under the \code{adjustment} element.
#'
#' @examples
#' library(survival)
#' set.seed(101)
#' n <- 250
#'
#' # Simulate true survival data
#' x <- rnorm(n)
#' true_hazard <- exp(0.5 * x)
#' true_time <- rexp(n, true_hazard)
#' true_status <- rbinom(n, 1, 0.8)
#'
#' # Induce linkage mismatch errors
#' match_score <- rbeta(n, 5, 1)
#' is_mismatch <- rbinom(n, 1, 1 - match_score)
#'
#' obs_time <- true_time
#' obs_status <- true_status
#' mismatch_idx <- which(is_mismatch == 1)
#'
#' # Shuffle time and status together for mismatched records
#' shuffled_idx <- sample(mismatch_idx)
#' obs_time[mismatch_idx] <- obs_time[shuffled_idx]
#' obs_status[mismatch_idx] <- obs_status[shuffled_idx]
#'
#' linked_data <- data.frame(time = obs_time, status = obs_status, x = x, match_score)
#'
#' # Specify the Adjustment Method
#' adj <- adjMixture(
#'   linked.data = linked_data,
#'   m.formula = ~ match_score
#' )
#'
#' # Fit the Adjusted Cox Proportional Hazards Model
#' fit <- plcoxph(
#'   Surv(time, status) ~ x,
#'   adjustment = adj,
#'   control = list(max.iter = 50)
#' )
#'
#' @seealso \code{\link{adjELE}}, \code{\link{adjMixture}}, \code{\link{coxphELE}}, \code{\link{coxphMixture}}
#' @export
plcoxph <- function(formula,
                    adjustment,
                    subset,
                    na.action,
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

 # Override if adjustment object has data
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
 if (attr(mt, "intercept") == 1) X_mat <- X_mat[, -1, drop = FALSE]

 # Dispatch
 fit <- fitcoxph(x = X_mat,
                 y = Y_obj,
                 adjustment = adjustment,
                 control = control,
                 ...)

 # Post-Processing
 fit$adjustment <- adjustment
 fit$call <- cl
 if (model) fit$model <- mf
 if (x) fit$x <- X_mat
 if (y) fit$y <- Y_obj

 class(fit) <- c(class(fit), "plcoxph", "plmodel")

 return(fit)
}
