#' Analysis of Contingency Tables with Linkage Error Adjustment
#'
#' \code{plctable} constructs a contingency table and adjusts the fitted model for
#' mismatch errors.
#'
#' @param formula a formula object with the left and right hand sides specifying the column
#' and row variable of the flat table, respectively.
#' @param adjustment An object inheriting from class \code{"adjustment"}, or a
#'   \code{list} containing the necessary parameter specifications.
#' @param subset An optional vector specifying a subset of observations to be used.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param exclude Vector of values to be excluded when forming the table (passed to \code{xtabs}).
#' @param control A list of parameters for controlling the linkage error
#' adjustment process.
#' @param ... Additional arguments passed to the internal function.
#'
#' @return
#' An object representing the fitted model. The specific class and structure of the
#' returned object depend directly on the `adjustment` method provided:
#' \itemize{
#'   \item If `adjustment` is of class `adjMixture`, returns an object of class \code{\link{ctableMixture}}.
#' }
#' The returned object is a list containing the original `adjustment` object
#' under the \code{adjustment} element.
#'
#' @examples
#' set.seed(102)
#' n <- 400
#'
#' # Simulate true categorical data
#' exposure <- sample(c("low", "high"), n, replace = TRUE)
#' # True relationship: high exposure -> higher chance of disease
#' prob_disease <- ifelse(exposure == "high", 0.7, 0.3)
#' true_outcome <- ifelse(runif(n) < prob_disease, "disease", "healthy")
#'
#' # Induce linkage (mismatch) errors at a fixed overall rate
#' true_mismatch_rate <- 0.20
#' is_mismatch <- rbinom(n, 1, true_mismatch_rate)
#'
#' obs_outcome <- true_outcome
#' mismatch_idx <- which(is_mismatch == 1)
#' # Shuffle outcomes for the mismatched records
#' obs_outcome[mismatch_idx] <- sample(obs_outcome[mismatch_idx])
#'
#' linked_df <- data.frame(exposure, outcome = obs_outcome)
#'
#' # Specify the Adjustment Method
#' adj <- adjMixture(
#'   linked.data = linked_df,
#'   m.rate = true_mismatch_rate
#' )
#'
#' # Fit the adjusted contingency table model
#' fit <- plctable(
#'   ~ exposure + outcome,
#'   adjustment = adj
#' )
#'
#' @seealso \code{\link{adjMixture}}, \code{\link{ctableMixture}}
#'
#' @export
plctable <- function(formula,
                     adjustment,
                     subset,
                     na.action,
                     exclude = c(NA, NaN),
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

 # Create Contingency Table
 xt_call <- call("xtabs", formula = formula, exclude = exclude)
 if (!is.null(data_linked)) {
  xt_call$data <- data_linked
 }

 if (!missing(subset)) xt_call$subset <- substitute(subset)
 if (!missing(na.action)) xt_call$na.action <- substitute(na.action)

 ftable <- eval(xt_call, envir = parent.frame())

 # Dispatch
 fit <- fitctable(ftable = ftable,
                  adjustment = adjustment,
                  control = control,
                  ...)

 # Post-Processing & Class Assignment
 fit$adjustment <- adjustment
 fit$call <- cl
 fit$formula <- formula

 # Append package-level class "plctable" to the internal function-level class
 class(fit) <- c(class(fit), "plctable", "plmodel")

 return(fit)
}
