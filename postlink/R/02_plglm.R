#' Fit Generalized Linear Models with Linkage Error Adjustment
#'
#' \code{plglm} fits generalized linear models (GLMs) to linked data, incorporating
#' adjustments for linkage error as specified in the provided \code{adjustment} object.
#' It mimics the interface of \code{\link[stats]{glm}} to ensure familiarity for users.
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param family A description of the error distribution and link function to be used
#'   in the model. This can be a character string naming a family function, a family
#'   function or the result of a call to a family function.
#' @param adjustment An object inheriting from class \code{"adjustment"} (e.g.,
#'   \code{adjELE}, \code{adjMixture}), or a \code{list} containing the necessary
#'   data and parameter specifications.
#' @param subset An optional vector specifying a subset of observations to be used
#'   in the fitting process.
#' @param na.action A function which indicates what should happen when the data
#'   contain NAs. The default is set by the \code{na.action} setting of \code{options},
#'   and is \code{\link[stats]{na.fail}} if that is unset.
#' @param model Logical; if \code{TRUE} (default), the model frame is returned.
#' @param x,y Logical; if \code{TRUE}, the model matrix (\code{x}) and response
#'   vector (\code{y}) are returned. Default is \code{FALSE} for both.
#' @param control A list of parameters for controlling the linkage error
#' adjustment process.
#' @param ... Additional arguments passed to the underlying fitting function.
#'
#' @return
#' An object representing the fitted model. The specific class and structure of the
#' returned object depend directly on the `adjustment` method provided:
#' \itemize{
#'   \item If `adjustment` is of class `adjELE`, returns an object of class \code{\link{glmELE}}.
#'   \item If `adjustment` is of class `adjMixture`, returns an object of class \code{\link{glmMixture}}.
#'   \item If `adjustment` is of class `adjMixBayes`, returns an object of class \code{\link{glmMixBayes}}.
#' }
#' In all cases, the returned object is a list containing the original `adjustment` object
#' under the \code{adjustment} element.
#'
#' @details
#' This function attempts to extract the linked data from the \code{adjustment}
#' object. It supports both reference-based storage (via \code{data_ref}) and
#' direct list components (\code{adjustment$data}). If the data is not present
#' (e.g., NULL), the function will attempt to resolve variables from the
#' environment of the \code{formula}.
#'
#' It applies the standard \code{model.frame} processing steps (formula parsing,
#' subsetting, NA handling) and dispatches the resulting design matrix and
#' response vector to the appropriate \code{fitglm} method.
#'
#' @examples
#' # Load the LIFE-M demo dataset
#' data(lifem)
#'
#' # Phase 1: Adjustment Specification
#' # We model the correct match indicator via logistic regression using
#' # name commonness scores (commf, comml) and a 5% expected mismatch rate.
#' adj_object <- adjMixture(
#'  linked.data = lifem,
#'  m.formula = ~ commf + comml,
#'  m.rate = 0.05,
#'  safe.matches = hndlnk
#' )
#'
#' # Phase 2: Estimation & Inference
#' # Fit a Gaussian regression model utilizing a cubic polynomial for year of birth.
#' fit <- plglm(
#'  age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'  family = "gaussian",
#'  adjustment = adj_object
#' )
#'
#' @seealso \code{\link{adjELE}}, \code{\link{adjMixture}}, \code{\link{adjMixBayes}}, \code{\link{glmELE}}, \code{\link{glmMixture}}, \code{\link{glmMixBayes}}
#' @export
plglm <- function(formula,
                  family = gaussian,
                  adjustment,
                  subset,
                  na.action,
                  model = TRUE,
                  x = FALSE,
                  y = FALSE,
                  control = list(),
                  ...) {

 # Capture Call
 cl <- match.call()

 # Validate Family (Robust Lookup)
 if (is.character(family)) {
  family <- tryCatch({
   get(family, mode = "function", envir = parent.frame())
  }, error = function(e) NULL)
 }
 if (is.function(family)) {
  family <- family()
 }
 if (is.null(family$family)) {
  stop("'family' not recognized", call. = FALSE)
 }

 # Data Retrieval from Adjustment Object
 if (missing(adjustment) || (!inherits(adjustment, "adjustment") && !is.list(adjustment))) {
  stop("'adjustment' must be a valid adjustment object or a list.", call. = FALSE)
 }

 data_linked <- NULL
 if (!is.null(adjustment$data_ref) && exists("data", envir = adjustment$data_ref)) {
  data_linked <- adjustment$data_ref$data
 } else if (is.list(adjustment) && !is.null(adjustment$data) && is.data.frame(adjustment$data)) {
  data_linked <- adjustment$data
 }

 # Standard Model Frame Construction
 mf <- match.call(expand.dots = FALSE)
 m <- match(c("formula", "subset", "na.action"), names(mf), 0L)
 mf <- mf[c(1L, m)]
 mf$drop.unused.levels <- TRUE
 mf[[1L]] <- quote(stats::model.frame)

 # 1. If adjustment has data, force it (overrides 'data' arg).
 # 3. If missing, model.frame uses formula environment.
 if (!is.null(data_linked)) {
  mf$data <- data_linked
 }

 # Evaluate model frame
 mf <- eval(mf, parent.frame())

 # Extract Model Matrix (X) and Response (Y)
 mt <- attr(mf, "terms")
 X_mat <- stats::model.matrix(mt, mf)
 Y_vec <- stats::model.response(mf, "any")

 # Dispatch to Internal Function
 fit <- fitglm(x = X_mat,
               y = Y_vec,
               family = family,
               adjustment = adjustment,
               control = control,
               ...)

 # Post-Processing
 fit$adjustment <- adjustment
 fit$call <- cl
 if (model) fit$model <- mf
 if (x) fit$x <- X_mat
 if (y) fit$y <- Y_vec

 # class hierarchy
 # 1. Function Class (e.g., "glmMixture") - preserved from fitglm
 # 2. Package Class ("plglm") - added here
 class(fit) <- c(class(fit), "plglm", "plmodel")

 return(fit)
}
