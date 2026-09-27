#' Secondary Analysis Constructor Based on Mixture Modeling
#'
#' Specifies the linked data and information on the underlying record linkage
#' process for the "General Framework for Regression with Mismatched Data"
#' developed by Slawski et al. (2025). This framework uses a mixture model for
#' pairs of linked records whose two components reflect distributions
#' conditional on match status, i.e., correct match or mismatch.
#' Inference is based on composite likelihood and the EM algorithm. Examples of
#' information about the underlying record linkage process that can be incorporated
#' into the method if available are the assumed overall mismatch rate,
#' safe matches, predictors of match status, or predicted probabilities of
#' correct matches.
#'
#' @param linked.data A data.frame containing the linked dataset. If \code{NULL},
#'   the function attempts to resolve variables specified in \code{m.formula}
#'   from the environment.
#' @param m.formula A one-sided formula object for the mismatch indicator model,
#'  with the covariates on the right of "~". The default is an intercept-only
#'  model corresponding to a constant mismatch rate.
#' @param m.rate Numeric; an optional estimate (a proportion between 0 and 1)
#' to constrain the global mismatch rate estimate. Defaults to \code{NULL}.
#' @param safe.matches A logical vector or an unquoted variable name found in
#' \code{linked.data}; an indicator variable for safe matches (TRUE : record can
#' be treated as a correct match and FALSE : record may be mismatched).
#' The default is FALSE for all matches.
#'
#' @return An object of class \code{c("adjMixture", "adjustment")}. To minimize
#' memory overhead, the underlying \code{linked.data} is stored by reference
#' within an environment inside this object.
#'
#' @details
#' The constructor assumes that any variables defined in \code{m.formula} and
#' \code{safe.matches} are in \code{linked.data} or in the same environment.
#' Explicit provision of \code{linked.data} is strongly recommended for
#' reproducibility and to ensure the adjustment object fully encapsulates
#' the necessary data for downstream model fitting.
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
#' @references
#' Slawski, M., West, B. T., Bukke, P., Wang, Z., Diao, G., &
#' Ben-David, E. (2025). A general framework for regression with mismatched
#' data based on mixture modelling. \emph{Journal of the Royal Statistical Society
#' Series A: Statistics in Society}, 188(3), 896-919. \doi{10.1093/jrsssa/qnae083}
#'
#' Bukke, P., Ben-David, E., Diao, G., Slawski, M., & West, B. T. (2025).
#' Cox Proportional Hazards Regression Using Linked Data: An Approach Based on
#' Mixture Modeling. In \emph{Frontiers of Statistics and Data Science} (pp. 181-200).
#' Singapore: Springer Nature Singapore. \doi{10.1007/978-981-96-0742-6_8}
#'
#' Slawski, M., Diao, G., Ben-David, E. (2021). A pseudo-likelihood approach to
#' linear regression with partially shuffled data. \emph{Journal of Computational
#' and Graphical Statistics}. 30(4), 991-1003. \doi{10.1080/10618600.2020.1870482}
#'
#' @seealso
#' * [plglm()] for generalized linear regression modeling
#' * [plcoxph()] for Cox proportional hazards regression modeling
#' * [plctable()] for contingency table analysis
#'
#' @export
adjMixture <- function(linked.data = NULL,
                       m.formula = ~1,
                       m.rate = NULL,
                       safe.matches = NULL) {

 # 1. Validate m.formula
 if (!inherits(m.formula, "formula")) {
  stop("'m.formula' must be a formula object.", call. = FALSE)
 }
 if (length(m.formula) != 2L) {
  stop("'m.formula' must be a one-sided formula (e.g., ~ x + y).", call. = FALSE)
 }

 # 2. Validate linked.data
 if (!is.null(linked.data)) {
  if (is.environment(linked.data) || is.list(linked.data)) {
   linked.data <- tryCatch(as.data.frame(linked.data), error = function(e) {
    stop("'linked.data' must be a data.frame or coercible to one.", call. = FALSE)
   })
  } else if (!is.data.frame(linked.data)) {
   stop("'linked.data' must be a data.frame, list, or environment.", call. = FALSE)
  }
 }

 # 3. Resolve m.formula environment if linked.data is missing
 if (is.null(linked.data)) {
  env_to_use <- environment(m.formula)
  if (is.null(env_to_use)) env_to_use <- parent.frame()

  mf <- tryCatch({
   stats::model.frame(m.formula, data = env_to_use, na.action = stats::na.pass)
  }, error = function(e) {
   stop("Could not resolve variables in 'm.formula' from the environment. Please provide 'linked.data'.", call. = FALSE)
  })
  linked.data <- mf
 } else {
  # Validate m.formula variables exist in linked.data
  m_vars <- all.vars(m.formula)
  if ("." %in% m_vars) stop("Usage of '.' in 'm.formula' is not supported.", call. = FALSE)

  missing_vars <- setdiff(m_vars, names(linked.data))
  if (length(missing_vars) > 0) {
   stop(paste("The following variables in 'm.formula' are missing from 'linked.data':",
              paste(missing_vars, collapse = ", ")), call. = FALSE)
  }
 }

 # 4. Resolve safe.matches (NSE vs Standard Evaluation)
 safe_matches_eval <- NULL

 # Capture the expression passed to safe.matches
 safe_expr <- substitute(safe.matches)

 if (!is.null(safe_expr)) {
  # Try to evaluate the expression within linked.data (NSE)
  # This handles cases like safe.matches = hndlnk where hndlnk is a column name
  safe_matches_eval <- tryCatch({
   eval(safe_expr, linked.data, enclos = parent.frame())
  }, error = function(e) NULL)

  # If NSE failed (returned NULL), or if the user passed a variable
  # from the global environment (e.g., safe.matches = my_vec), try standard evaluation.
  if (is.null(safe_matches_eval)) {
   safe_matches_eval <- tryCatch({
    eval(safe_expr, envir = parent.frame())
   }, error = function(e) {
    stop("Could not find object '", deparse(safe_expr), "' in 'linked.data' or the environment.", call. = FALSE)
   })
  }
 }

 # 5. Validate the resolved safe.matches vector
 if (!is.null(safe_matches_eval)) {
  if (!is.logical(safe_matches_eval)) {
   stop("'safe.matches' must be a logical vector.", call. = FALSE)
  }
  if (any(is.na(safe_matches_eval))) {
   stop("'safe.matches' cannot contain NA values.", call. = FALSE)
  }
  if (!all(safe_matches_eval %in% c(TRUE, FALSE))) {
   stop("'safe.matches' must only contain TRUE or FALSE.", call. = FALSE)
  }
  if (length(safe_matches_eval) != nrow(linked.data)) {
   stop("Length of 'safe.matches' does not match the number of rows in 'linked.data'.", call. = FALSE)
  }
 }

 # 6. Validate m.rate
 if (!is.null(m.rate)) {
  if (!is.numeric(m.rate) || length(m.rate) != 1L || m.rate <= 0 || m.rate >= 1) {
   stop("'m.rate' must be a single numeric value strictly between 0 and 1.", call. = FALSE)
  }
 }

 # 7. Force Unique Row Identifiers
 # To ensure the internal engines (fitglm, fitcoxph) can accurately map
 # the model matrix back to this adjustment object, we enforce unique
 # character row names. This prevents issues with duplicate keys or
 # ambiguous integer indexing.

 # Check for duplicates or missing row names
 if (is.null(rownames(linked.data)) || anyDuplicated(rownames(linked.data)) > 0) {

   # Assign deterministic, prefixed IDs (e.g., ".adj_1", ".adj_2")
   # The prefix ".adj_" ensures these are treated as characters, not integers.
   rownames(linked.data) <- paste0(".adj_", seq_len(nrow(linked.data)))

   # Notify the user if we modified their data structure
   message("Note: 'linked.data' contained missing or duplicate row names. ",
           "Unique internal IDs have been assigned to ensure alignment.")
 }

 # 8. Construct and Return the S3 Object with Reference Semantics
 data_ref <- new.env(parent = emptyenv())
 data_ref$data <- linked.data

 out <- structure(
  list(
   data_ref = data_ref,
   m.formula = m.formula,
   m.rate = m.rate,
   safe.matches = safe_matches_eval # Store the evaluated vector, not the expression
  ),
  class = c("adjMixture", "adjustment")
 )

 return(out)
}
