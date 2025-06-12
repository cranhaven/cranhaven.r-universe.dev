#' Generate constraints to encourage covariate balance
#'
#' This function generates constraints that encourage covariate balance as specified.
#' The main inputs are formula like objects, where the left hand side indicates
#' the covariate to be balanced and the right hand side indicates the
#' groups within which to balance. The constraints are
#' weighted and standardized by \code{\link{stand}()} to be used in \code{\link{optimize_controls}()}. Missingness
#' indicators can also be added and weighted for any covariate that has \code{NA} values.
#'
#' @inheritParams stand
#' @inheritParams parse_formula
#' @param balance_formulas a list of formulas where the left hand side represents
#'   the covariate to be balanced, and the terms on the right hand side represent
#'   the populations within which the covariate should be balanced. More information can
#'   be found in the details below.
#' @param weight_by_size numeric between 0 and 1 stating how to adjust constraints
#'   for the size of the population they represent. Default is 0, meaning imbalance
#'   within populations is viewed in absolute terms, not relative to the population size.
#'   The program may thus prioritize
#'   balancing the covariate in larger populations compared to smaller populations. A value
#'   of 1 means that imbalance will be measured relative to the population's size, not
#'   in absolute terms, implying that it is equally important to balance in every population.
#' @param treated which treatment value should be considered the treated group. This
#' must be one of the values of \code{z}. This
#' is used if \code{denom_variance = "treated"} for calculating the variance
#' to use in the standardization or if \code{weight_by_size > 0} to determine which
#' treatment group to use to calculate population sizes.
#' @param autogen_missing whether to automatically generate missingness constraints
#'   and how heavily to prioritize them. Should be a numeric
#'   or \code{NULL}. \code{NULL} indicates that
#'   constraints to balance the rate of missingness (denoted by \code{NA}s
#'   in \code{data}) should not be automatically generated. Note that this is not
#'   recommended unless the user has already accounted for missing values.
#'   If not \code{NULL}, \code{autogen_missing} should be a numeric stating how heavily
#'   to prioritize generated missingness constraints over covariate constraints.
#'   The default is 50.
#' @return A list with two named components:
#' \describe{
#'   \item{\code{X}}{a matrix with constraints as columns and the same number of rows as the inputs.
#'   The column names provide information about the constraints, including the covariate
#'   names and the factor and level to which it pertains.}
#'   \item{\code{importances}}{a named vector with names corresponding to the constraint names
#'   and values corresponding to how heavily that constraint should be prioritized,
#'   based on the information provided through \code{balance_formulas}, \code{weight_by_size},
#'   and \code{autogen_missing}.}
#'   }
#'
#' @section Details:
#'   The \code{balance_formulas} argument can include formulas beyond those interpreted
#'   by \code{R} to be \code{formulas}. Their interpretation is also different, as
#'   explained below:
#'
#' \describe{
#' \item{Left hand side}{The left hand side of the formula contains the covariate
#'   to be balanced. It can also be the sum of multiple covariates, in which case
#'   each term will be balanced individually according to the right hand side. Additionally,
#'   '.' on the left hand side will designate that all covariates in \code{data}
#'   should be balanced according to the designated or default right hand side
#'   (as usual, terms may be subtracted to remove them).}
#' \item{Right hand side}{The right hand side should be the sum of factor, character,
#'   or boolean variables. The covariate of the left hand side will be balanced within
#'   each level of each term on the right hand side. The right hand side can also
#'   contain '.', meaning the covariate will be balanced across all levels of all
#'   categorical, character, or boolean variables found in \code{data} (as usual,
#'   terms may be subtracted to remove them). In the most common case, the user
#'   will have one term on the right hand side consisting of the strata within
#'   which balance in desired.}
#' \item{Coefficients}{The formulas can contain coefficients specifying how much
#'   to weight a certain set of constraints. Coefficients of the left hand side terms will
#'   weight all constraints generated for that covariate, and coefficients of the
#'   right hand side will weight the constraints generated for each level of that
#'   term.}
#' \item{Intercept}{The intercept term, 1, is automatically included on the right
#'   hand side of the formula, and designates that the covariate of the left hand side
#'   will be balanced across all control units. You may enter a different numeric > 0
#'   that will signify how much to weight the constraint, or you may enter "- 1" or "+ 0"
#'   to remove the intercept and its associated constraint, as per usual.}}
#'
#'
#' @export
#' @importFrom caret dummyVars
#' @importFrom stats predict median model.frame.default as.formula na.pass terms setNames
#'
#' @examples
#' data('nh0506')
#'
#' # Create strata
#' age_cat <- cut(nh0506$age,
#'                breaks = c(19, 39, 50, 85),
#'                labels = c('< 40 years', '40 - 50 years', '> 50 years'))
#' strata <- age_cat : nh0506$sex
#'
#' # Balance age, race, education, poverty ratio, and bmi both across and within the levels of strata
#' constraints <- generate_constraints(
#'                  balance_formulas = list(age + race + education + povertyr + bmi ~ 1 + strata),
#'                  z = nh0506$z,
#'                  data = nh0506)
#'
#' # Balance age and race both across and within the levels of strata,
#' # with balance for race being prioritized twice as much as for age,
#' # and balance across strata prioritized twice as much as within.
#' # Balance education across and within strata,
#' # with balance within strata prioritized twice as much as across.
#' # Balance poverty ratio and bmi only within the levels of strata,
#' # as specified in the default_rhs argument
#' constraints <- generate_constraints(
#'                  balance_formulas = list(age + 2 * race ~ 2 + strata,
#'                                          education ~ 1 + 2 * strata,
#'                                          'povertyr',
#'                                          'bmi'),
#'                  z = nh0506$z,
#'                  data = nh0506,
#'                  default_rhs = '0 + strata')
#'

generate_constraints <- function(balance_formulas, z, data, default_rhs = NULL,
                              weight_by_size = 0, denom_variance = "treated",
                              treated = 1, autogen_missing = 50) {
  constraints <- NULL
  importances <- NULL
  for (balance_formula in balance_formulas) {
    # Parse the formula ----
    parsed_formula <- parse_formula(balance_formula, default_rhs, data)
    new_formula <- parsed_formula$new_formula
    rhs_weights <- parsed_formula$rhs_weights
    lhs_weights <- parsed_formula$lhs_weights
    # Generate a new data frame based on all the variables included on the LHS or RHS of formula
    new_data <- model.frame.default(
      as.formula(paste0("~ ", paste0(names(rhs_weights), collapse = " + "), " + ",
                        paste0(names(lhs_weights), collapse = " + "))),
      data, na.action = na.pass)

    # Parse coefficients for left hand side ----
    for (lhs_term in names(lhs_weights)) {
      if (is.factor(new_data[, lhs_term]) | is.character(new_data[, lhs_term])) {
        lhs_term_dummy_data <- predict(dummyVars(as.formula(paste0(" ~ ", lhs_term)), data = new_data),
                                       newdata = new_data)
        new_data <- cbind(new_data, lhs_term_dummy_data)
        for (new_term in colnames(lhs_term_dummy_data)) {
          lhs_weights[new_term] <- lhs_weights[lhs_term]
        }
        lhs_weights <- lhs_weights[names(lhs_weights) != lhs_term]
      }
    }

    # Figure out all expanded right hand side terms ----
    dummies <- dummyVars(as.formula(paste0(" ~ ",
                                           paste(deparse(new_formula[[3]]), collapse=' '))),
                         data = new_data, sep = "")
    mat <- predict(dummies, newdata = new_data)
    if (attr(terms(new_formula, data = new_data), "intercept")) {
      mat <- cbind(1, mat)
      colnames(mat)[1] <- "1"
    }
    non_cat <- colnames(mat)[apply(mat, 2, function(x) any(!unique(x) %in% c(0,1)))]
    mat <- mat[, !colnames(mat) %in% non_cat, drop = FALSE]
    mat <- matrix(as.logical(mat), nrow = nrow(mat), dimnames = dimnames(mat))

    # Generate constraints for each expanded right hand term for each left hand term ----
    for (lhs_term in names(lhs_weights)) {
      if (length(non_cat[non_cat != lhs_term]) >= 1) {
        warning(paste0("The right hand side of the balancing formula for `", lhs_term,
                       "` may only contain factor, boolean, or character variables. \n The following variable(s) have thus been excluded: ",
                       paste(non_cat[non_cat != lhs_term], collapse = ", "),
                       ". To include them, please convert them to an appropriate form."))
      }

      cov_mat <- mat[, colnames(mat) != lhs_term, drop = FALSE]

      constraint_mat <- matrix(data = 0, nrow = nrow(cov_mat), ncol = 2 * ncol(cov_mat),
                               dimnames = list(NULL,
                                               paste0(c("", "missing_"), rep(colnames(cov_mat), each = 2))))
      constraint_importances <- setNames(rep(1, ncol(constraint_mat)), colnames(constraint_mat))

      # Create and standardize constraints
      drop_cols <- NULL
      for (col in 1:ncol(cov_mat)) {
        ind <- as.numeric(cov_mat[, col])
        xstand <- stand(z = z, x = new_data[, lhs_term],
                        denom_variance = denom_variance,
                        treated = treated, autogen_missing = autogen_missing)
        constraint_mat[ind == 1, (2*col-1)] <- xstand$covariate[ind == 1]
        if (is.null(xstand$missingness)) {
          drop_cols <- c(drop_cols, 2*col)
        } else {
          constraint_mat[ind == 1, (2*col)] <- xstand$missingness[ind == 1]
          # Weight missingness constraints
          constraint_importances[2*col] <- autogen_missing * constraint_importances[2*col]
        }
      }
      # Drop any missingness constraints where there is no missingness in that covariate
      constraint_mat <- constraint_mat[, ! 1:ncol(constraint_mat) %in% drop_cols, drop = FALSE]
      constraint_importances <- constraint_importances[! 1:length(constraint_importances) %in% drop_cols]

      for (term in names(rhs_weights)) {
        if (term == "1") {
          term_cols <- c("1")
        } else {
          term_cols <- colnames(predict(dummyVars(paste0('`', lhs_term, '`', " ~ ", "`", term, "`"),
                                                  data = new_data, sep = ""),
                                        newdata = new_data))
        }

        term_cols <- term_cols[term_cols %in% colnames(constraint_mat)]
        term_cols_w_missing <- paste0(c("", "missing_"), rep(term_cols, each = 2))
        term_cols_w_missing <- term_cols_w_missing[term_cols_w_missing %in% colnames(constraint_mat)]

        col_ns <- colSums(mat[z == treated, term_cols, drop = FALSE])

        # Weight constraints based on coefficients and weight_by_size
        if (0 %in% col_ns) {
          warning(paste0("`", term, "`",
                         " has no treated units in one of its levels and thus constraints will not be generated for any levels of this term."))
          constraint_mat <- constraint_mat[, !colnames(constraint_mat) %in% term_cols_w_missing, drop = FALSE]
          constraint_importances <- constraint_importances[, !names(constraint_importances) %in% term_cols_w_missing]
        } else {
          constraint_importances[term_cols_w_missing] <- rhs_weights[term] * constraint_importances[term_cols_w_missing]
          for (col in term_cols) {
            multiplier <- (col_ns[col] + weight_by_size * (mean(col_ns) - col_ns[col])) / col_ns[col]
            constraint_importances[col] <- constraint_importances[col] * multiplier
            col_missing <- paste0( "missing_", col)
            if (col_missing %in% names(constraint_importances)) {
              constraint_importances[col_missing] <- constraint_importances[col_missing] * multiplier
            }

          }
        }

      }

      constraint_importances <- lhs_weights[lhs_term] * constraint_importances

      colnames(constraint_mat) <- paste(lhs_term, colnames(constraint_mat), sep = "_")
      names(constraint_importances) <- paste(lhs_term, names(constraint_importances), sep = "_")

      constraints <- cbind(constraints, constraint_mat)
      importances <- c(importances, constraint_importances)
    }
  }

  return(list(X = constraints, importances = importances))

}

