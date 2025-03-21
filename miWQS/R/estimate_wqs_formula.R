#' Formula for WQS Regression
#'
#' @family wqs
#' @keywords imputation wqs

#' @description
#' A wrapper function for \code{\link{estimate.wqs}} to use a formula instead.

#' @param formula  An object of class "formula" that consists of an outcome, chemical mixture, and covariates, if any. See \code{stats::\link[stats]{formula}}.
#' @param data     The data in a data-frame format
#' @param chem_mix Indices or column names of variables to be combined into an index.
#' @param ...      Additional WQS parameters passed to \code{estimate.wqs}. Note: data arguments (y, X, and Z) have no effect.
#' @inheritParams estimate.wqs
#' @export

# Eventually when I model a formula like y ~ x1 + x2 | z1 + z2 ... which is better than I have. #' @import Formula
# #' @rdname estimate.wqs

#' @examples
#'
#' # Example 1
#' set.seed(232)
#' test.data <- data.frame(x1 = rlnorm(100, 3, 1), x2 = rlnorm(100, 5, 1),
#'   z1 = rlnorm(100, 10, 3), z2 = rbinom(100, 1, 0.7),
#'   y = rnorm(100, 100, 15)
#' )
#' estimate.wqs.formula(y ~ ., data = test.data, chem_mix = c("x1", "x2"))
#' \dontrun{
#' # Example 2: No covariates
#' estimate.wqs.formula(y ~ x1 + x2, data = test.data, chem_mix = 1:2)
#'
#' # Example 3: NA in Z
#' test.data$z1[10] <- NA
#' estimate.wqs.formula(y ~ ., data = test.data, chem_mix = c("x1", "x2"))
#'
#' # Example 4: NA in Z and y
#' test.data$y[1] <- NA
#' estimate.wqs.formula(y ~ ., data = test.data, chem_mix = c("x1", "x2"))
#'
#'
#' # Example 5: NA in Z, X, and y
#' test.data$x1[2] <- NA
#' estimate.wqs.formula(y ~ ., data = test.data, chem_mix = c("x1", "x2"),
#'   place.bdls.in.Q1 = TRUE
#' )
#' # due to time constraints
#' }
#'
estimate.wqs.formula <- function(formula, data, chem_mix, ..., verbose = FALSE) {
  # Obtain data frame actually used in formula
  subset <- model.frame(formula, data = data, na.action = na.pass,
    drop.unused.levels = FALSE, xlev = NULL)

  # Find the outcome y and remove any missing outcomes...because
  y_name <- all.vars(formula)[1]
  if (anyNA(subset$y)) {
    warning("All missing outcomes are ignored.")
    subset <- subset[complete.cases(subset$y), ]
  }

  # Find X name (eg. ("as", "cd", "pcb_105") if chem_mix is an index eg. c(2,3, 15).
  if (is.character(chem_mix)) {
    chem_name <- chem_mix
  } else {
    chem_name <-  colnames(data)[chem_mix]
  }

  # Find covariates Z and define as matrix. Use model_matrix() which removes all missing y & Z's so that all factors, numbers, etc. is a matrix.
  # Find Covariate Names
  h <- colnames(subset) %in% c(y_name, chem_name)
  cov_name <- colnames(subset)[!h]
  if (length(cov_name) == 0) {
    Z <- NULL
  } else {
    if (anyNA(subset[, cov_name])) {
      warning("Any missing covariates are ignored.", call. = FALSE)
      index <- complete.cases(subset[, cov_name])
      subset <- subset[index, ]
    }
    Z <-  model.matrix(formula, data = subset[, c(y_name, cov_name)]) [, -1]
  }

  # Find y and X
  y <- subset[, y_name]
  X <- as.matrix(subset [, chem_name, drop = FALSE])

  # Checks
  if (verbose) {
    cat("                      : y, X, Z \n")
    cat("Number of Rows in each component:", c(length(y), nrow(X), nrow(Z)), "\n")
    cat("Any NA's?", c(anyNA(y), anyNA(X), anyNA(Z)), "\n")
    # X is only one allowed to have NA; y's NA is removed inside function.
    cat("Classes: ", c(class(y), class(X), class(Z)), "\n")
    df2 <- list(y = y, X = X, Z = Z)
    cat("Summary of Data Output \n")
    lapply(df2, summary)
  }

  # Run estimate.wqs
  out <- estimate.wqs(y, X, Z, ...)

  return(out)
}
