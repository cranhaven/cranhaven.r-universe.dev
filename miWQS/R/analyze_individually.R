#' Performing Individual Chemical Analysis
#'
#' @family wqs
#' @keywords wqs
#'
#' @description
#' An accessory function for \code{estimate.wqs()}. Performs individual chemical analyses to determine the constraint for the overall mixture effect on the outcome (\eqn{\beta_1}) in WQS regression. After adjusting for any covariates, the outcome regresses on each chemical \emph{individually}. Returns a data-frame of statistics from these analyses.
#'
#' @details
#' Individual chemical analyses with the outcome can be used to determine whether the mixture of chemicals is positively or negatively related to the outcome. The constraint whether the overall mixture effect,  \eqn{\beta_1}, is positive or negative is controlled by b1.pos argument in \code{\link{estimate.wqs}}.  The b1.pos argument is TRUE if the overall chemical mixture effect is positively related to the outcome; otherwise, it is negatively related to the outcome.

#' For each analysis, the outcome is regressed on the log of the observed values for each chemical and any other covariates Z, if they exist. This was accomplished using \code{\link[glm2]{glm2}}.  We summarized the results by recording the chemical name, estimating the log chemical effect and its standard error on the outcome, and using the Akaike Information Criterion (AIC) to indicate model fit.
#'
#' By looking at the output, one can decide whether the chemical mixture is positive or negative. Generally, if the sign of estimates is mainly positive, we would decide to make b1.pos in \code{\link{estimate.wqs}} to be TRUE. This is just one approach to determine the direction of this constraint. Alternatively, one can conduct a WQS analysis for the positively related chemicals and another WQS analysis for the negatively related chemicals.
#'
#' @inheritParams estimate.wqs
#'
#' @return  A data-frame from statistics of individual chemical analyses is returned: \describe{
#'  \item{chemical.name}{name of the component}
#'  \item{estimate}{the estimate of log chemical effect}
#'  \item{Std.Error}{the standard error of log chemical effect}
#'  \item{AIC}{Model Fit. See \code{stats::\link[stats]{AIC}}.}
#'  }
#'
#' @examples
#' # Binomial Example
#' data("simdata87")
#' analyze.individually(
#'   y = simdata87$y.scenario, X = simdata87$X.true, Z = simdata87$Z.sim,
#'   family = "binomial"
#' )
#' # The "Estimate" column contains the log_odds of each log of the
#' # chemical on the outcome. Most are positive, which indicates a possible
#' # positive relationship between mixture of chemicals and the outcome.
#' @import stats
#' @importFrom glm2 glm2
#' @export analyze.individually

analyze.individually <- function(y, X, Z = NULL,
                                 family = c("gaussian", "binomial", "poisson"), offset = NULL) {
  # Checks
  family <- tolower(family)
  family <- match.arg(family)
  if (!is(family, "character")) stop("family must be a character name of family")
  if (family == "poisson" & is.null(offset)) {
    warning("There is no offset specified. A count Poisson regression is performed.")
  }

  # Save Number of Chemicals and Loop for all chemicals.
  c <- ncol(X)
  df <- NA
  for (j in 1:c) {
    # Depending whether you have covariates or not, make a dataset
    temp <- if (is.null(Z)) {
      data.frame(y, chem = log(X[, j]))
    } else {
      data.frame(y, chem = log(X[, j]), Z)
    }

    # Regress on each chemical
    fit <- glm2::glm2(y ~ ., data = temp, family = family, offset = offset)
    # Keep vital statistics and save
    fit.keep <- data.frame(
      Chemical.Name = colnames(X)[j],
      Estimate = round(t((summary(fit)$coefficients["chem", 1])), 3),
      Std.Error = round(t((summary(fit)$coefficients["chem", 2])), 3),
      AIC = AIC(fit)
    )
    df <- rbind(df, fit.keep)
  }

  # Remove first row and reformat rownames
  df <- df[-1, ]
  rownames(df) <- NULL

  return(df)
}

# #Other Examples
# #BDL example
# analyze.individually( y = simdata87$y.scenario, X = simdata87$X.bdl, Z = simdata87$Z.sim,
#                       family = "binomial")
#
#
# #Gaussian Example
# data("WQSdata")
# analyze.individually( y = WQSdata$y, X = WQSdata[ , 1:9], family = "gaussian")
#
# #Default Example
# analyze.individually( y = WQSdata$y, X = WQSdata[ , 1:9])
#
# #Rate Example
