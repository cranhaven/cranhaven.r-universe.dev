#' Bootstrapping Imputation for Many Chemicals
#'
#' @family imputation
#' @keywords imputation
#'
#' @description
#' If many chemicals have values below the detection limit, this function creates an imputed dataset using a bootstrap procedure as described in Lubin et al. 2004. It repeatedly invokes \code{\link{impute.Lubin}}().
#'
#' @details
#'   Lubin et al. (2004) evaluate several imputation approaches and show that a multiple imputation procedure using bootstrapping creates unbiased estimates and nominal confidence intervals unless the proportion of missing data is extreme. The authors coded the multiple imputation procedure in a SAS macro that is currently available. We converted the SAS macro into R code.
#'
#' The \code{impute.Lubin}() function imputes a single chemical with missing values. The distribution for the interval-censored data \emph{chemcol} is assumed to be   lognormal and censored between 0 and \emph{DL}. After bootstrapping, the values BDL are imputed  using the inverse transform method. In other words, generate \eqn{u_i \sim Unif( 0.0001, dlcol)} and assign value \eqn{F^{-1}(u)} to \eqn{x_{i}} for \eqn{i = 1,...n_{0}} subjects with chemical values BDL.
#'
#' In order to impute a single chemical:
#' \enumerate{
#'   \item Input arguments.
#'   \item Obtain bootstrap samples.
#'   \item Generate weights vector.
#'   \item Use \code{\link[survival]{Surv}} function from Survival package to obtain survival object.
#'   \item Use \code{\link[survival]{survreg}} function from Survival package to obtain survival model.
#'   \item Sample from lognormal distribution with beta and variance from survival model as the parameters to obtain upper and lower bounds.
#'   \item Randomly generate value from uniform distribution between the previously obtained upper and lower bounds.
#'   \item Sample from the lognormal distribution to obtain the imputed data value associated with the above uniform value.
#' }
#'
#' \code{impute.boot()} repeatedly performs this procedure for all chemicals.
#'
#' @note
#' Note #1: Code was adapted from Erin E. Donahue's original translation of the SAS macro developed from the paper.
# Please contact Jay Lubin directly for the SAS macro.
#'
#' Note #2: No seed is set. Please set seed so the same bootstraps are selected.
#'
#' Note #3: If the length of the DL parameter is greater than the number of components, the smallest value is assumed to be a detection limit. A warning is printed to the screen.
#'
#  Note #4: For debugging, see impute.Lubin() and turn verbose = TRUE.
#'
#' @references
#' Lubin, J. H., Colt, J. S., Camann, D., Davis, S., Cerhan, J. R., Severson, R. K., … Hartge, P. (2004).
#' Epidemiologic Evaluation of Measurement Data in the Presence of Detection Limits. Environmental Health Perspectives,
#'  112(17), 1691–1696. https://doi.org/10.1289/ehp.7199

######## Parameters #################
#' @inheritParams impute.multivariate.bayesian
#' @return A list of: \describe{
#'      \item{X.imputed}{A number of subjects (n) x number of chemicals (c) x K array of imputed X values.}
#'      \item{bootstrap_index}{A n x K matrix of bootstrap indices selected for the imputation.}
#'      \item{indicator.miss}{A check; the sum of imputed  missing values above detection limit,
#'          which should be 0.}
#' }
#'
#' @examples
#' data("simdata87")
#' # Impute using one covariate.
#' l <- impute.boot(X = simdata87$X.bdl, DL = simdata87$DL, Z = simdata87$Z.sim[, 1],
#'   K = 2, verbose = TRUE
#' )
#' apply(l$X.imputed, 2:3, summary)
#' @export impute.boot

impute.boot <- function(X, DL, Z = NULL, K = 5L, verbose = FALSE) {
  # ptm <- proc.time()    # Start the clock!(data)

  ### Check for proper execution: T & n.burn are not needed so it could be anything.
  check <- check_imputation(X = X, DL = DL, Z = Z, K = K, T = 5, n.burn = 4, verbose)
  X  <- check$X
  DL <- check$DL

  ## Check Z: Create a model matrix.  Desired class is COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
  if (is.null(Z)) {
    # Make a matrix of 1's if no covariates.
    Z <- matrix(1, nrow = nrow(X), ncol = 1, dimnames = list(NULL, "Intercept"))
  } else {
    # Remove any Missing Covariates with warning. Uses helper check_covariates().
    l <- check_covariates(Z, X)
    X <- l$X   # X matrix without
    Z <- l$Z
  }
  K  <- check$K
  if (verbose) {
    cat("X \n"); print(summary(X))
    cat("DL \n"); print(DL)
    cat("Z \n"); print(summary(Z))
    cat("K =", K, "\n")
  }

  # Extract parameters
  n <- nrow(X)
  c <- ncol(X)
  chemical.name <- colnames(X)

  ## Run loop for each component in X
  results_Lubin <- array(dim = c(n, c, K),
    dimnames = list(NULL, chemical.name, paste0("Imp.", 1:K))
  )
  bootstrap_index <- array(dim = c(n, c, K),
    dimnames = list(NULL, chemical.name, paste0("Imp.", 1:K))
  )
  indicator.miss <- rep(NA, c)
  for (j in 1:c) {
    # cat("Imputing chemical", j, "\n")
    answer <-  impute.Lubin(chemcol = X[, j],  dlcol = DL[j], Z = Z, K = K)
    results_Lubin[, j, ] <- as.array(answer$imputed_values, dim = c(n, j, K))
    bootstrap_index[, j, ] <- array(answer$bootstrap_index, dim = c(n, 1, K))
    indicator.miss[j] <- answer$indicator.miss
  }

  ## Check if imputation is done correctly.
  total.miss <- sum(indicator.miss)
  if (total.miss == 0) {
    message(
      "#> Check: The total number of imputed values that are above the detection limit is ",
      sum(indicator.miss), "."
    )
  } else {
    print(indicator.miss)
    stop("#> Error: Incorrect imputation is performed; some imputed values are above the detection limit. Could some of `X` have complete data under a lower bound (`DL`)?",
      call. = FALSE)
  }

  ## Time in seconds.
  # time.impute <- (proc.time() - ptm)[3]

  return(list(
    X.imputed = results_Lubin,
    bootstrap_index = answer$bootstrap_index,
    indicator.miss = total.miss
  )
  )
}
