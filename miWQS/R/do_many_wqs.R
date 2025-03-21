#' Performing Many WQS Regressions
#'
#' @family wqs
#' @keywords wqs

#' @description
#' Second Stage of Multiple Imputation:  In order to analyze a complete imputed chemical array (\code{X.imputed}), _n_ subjects by _C_ chemicals by _K_ imputations) via weighted quantile sum regression, \code{do.many.wqs}() repeatedly performs the same WQS analysis on each imputed dataset. It repeatedly executes the \code{\link{estimate.wqs}}() function.
#'
#' @note
#'  Note #1: We only impute the missing values of the components, X. Any missing data in the outcome and covariates are removed and ignored.
#'
#'  Note #2: No seed is set in this function. Because bootstraps and splitting is random,  a seed should be set before every use.
#'
#'  Note #3: If there is one imputed dataset, use the \code{\link{estimate.wqs}} function as \code{do.many.wqs} is not necessary.

############### Arguments & Return ##########################
# Imputation parameter
#' @inheritParams estimate.wqs
#' @param X.imputed Array of complete components with n subjects and C components and K imputations. Must be complete.
#' @param ... Additional arguments passed to \code{estimate.wqs}, but the arguments y, X, Z, and place.bdls.Q1 have no effect.
# #'@param seed.boot Seed for reproducibility for WQS analysis. Default is NULL, which is a Random seed.
#
#' @return Returns a list with elements that consist of matrix and list versions of \code{estimate.wqs()} output: \itemize{
# Basic Information
#' \item call: the function call, processed by \pkg{rlist}.
#  \item time: Elapsed Time taken to run this analysis. Useful for simulation studies.
#' \item C:  the number of chemicals in mixture, number of columns in X.
#' \item n: the sample size.
# Results
#'    \item wqs.imputed.estimates: Array with rows = # of parameters, 2 columns = mean and standard deviation, and 3rd dimension = K.
#'     \item AIC: The overall fit of WQS models taken as the mean AIC and standard error across all imputation models. Saved as a character element.  Calling wqs.fit allows us to see all models.
# Checking WQS
#'     \item train.index: Observations that are selected to train the data in the last WQS model.
#'     \item q.train: Vector of quantiles used in training data from the last WQS model
#'     \item train.comparison: A list of data-frames that compares the training and validation dataset for all WQS models.
#'     \item initial: Matrix with K columns that contains the initial values used for each WQS analysis.
#  Datasets \itemize{
#'     \item wqs.train.estimates: Data-frame with rows = B. Summarizes statistics from nonlinear regression in the training datasets of all analyses: \describe{
#'      \item{beta1}{estimate using solnp}
#'      \item{beta1_glm, SE_beta1, test_stat, pvalue}{estimates of WQS parameter in model using glm2.}
#'      \item{convergence}{whether or not the samples have converged}
#'      \item{weight estimates}{estimates of weight for each bootstrap.}
#'      \item{imputed}{A number indicating the completed dataset used in WQS analysis.}
#'      }
#'      \item wqs.fit: A list (length = K) of glm2 objects of the WQS model fit to validation data. These are all the WQS estimates for all analyses. See \code{\link[glm2]{glm2}}.
#'      }

############### EXAMPLES ############################
#' @examples
#' data("simdata87")
#' # Create 2 multiple imputed datasets using bootstrapping, but only use first 2 chemicals.
#' set.seed(23234)
#' l <- impute.boot(
#'   X = simdata87$X.bdl[, 1:2], DL = simdata87$DL[1:2],
#'   Z = simdata87$Z.sim[, 1], K = 2
#' )
#' # Perform WQS regression on each imputed dataset
#' set.seed(50679)
#' bayes.wqs <- do.many.wqs(
#'   y = simdata87$y.scenario, X.imputed = l$X.imputed,
#'   Z = simdata87$Z.sim,
#'   B = 10, family = "binomial"
#' )
#' bayes.wqs$wqs.imputed.estimates
#'
#'
#'
#' # #' @importFrom scales ordinal
#' @export

do.many.wqs <- function(y, X.imputed, Z = NULL, ...) {
  # Check
  if (anyNA(X.imputed)) {
    stop("Some components in X.imputed are missing")
  }

  # Find number of parameters.
  n <- dim(X.imputed)[[1]]
  C <- dim(X.imputed)[[2]]
  K <- if (is(X.imputed, "array")) {  dim(X.imputed)[[3]] } else { 1 }
  Z.model <-
    if (is.null(Z)) {
      NULL
    } else {
      model.matrix(y ~ ., data = data.frame(y = y, Z))[, -1, drop = FALSE]
    }
  p <- if (is.null(Z)) { 0 } else { ncol(Z.model) }
  t.p <- C + 2 + p
  # Note: If data-frame Z is present, any categorical covariates are translated in estimate.wqs() to
  # a matrix with more columns than those in Z. These additional columns are dummy variables reflecting
  # the number of levels in each categorical covariate - 1. 50 is a high-end guess of number of
  # additional columns needed.
  cat("#> Sample size: ", n, "; ",
    "Number of chemicals: ", C, "; \n",
    "Number of completed datasets: ", K, "; ",
    "Number of covariates modeled:  ", p,
    "\n",  sep = ""
  )
  # "; Number of total parameters:", t.p , #all weights + intercept & WQS + covariates

  # Stage 2: Perform WQS on each dataset.
  AIC <- rep(NA, K) #**
  train.comparison <- vector("list", length = K)
  initial <- NA
  train.estimates <- NA
  fit.validate <- vector("list", length = K)
  WQS <- NA
  wqs.imputed.estimates <- array(NA,
    dim = c(t.p, 2, K),
    dimnames = list(
      NULL, c("Estimate", "Std.Error"),
      paste0("Imputed.", 1:K)
    )
  ) # same as X.imputed
  for (k in 1:K) {
    # cat(paste0("WQS is starting using the ", scales::ordinal(k), " imputed sample."), "\n")
    wqs.imputed <- suppressMessages(estimate.wqs(
      y = y, X = X.imputed[, , k], Z = Z,
      place.bdls.in.Q1 = FALSE, ...
    ))
    # if(verbose){print(wqs.imputed)}

    # Save things from WQS
    AIC[k] <- AIC(wqs.imputed$fit)      # Assessing Fit of Model
    train.comparison[[k]] <-            # comparision of train and valdiation sets.
      wqs.imputed$train.comparison
    initial <- cbind(initial,            # initial values
      wqs.imputed$initial
    )
    train.estimates <- rbind(train.estimates,
      data.frame(wqs.imputed$train.estimates,
        boot = 1:wqs.imputed$call$B,
        imputed = k)
    )
    fit.validate[[k]] <- wqs.imputed$fit    # Keep validation model glm2 object;
    WQS <- cbind(WQS, wqs.imputed$WQS)
    # boot.index.sample[ , , k] <- wqs.imputed$boot.index

    # Keep estimates for pooling
    a <- rbind(
      wqs.imputed$processed.weights, # weights
      summary(wqs.imputed$fit)$coefficients[, 1:2] # Intercept and WQS, covariates.
    )
    # Save the estimates, including rownames. Get rid of extra parameters not needed in Z.
    wqs.imputed.estimates[1:nrow(a), , k] <- a
    wqs.imputed.estimates <- wqs.imputed.estimates[1:nrow(a), , , drop = FALSE] # Get rid of the extras.
    dimnames(wqs.imputed.estimates)[[1]] <- dimnames(a)[[1]]
  } # end K draw loop

  # Format Initial Dataset
  initial <- initial[, -1, drop = FALSE]
  colnames(initial) <- paste0("Imputed.", 1:K)

  # Format train.estimates
  train.estimates <- train.estimates[-1, ] # remove NA row when I initialize it.
  rownames(train.estimates) <- NULL

  # Format WQS
  WQS <- WQS[, -1]
  dimnames(WQS) <- list(NULL, paste0("Imputed.", 1:K))

  # Save the call:
  D <- formals(do.many.wqs) # Make defaults of function as a list.
  A <- as.list(match.call()) # Make the call saved as a list.
  U <- rlist::list.merge(D, A) # Want a list of defaults, but the names are overriden by the actual functional call.

  #----------------------------------------------------------------------------------------------------------
  out <- list(
    call = U, C = wqs.imputed$C, n = wqs.imputed$n,
    # Results
    wqs.imputed.estimates = wqs.imputed.estimates, # All of the WQS estimates for all the imputed datasets.
    AIC = AIC,

    # Checking WQS
    train.index = wqs.imputed$train.index, # Training indeces saved from last imputed WQS analysis.
    q.train = wqs.imputed$q.train, # 2.2 q.train	matrix of quantiles saved from last imputed WQS analysis.
    # q.valid
    # boot.index
    train.comparison = train.comparison,
    initial = initial,

    # Datasets
    WQS = WQS,
    wqs.train.estimates = train.estimates,
    wqs.fit = fit.validate
  )
  return(out)
}
