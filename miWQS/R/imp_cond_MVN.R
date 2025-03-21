#' Imputation of Missing Values given Observed Values

#' @description Finds the imputed value taken from conditional truncated multivariate normal and evaluates the density in a sample where missing values differs depending on individual.

#' @param X.bdl   A matrix of partially observed numeric data. In my context: chemical exposure data.
#' @param Z   A model.matrix of any covariates used to impute
#' @param Gamma.post Estimate of covariate matrix used
#' @param Sigma.post Estimated covariance matrix of chemicals X.bdl.
#' @inheritParams impute.multivariate.bayesian

#' @details
#' Given completely observed covariates Z, the X's are partially observed data. In a sample of n individuals, the number of missing values or below the detection limit (BDL) values varies dependent on the individual. Consider complete data W_i = (X.miss, X.obs). I am assuming that W_i is assumed to follow a multivariate normal distribution of mean Gamma.post * Z_i and covariance matrix Sigma.post.  I wish to impute X.miss,i | X.obs = x.obs,i ~ N (mu_i, Sigma.post), but the dimensionality changes depending on subject i. The missing values are known to be truncated between 0 and some upper bound DL.
#'
#'  Think individually (Row-wise, not column0wise)
#'
#' @return A list of 2 elements: \describe{
#' {log.imp.value2}{A list of length n. Each element contains the logarithmic imputed BDL values for each individual}
#' {log.dens}{A vector of length n that is logarithmic density of imputation model for each individual.}
#' }
#'
#' @importFrom condMVNorm condMVN
# Finds the parameters of conditional mulltivariate normal of BDL values given observed values for each subject.
#' @importFrom tmvmixnorm rtmvn
# Impute BDL values from log truncated multivariate normal
#' @importFrom tmvtnorm dtmvnorm
# Calculate the density of log truncated multivariate normal
#'
#'
#' @examples
#' \dontrun{
#' # The "rda" file below contains 10 observations from Multivariate
#' # Simulation Study design (10% BDL case, OR = 1.75, Dataset #80)
#' # and then conducting Multivariate Bayesian Regression. All code was run
#' # until missing values needed to be imputed. For the first iteration of
#' # the chain, the coefficient matrix, Gamma.post, and covariance matrix,
#' # Sigma.post, was run.
#' # Error in DL used -- need to fix.
#' # load("/Users/pablo/VCU Biostatistics/Computer Programs/R_functions/cond/condTruncMVN/data-raw/Fixing_Imputing_BDL_Bayesian_Multi_2019Jan7.rda")
#'
#'
#' # Coefficient matrix
#' print(Gamma.post)
#' # Covariance matrix
#' print(Sigma.post)
#'
#' # Impute BDL values given observed values: Imputed Values & Density
#' eg <- imp.cond.MVN(X = W10$X.bdl, Z,
#'   Gamma.post, Sigma.post,
#'   DL = W10$DL,
#'   mcmc.impute = list(init.impute = DL / 2, burn.impute = 0),
#'   verbose = TRUE
#' )
#'
#' # Transform into complete matrix.
#' log.imp.value <- na.omit(unlist(eg$log.imp.value.list))
#' vec.log.X[miss.vecX] <-  log.imp.values
#' # Convert to complete n x C matrix
#' X.compl <- exp(matrix(vec.log.X, nrow = n, ncol = C, byrow = TRUE))
#' }
#'
#' @noRd

imp.cond.MVN <- function(X.bdl, Z, Gamma.post, Sigma.post, DL,
                         mcmc.impute = list(init.impute = DL / 2, burn.impute = 0),
                         verbose = FALSE) {

  init.impute <- mcmc.impute$init.impute
  burn.impute <- mcmc.impute$burn.impute

  # List of individual's means.
  n <- nrow(X.bdl)
  C <- ncol(X.bdl)
  mu <- split(Z %*% Gamma.post, 1:n)
  mu <- lapply(mu, setNames, colnames(X.bdl))  # Add names to the mean so I know what is selected later.

  # Number of missing exposures in each individual.
  n0.i <- as.list(rowSums(is.na(X.bdl)))
  # Lists of which chemicals are BDL (or observed) => needed to use condMVN function
  which.BDL <- apply(X.bdl, 1, function(x) {which(is.na(x))})
  which.obs <- apply(X.bdl, 1, function(x) {which(!is.na(x))})

  ## What I am doing.
  if (verbose) {
    print("Data Summary")
    print(cbind(X.bdl, n0.i, mu, Sigma.post = "Matrix,4x4", which.BDL, which.obs))
    cat("Mean for Indvl 1-2: \n"); print(mu[1:2])
    cat("Sigma.post \n"); print(Sigma.post)
  }

  # Capture the output: Log imputed BDL values and log density for each individual
  log.imp.value2 <- setNames(vector(mode = "list", length = n), 1:n)
  # sapply(as.character(1:n), function(x) NULL)  #An empty named list.
  log.g <- setNames(vector(mode = "numeric", length = n),  1:n)
  for (i in 1:n) {
    # In cases where X.bdl is completely observed, there is no need to impute.
    if (n0.i[[i]] == 0) {
      log.imp.value2[[i]] <- NA
      log.g[i] <- NA
      next
    } else if (0 < n0.i[[i]] & n0.i[[i]] < C) {
      # (Note: All these steps are inefficiently captured in conditional Truncated Multivariate Normal Functions.)

      # Conditional MVN of Y=X.BDL|X=X.obs for subject i truncated between condZero and condDL. Initial values are also given to impute in truncated MVN normal.
      params <- c(
        condMVNorm::condMVN(mean = mu[[i]], sigma = Sigma.post, check.sigma = FALSE,
          dependent.ind = which.BDL[[i]],
          given.ind = which.obs[[i]], X.given = X.bdl[i, which.obs[[i]]]
        ),
        # Adjust truncation limits and initial values to reflect # of missing values in subject i.
        list(
          condZero =  rep(0, n0.i[[i]]),
          condDL = DL[which.BDL[[i]]],
          cond.init = init.impute[which.BDL[[i]]]
      ))
      # cat("i = ", i, ";\n"); print(params)

      # Check if Conditional Sigma is positive definite
      if (!isTRUE(matrixNormal::is.positive.definite(params$condVar))) {
        cat("#> The variance of BDL values given observed exposures in subject", i, "is not positive-definite. Huh? Investigate! \n")
        break()
      }
    } else if (n0.i[[i]] == C) {  # added 3/9/2020
      # If both values of X.bdl are missing, I cannot condition and the parameters are just a unconditional MVN.
      params <- list(
        condMean = mu[[i]],
        condVar  = Sigma.post,
        condZero = rep(0, n0.i[[i]]),
        condDL   = DL,
        cond.init = init.impute
      )
    }

    ### (3) Impute BDL values from log truncated multivariate normal using tmvmixnorm. Then, these BDL values are vectorized. Since the means are different by individual, the code operates on rows of mu where at least one BDL exist. For each of these means, BDL values are imputed from myrtmvnorm.
    log.imp.value2[[i]] <- log(tmvmixnorm::rtmvn(
      n = 1,
      Mean =  params$condMean,
      Sigma = params$condVar,
      lower = params$condZero,
      upper = params$condDL,
      int = params$cond.init,
      burn = burn.impute
    ))
    # Perhaps tmvmixnorm::rtmvn does not save names. Save column names so that it is easier to impute later.
    names(log.imp.value2[[i]]) <- colnames(params$condVar)

    if (anyNA(log.imp.value2[[i]])) {
      print(params)
      print(burn.impute)
      warning("NA values are detected in the imputed chain for subject ", i, "Investigate.",
        call. = FALSE)
      break()
    }

    ## Calculate the density of the logarithmic imputed values (X.miss)
    log.g[i] <-
      suppressWarnings(
        tmvtnorm::dtmvnorm(
          log.imp.value2[[i]],
          mean = params$condMean,
          sigma = params$condVar,
          lower = params$condZero,
          upper = params$condDL,
          log = TRUE,
          margin = NULL
        )
      )
  }

  return(
    list(log.imp.value.list = log.imp.value2,
      log.dens = log.g)
  )
}

## Need to combine imputed values with observed values.
