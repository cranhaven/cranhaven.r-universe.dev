#####################################################################################################
##  MULTIVARIATE REGRESSION MI
##  Date Created: June 14, 2018
##  Last Updated: March 12, 2019

## Purpose of Document:

## Thinning does not seem like it matters here as I will just be doing random draws from posterior predictive distribution.
##
## Updates
# 12/31/20: Removed mcmc.impute and numCores arguments from `impute.multivariate.bayesian function`. Set to defaults within the function. They were placed in sample.mregress.impute.

####################################################################################################
################################   (I) Preamble  ###################################################
#' @title Multivariate Bayesian Imputation
#' @family imputation
#' @keywords imputation Bayesian

#' @description
#' Given lognormal interval-censored chemical concentrations between zero and different detection limits \emph{DL}, the chemical concentrations are modelled using Bayesian multivariate regression. Drawing from the posterior predictive density of the BDL chemical concentrations given the observed ones yields multiple (or K) imputed datasets. These datasets are then used in WQS regression.

## Arguments & Values -------------------------------------------------------------------------------
## Data
#' @param X A numeric vector, matrix, or data-frame of chemical concentration levels with n subjects and C chemicals to be imputed. Missing values are indicated by NA's.  Ideally, a numeric matrix.
#' @param DL The detection limit for each chemical as a numeric vector with length equal to C chemicals. Vector must be complete (no NA's); any chemical that has a missing detection limit is not imputed. If DL is a data-frame or matrix with 1 row or 1 column, it is forced as a numeric vector.
#' @param Z Any covariates used in imputing the chemical concentrations.  Ideally, a numeric matrix; however, Z can be a factor, vector, or data-frame. Assumed to be complete; observations with missing covariate variables are ignored in the imputation, with a warning printed. If none, enter NULL.

## Priors
#' @param prior.coeff.mean The prior mean of number of covariates (p) x C coefficient matrix. The default, entered as NULL, will be a matrix of 1's, given by \code{\link[matrixNormal]{special.matrix}}.
#' @param prior.cov.mean  The prior mean of covariance matrix. The default, entered as NULL, is an identity matrix with size equal to the number of chemicals, given by \code{\link[matrixNormal]{special.matrix}}.

## MCMC Chain
#' @param T Number of total iterations for the Gibbs Sampler. Default: 1000L.
#' @param n.burn The burn-in, which is the number of initial iterations to be discarded. Generally, the burn-in can be quite large as the imputed chemical matrices, X.imputed, are formed from the end of the chain -- the lowest state used is \eqn{T - 10*K}. Default: 1L (no burn-in).
#' @param initial  An optional two-item list that consists of initial values for the log imputed BDL values vectorized by subject in the Gibbs Sampler. The list contains two elements, one for each chain in the Gibbs Sampler. Each element is a vector of length n0C containing the log imputed BDL values vectorized by subject, (n0 is total # of missing values). If unknown for each chain, enter NA, and the initial values are automatically generated.

## Generating the posterior predictive density of missing values given observed value--Passed to sample.mregress.impute
# #' @param mcmc.impute
# #' @param numCores

## Number of Datasets to produce
#' @param K A natural number of imputed datasets to generate. Default: 5L.

## Misc (verbose)
#' @inheritParams estimate.wqs
#'
#' @return A list that consists of the following elements:
#' \describe{\item{call}{A list of arguments used in this function.}}
#'      Section - Imputed Dataset (from accessory draw.multi.imputed.samples())   \describe{
#'        \item{X.imputed}{An array of n subjects x C chemicals x K imputed sets on the normal scale. The main result and purpose of the function.}
#'        }
#'     Section - Convergence \describe{
#'       \item{convgd.table}{A data-frame summarizing convergence with C rows and columns of the Gelman-Rubin statistic and whether the point estimate is less than 1.2. Also printed to the screen.}
#'       \item{auto.corr}{Summary of autocorrelations of missing data, which are used to justify states taken as imputed datasets. Also printed to screen.}
#'       \item{last.states}{A list of the last (Tth) states of the imputed values saved to be used for initial values with the first element being from chain1 and second element from chain2.}
#'     }
#'     Section - convgd.surrogates. Surrogates used to check for convergence saved as mcmc.list objects. Returning in case trace plots, autocorrelation plots, etc. wants to be calculated. \describe{
#'         \item{eigen.Gamma}{An mcmc.list object of the eigenvalues from the p x p Gamma *Gamma^T matrix from the two BURNED chains.  The eigenvalues were used as surrogates for the convergence of the coefficient matrix, Gamma.}
#'         \item{eigen.Sigma}{An mcmc.list object of the eigenvalues for covariance matrix Sigma from two BURNED chains. The eigenvalues were used as surrogates for the convergence of Sigma.}
#'         \item{vec.log.X.imputed}{An mcmc.list object of the vectorized log imputed chemical values from the two BURNED chains.}
#'       }
#'     Section - Checking Imputation Procedure \describe{
#'       \item{indicator.miss}{A check; a sum of indicator variables where the number of imputed missing values > detection limit. Should be 0. Printed to screen.}
# #'       \item{rtmvnorm.na}{Vector. First number: Number of times a missing value is returned when imputing BDL values from rtmvnorm(). Zero or detection limit of a truncated distribution is substituted to whatever is closer. (Usually zero, the lower limit). Second number: Total Times Gibbs Sampler is run. Printed to screen.}
#'     }

## Details  ------------------------------------------------------------------------------------
#' @section Introduction:
#'
#' We wish to assess the association of the mixture *X* and an outcome *y* while accounting for other covariates *Z*. However, the components in *X* are interval-censored between zero and different detection limits *DL*. The multivariate Bayesian imputation method in the MI-WQS framework (MBMI) jointly imputes the chemical mixture *K* times by taking full advantage of the chemical mixture data.
#'
#' The logarithmic chemical concentrations *X* are assumed to follow a matrix normal distribution, which is an extension of the multivariate normal:
#'   \deqn{  \log(X)|Z \sim MatNorm( \mu_{i} = z'_{i} \Gamma , \Sigma) , i = 1, ... n }
#'  (Iranmanesh et al., 2010).  Like other imputation methods in miWQS, we wish to find the posterior predictive density of log(X_miss)|log(X_obs). In \code{impute.multivariate.bayesian()}, the missing chemical concentrations are imputed using estimates from a Bayesian Multivariate regression.
#'
#' @section Step 1 - Generate a posterior sample:
#'
#' The accessory \code{sample.mregress.impute()} function generate a posterior samples using the data augmentation technique. The conjugate priors for a multivariate regression are multivariate extensions of those in the univariate linear regression case. Given complete data, the conjugate priors for the coefficient matrix is another matrix normal with mean \code{prior.coeff.mean}, individual variance matrix \eqn{Z'Z}, and chemical variance matrix \eqn{\Sigma}.  The prior distribution for the covariance matrix is the inverse-Wishart distribution. In this function, we used a matrix of 1's as the default prior coefficient mean of the matrix normal.  The prior parameters chosen for the covariance matrix are vague with the degree of freedoms equal to the number of components, and the mean matrix, by default, is an identity of ones. Instead of attempting to impute a n x C matrix X, we vectorized the logarithmic concentrations by individual, such as:
#' > vec(t(X))             \cr
#'  ...           ...      \cr
#'  dieldrin.18  NA        \cr
#'  pcb_180.18  -0.2514225 \cr
#'  pcb_180.19  -0.2929334 \cr
#'  dieldrin.20 -4.4849838 \cr
#'  pcb_180.20  -1.0441849 \cr
#'  ... \cr
#'
#'  The initial missing values were a sample taken from log(uniform(0,DL_j )). If the initial values are set by the user, the initial log imputed values, which is vectorized by subject, has to be n0C x T.
#'
#'  For each step in the data augmentation, \enumerate{
#'    \item Calculate the MLE, the sample covariance matrix, and the posterior matrix of inverse-Wishart.
#'    \item Simulate the covariance matrix using inverse Wishart (MCMCpack::riwish()). See \code{\link[MCMCpack]{InvWishart}}.
#'    \item Simulate the coefficient matrix using the matrix normal. See \code{\link[matrixNormal]{matrixNormal_Distribution}}.
#'    \item Impute the vectorized missing log concentrations BDL for each individual from a multivariate normal using current parameter estimates truncated between zero and the detection limits using
#' }
#'
#' Note: The exact MCMC chains are not returned to save computer space.
#'
#   The sample.mregress.impute() also returns the last States of Coefficient and Covariance Matrix are saved to use as initial values in a second chain or adjust initial values. \itemize{
#   \item Gamma.post.last  The last (Tth) state of the coefficient matrix. Used as initial values in a second chain
#   \item Sigma.post.last  The last (Tth) state of the covariance matrix. Used as initial values in a second chain
# }

#' @section Step 2 - Assess convergence:
#'
#' To save space, the eigenvalues of the c x c matrix Gamma^T*Gamma and c x c covariance matrix Sigma are saved as surrogates to check for convergence in a Markov Chain. \describe{
#'   \item{eigen.Gamma.post}{A coda object of eigenvalues taken from a p X C posterior coefficient matrix converted into a square matrix C x C matrix (Gamma^T*Gamma). The  eigenvalues as surrogates to check for convergence.}
#'   \item{eigen.Sigma.post}{A coda object of eigenvalues for covariance matrix (C X C) used as surrogates to check for convergence. The covariance matrix is already square so no conversion is needed.}
#'   \item{vec.log.X.imputed}{A coda object of the n0 missing values. \cr
#'   Example: The following chemicals shown are those that are missing. \cr
#'           [,1]        [,2]  \cr
#'   dieldrin.18 -0.7573897 -0.60540942   ...    \cr
#'  pcb_180.18  -0.2514225 -1.18717066 \cr
#'   pcb_180.19  -0.2929334 -0.01894021 \cr
#'   dieldrin.20 -4.4849838 -0.78641994  \cr
#'   pcb_180.20  -1.0441849 -0.1349498 \cr
#'  ... \cr
#'  }
#'  }
#'
#  For $p$ covariates and $C$ chemicals, the total number of MCMC parameters assessed is \eqn{pC+C^2+sum(n_0j)}, where \eqn{n_{0j}} refers to the number of missing values in jth chemical. However, we used eigenvalues to assess the matrices, so we assess the convergence of \eqn{2C+sum(n_0j)} parameters. In the toy example shown, there are C = 2 chemicals with 100 each missing and no covariates (only intercept is present). So, the total number of MCMC parameters is  2+2^2+2*100 = 206 MCMC parameters.
#'
#' The accessory \code{converge.multi.chain()} function assesses convergence on matrices using the Brook-Gelman’s multivariate potential scale reduction factor (MPSRF). The \code{\link[coda]{gelman.diag}} function calculates the MPSRF on eigenvalues and the vectorized imputed values chain. If the MPSRF is less than 1.24, the stationary distribution of the Markov chains was assumed to occur. The results in \code{convgd.table} element are printed to the screen. If at least one chain fails to converge, a warning is printed to occur; in this case, it is suggested to increase \code{T}.
#'
#' @section Step 3 - Processing MCMC chains:
#'
#' The MCMC chains have already been burned using argument \code{n.burn} when generated.
# #' The MCMC chains may be burned with argument n.burn, but the sets of imputed values are taken at the end of the chain. Generally, n.burn has no effect on the imputed values BDL taken to # form the imputed chemical matrices, X.imputed.
#'
#' @section Step 4 - Making imputed value array:
#'
#'  The accessory \code{draw.multi.imputed.samples()} function forms X.imputed using the posterior predictive distribution of log.miss|log.obs. Using the first MCMC chain of the vectorized log imputed chemical values (\code{vec.log.X.imputed}) with length (\code{T}), the following states are selected:
#'  \deqn{ t_k=T-(k-1)*10   for k=1,2,…K imputations }
#' The "10" may be justified using autocorrelation summaries, which are printed & returned.

## Package Imports and Export --------------------------------------------------

# MCMC analysis
#' @import coda
# Priors used in MCMC
#' @import matrixNormal
# convergence checking.
#' @importFrom tidyr replace_na
# Used to conducted two MCMC chains at once
#' @import parallel

## Used in MCMC chain in sample.mregress.impute()
# Covariance (Sigma) inverse wishart distribution generation
#' @importFrom MCMCpack riwish
# Calculating acceptance probability to decide which trunc.multv. normal to use
#' @importFrom mvtnorm pmvnorm
# Imputing missing values
#' @importFrom tmvmixnorm rtmvn

#' @export impute.multivariate.bayesian

#' @note
#' No seed is set in this function. Because bootstraps and data augmentation are random, a seed should be set before every use.

# Example ---------------------------------------------------------------------------------------
#' @examples
#' \dontrun{
#' #Example takes too long.
#' system.time({
#'   set.seed(2345)
#'   l  <- impute.multivariate.bayesian(
#'     X =  simdata87$X.bdl[, c(1, 14)], DL = simdata87$DL[c(1, 14)],
#'     Z =  NULL, T = 200, n.burn = 10, K = 2
#'   )
#' })
#' }


################################  (II) Main Function: Impute Bayesian MI ######################

impute.multivariate.bayesian <- function(
  X, DL, Z = NULL, K = 5L,
  prior.coeff.mean = NULL, prior.cov.mean = NULL,
  T = 250L, n.burn = 50L, initial = list(NA, NA),
  # mcmc.impute = list(init.impute = 0, burn.impute = 80), numCores = 2L,
  verbose = FALSE
) {

  ## Check for proper execution ---------------------------------------
  l <- check_multi(X = X, DL = DL, Z = Z, K = K, prior.coeff.mean = prior.coeff.mean, prior.cov.mean = prior.cov.mean, T = T, n.burn = n.burn, initial = initial, numCores = 2, mcmc.impute = list(init.impute = 0, burn.impute = 80), verbose = verbose)
  X <- l$X
  DL <- l$DL
  Z <- l$Z  # Z includes an intercept
  T <- l$T
  n.burn <- l$n.burn
  prior.cov.mean <- l$prior.cov.mean
  initial <- l$initial
  # numCores <- l$numCores
  mcmc.impute <- list(init.impute = 0, burn.impute = 80)
  verbose <- l$verbose
  # Z
  #   Intercept   ch_age ch_sexFemale
  #  20         1 3.943124            1
  #  21         1 2.946630            1
  #  22         1 4.298811            0

  ### General Parameters
  C <- ncol(X)     # Number of chemicals/components
  n <- nrow(X)     # Sample Size
  p <- ncol(Z)     # Number of covariates
  n0 <- colSums(is.na(X))  # Vector of number missing values
  chemical.name <- colnames(X)
  mcmc.length <- T - n.burn
  if (verbose) {
    cat("General Parameters: ====================== \n")
    cat("Number of chemicals/components (C): ", C, "\n")
    cat("Sample Size (n): ", n, "\n")
    cat("Number of covariates (p): ", p, "\n")
    cat("Vector of number missing values(n0): ", n0, "\n")
    cat("Chemical Names (colnames(X)): ", chemical.name, "\n")
    cat("Length of Burned Chain (mcmc.length): ", mcmc.length, "\n")
  }

  ### Find the missing values of vectorized log concentration vec.log.X by individual: ------------
  ### like pcb170:18 dieldrin:18 pcb180:19 ...
  vec.log.X <-  matrixNormal::vec(t(log(X)))           # nC vector.
  miss.vecX <- which(is.na(vec.log.X))    # which subjects are BDL in vectorized log concentrations
  if (is.null(prior.coeff.mean)) {
    prior.coeff.mean <- matrixNormal::J(p, C)
    dimnames(prior.coeff.mean) <- list(colnames(Z), colnames(X))
  }
  if (is.null(prior.cov.mean)) {
    prior.cov.mean   <- matrixNormal::I(C)
    dimnames(prior.cov.mean) <- list(dimnames(X)[[2]], dimnames(X)[[2]])
  }
  if (verbose) {
    cat("First 20 missing values in X (vec.log.X[miss.vecX]): \n")
    print(vec.log.X[miss.vecX[1:20]])
    cat("Prior Coeff Mean Used (p x C): \n"); print(prior.coeff.mean)
    cat("Prior Covariance Mean Used (C x C): \n"); print(prior.cov.mean)
  }

  ### (1a) Find posterior predictive distribution of the vectorized log imputed chemical concentrations given observed. ===================

  ### Initial Values
  ### In each chain: For coefficient matrix Gamma and covariance matrix Sigma are not needed.
  ### For vectorized log imputed chemical concentrations stacked by individual.
  ### (A) We generate initial log.miss.values as a list by chemical.
  ### (B) we replace missing values in missing chemical concentration log(X) with  a list of initial.log.miss.values, by filling-in imputed values in right spot in n x C matrix.
  ### (C) Save initial values for vectorized missing log concentrations by individual.


  if (is.null(initial)) {
    initial <- list(vec.uniform = NA, vec.vague = NA)
  }

  ## Initial Values for First Chain
   if (anyNA(initial[[1]])) {
    initial.log.miss.values <- generate.log.xmiss.prior(
      x.miss.type = "uniform",
      n0 = n0,
      DL = DL
    )
    logX.initial <-  replace_na(log(X), initial.log.miss.values)
    vec.uniform <- matrixNormal::vec(t(logX.initial))[miss.vecX]

  } else if (length(initial[[1]]) != sum(n0)) {
    stop("The initial values of vectorized log imputed chemical concentrations
             stacked by individual does not have length ", sum(n0), " in chain 1.",
      call. = FALSE
    )

  } else {
    # Initial value is user-supplied.
    vec.uniform <- initial[[1]]
  }

  ## Initial Values for Second Chain
  if (anyNA(initial[[2]])) {
    logX.initial <- replace_na(
      log(X),
      generate.log.xmiss.prior(x.miss.type = "vague", n0 = n0, DL = DL)
    )
    vec.vague <-  matrixNormal::vec(t(logX.initial))[miss.vecX]
  } else if (length(initial[[2]]) != sum(n0)) {
    stop("The initial values of vectorized log imputed chemical concentrations
                 stacked by individual does not have length ", sum(n0),  " in chain 2.",
      call. = FALSE
    )
  } else {  # Initial value is user-supplied.
    vec.vague <- initial[[2]]
  }

  ## Print top initial values for debug
  if (verbose) {
    cat("First 23 values of initial values for Gibbs Sampler ...")
    print(list(chain1 = vec.uniform[1:23], chain2 = vec.vague[1:23]))
  }

  ### Run BOTH MCMC Chains TOGETHER -- 10/18/19
  cat("#> Starting MCMC Data Augmentation Algorithm... \n")
  g <- function(vec.xmiss.initial) {
    sample.mregress.impute(
      X = X, DL = DL, Z = Z,
      prior.coeff.mean = prior.coeff.mean,
      prior.cov.mean = prior.cov.mean,
      n.burn = n.burn, mcmc.length = mcmc.length,
      mcmc.impute = mcmc.impute,
      vec.xmiss.initial = vec.xmiss.initial,
      vec.log.X = vec.log.X
    )
  }

  # 3/9/2020: Add tryCatch, so if warnings come up in parallel processing I can see the issue
  # tryCatch({
  mcmc.chains <- parallel::mclapply(
    list(chain1 = vec.uniform, chain2 = vec.vague),
    g,
    mc.preschedule = TRUE,
    mc.cores = 2,  # default is 2 cores
    mc.set.seed = FALSE,   # Same seed in both instances are created.
    mc.silent = verbose,
    mc.cleanup = TRUE  # Debugging options: TURN TO TRUE when done.
  )
  #   }, warning = function(w){
  #       print(w)
  #      # print(mcmc.chains)
  #       #If a warning exists, return an empty list of sample.mregress.impute.
  #       l <- list(eigen.Gamma =  NA,
  #              eigen.Sigma = NA,
  #              vec.log.X.imputed = NA ,
  #              Gamma.post.last = NA,
  #              Sigma.post.last = NA,
  #              #Checks ...
  #              indicator.miss = NA,
  #              vec.xmiss.initial = NA
  #         )
  #     #  return(list(chain1 = l, chain2 = l))
  # }
  # )

  ## Posterior Quantaties
  # chain1$Gamma.post        #List of p x C matrices with T elements
  # chain1$Sigma.post        #List of C x C matrices with T elements.
  # chain1$vec.log.X.imputed # nC x (T+1) matrix.

  ### (1b) Burn-in from each chain already removed. Keep first chain.
  ### (1c) Check for convergence ...  ----------------------------
  cat("#> Brook-Gelman MPSRF is assessing convergence using eigenvalues as surrogates ... \n ")
  multi.converged <- converge.multi.chain(mcmc.chains)
  chain1 <- mcmc.chains$chain1

  ### (1d) Form K imputed complete datasets using the posterior predictive distribution of log.miss | log.obs. Starting from a burned Markov chain, K missing values are taken every 10. The missing values from these states replace the NAs in X. -------------------------------
  X.imputed <- draw.multi.imputed.samples(
    vec.log.X, chain1$vec.log.X.imputed,
    K = K, mcmc.length = mcmc.length, n.burn = n.burn,
    n = n, C = C, chemical.name = chemical.name,
    verbose = verbose
  )

  ### Justification of Every Tenth State
  cat("#> Justification of MCMC States Taken Every Tenth State: Autocorrelation Summary of log(X_miss). \n")
  if (anyNA(chain1$vec.log.X.imputed)) {
    summ.corr <- NA
  } else {
    summ.corr <- summary(t(
      coda::autocorr.diag(chain1$vec.log.X.imputed,
        lags = c(1, 10, 25),
        relative = FALSE
      )
    ))
    print(summ.corr[c(1, 3, 6), , drop = FALSE])
  }

  ### (1e) Check ----------------------------------------------------
  cat("#> Check: Indicator of # of missing values above detection limit for both chains \n")
  print(mcmc.chains$chain1$indicator.miss + mcmc.chains$chain2$indicator.miss)

  ### (1f) Function Call and Output to Return -----------------------
  D <- formals(impute.multivariate.bayesian)   # Make defaults of function as a list
  A <- as.list(match.call())     # Make the call saved as a list.
  # I want a list of defaults, but the names are overriden by the actual functional call.
  U <- rlist::list.merge(D, A)

  return(
    list(call = U,
      # Main Result
      X.imputed = X.imputed,
      ## MCMC Chains and Convergence
      convgd.table = multi.converged$convgd.table,
      # summary of autocorrelations of missing data -- used to justify states taken as imputed datasets.
      auto.corr.summ  = summ.corr,
      ## Last states saved to be used for initial values.
      last.states = list(
      #  Gamma.post.last =  chain1$Gamma.post.last,
      #  Sigma.post.last =  chain1$Sigma.post.last,
        vec.log.X.imputed.last.chain1 = chain1$vec.log.X.imputed[mcmc.length, ],  # 11/25: added .chain1
        vec.log.X.imputed.last.chain2 =   mcmc.chains$chain2$vec.log.X.imputed[mcmc.length, ]   # 11/25: added this line.
      ),
      ## Surrogates used to check for convergence saved as coda::mcmc.lists....returning in case trace plots, autocorrelation plots, etc. wants to be calculated. FROM BOTH CHAINS.
      #    convgd.surrogates = list(
      eigen.Gamma = multi.converged$eigen.Gamma,
      eigen.Sigma = multi.converged$eigen.Sigma,
      vec.log.X.imputed = multi.converged$vec.log.X.imputed,
      #    ),
      ## Checking Imputation # Number of imputed > DL
      indicator.miss = mcmc.chains$chain1$indicator.miss + mcmc.chains$chain2$indicator.miss
    )
  )
}

### (D) Check initial values
# verbose <- FALSE
# if(verbose){
#   cat("#> str(initial.log.miss.values) \n"); str(initial.log.miss.values);
#   cat("#> Summary of vec.log.X (Should have NA's) \n");  print(summary( vec.log.X))
#   cat("#> Summary of vec.uniform (No NA's initial values) \n"); print(summary(vec.uniform))
# }


################################  (III) General Accessory Functions for BMI  ####################
## Check for execution ---------------------

check_multi  <- function(X, DL, Z, K, prior.coeff.mean, prior.cov.mean, T, n.burn, initial, numCores, mcmc.impute, verbose) {
  # if(verbose){
  #   cat("Before Modification: \n")
  #   str(list(X = X, DL = DL, Z = Z, K = K, prior.coeff.mean = prior.coeff.mean, prior.cov.mean = prior.cov.mean, T = T, n.burn = n.burn, initial = initial, mcmc.impute = mcmc.impute, verbose = verbose))
  #   }

  check <- check_imputation(X = X, DL = DL, Z = Z, K = K, T = T, n.burn = n.burn, verbose)

  ### Check Data
  X  <- check$X
  DL <- check$DL

  ## Check Z: Create a model matrix.
  ## Desired class is COMPLETE numeric matrix (with columns being continuous or 0/1 dummy variables for categorical)
  if (is.null(Z)) {
    # Make a matrix of 1's if no covariates.
    Z <- matrix(1, nrow = nrow(X), ncol = 1, dimnames = list(NULL, "Intercept"))
  } else {
    # `check_covariates()` convert dataframe/factor Z into a matrix.
    l <- check_covariates(Z, X)
    Z <- cbind(1, l$Z)             # ADDED 10/5
    colnames(Z)[1] <- "Intercept"   # ADDED 10/5
    X <- l$X   # X matrix excluding missing covariates.
  }

  ## Check K & Data
  K  <- check$K
  if (verbose) {
    cat("X \n"); print(summary(X))
    cat("DL \n"); print(DL)
    cat("Z \n"); print(summary(Z)); cat("Number of Subjects Analyzed: ", nrow(Z), "\n")
    cat("K =", K, "\n")
  }

  ### Check Prior Parameters
  # Check supplied prior means.
  if (!is.null(prior.coeff.mean)) {
    if (nrow(prior.coeff.mean) !=  ncol(Z)) {
      warning(" #> `prior.coeff.mean` matrix must equal the # of coefficients in the model. \n",
        " #> Did you forget to set prior means for the intercept? \n",
        " #> Setting `prior.coeff.mean` to be the default.",
        call. = FALSE
      )
      prior.coeff.mean <- NULL
    } else if (ncol(prior.coeff.mean) != ncol(X)) {
      warning("#> `prior.coeff.mean` must equal the # of chemicals in the model. \n Setting prior.coeff.mean` to be the default.", call. = FALSE)
      prior.coeff.mean <- NULL
    }
  }

  # Check supplied covariance means
  if (!is.null(prior.cov.mean)) {
    if (!isTRUE(matrixNormal::is.positive.definite(prior.cov.mean))) {
      warning("`prior.cov.mean` is not positive definite. Setting `prior.cov.mean` to default.")
      prior.cov.mean <- NULL
    } else if (ncol(prior.cov.mean) !=  ncol(X)) {
      warning("`prior.cov.mean` must equal the # of chemicals in the model. \n Setting prior.coeff.mean` to default.", call. = FALSE)
      prior.cov.mean <- NULL
    }
  }

  ### Check MCMC Parameters
  T  <- check$T

  ## Check numCores
  numCores <- check_constants(numCores)
  if (numCores < 2) stop("Number of cores used must be at least 2.", call. = FALSE)

  ## mcmc.impute
  stopifnot(is.list(mcmc.impute))
  if (length(mcmc.impute) != 2  &
    sum(names(mcmc.impute) == c("init.impute", "burn.impute")) != 2) {
    stop("mcmc.impute must be a named list of two elements:
      'init.impute' and 'burn.impute'.", call. = FALSE)
  }
  if (sum(sapply(mcmc.impute, is.numeric)) != 2)
    stop("The elements of mcmc.impute must be numeric", call. = FALSE)
  if (length(mcmc.impute$init.impute) != ncol(X)) {
    mcmc.impute$init.impute <- rep(mcmc.impute$init.impute, ncol(X))
  }
  if (length(mcmc.impute$burn.impute) > 1) {
    warning("The chain length will be first element.")
    mcmc.impute$burn.impute <- mcmc.impute$burn.impute
  }

  # Final list of parameters
  l <- list(X  = X,
    DL = DL,
    Z  = Z,
    prior.coeff.mean = prior.coeff.mean,
    prior.cov.mean = prior.cov.mean,
    T = T,
    n.burn = n.burn,
    initial = initial,
    numCores = numCores,
    mcmc.impute = mcmc.impute,
    verbose = verbose)

  #  if(verbose) cat("After Modification: \n"); str(l)

  return(l)
}

## (1a) sample.mregress.impute  -----------------------------------------------------------------------

sample.mregress.impute  <- function(
  X, DL, Z, prior.coeff.mean, prior.cov.mean, n.burn,
  mcmc.length, mcmc.impute, vec.xmiss.initial,
  vec.log.X, verbose = FALSE # additional objects needed passed from impute.multivariate()
){

  # Print warnings as they occur.
  options(warn = 1)

  ### Recalculate General Parameters
  C <- ncol(X)     # Number of chemicals/components
  n <- nrow(X)     # Sample size
  p <- ncol(Z)     # Number of covariates
  n0 <- colSums(is.na(X))  # Vector of number missing values
  chemical.name <- colnames(X)
  miss.vecX <- which(is.na(vec.log.X))    # which subjects are BDL in vectorized log

  ## Truncated Multivariate Normal Chain Information
  # init.impute <-  mcmc.impute$init.impute
  # burn.impute <- mcmc.impute$burn.impute

  ### Set-up the sampler
  eigen.Gamma.post <- matrix(NA, nrow = mcmc.length,  ncol = p,
    dimnames = list(NULL, paste0(colnames(Z), "_eval"))
  )
  eigen.Sigma.post <- matrix(NA, nrow = mcmc.length,  ncol = C,
    dimnames = list(NULL, paste0(colnames(X), "_eval"))
  )
  vec.log.X.imputed <- array(NA, dim = c(mcmc.length, n * C),
    dimnames = list(NULL, paste(chemical.name, rep(1:n, each = C), sep = "."))
  )[, miss.vecX]  # Only keep those observations with missing data.

  ### Initial values
  log.imp.values  <- vec.xmiss.initial
  indicator.miss  <- 0
  ### Run the sampler but discard burn-in. Also only save eigenvalues to save space.
  for (t in (1 - n.burn):mcmc.length) {
    # cat(">>> state: ", t, "\n")

    ### (0a) Recreate the log complete data matrix using the imputed log vectorized quantities
    vec.log.X[miss.vecX] <-  log.imp.values        # Make complete vectorized log.data qi.
    log.X <-  matrix(vec.log.X, nrow = n, ncol = C, byrow = TRUE, dimnames = dimnames(X))  # Convert to complete n x C matrix

    ### (0b) Given complete data, calculate
    Gammahat <- solve(t(Z) %*% Z) %*% t(Z) %*% log.X       # MLE
    S <- t(log.X  - Z %*% Gammahat) %*% (log.X - Z %*% Gammahat)  # MSE, estimated covariance between chemicals
    Sstar <- prior.cov.mean + S +
      t(Gammahat - prior.coeff.mean) %*% solve(2 * t(Z) %*% Z) %*% (Gammahat - prior.coeff.mean)
    if (verbose & t == mcmc.length) {
      cat("Sample Statistics for State", t, ": \n")
      cat("\n Gammahat: \n"); print(Gammahat)
      cat("\n S (MSE) \n"); print(S)
      cat("\n Sstar \n"); print(Sstar)
    }

    # If S is not positive-definite, I want to catch the chain. It will give an error anyway in riwish.
    if (!isTRUE(matrixNormal::is.positive.definite(Sstar))) {
      cat("#> In chain ", t, "Sstar is not positive-definite. \n")
      break()
    }

    ### (1) Simulate the variance given complete data
    Sigma.post <- MCMCpack::riwish(v = C + n, Sstar)

    ### (2) Simulate the coefficient matrix Gamma given variance and complete data.
    Gamma.post <- matrixNormal::rmatnorm(
      M = (prior.coeff.mean + Gammahat) / 2,
      U = solve(2 * t(Z) %*% Z),
      V = Sigma.post,
      method = "eigen"
    )

    ### (3) Impute BDL values from log conditional truncated multivariate normal. Since the means are different by individual, the code operates on rows of mu where at least one BDL exist.
    imput <- imp.cond.MVN(
      X, Z, Gamma.post, Sigma.post, DL,
      mcmc.impute = mcmc.impute,
      verbose = FALSE
    )
    log.imp.values <- na.omit(unlist(imput$log.imp.value.list))
    log.dens <- imput$log.dens

    ### Check indicator.miss
    indicator.miss <- indicator.miss +
      sum(exp(log.imp.values) > rep(DL, n)[miss.vecX])

    ### Save the steady-state part of chain.
    if (t > 0) {
      # Convert a p X C posterior coefficient matrix into p x p matrix and take eigenvalues as surrogates to check for convergence.
      eigen.Gamma.post[t, ] <- eigen(Gamma.post %*% t(Gamma.post), only.values = TRUE)$values
      # Use eigenvalues for covariance matrix (C X C) as surrogates to check for convergence.
      eigen.Sigma.post[t, ] <- eigen(Sigma.post, only.values = TRUE)$values   # C X C matrix
      # Save the log of vector imputed values
      vec.log.X.imputed[t, ] <- log.imp.values
    }
  } # End Markov Chain

  ## Save MCMC parameters as coda::mcmc objects
  eigen.Gamma.post  <- coda::as.mcmc(eigen.Gamma.post,  start = 1, end = mcmc.length, thin = 1)
  eigen.Sigma.post  <- coda::as.mcmc(eigen.Sigma.post,  start = 1, end = mcmc.length, thin = 1)
  vec.log.X.imputed <- coda::as.mcmc(vec.log.X.imputed, start = 1, end = mcmc.length, thin = 1)

  options(warn = 0) # return warning option to default

  return(
    list(
      #Coda Objects -- Surrogrates used to check for convergence.
      eigen.Gamma =  eigen.Gamma.post,
      eigen.Sigma = eigen.Sigma.post,
      vec.log.X.imputed = vec.log.X.imputed,
      # Last States of Coefficient and Covariance Matrix are saved to use as initial values in a second chain or adjust initial values.
      Gamma.post.last = Gamma.post,
      Sigma.post.last = Sigma.post,
      # Checks ...
      indicator.miss = indicator.miss
     )
  )
}

## (1b) Convergence using Eigenvalues of matrices  ------------------------------------------------
#' @description Checks convergence of a Gibbs sampler using matrices. Uses eigenvalues f as surrogates for convergence. Uses Brook-Gelman’s multivariate potential scale reduction factor (MPSRF) on eigenvalues and vectorized imputed values chain from \code{\link[coda]{gelman.diag}} Along with the imputed values, the stationary distribution of the Markov chains was said to occur if the multivariate Gelman-Rubin statistic was less than 1.24. A summary of the statistics is printed, and if one chain fails to converge, a warning is printed.

#' @param mcmc.chains A list with 2 elements, each being a coda::mcmclist.
#' @inheritParams impute.multivariate.bayesian
#'
#' @return A list
#       # Convergence Summary Statistics

#'    # Surrogates used to check for convergence saved as mcmc.list objects:

#' @noRd
#' @importFrom purrr possibly

converge.multi.chain <- function(mcmc.chains) {

  ## Split into the two different chains
  chain1 <- mcmc.chains$chain1
  chain2 <- mcmc.chains$chain2

  ## Assess convergence of matrices using eigenvalues.
  eigen.Gamma <- coda::mcmc.list(chain1$eigen.Gamma, chain2$eigen.Gamma) # T x C evalues
  eigen.Sigma <- coda::mcmc.list(chain1$eigen.Sigma, chain2$eigen.Sigma) # T x C evalues
  vec.log.X.imputed <- coda::mcmc.list(chain1$vec.log.X.imputed, chain2$vec.log.X.imputed)     # T x n0 matrix

  ## Calculating Gelman Statistics
  pgd <- purrr::possibly(coda::gelman.diag,
    list(psrf = NA, mpsrf = NA),
    quiet = FALSE
  )
  gelman.list <- list(
    pgd(eigen.Gamma, autoburnin = FALSE, multivariate = TRUE),
    pgd(eigen.Sigma, autoburnin = FALSE, multivariate = TRUE),
    pgd(vec.log.X.imputed, autoburnin = FALSE, multivariate = TRUE)
  )

  ## MPSRF: Multivariate Potential Scale Reduction Factor
  impute.gelman <- lapply(gelman.list, FUN = "[[", "mpsrf")

  # When there are no covariates (Z = NULL) or 1 covariate or only one chemical is missing across all individuals (unlikely), there is no mpsrf (only 1 variable) and MPSRF is NULL. Use univariate potential scale reduction factor. Added 8/13/19
  if (coda::nvar(eigen.Gamma) == 1) {
    impute.gelman[[1]] <- gelman.list[[1]]$psrf[, 1]
  }
  if (coda::nvar(eigen.Sigma) == 1) {
    impute.gelman[[2]] <- gelman.list[[2]]$psrf[, 1]
  }
  if (coda::nvar(vec.log.X.imputed) == 1) {
    impute.gelman[[3]] <- gelman.list[[3]]$psrf[, 1]
  }
  # Convert to number. For any other reason that impute.gelman` is NULL, we don't know why this is case...we want to print out gelman statistics and stop with error since we cannot convert to number if null (error `... cannot be coerced to type 'double'`)
  impute.gelman <- tryCatch(
    as.numeric(impute.gelman),
    error = function(e) {
      cat("Gelman MPSRF Statistics (impute.gelman):\n ")
      names(impute.gelman) <-
        c("coeff_eigenvalues", "cov_eigenvalues", "imputed_values")
      print(impute.gelman)
      stop("Gelman MPSRF Statistic contains NULL value.", call. = FALSE)
    }
  )

  ## A summary of multivariate gelman statistics. Convergence occurred if # of times gelman.mpsrf < 1.2 equals the number of MCMC parameters = 3.
  gelman.summary <- data.frame(
    gelman.mpsrf = impute.gelman,
    is.converge = impute.gelman < 1.24
  )
  rownames(gelman.summary) <- c("Coeff.eigen", "Cov.eigen", "Imp.BDLs")
  print(gelman.summary)

  ## A nice message follows.
  if (anyNA(gelman.summary$gelman.mpsrf)) {
    warning("Error exits in calculating Gelman statistics. Likely that a within-chain variance matrix W is nearly not positive-definite. \n", call. = FALSE,
      immediate. = TRUE)
  } else {
    if (sum(gelman.summary$is.converge)  == 3) {
      cat("#> Evidence suggests that all parameters have converged.\n")
    } else {
      warning("The imputation may have failed to converged. Details: \n",
        call. = FALSE, immediate. = TRUE)
      impute.univ.gelman <- unlist(
        sapply(gelman.list, function(l) {l$psrf[, 1]}, simplify = TRUE)
      )
      summary(impute.univ.gelman)
    }
  }

  return(list(convgd.table  = gelman.summary,

   # Save these chains for MCMC analysis!
    eigen.Gamma = eigen.Gamma,
    eigen.Sigma = eigen.Sigma,
    vec.log.X.imputed = vec.log.X.imputed
  )
  )
}


## (1d) DRAW IMPUTED SAMPLES  ----------------------------------------------------------------------
# Edited slightly from univariate case.
#  #' @description  Creates a complete imputed dataset. Starting from a burned Markov chain, K missing values are taken every 10. The missing values from these states replace the NAs in the orginal missing data matrix, X.
# Additional Parameters:
#  #' @param vec.log.X.imputed  The processed MCMC posterior sample of the log(missing data) generated by sample.mregress.impute function. The sample has already been burned. ( # n0C x (T-n.burn+1) matrix )
#  #' @param K  see arguments for impute.bayesian.mi. -- Number of imputed datasets to form in X.imputed
#  #' @  mcmc.length  Length of burned/thinned posterior sample. Sample is already burned. Default is length of the chain already burned.
#  #' @param vec.log.X  The log missing component chemical matrix X in vectorized form. Structure used to re-impute values. Passed from body of impute.bayesian.mi.
#  #' @param miss.vecX, chemical.name, n, C  Additional arguments needed and passed from body of impute.bayesian.mi.
#  #' @return X.imputed  An imputed chemical concentration array of n subjects x C chemicals x K imputations on the normal scale.

draw.multi.imputed.samples <- function(vec.log.X,
                                       vec.log.X.imputed,
                                       K,
                                       mcmc.length, n.burn,
                                       n, C, chemical.name, verbose = FALSE
) {

  ### General Parameters
  miss.vecX <- which(is.na(vec.log.X))    # which subjects are BDL in vectorized log

  ## Finding which states of posterior imputed values to use
  step <- 10           # verified via looking at coda::autocorr() #trunc( mcmc.length / K )
  state <- seq(mcmc.length, mcmc.length - (K - 1) * step, length = K)
  if (any(state < 0)) {
    warning("K is greater than the burned chain. Any negative states taken are removed.", call. = FALSE)
    numb <- sum(state < 0)
    state  <- state[state > 0]
    K <- K - numb
    if (K <= 0)
      stop("Error: Can't select complete datasets (K<= 0). Possibly, the MCMC chain is too short.")
  }
  cat("#> Draw ",  K, " Multiple Imputed Set(s) from states of chain:", "\n")
  print(state + n.burn)

  ## Draw ...
  X.imputed <- array(NA,  dim = c(n, C, K),
    dimnames = list(NULL, chemical.name, paste0("Imputed.", 1:K))
  )
  for (k in 1:K) {
    vec.log.X[miss.vecX] <- vec.log.X.imputed[state[k], , drop = FALSE]  # Replace with kth imputed values.
    log.X <- matrix(vec.log.X, nrow = n, ncol = C, byrow = TRUE)  # Convert to complete n x C matrix
    X.imputed[, , k] <- exp(log.X)                                   # return on normal scale.
    # If the posterior of vec.log.X.imputed is just a posterior predictive distribution ....
  }

  ## Check --only useful if doing the example.
  # if(verbose){
  #   ##Before doing first line.
  #   cat(" >> imputed draw", k, "; state",  state[k], "\n")
  #   cat(" >> A check \n >> head(vec.log.X[miss.vecX]) \n")
  #   vec.log.X [miss.vecX] <- NA    #Replace with NA's
  #   print( head(vec.log.X[miss.vecX]) )
  #   cat(" >> vec.log.X.imputed  [state[1], ] \n")
  #   print( vec.log.X.imputed  [ state[1],  1:6 ]   )
  #   ##After doing the loop  work.
  #   cat(" >> Afterwards (using example): \n" )
  #   print( log.X[18:20, ] )
  # }

  # Return on normal scale.
  return(X.imputed)
}


##### (V) EXAMPLES =================================================================================
### To debug accessory functions, see debugging_impute_multivariate_accessories.R
###
###

## What accessory functions return ... may be helpful---------------------------------------------
# sample.mregress.impute
# return( list(convgd.table  = gelman.summary,
#              univ.gelman.diag = impute.univ.gelman,
#              #Save these chains for MCMC analysis!
#              eigen.Gamma = eigen.Gamma,
#              eigen.Sigma = eigen.Sigma,
#              vec.log.X.imputed = vec.log.X.imputed
# )
# )

# #converge.multi.converge()
# #  return( list(convgd.table  = gelman.summary,
# univ.gelman.diag = impute.univ.gelman,
# #Save these chains for MCMC analysis!
# eigen.Gamma = eigen.Gamma,
# eigen.Sigma = eigen.Sigma,
# vec.log.X.imputed = vec.log.X.imputed
# )
# )
