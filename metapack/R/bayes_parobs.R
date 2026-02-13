#' Fit Bayesian Inference for Meta-Regression
#'
#' This is a function for running the Markov chain Monte Carlo algorithm for the *Bayesian inference for multivariate meta-regression with a partially observed within-study sample covariance matrix* model. The first six arguments are required.
#' fmodel can be one of 5 numbers: 1, 2, 3, 4, and 5. The first model, fmodel = 1 denoted by M1, indicates that the \eqn{\Sigma_{kt}}
#' are diagonal matrices with zero covariances. M2 indicates that \eqn{\Sigma_{kt}} are all equivalent but allowed to be full symmetric
#' positive definite. M3 is where \eqn{\Sigma_{kt}} are allowed to differ across treatments, i.e., \eqn{\Sigma_{kt}=\Sigma_t}.
#' M4 assumes thata the correlation matrix, \eqn{\rho}, is identical for all trials/treatments, but the variances are allowed to vary.
#' Finally, M5 assumes a hierarchical model where \eqn{(\Sigma_{kt} | \Sigma)} follows an inverse-Wishart distribution with fixed
#' degrees of freedom and scale matrix \eqn{\Sigma}. \eqn{\Sigma} then follows another inverse-Wishart distribution with fixed parameters.
#' @author Daeyoung Lim, \email{daeyoung.lim@uconn.edu}
#' @param Outcome the aggregate mean of the responses for each arm of every study.
#' @param SD the standard deviation of the responses for each arm of every study.
#' @param XCovariate the aggregate covariates for the fixed effects.
#' @param WCovariate the aggregate covariates for the random effects.
#' @param Treat the treatment identifiers. This is equivalent to the arm number of each study. The number of unique treatments must be equal across trials. The elements within will be coerced to consecutive integers.
#' @param Trial the trial identifiers. This is equivalent to the arm labels in each study. The elements within will be coerced to consecutive integers
#' @param Npt the number of observations/participants for a unique `(k,t)`, or each arm of every trial.
#' @param fmodel the model number. The possible values for `fmodel` are 1 to 5, each indicating a different prior specification for \eqn{\Sigma_{kt}}. It will default to M1, `fmodel=1` if not specified at function call. See the following model descriptions. The objects enclosed in parentheses at the end of every bullet point are the hyperparameters associated with each model.
#' 
#' + `fmodel=1` - \eqn{\Sigma_{kt} = diag(\sigma_{kt,11}^2,\ldots,\sigma_{kt,JJ}^2)} where \eqn{\sigma_{kt,jj}^2 \sim IG(a_0,b_0)} and \eqn{IG(a,b)} is [the inverse-gamma distribution](https://en.wikipedia.org/wiki/Inverse-gamma_distribution). This specification is useful if the user does not care about the correlation recovery. (`c0`, `dj0`, `a0`, `b0`, `Omega0`)
#' + `fmodel=2` - \eqn{\Sigma_{kt}=\Sigma} for every combination of \eqn{(k,t)}{`(k,t)`} and \eqn{\Sigma^{-1}\sim Wish_{s_0}(\Sigma_0)}{Sig^{-1} ~ Wish(s0, Sigma0)}. This specification assumes that the user has prior knowledge that the correlation structure does not change across the arms included. (`c0`, `dj0`, `s0`, `Omega0`, `Sigma0`)
#' + `fmodel=3` - \eqn{\Sigma_{kt}=\Sigma_t} and \eqn{\Sigma_t^{-1}\sim  Wish_{s_0}(\Sigma_0)}. This is a relaxed version of `fmodel=2`, allowing the correlation structure to differ across trials but forcing it to stay identical within a trial. (`c0`, `dj0`, `s0`, `Omega0`, `Sigma0`)
#' + `fmodel=4` - \eqn{\Sigma_{kt}=\delta_{kt} \rho \delta_{kt}} where \eqn{\delta_{kt}=diag(\Sigma_{kt,11}^{1/2},\ldots,\Sigma_{kt,JJ}^{1/2})}, and \eqn{\rho} is the correlation matrix. This specification allows the variances to vary across arms but requires that the correlations be the same. This is due to the lack of correlation information in the data, which would in turn lead to the nonidentifiability of the correlations if they were allowed to vary. However, this still is an ambitious model which permits maximal degrees of freedom in terms of variance and correlation estimation. (`c0`, `dj0`, `a0`, `b0`, `Omega0`)
#' + `fmodel=5` - The fifth model is hierarchical and thus may require more data than the others: \eqn{(\Sigma_{kt}^{-1}\mid \Sigma)\sim  Wish_{\nu_0}((\nu_0-J-1)^{-1}\Sigma^{-1})} and \eqn{\Sigma \sim  Wish_{d_0}(\Sigma_0)}. \eqn{\Sigma_{kt}} encodes the within-treatment-arm variation while \eqn{\Sigma} captures the between-treatment-arm variation. The hierarchical structure allows the "borrowing of strength" across treatment arms. (`c0`, `dj0`, `d0`, `nu0`, `Sigma0`, `Omega0`)

#' @param prior (Optional) a list of hyperparameters. Despite `theta` in every model, each `fmodel`, along with the `group` argument, requires a different set of hyperparameters. See `fmodel` for the model specifications.
#' @param mcmc (Optional) a list for MCMC specification. `ndiscard` is the number of burn-in iterations. `nskip` configures the thinning of the MCMC. For instance, if `nskip=5`, `bayes_parobs` will save the posterior sample every 5 iterations. `nkeep` is the size of the posterior sample. The total number of iterations will be `ndiscard + nskip * nkeep`.
#' @param control (Optional) a list of tuning parameters for [the Metropolis-Hastings algorithm](https://en.wikipedia.org/wiki/Metropolis-Hastings_algorithm). `Rho`, `R`, and `delta` are sampled through either localized Metropolis algorithm or delayed rejection robust adaptive Metropolis algorithm. `*_stepsize` with the asterisk replaced with one of the names above specifies the stepsize for determining the sample evaluation points in the localized Metropolis algorithm. `sample_Rho` can be set to `FALSE` to suppress the sampling of `Rho` for `fmodel=4`. When `sample_Rho` is `FALSE`, \eqn{\rho} will be fixed using the value given by the `init` argument, which defaults to \eqn{0.5 I+0.511'} where \eqn{1} is the vector of ones.
#' @param init (Optional) a list of initial values for the parameters to be sampled: `theta`, `gamR`, `Omega`, and `Rho`. The initial value for `Rho` will be effective only if `fmodel=4`.
#' @param Treat_order (Optional) a vector of unique treatments to be used for renumbering the `Treat` vector. The first element will be assigned treatment zero, potentially indicating placebo. If not provided, the numbering will default to an alphabetical/numerical order.
#' @param Trial_order (Optional) a vector of unique trials. The first element will be assigned zero. If not provided, the numbering will default to an alphabetical/numerical order.
#' @param group (Optional) a vector containing binary variables for \eqn{u_{kt}}. If not provided, `bayes_parobs` will assume that there is no grouping and set \eqn{u_{kt}=0} for all `(k,t)`.
#' @param group_order (Optional) a vector of unique group labels. The first element will be assigned zero. If not provided, the numbering will default to an alphabetical/numerical order. `group_order` will take effect only if `group` is provided by the user.
#' @param scale_x (Optional) a logical variable indicating whether `XCovariate` should be scaled/standardized. The effect of setting this to `TRUE` is not limited to merely standardizing `XCovariate`. The following generic functions will scale the posterior sample of `theta` back to its original unit: `plot`, `fitted`, `summary`, and `print`.
#' @param verbose (Optional) a logical variable indicating whether to print the progress bar during the MCMC sampling.
#' @references 
#' Yao, H., Kim, S., Chen, M. H., Ibrahim, J. G., Shah, A. K., & Lin, J. (2015). Bayesian inference for multivariate meta-regression with a partially observed within-study sample covariance matrix. *Journal of the American Statistical Association*, **110(510)**, 528-544.

#' @return `bayes_parobs` returns an object of class `"bayesparobs"`. The functions `summary` or `print` are used to obtain and print a summary of the results. The generic accessor function `fitted` extracts the posterior mean, posterior standard deviation, and the interval estimates of the value returned by `bayes_parobs`.
#' 
#' An object of class `bayesparobs` is a list containing the following components:
#' 
#' + `Outcome` - the aggregate response used in the function call.
#' + `SD` - the standard deviation used in the function call.
#' + `Npt` - the number of participants for `(k,t)` used in the function call.
#' + `XCovariate` - the aggregate design matrix for fixed effects used in the function call. Depending on `scale_x`, this may differ from the matrix provided at function call.
#' + `WCovariate` - the aggregate design matrix for random effects.
#' + `Treat` - the *renumbered* treatment indicators. Depending on `Treat_order`, it may differ from the vector provided at function call.
#' + `Trial` - the *renumbered* trial indicators. Depending on `Trial_order`, it may differ from the vector provided at function call.
#' + `group` - the *renumbered* grouping indicators in the function call. Depending on `group_order`, it may differ from the vector provided at function call. If `group` was missing at function call, `bayes_parobs` will assign `NULL` for `group`.
#' + `TrtLabels` - the vector of treatment labels corresponding to the renumbered `Treat`. This is equivalent to `Treat_order` if it was given at function call.
#' + `TrialLabels` - the vector of trial labels corresponding to the renumbered `Trial`. This is equivalent to `Trial_order` if it was given at function call.
#' + `GroupLabels` - the vector of group labels corresponding to the renumbered `group`. This is equivalent to `group_order` if it was given at function call. If `group` was missing at function call, `bayes_parobs` will assign `NULL` for `GroupLabels`.
#' + `K` - the total number of trials.
#' + `T` - the total number of treatments.
#' + `fmodel` - the model number as described [here](#model-spec).
#' + `scale_x` - a Boolean indicating whether `XCovariate` has been scaled/standardized.
#' + `prior` - the list of hyperparameters used in the function call.
#' + `control` - the list of tuning parameters used for MCMC in the function call.
#' + `mcmctime` - the elapsed time for the MCMC algorithm in the function call. This does not include all the other preprocessing and post-processing outside of MCMC.
#' + `mcmc` - the list of MCMC specification used in the function call.
#' + `mcmc.draws` - the list containing the MCMC draws. The posterior sample will be accessible here.

#' @examples
#' library(metapack)
#' data("cholesterol")
#' Outcome <- model.matrix(~ 0 + pldlc + phdlc + ptg, data = cholesterol)
#' SD <- model.matrix(~ 0 + sdldl + sdhdl + sdtg, data = cholesterol)
#' Trial <- cholesterol$trial
#' Treat <- cholesterol$treat
#' Npt <- cholesterol$n
#' XCovariate <- model.matrix(~ 0 + bldlc + bhdlc + btg + age + durat +
#'  white + male + dm, data = cholesterol)
#' WCovariate <- model.matrix(~ treat, data = cholesterol)
#' 
#' fmodel <- 1
#' set.seed(2797542)
#' fit <- bayes_parobs(Outcome, SD, XCovariate, WCovariate, Treat, Trial,
#'    Npt, fmodel, mcmc = list(ndiscard = 1, nskip = 1, nkeep = 1),
#'    scale_x = TRUE, group = cholesterol$onstat, verbose = FALSE)
#' @importFrom stats model.matrix
#' @importFrom methods is
#' @seealso \code{\link{bmeta_analyze}} for using the \code{\link[Formula]{Formula}} interface
#' @md
#' @export
bayes_parobs <- function(Outcome, SD, XCovariate, WCovariate, Treat, Trial, Npt, fmodel = 1, prior = list(), mcmc = list(), control = list(), init = list(), Treat_order = NULL, Trial_order = NULL, group = NULL, group_order = NULL, scale_x = FALSE, verbose = FALSE) {
  if (!is(Outcome, "matrix")) {
    tmp <- try(Outcome <- model.matrix(~ 0 + ., data = Outcome), silent = TRUE)
    if (is(tmp, "try-error")) {
      stop("Outcome must be a matrix or able to be coerced to a matrix")
    }
  }
  if (!is(SD, "matrix")) {
    tmp <- try(SD <- model.matrix(~ 0 + ., data = SD), silent = TRUE)
    if (is(tmp, "try-error")) {
      stop("SD must be a matrix or able to be coerced to a matrix")
    }
  }
  if (!is(XCovariate, "matrix")) {
    tmp <- try(XCovariate <- model.matrix(~ 0 + ., data = XCovariate), silent = TRUE)
    if (is(tmp, "try-error")) {
      stop("XCovariate must be a matrix or able to be coerced to a matrix")
    }
  }
  if (!is(WCovariate, "matrix")) {
    tmp <- try(WCovariate <- model.matrix(~ 0 + ., data = WCovariate), silent = TRUE)
    if (is(tmp, "try-error")) {
      stop("WCovariate must be a matrix or able to be coerced to a matrix")
    }
  }
  if (!is(Treat, "vector")) {
    tmp <- try(Treat <- as.vector(Treat))
    if (is(tmp, "try-error")) {
      stop("Treat must be a vector or able to be coerced to a vector")
    }
  }
  if (!is(Trial, "vector")) {
    tmp <- try(Trial <- as.vector(Trial))
    if (is(tmp, "try-error")) {
      stop("Trial must be a vector or able to be coerced to a vector")
    }
  }
  if (!is(Npt, "numeric")) {
    tmp <- try(Npt <- as.numeric(Npt))
    if (is(tmp, "try-error")) {
      stop("Npt must be numeric or able to be coerced to numeric")
    }
  }
  if (any(is.na(Outcome)) | any(is.na(SD)) | any(is.na(XCovariate)) | any(is.na(WCovariate)) | any(is.na(Treat)) | any(is.na(Trial))) {
    stop("Missing data (NA) detected. Handle missing data (e.g., delete missings, delete variables, imputation) before passing it as an argument")
  }

  nr <- 1
  second.exist <- FALSE
  if (!is.null(group)) {
    second.exist <- TRUE
    sl <- unique(group)
    if (length(sl) != 2) {
      stop(paste(sQuote("group"), "must be binary"))
    }
    nr <- 2
    if (is.null(group_order)) {
      group.order <- sort(sl)
    } else {
      group.order <- group_order
    }
    group.n <- relabel.vec(group, group.order) - 1
  } else {
    group.n <- NULL
    group.order <- NULL
  }


  mcvals <- list(ndiscard = 5000L, nskip = 1L, nkeep = 20000L)
  mcvals[names(mcmc)] <- mcmc
  ndiscard <- mcvals$ndiscard
  nskip <- mcvals$nskip
  if (nskip < 1) {
    stop(paste0(sQuote("nskip"), "can't be smaller than 1"))
  }
  nkeep <- mcvals$nkeep

  ctrl <- list(R_stepsize = 0.02, Rho_stepsize = 0.02, delta_stepsize = 0.2, sample_Rho = TRUE)
  ctrl[names(control)] <- control
  R_stepsize <- ctrl$R_stepsize
  Rho_stepsize <- ctrl$Rho_stepsize
  delta_stepsize <- ctrl$delta_stepsize
  sample_Rho <- ctrl$sample_Rho # for fmodel = 4

  J <- ncol(Outcome)
  nw <-  ncol(WCovariate)
  priorvals <- list(c0 = 1.0e05, dj0 = 0.1 + nw, d0 = 0.1 + J, s0 = 0.1, a0 = 0.1, b0 = 0.1, Omega0 = diag(10, nw), Sigma0 = diag(10, J), nu0 = 10)
  priorvals[names(prior)] <- prior
  a0 <- priorvals$a0
  b0 <- priorvals$b0
  c0 <- priorvals$c0
  dj0 <- priorvals$dj0
  d0 <- priorvals$d0
  s0 <- priorvals$s0
  Omega0 <- priorvals$Omega0
  Sigma0 <- priorvals$Sigma0
  nu0 <- priorvals$nu0

  if (is.null(Treat_order)) {
	  Treat.order <- sort(unique(Treat))
  } else {
  	Treat.order <- Treat_order
  }
  Treat.n <- relabel.vec(Treat, Treat.order) - 1 # relabel the treatment numbers

  if (is.null(Trial_order)) {
	  Trial.order <- sort(unique(Trial))
  } else {
  	Trial.order <- Trial_order
  }
  Trial.n <- relabel.vec(Trial, Trial.order) - 1 # relabel the trial numbers

  K <- length(unique(Trial))
  T <- length(unique(Treat))

  xcols <- ncol(XCovariate)


  init_final <- list(
    theta = numeric((xcols + nw*nr) * J),
    gamR = matrix(0, nw * nr * J, K), Omega = diag(1, nrow = nw*nr*J),
    Rho = diag(1, nrow = J)
  )
  init_final$Rho[upper.tri(init_final$Rho)] <- 0.5
  init_final$Rho[lower.tri(init_final$Rho)] <- 0.5
  init_final[names(init)] <- init
  theta_init <- init_final$theta
  gamR_init <- init_final$gamR
  Omega_init <- init_final$Omega
  Rho_init <- init_final$Rho
  if (length(theta_init) != (xcols + nw*nr) * J) {
    stop(paste("theta initialized with length", sQuote(length(theta_init)), "but", sQuote((xcols + nw*nr) * J), "wanted"))
  }
  if (dim(Omega_init)[1] != nw*nr*J || dim(Omega_init)[2] != nw*nr*J) {
    stop(paste("Omega initialized with dimensions", sQuote(dim(Omega_init)), "but", sQuote(nw*nr*J), "wanted"))
  }
  if (dim(Rho_init)[1] != J || dim(Rho_init)[2] != J) {
    stop(paste("Rho initialized with dimensions", sQuote(dim(Rho_init)), "but", sQuote(J), "wanted"))
  }
  if (dim(gamR_init)[1] != nw*nr*J) {
    stop(paste("gamR initialized with", sQuote(dim(gamR_init)[1]), "rows but", sQuote(nw*nr*J), "wanted"))
  }
  if (dim(gamR_init)[2] != K) {
    stop(paste("gamR initialized with", sQuote(dim(gamR_init)[2]), "columns but", sQuote(K), "wanted"))
  }

  if (any(eigen(Omega_init, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
    stop(paste("The initial value for", sQuote("Omega"), "is not positive definite"))
  }
  if (any(eigen(Rho_init, symmetric = TRUE, only.values = TRUE)$values <= 0)) {
    stop(paste("The initial value for", sQuote("Rho"), "is not positive definite"))
  }
  
  if (scale_x) {
    XCovariate_ <- scale(XCovariate, center = TRUE, scale = TRUE)
  } else {
    XCovariate_ <- XCovariate
  }


  mcmctime <- system.time({
    if (second.exist) {
      if (fmodel == 1) {
        fout <- .Call(
          `_metapack_fmodel1p`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.integer(group.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(a0),
          as.double(b0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else if (fmodel == 2) {
        fout <- .Call(
          `_metapack_fmodel2p`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.integer(group.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(s0),
          as.matrix(Omega0),
          as.matrix(Sigma0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
    } else if (fmodel == 3) {
      fout <- .Call(
          `_metapack_fmodel2p5p`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.integer(group.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(s0),
          as.matrix(Omega0),
          as.matrix(Sigma0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else if (fmodel == 4) {
        fout <- .Call(
          `_metapack_fmodel3pp`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.integer(group.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(a0),
          as.double(b0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(delta_stepsize),
          as.double(Rho_stepsize),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.matrix(Rho_init),
          as.logical(sample_Rho),
          as.logical(verbose)
        )
      } else if (fmodel == 5) {
        fout <- .Call(
          `_metapack_fmodel4p`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.integer(group.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(d0),
          as.double(nu0),
          as.matrix(Sigma0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(delta_stepsize),
          as.double(Rho_stepsize),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else {
        stop(paste(sQuote("fmodel"), "is invalid. Please pick from {1, 2, 3, 4, 5}"))
      }
    } else {
      if (fmodel == 1) {
        fout <- .Call(
          `_metapack_fmodel1`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(a0),
          as.double(b0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else if (fmodel == 2) {
        fout <- .Call(
          `_metapack_fmodel2`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(s0),
          as.matrix(Omega0),
          as.matrix(Sigma0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
  	} else if (fmodel == 3) {
  		fout <- .Call(
          `_metapack_fmodel2p5`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(s0),
          as.matrix(Omega0),
          as.matrix(Sigma0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else if (fmodel == 4) {
        fout <- .Call(
          `_metapack_fmodel3`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(a0),
          as.double(b0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(delta_stepsize),
          as.double(Rho_stepsize),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.matrix(Rho_init),
          as.logical(sample_Rho),
          as.logical(verbose)
        )
      } else if (fmodel == 5) {
        fout <- .Call(
          `_metapack_fmodel4`,
          as.matrix(Outcome),
          as.matrix(SD),
          as.matrix(XCovariate_),
          as.matrix(WCovariate),
          as.integer(Treat.n),
          as.integer(Trial.n),
          as.double(Npt),
          as.double(c0),
          as.double(dj0),
          as.double(d0),
          as.double(nu0),
          as.matrix(Sigma0),
          as.matrix(Omega0),
          as.integer(K),
          as.integer(T),
          as.integer(ndiscard),
          as.integer(nskip),
          as.integer(nkeep),
          as.double(delta_stepsize),
          as.double(Rho_stepsize),
          as.double(R_stepsize),
          as.double(theta_init),
          as.matrix(gamR_init),
          as.matrix(Omega_init),
          as.logical(verbose)
        )
      } else {
        stop(paste(sQuote("fmodel"), "is invalid. Please pick from {1, 2, 3, 4, 5}"))
      }
    }
  })
  xcc <- if (!is.null(colnames(XCovariate))) colnames(XCovariate) else paste0("beta", 1:ncol(XCovariate))
  wcc <- if (!is.null(colnames(WCovariate))) colnames(WCovariate) else paste0("gam", 1:ncol(WCovariate))
  if (is.null(group)) {
		rownames(fout$theta) <- c(paste0(rep(xcc, J), "_", rep(1:J, each=length(xcc))), paste0(rep(wcc, J), "_", rep(1:J, each=length(wcc))))
	} else {
		rownames(fout$theta) <- c(paste0(rep(xcc, J), "_", rep(1:J, each=length(xcc))),
						 paste0(rep(wcc, 2*J), rep(rep(c("*(1-2nd)", "*2nd"), each = length(wcc)), J), "_", rep(1:J, each = 2*length(wcc))))
	}

  out <- list(
    Outcome = Outcome,
    SD = SD,
    Npt = Npt,
    XCovariate = XCovariate_,
    WCovariate = WCovariate,
    Treat = Treat.n,
    Trial = Trial.n,
    group = group.n,
    TrtLabels = Treat.order,
    TrialLabels = Trial.order,
    GroupLabels = group.order,
    K = K,
    T = T,
    fmodel = fmodel,
    scale_x = scale_x,
    prior = priorvals,
    control = ctrl,
    mcmctime = mcmctime,
    mcmc = mcvals,
    mcmc.draws = fout
  )
  class(out) <- "bayesparobs"
  out
}