#' Normalized Random Measures Mixture of Type I for censored data
#'
#' Bayesian nonparametric estimation based on normalized measures driven
#' mixtures for locations.
#'
#' This generic function fits a normalized random measure (NRMI) mixture model
#' for density estimation (James et al. 2009) with censored data. Specifically,
#' the model assumes a normalized generalized gamma (NGG) prior for the
#' locations (means) of the mixture kernel and a parametric prior for the
#' common smoothing parameter sigma, leading to a semiparametric mixture model.
#'
#' This function coincides with \code{\link{MixNRMI1}} when the lower (xleft)
#' and upper (xright) censoring limits correspond to the same exact value.
#'
#' The details of the model are: \deqn{X_i|Y_i,\sigma \sim k(\cdot
#' |Y_i,\sigma)}{X_i|Y_i,sigma ~ k(.|Y_i,sigma)} \deqn{Y_i|P \sim P,\quad
#' i=1,\dots,n}{Y_i|P ~ P, i=1,...,n} \deqn{P \sim \textrm{NGG(\texttt{Alpha,
#' Kappa, Gama; P\_0})}}{P ~ NGG(Alpha, Kappa, Gama; P_0)} \deqn{\sigma \sim
#' \textrm{Gamma(asigma, bsigma)}}{sigma ~ Gamma(asigma, bsigma)} where
#' \eqn{X_i}'s are the observed data, \eqn{Y_i}'s are latent (location)
#' variables, \code{sigma} is the smoothing parameter, \code{k} is a parametric
#' kernel parameterized in terms of mean and standard deviation, \code{(Alpha,
#' Kappa, Gama; P_0)} are the parameters of the NGG prior with \code{P_0} being
#' the centering measure whose parameters are assigned vague hyper prior
#' distributions, and \code{(asigma,bsigma)} are the hyper-parameters of the
#' gamma prior on the smoothing parameter \code{sigma}. In particular:
#' \code{NGG(Alpha, 1, 0; P_0)} defines a Dirichlet process; \code{NGG(1,
#' Kappa, 1/2; P_0)} defines a Normalized inverse Gaussian process; and
#' \code{NGG(1, 0, Gama; P_0)} defines a normalized stable process.
#'
#' The evaluation grid ranges from \code{min(x) - epsilon} to \code{max(x) +
#' epsilon}. By default \code{epsilon=sd(x)/4}.
#'
#' @param xleft Numeric vector. Lower limit of interval censoring. For exact
#' data the same as xright
#' @param xright Numeric vector. Upper limit of interval censoring. For exact
#' data the same as xleft.
#' @param probs Numeric vector. Desired quantiles of the density estimates.
#' @param Alpha Numeric constant. Total mass of the centering measure. See
#' details.
#' @param Kappa Numeric positive constant. See details.
#' @param Gama Numeric constant. \eqn{0\leq \texttt{Gama} \leq 1}{0 <= Gama <=
#' 1}.  See details.
#' @param distr.k The distribution name for the kernel. Allowed names are "normal", "gamma", "beta", "double exponential", "lognormal" or their common abbreviations "norm", "exp", or an integer number identifying the mixture kernel: 1 = Normal; 2 = Gamma; 3 = Beta; 4 = Double Exponential; 5 = Lognormal.
#' @param distr.p0 The distribution name for the centering measure. Allowed names are "normal", "gamma", "beta", or their common abbreviations "norm", "exp", or an integer number identifying the centering measure: 1 = Normal; 2 = Gamma; 3 = Beta.
#' @param asigma Numeric positive constant. Shape parameter of the gamma prior
#' on the standard deviation of the mixture kernel \code{distr.k}.
#' @param bsigma Numeric positive constant. Rate parameter of the gamma prior
#' on the standard deviation of the mixture kernel \code{distr.k}.
#' @param delta_S Numeric positive constant. Metropolis-Hastings proposal
#' variation coefficient for sampling sigma.
#' @param delta_U Numeric positive constant. Metropolis-Hastings proposal
#' variation coefficient for sampling the latent U.
#' @param Meps Numeric constant. Relative error of the jump sizes in the
#' continuous component of the process. Smaller values imply larger number of
#' jumps.
#' @param Nx Integer constant. Number of grid points for the evaluation of the
#' density estimate.
#' @param Nit Integer constant. Number of MCMC iterations.
#' @param Pbi Numeric constant. Burn-in period proportion of Nit.
#' @param epsilon Numeric constant. Extension to the evaluation grid range.
#' See details.
#' @param printtime Logical. If TRUE, prints out the execution time.
#' @param extras Logical. If TRUE, gives additional objects: means, weights and
#' Js.
#' @param adaptive Logical. If TRUE, uses an adaptive MCMC strategy to sample the latent U (adaptive delta_U).
#'
#' @return The function returns a list with the following components:
#' \item{xx}{Numeric vector. Evaluation grid.} \item{qx}{Numeric array. Matrix
#' of dimension \eqn{\texttt{Nx} \times (\texttt{length(probs)} + 1)}{Nx x
#' (length(probs)+1)} with the posterior mean and the desired quantiles input
#' in \code{probs}.} \item{cpo}{Numeric vector of \code{length(x)} with
#' conditional predictive ordinates.} \item{R}{Numeric vector of
#' \code{length(Nit*(1-Pbi))} with the number of mixtures components
#' (clusters).} \item{S}{Numeric vector of \code{length(Nit*(1-Pbi))} with the
#' values of common standard deviation sigma.} \item{U}{Numeric vector of
#' \code{length(Nit*(1-Pbi))} with the values of the latent variable U.}
#' \item{Allocs}{List of \code{length(Nit*(1-Pbi))} with the clustering
#' allocations.} \item{means}{List of \code{length(Nit*(1-Pbi))} with the
#' cluster means (locations). Only if extras = TRUE.} \item{weights}{List of
#' \code{length(Nit*(1-Pbi))} with the mixture weights. Only if extras = TRUE.}
#' \item{Js}{List of \code{length(Nit*(1-Pbi))} with the unnormalized weights
#' (jump sizes). Only if extras = TRUE.} \item{Nm}{Integer constant. Number of
#' jumps of the continuous component of the unnormalized process.}
#' \item{Nx}{Integer constant. Number of grid points for the evaluation of the
#' density estimate.} \item{Nit}{Integer constant. Number of MCMC iterations.}
#' \item{Pbi}{Numeric constant. Burn-in period proportion of \code{Nit}.}
#' \item{procTime}{Numeric vector with execution time provided by
#' \code{proc.time} function.}
#' \item{distr.k}{Integer corresponding to the kernel chosen for the mixture}
#' \item{data}{Data used for the fit}
#' \item{NRMI_params}{A named list with the parameters of the NRMI process}
#' @param adaptive Logical. If TRUE, uses an adaptive MCMC strategy to sample the latent U (adaptive delta_U).

#'
#' @section Warning : The function is computing intensive. Be patient.
#' @author Barrios, E., Kon Kam King, G. and Nieto-Barajas, L.E.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}, \code{\link{multMixNRMI1}}
#' @references 1.- Barrios, E., Lijoi, A., Nieto-Barajas, L. E. and Prünster,
#' I. (2013). Modeling with Normalized Random Measure Mixture Models.
#' Statistical Science. Vol. 28, No. 3, 313-334.
#'
#' 2.- James, L.F., Lijoi, A. and Prünster, I. (2009). Posterior analysis for
#' normalized random measure with independent increments. Scand. J. Statist 36,
#' 76-97.
#'
#' 3.- Kon Kam King, G., Arbel, J. and Prünster, I. (2016). Species
#' Sensitivity Distribution revisited: a Bayesian nonparametric approach. In
#' preparation.
#' @keywords distribution models nonparametrics
#' @examples
#'
#' ### Example 1
#' \dontrun{
#' # Data
#' data(acidity)
#' x <- acidity
#' # Fitting the model under default specifications
#' out <- MixNRMI1cens(x, x)
#' # Plotting density estimate + 95% credible interval
#' plot(out)
#' }
#'
#' \dontrun{
#' ### Example 2
#' # Data
#' data(salinity)
#' # Fitting the model under default specifications
#' out <- MixNRMI1cens(xleft = salinity$left, xright = salinity$right, Nit = 5000)
#' # Plotting density estimate + 95% credible interval
#' attach(out)
#' plot(out)
#' # Plotting number of clusters
#' par(mfrow = c(2, 1))
#' plot(R, type = "l", main = "Trace of R")
#' hist(R, breaks = min(R - 0.5):max(R + 0.5), probability = TRUE)
#' detach()
#' }
#'
#' @export MixNRMI1cens
MixNRMI1cens <-
  function(xleft, xright, probs = c(0.025, 0.5, 0.975), Alpha = 1,
           Kappa = 0, Gama = 0.4, distr.k = "normal", distr.p0 = "normal", asigma = 0.5,
           bsigma = 0.5, delta_S = 3, delta_U = 2, Meps = 0.01, Nx = 150,
           Nit = 1500, Pbi = 0.1, epsilon = NULL, printtime = TRUE,
           extras = TRUE, adaptive = FALSE) {
    if (is.null(distr.k)) {
      stop("Argument distr.k is NULL. Should be provided. See help for details.")
    }
    if (is.null(distr.p0)) {
      stop("Argument distr.p0 is NULL. Should be provided. See help for details.")
    }
    distr.k <- process_dist_name(distr.k)
    distr.p0 <- process_dist_name(distr.p0)
    tInit <- proc.time()
    cens_data_check(xleft, xright)
    xpoint <- as.numeric(na.omit(0.5 * (xleft + xright)))
    npoint <- length(xpoint)
    censor_code <- censor_code_rl(xleft, xright)
    censor_code_filters <- lapply(0:3, FUN = function(x) {
      censor_code ==
        x
    })
    names(censor_code_filters) <- 0:3
    n <- length(xleft)
    y <- seq(n)
    xsort <- sort(xpoint)
    y[seq(n / 2)] <- mean(xsort[seq(npoint / 2)])
    y[-seq(n / 2)] <- mean(xsort[-seq(npoint / 2)])
    u <- 1
    sigma <- sd(xpoint)
    if (is.null(epsilon)) {
      epsilon <- sd(xpoint) / 4
    }
    xx <- seq(min(xpoint) - epsilon, max(xpoint) + epsilon, length = Nx)
    Fxx <- matrix(NA, nrow = Nx, ncol = Nit)
    fx <- matrix(NA, nrow = n, ncol = Nit)
    R <- seq(Nit)
    S <- seq(Nit)
    U <- seq(Nit)
    Nmt <- seq(Nit)
    Allocs <- vector(mode = "list", length = Nit)
    if (adaptive) {
      optimal_delta <- rep(NA, n)
    }
    if (extras) {
      means <- vector(mode = "list", length = Nit)
      weights <- vector(mode = "list", length = Nit)
      Js <- vector(mode = "list", length = Nit)
      if (adaptive) {
        delta_Us <- seq(Nit)
      }
    }
    mu.p0 <- mean(xpoint)
    sigma.p0 <- sd(xpoint)
    for (j in seq(Nit)) {
      if (floor(j / 500) == ceiling(j / 500)) {
        cat("MCMC iteration", j, "of", Nit, "\n")
      }
      tt <- comp1(y)
      ystar <- tt$ystar
      nstar <- tt$nstar
      r <- tt$r
      # if (is.na(optimal_delta[r])) {
      #   optimal_delta[r] <- compute_optimal_delta_given_r(r = r, gamma = Gama, kappa = Kappa, a = Alpha, n = n)
      # }
      idx <- tt$idx
      Allocs[[max(1, j - 1)]] <- idx
      if (Gama != 0) {
        if (adaptive) {
          tmp <- gs3_adaptive3(u, n = n, r = r, alpha = Alpha, kappa = Kappa, gama = Gama, delta = delta_U, U = U, iter = j, adapt = adaptive)
          u <- tmp$u_prime
          delta_U <- tmp$delta
        } else {
          u <- gs3(u,
            n = n, r = r, alpha = Alpha, kappa = Kappa,
            gama = Gama, delta = delta_U
          )
        }
      }
      JiC <- MvInv(
        eps = Meps, u = u, alpha = Alpha, kappa = Kappa,
        gama = Gama, N = 50001
      )
      Nm <- length(JiC)
      TauiC <- rk(Nm, distr = distr.p0, mu = mu.p0, sigma = sigma.p0)
      ystar <- gs4cens2(
        ystar = ystar, xleft = xleft, xright = xright,
        censor_code = censor_code, idx = idx, distr.k = distr.k,
        sigma.k = sigma, distr.p0 = distr.p0, mu.p0 = mu.p0,
        sigma.p0 = sigma.p0
      )
      Jstar <- rgamma(r, nstar - Gama, Kappa + u)
      Tau <- c(TauiC, ystar)
      J <- c(JiC, Jstar)
      tt <- gsHP(ystar, r, distr.p0)
      mu.p0 <- tt$mu.py0
      sigma.p0 <- tt$sigma.py0
      y <- fcondYXAcens2(
        xleft = xleft, xright = xright, censor_code_filters = censor_code_filters,
        distr = distr.k, Tau = Tau, J = J, sigma = sigma
      )
      sigma <- gs5cens2(
        sigma = sigma, xleft = xleft, xright = xright,
        censor_code = censor_code, y = y, distr = distr.k,
        asigma = asigma, bsigma = bsigma, delta = delta_S
      )
      Fxx[, j] <- fcondXA(xx,
        distr = distr.k, Tau = Tau, J = J,
        sigma = sigma
      )
      fx[, j] <- fcondYXAcens2(
        xleft = xleft, xright = xright,
        censor_code_filters = censor_code_filters, distr = distr.k,
        Tau = Tau, J = J, sigma = sigma
      )
      R[j] <- r
      S[j] <- sigma
      U[j] <- u
      Nmt[j] <- Nm
      if (extras) {
        means[[j]] <- Tau
        weights[[j]] <- J / sum(J)
        Js[[j]] <- J
        if (adaptive) {
          delta_Us[j] <- delta_U
        }
      }
    }
    tt <- comp1(y)
    Allocs[[Nit]] <- tt$idx
    biseq <- seq(floor(Pbi * Nit))
    Fxx <- Fxx[, -biseq]
    qx <- as.data.frame(t(apply(Fxx, 1, quantile, probs = probs)))
    names(qx) <- paste("q", probs, sep = "")
    qx <- cbind(mean = apply(Fxx, 1, mean), qx)
    R <- R[-biseq]
    S <- S[-biseq]
    U <- U[-biseq]
    Allocs <- Allocs[-biseq]
    if (extras) {
      means <- means[-biseq]
      weights <- weights[-biseq]
      Js <- Js[-biseq]
      if (adaptive) {
        delta_Us <- delta_Us[-biseq]
      }
    }
    cpo <- 1 / apply(1 / fx[, -biseq], 1, mean)
    if (printtime) {
      cat(" >>> Total processing time (sec.):\n")
      print(procTime <- proc.time() - tInit)
    }
    res <- list(
      xx = xx, qx = qx, cpo = cpo, R = R, S = S,
      U = U, Allocs = Allocs, Nm = Nmt, Nx = Nx, Nit = Nit,
      Pbi = Pbi, procTime = procTime, distr.k = distr.k, data = data.frame(left = xleft, right = xright),
      NRMI_params = list("Alpha" = Alpha, "Kappa" = Kappa, "Gamma" = Gama)
    )
    if (extras) {
      res$means <- means
      res$weights <- weights
      res$Js <- Js
      if (adaptive) {
        res$delta_Us <- delta_Us
      }
    }
    return(structure(res, class = "NRMI1"))
  }
