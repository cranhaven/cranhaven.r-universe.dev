#' Normalized Random Measures Mixture of Type I
#'
#' Bayesian nonparametric estimation based on normalized measures driven
#' mixtures for locations.
#'
#' This generic function fits a normalized random measure (NRMI) mixture model
#' for density estimation (James et al. 2009). Specifically, the model assumes
#' a normalized generalized gamma (NGG) prior for the locations (means) of the
#' mixture kernel and a parametric prior for the common smoothing parameter
#' sigma, leading to a semiparametric mixture model.
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
#' @param x Numeric vector. Data set to which the density is fitted.
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
#' @return The function returns a MixNRMI1 object. It is based on a list with the following components:
#' \item{xx}{Numeric vector. Evaluation grid.}
#' \item{qx}{Numeric array. Matrix
#' of dimension \eqn{\texttt{Nx} \times (\texttt{length(probs)} + 1)}{Nx x
#' (length(probs)+1)} with the posterior mean and the desired quantiles input
#' in \code{probs}.}
#' \item{cpo}{Numeric vector of \code{length(x)} with
#' conditional predictive ordinates.}
#' \item{R}{Numeric vector of
#' \code{length(Nit*(1-Pbi))} with the number of mixtures components
#' (clusters).}
#' \item{S}{Numeric vector of \code{length(Nit*(1-Pbi))} with the
#' values of common standard deviation sigma.}
#' \item{U}{Numeric vector of
#' \code{length(Nit*(1-Pbi))} with the values of the latent variable U.}
#' \item{Allocs}{List of \code{length(Nit*(1-Pbi))} with the clustering
#' allocations.}
#' \item{means}{List of \code{length(Nit*(1-Pbi))} with the
#' cluster means (locations). Only if extras = TRUE.}
#' \item{weights}{List of
#' \code{length(Nit*(1-Pbi))} with the mixture weights. Only if extras = TRUE.}
#' \item{Js}{List of \code{length(Nit*(1-Pbi))} with the unnormalized weights
#' (jump sizes). Only if extras = TRUE.}
#' \item{Nm}{Integer constant. Number of
#' jumps of the continuous component of the unnormalized process.}
#' \item{Nx}{Integer constant. Number of grid points for the evaluation of the
#' density estimate.}
#' \item{Nit}{Integer constant. Number of MCMC iterations.}
#' \item{Pbi}{Numeric constant. Burn-in period proportion of \code{Nit}.}
#' \item{procTime}{Numeric vector with execution time provided by
#' \code{proc.time} function.}
#' \item{distr.k}{Integer corresponding to the kernel chosen for the mixture}
#' \item{data}{Data used for the fit}
#' \item{NRMI_params}{A named list with the parameters of the NRMI process}
#' @section Warning : The function is computing intensive. Be patient.
#' @author Barrios, E., Kon Kam King, G., Lijoi, A., Nieto-Barajas, L.E. and Prüenster, I.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}, \code{\link{multMixNRMI1}}
#' @references 1.- Barrios, E., Lijoi, A., Nieto-Barajas, L. E. and Prünster,
#' I. (2013). Modeling with Normalized Random Measure Mixture Models.
#' Statistical Science. Vol. 28, No. 3, 313-334.
#'
#' 2.- James, L.F., Lijoi, A. and Prünster, I. (2009). Posterior analysis for
#' normalized random measure with independent increments. Scand. J. Statist 36,
#' 76-97.
#' @keywords distribution models nonparametrics
#' @examples
#'
#' ### Example 1
#' \dontrun{
#' # Data
#' data(acidity)
#' x <- acidity
#' # Fitting the model under default specifications
#' out <- MixNRMI1(x)
#' # Plotting density estimate + 95% credible interval
#' plot(out)
#' ### Example 2
#' set.seed(150520)
#' data(enzyme)
#' x <- enzyme
#' Enzyme1.out <- MixNRMI1(x,
#'   Alpha = 1, Kappa = 0.007, Gama = 0.5,
#'   distr.k = "gamma", distr.p0 = "gamma",
#'   asigma = 1, bsigma = 1, Meps = 0.005,
#'   Nit = 5000, Pbi = 0.2
#' )
#' attach(Enzyme1.out)
#' # Plotting density estimate + 95% credible interval
#' plot(Enzyme1.out)
#' # Plotting number of clusters
#' par(mfrow = c(2, 1))
#' plot(R, type = "l", main = "Trace of R")
#' hist(R, breaks = min(R - 0.5):max(R + 0.5), probability = TRUE)
#' # Plotting sigma
#' par(mfrow = c(2, 1))
#' plot(S, type = "l", main = "Trace of sigma")
#' hist(S, nclass = 20, probability = TRUE, main = "Histogram of sigma")
#' # Plotting u
#' par(mfrow = c(2, 1))
#' plot(U, type = "l", main = "Trace of U")
#' hist(U, nclass = 20, probability = TRUE, main = "Histogram of U")
#' # Plotting cpo
#' par(mfrow = c(2, 1))
#' plot(cpo, main = "Scatter plot of CPO's")
#' boxplot(cpo, horizontal = TRUE, main = "Boxplot of CPO's")
#' print(paste("Average log(CPO)=", round(mean(log(cpo)), 4)))
#' print(paste("Median log(CPO)=", round(median(log(cpo)), 4)))
#' detach()
#' }
#'
#' ### Example 3
#' ## Do not run
#' # set.seed(150520)
#' # data(galaxy)
#' # x <- galaxy
#' #  Galaxy1.out <- MixNRMI1(x, Alpha = 1, Kappa = 0.015, Gama = 0.5,
#' #                          distr.k = "normal", distr.p0 = "gamma",
#' #                          asigma = 1, bsigma = 1, delta = 7, Meps=0.005,
#' #                          Nit = 5000, Pbi = 0.2)
#'
#' # The output of this run is already loaded in the package
#' # To show results run the following
#' # Data
#' data(galaxy)
#' x <- galaxy
#' data(Galaxy1.out)
#' attach(Galaxy1.out)
#' # Plotting density estimate + 95% credible interval
#' plot(Galaxy1.out)
#' # Plotting number of clusters
#' par(mfrow = c(2, 1))
#' plot(R, type = "l", main = "Trace of R")
#' hist(R, breaks = min(R - 0.5):max(R + 0.5), probability = TRUE)
#' # Plotting sigma
#' par(mfrow = c(2, 1))
#' plot(S, type = "l", main = "Trace of sigma")
#' hist(S, nclass = 20, probability = TRUE, main = "Histogram of sigma")
#' # Plotting u
#' par(mfrow = c(2, 1))
#' plot(U, type = "l", main = "Trace of U")
#' hist(U, nclass = 20, probability = TRUE, main = "Histogram of U")
#' # Plotting cpo
#' par(mfrow = c(2, 1))
#' plot(cpo, main = "Scatter plot of CPO's")
#' boxplot(cpo, horizontal = TRUE, main = "Boxplot of CPO's")
#' print(paste("Average log(CPO)=", round(mean(log(cpo)), 4)))
#' print(paste("Median log(CPO)=", round(median(log(cpo)), 4)))
#' detach()
#' @export MixNRMI1
MixNRMI1 <-
  function(x, probs = c(0.025, 0.5, 0.975), Alpha = 1, Kappa = 0,
           Gama = 0.4, distr.k = "normal", distr.p0 = 1, asigma = 0.5, bsigma = 0.5,
           delta_S = 3, delta_U = 2, Meps = 0.01, Nx = 150, Nit = 1500,
           Pbi = 0.1, epsilon = NULL, printtime = TRUE, extras = TRUE, adaptive = FALSE) {
    if (is.null(distr.k)) {
      stop("Argument distr.k is NULL. Should be provided. See help for details.")
    }
    if (is.null(distr.p0)) {
      stop("Argument distr.p0 is NULL. Should be provided. See help for details.")
    }
    distr.k <- process_dist_name(distr.k)
    distr.p0 <- process_dist_name(distr.p0)
    tInit <- proc.time()
    n <- length(x)
    y <- x
    xsort <- sort(x)
    y[seq(n / 2)] <- mean(xsort[seq(n / 2)])
    y[-seq(n / 2)] <- mean(xsort[-seq(n / 2)])
    u <- 1
    sigma <- 1
    if (is.null(epsilon)) {
      epsilon <- sd(x) / 4
    }
    xx <- seq(min(x) - epsilon, max(x) + epsilon, length = Nx)
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
    mu.p0 <- mean(x)
    sigma.p0 <- sd(x)
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
      ystar <- gs4(ystar, x, idx,
        distr.k = distr.k, sigma.k = sigma,
        distr.p0 = distr.p0, mu.p0 = mu.p0, sigma.p0 = sigma.p0
      )
      Jstar <- rgamma(r, nstar - Gama, Kappa + u)
      Tau <- c(TauiC, ystar)
      J <- c(JiC, Jstar)
      tt <- gsHP(ystar, r, distr.p0)
      mu.p0 <- tt$mu.py0
      sigma.p0 <- tt$sigma.py0
      y <- fcondYXA(x, distr = distr.k, Tau = Tau, J = J, sigma = sigma)
      sigma <- gs5(sigma, x, y,
        distr = distr.k, asigma = asigma,
        bsigma = bsigma, delta = delta_S
      )
      Fxx[, j] <- fcondXA(xx,
        distr = distr.k, Tau = Tau, J = J,
        sigma = sigma
      )
      fx[, j] <- fcondXA(x,
        distr = distr.k, Tau = Tau, J = J,
        sigma = sigma
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
      Pbi = Pbi, procTime = procTime, distr.k = distr.k, data = x,
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



#' Plot the density estimate and the 95\% credible interval
#'
#' The density estimate is the mean posterior density computed on the data
#' points.
#'
#'
#' @param x A fitted object of class NRMI1
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#' @return A graph with the density estimate, the 95\% credible interval and a
#' histogram of the data
#' @export
#' @examples
#'
#' ## Example for non censored data
#'
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' plot(out)
#'
#' ## Example for censored data
#'
#' data(salinity)
#' out <- MixNRMI1cens(salinity$left, salinity$right, Nit = 50)
#' plot(out)
plot.NRMI1 <- function(x, ...) {
  if (is_censored(x$data)) {
    plotfit_censored(x)
  } else {
    plotfit_noncensored(x)
  }
}

#' S3 method for class 'MixNRMI1'
#'
#' @param x A fitted object of class NRMI1
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return A visualization of the important information about the object
#' @export
#'
#' @examples
#'
#' ## Example for non censored data
#'
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' print(out)
#'
#' ## Example for censored data
#'
#' data(salinity)
#' out <- MixNRMI1cens(salinity$left, salinity$right, Nit = 50)
#' print(out)
print.NRMI1 <- function(x, ...) {
  kernel_name <- tolower(give_kernel_name(x$distr.k))
  writeLines(paste("Fit of a semiparametric", kernel_name, "mixture model on", length(x$data), "data points.\nThe MCMC algorithm was run for", x$Nit, "iterations with", 100 * x$Pbi, "% discarded for burn-in."))
}

#' S3 method for class 'MixNRMI1'
#'
#' @param object A fitted object of class NRMI1
#' @param number_of_clusters Whether to compute the optimal number of clusters, which can be a time-consuming operation (see \code{\link{compute_optimal_clustering}})
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return Prints out the text for the summary S3 methods
#' @export
#'
#' @examples
#'
#' ## Example for non censored data
#'
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' summary(out)
summary.NRMI1 <- function(object, number_of_clusters = FALSE, ...) {
  kernel_name <- tolower(give_kernel_name(object$distr.k))
  kernel_comment <- paste("A semiparametric", kernel_name, "mixture model was used.")
  NRMI_comment <- paste("Density estimation using a", comment_on_NRMI_type(object$NRMI_params))
  summarytext(object, kernel_comment, NRMI_comment, number_of_clusters = number_of_clusters)
}

#' Extract the Conditional Predictive Ordinates (CPOs) from a fitted object
#'
#' @param object A fit obtained through from the functions MixNRMI1/MixNRMI1cens
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return A vector of Conditional Predictive Ordinates (CPOs)
#' @export
#'
#' @examples
#' data(acidity)
#' out <- MixNRMI1(acidity, Nit = 50)
#' cpo(out)
cpo.NRMI1 <- function(object, ...) {
  return(object$cpo)
}
