#' Pitman-Yor process mixture  of Type I
#'
#' This function calls the PYdensity function from package BNPmix, to allow fitting a Pitman-Yor process mixture to the data.
#'
#' @param x Numeric vector. Data set to which the density is fitted.
#' @param probs Numeric vector. Desired quantiles of the density estimates.
#' @param Alpha  Numeric constant. Total mass of the centering measure. See
#' @param Gama Numeric constant. \eqn{0\leq \texttt{Gama} \leq 1}{0 <= Gama <=
#' 1}.  See details.
#' @param asigma Numeric positive constant. Shape parameter of the gamma prior
#' on the standard deviation of the mixture kernel. Default value suggested by package BNPmix.
#' @param bsigma Numeric positive constant. Rate parameter of the gamma prior
#' on the standard deviation of the mixture kernel. Default value suggested by package BNPmix.
#' @param Nx Integer constant. Number of grid points for the evaluation of the
#' density estimate.
#' @param Nit Integer constant. Number of MCMC iterations.
#' @param Pbi Numeric constant. Burn-in period proportion of Nit.
#' @param epsilon Numeric constant. Extension to the evaluation grid range.
#' See details.
#' @param printtime Logical. If TRUE, prints out the execution time.
#' @param extras Logical. If TRUE, gives additional objects: means and weights
#' @return The function returns a MixPY1 object. It is based on a list with the following components:
#' \item{xx}{Numeric vector. Evaluation grid.}
#' \item{qx}{Numeric array. Matrix
#' of dimension \eqn{\texttt{Nx} \times (\texttt{length(probs)} + 1)}{Nx x
#' (length(probs)+1)} with the posterior mean and the desired quantiles input
#' in \code{probs}.}
#' \item{R}{Numeric vector of
#' \code{length(Nit*(1-Pbi))} with the number of mixtures components
#' (clusters).}
#' \item{S}{Numeric vector of \code{length(Nit*(1-Pbi))} with the
#' values of common standard deviation sigma.}
#' \item{Allocs}{List of \code{length(Nit*(1-Pbi))} with the clustering
#' allocations.}
#' \item{means}{List of \code{length(Nit*(1-Pbi))} with the
#' cluster means (locations). Only if extras = TRUE.}
#' \item{weights}{List of
#' \code{length(Nit*(1-Pbi))} with the mixture weights. Only if extras = TRUE.}
#' \item{Nit}{Integer constant. Number of MCMC iterations.}
#' \item{Pbi}{Numeric constant. Burn-in period proportion of \code{Nit}.}
#' \item{distr.k}{Integer corresponding to the kernel chosen for the mixture. Always 1, since the Pitman-Yor process is only written to work with Gaussian kernels.}
#' \item{data}{Data used for the fit}
#' \item{PY_params}{A named list with the parameters of the Pitman-Yor process}
#'
#' @export
#'
#' @examples
#' # Data
#' data(acidity)
#' x <- acidity
#' # Fitting the model under default specifications
#' out <- MixPY1(x)
#' # Plotting density estimate + 95% credible interval
#' plot(out)
MixPY1 <- function(x, probs = c(0.025, 0.5, 0.975), Alpha = 1, Gama = 0.4, asigma = 2, bsigma = 1 / var(x), Nx = 100, Nit = 1500, Pbi = 0.5, epsilon = NULL, printtime = TRUE, extras = TRUE) {
  if (!requireNamespace("BNPmix", quietly = TRUE)) {
    stop("Package \"BNPmix\" is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  nburn <- floor(Pbi * Nit)
  if (is.null(epsilon)) {
    epsilon <- sd(x) / 4
  }
  xx <- seq(min(x) - epsilon, max(x) + epsilon, length = Nx)

  restmp <- BNPmix::PYdensity(
    y = x,
    mcmc = list(niter = Nit, nburn = nburn, model = "L", print_message = printtime),
    prior = list(strength = Alpha, discount = Gama, a0 = asigma, b0 = 1 / bsigma),
    output = list(grid = xx, out_param = TRUE, out_type = "FULL")
  )

  clust <- restmp$clust + 1 # 1-based indexing
  Allocs <- lapply(X = seq_len(nrow(restmp$clust)), FUN = function(irow) clust[irow, ])

  probs <- sort(probs)

  qx <- as.data.frame(t(apply(X = restmp$density, MARGIN = 2, FUN = quantile, probs = probs)))
  names(qx) <- paste("q", probs, sep = "")
  qx <- cbind(mean = apply(restmp$density, 2, mean), qx)

  res <- list(
    xx = xx, qx = qx, R = unlist(lapply(X = Allocs, FUN = function(x) length(unique(x)))), S = sqrt(restmp$sigma2),
    distr.k = 1, Allocs = Allocs, data = x, Nit = Nit, Pbi = Pbi,
    PY_params = list("Alpha" = Alpha, "Gamma" = Gama)
  )
  if (extras) {
    res$means <- lapply(restmp$mean, FUN = function(x) x[, 1])
    res$weights <- lapply(restmp$probs, FUN = function(x) x[, 1])
  }
  return(structure(res, class = "PY1"))
}

#' Plot the density estimate and the 95\% credible interval
#'
#' @param x A fitted object of class PY1
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return A graph with the density estimate, the 95\% credible interval and a
#' histogram of the data
#' @export
#'
#' @examples
#' data(acidity)
#' out <- MixPY1(acidity, Nit = 50)
#' plot(out)
plot.PY1 <- function(x, ...) {
  plotfit_noncensored(x)
}

#' S3 method for class 'PY1'
#'
#' @param x A fitted object of class PY1
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
#' out <- MixPY1(acidity, Nit = 50)
#' print(out)
print.PY1 <- function(x, ...) {
  kernel_name <- tolower(give_kernel_name(x$distr.k))
  writeLines(paste("Fit of a semiparametric", kernel_name, "mixture model on", length(x$data), "data points.\nThe MCMC algorithm was run for", x$Nit, "iterations with", 100 * x$Pbi, "% discarded for burn-in."))
}

#' S3 method for class 'PY1'
#'
#' @param object A fitted object of class PY1
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
#' out <- MixPY1(acidity, Nit = 50)
#' summary(out)
summary.PY1 <- function(object, number_of_clusters = FALSE, ...) {
  kernel_name <- tolower(give_kernel_name(object$distr.k))
  kernel_comment <- paste("A semiparametric", kernel_name, "mixture model was used.")
  PY_comment <- paste("Density estimation using a Pitman-Yor process, \nwith total mass parameter Alpha =", object$PY_params$Alpha, "and discount parameter Gamma =", object$PY_params$Gamma)
  summarytext(fit = object, kernel_comment = kernel_comment, BNP_process_comment = PY_comment, number_of_clusters = number_of_clusters)
}
