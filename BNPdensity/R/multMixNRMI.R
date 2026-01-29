#' Multiple chains of MixNRMI1
#'
#' @inheritParams MixNRMI1
#' @param nchains The number of chains to run.
#' @param parallel Whether to run the chains in parallel. Only works on UNIX-like systems as it rests on Fork parallelism
#' @param ncores Number of cores for the parallel run. Defaults to parallel::detectCores(), i.e. the maximum number of cores detected by R on your system.
#'
#' @return a list containing the multiple fits.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}
#' @examples
#'
#' data(acidity)
#' multMixNRMI1(acidity, parallel = TRUE, Nit = 10, ncores = 2)
#' @export multMixNRMI1
multMixNRMI1 <- function(x, probs = c(0.025, 0.5, 0.975), Alpha = 1, Kappa = 0,
                         Gama = 0.4, distr.k = "normal", distr.p0 = "normal", asigma = 0.5, bsigma = 0.5,
                         delta_S = 3, delta_U = 2, Meps = 0.01, Nx = 150, Nit = 1500,
                         Pbi = 0.1, epsilon = NULL, printtime = TRUE, extras = TRUE, adaptive = FALSE,
                         nchains = 4, parallel = TRUE, ncores = parallel::detectCores()) {
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE
  res <- parallel::mclapply(
    X = 1:nchains,
    FUN = function(chainID) {
      MixNRMI1(
        x, probs, Alpha, Kappa,
        Gama, distr.k, distr.p0, asigma, bsigma,
        delta_S, delta_U, Meps, Nx, Nit, Pbi,
        epsilon, printtime, extras, adaptive
      )
    },
    mc.cores = ifelse(test = parallel, yes = ncores, no = 1),
    mc.set.seed = TRUE
  )
  return(structure(res, class = c("multNRMI")))
}





#' Multiple chains of MixNRMI2
#'
#' @inheritParams MixNRMI2
#' @param nchains The number of chains to run.
#' @param parallel Whether to run the chains in parallel. Only works on UNIX-like systems as it rests on Fork parallelism
#' @param ncores Number of cores for the parallel run. Defaults to parallel::detectCores(), i.e. the maximum number of cores detected by R on your system.
#'
#' @return a list containing the multiple fits.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}, \code{\link{multMixNRMI1}}
#' @examples
#'
#' data(acidity)
#' multMixNRMI2(acidity, parallel = TRUE, Nit = 10, ncores = 2)
#' @export multMixNRMI2
multMixNRMI2 <- function(x, probs = c(0.025, 0.5, 0.975), Alpha = 1, Kappa = 0,
                         Gama = 0.4, distr.k = "normal", distr.py0 = "normal", distr.pz0 = "gamma",
                         mu.pz0 = 3, sigma.pz0 = sqrt(10), delta_S = 4, kappa = 2, delta_U = 2,
                         Meps = 0.01, Nx = 150, Nit = 1500, Pbi = 0.1, epsilon = NULL,
                         printtime = TRUE, extras = TRUE, adaptive = FALSE,
                         nchains = 4, parallel = FALSE, ncores = parallel::detectCores()) {
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE

  res <- parallel::mclapply(
    X = 1:nchains,
    FUN = function(chainID) {
      MixNRMI2(
        x, probs, Alpha, Kappa,
        Gama, distr.k, distr.py0, distr.pz0, mu.pz0,
        sigma.pz0, delta_S, kappa, delta_U, Meps,
        Nx, Nit, Pbi, epsilon, printtime, extras,
        adaptive
      )
    },
    mc.cores = ifelse(test = parallel, yes = ncores, no = 1),
    mc.set.seed = TRUE
  )
  return(structure(res, class = c("multNRMI")))
}




#' Multiple chains of MixNRMI1cens
#'
#' @inheritParams MixNRMI1cens
#' @param nchains The number of chains to run.
#' @param parallel Whether to run the chains in parallel. Only works on
#' UNIX-like systems as it rests on Fork parallelism
#' @param ncores Number of cores for the parallel run. Defaults to
#' parallel::detectCores(), i.e. the maximum number of cores detected by R on
#' your system.
#' @return a list containing the multiple fits.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}, \code{\link{multMixNRMI1}}
#' @examples
#'
#' data(salinity)
#' multMixNRMI1cens(salinity$left, salinity$right, parallel = TRUE, Nit = 10, ncores = 2)
#' @export multMixNRMI1cens
multMixNRMI1cens <- function(xleft, xright, probs = c(0.025, 0.5, 0.975), Alpha = 1, Kappa = 0,
                             Gama = 0.4, distr.k = "normal", distr.p0 = "normal", asigma = 0.5, bsigma = 0.5,
                             delta_S = 3, delta_U = 2, Meps = 0.01, Nx = 150, Nit = 1500,
                             Pbi = 0.1, epsilon = NULL, printtime = TRUE, extras = TRUE, adaptive = FALSE,
                             nchains = 4, parallel = TRUE, ncores = parallel::detectCores()) {
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE

  res <- parallel::mclapply(
    X = 1:nchains,
    FUN = function(chainID) {
      MixNRMI1cens(
        xleft, xright, probs, Alpha, Kappa,
        Gama, distr.k, distr.p0, asigma, bsigma,
        delta_S, delta_U, Meps, Nx, Nit, Pbi,
        epsilon, printtime, extras, adaptive
      )
    },
    mc.cores = ifelse(test = parallel, yes = ncores, no = 1),
    mc.set.seed = TRUE
  )
  return(structure(res, class = c("multNRMI")))
}



#' Multiple chains of MixNRMI2cens
#'
#' @inheritParams MixNRMI2cens
#' @param nchains The number of chains to run.
#' @param parallel Whether to run the chains in parallel. Only works on
#' UNIX-like systems as it rests on Fork parallelism
#' @param ncores Number of cores for the parallel run. Defaults to
#' parallel::detectCores(), i.e. the maximum number of cores detected by R on
#' your system.
#' @return a list containing the multiple fits.
#' @seealso \code{\link{MixNRMI2}}, \code{\link{MixNRMI1cens}},
#' \code{\link{MixNRMI2cens}}, \code{\link{multMixNRMI1}}
#' @examples
#'
#' data(salinity)
#' \dontrun{
#' multMixNRMI2cens(salinity$left, salinity$right, parallel = TRUE, Nit = 20, ncores = 2)
#' }
#'
#' @export multMixNRMI2cens
multMixNRMI2cens <- function(xleft, xright, probs = c(0.025, 0.5, 0.975), Alpha = 1,
                             Kappa = 0, Gama = 0.4, distr.k = "normal", distr.py0 = "normal", distr.pz0 = "gamma",
                             mu.pz0 = 3, sigma.pz0 = sqrt(10), delta_S = 4, kappa = 2, delta_U = 2,
                             Meps = 0.01, Nx = 150, Nit = 1500, Pbi = 0.1, epsilon = NULL,
                             printtime = TRUE, extras = TRUE, adaptive = FALSE,
                             nchains = 4, parallel = TRUE, ncores = parallel::detectCores()) {
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE

  res <- parallel::mclapply(
    X = 1:nchains,
    FUN = function(chainID) {
      MixNRMI2cens(
        xleft, xright, probs, Alpha, Kappa,
        Gama, distr.k, distr.py0, distr.pz0, mu.pz0,
        sigma.pz0, delta_S, kappa, delta_U, Meps,
        Nx, Nit, Pbi, epsilon, printtime, extras, adaptive
      )
    },
    mc.cores = ifelse(test = parallel, yes = ncores, no = 1),
    mc.set.seed = TRUE
  )
  return(structure(res, class = c("multNRMI")))
}



#' Convert the output of multMixNRMI into a coda mcmc object
#'
#' @importFrom  coda as.mcmc
#' @param x Output of multMixNRMI.
#' @param ... Further arguments to be passed to specific methods
#' @param thinning_to Final length of the chain after thinning.
#' @param ncores Specify the number of cores to use in the conversion
#' @return a coda::mcmc object
#' @method as.mcmc multNRMI
#' @export
#' @examples
#' data(acidity)
#' out <- multMixNRMI1(acidity, parallel = TRUE, Nit = 10, ncores = 2)
#' coda::as.mcmc(out, ncores = 2)
as.mcmc.multNRMI <- function(x, ..., thinning_to = 1000, ncores = parallel::detectCores()) {
  res <- coda::as.mcmc(lapply(Convert_to_matrix_list(x, thinning_to = thinning_to, ncores = ncores), coda::mcmc))
  class(res) <- c("multNRMI", class(res))
  return(res)
}

#' Plot the density estimate and the 95\% credible interval
#'
#' The density estimate is the mean posterior density computed on the data
#' points.
#'
#'
#' @param x An object of class multNRMI
#' @param ... Further arguments to be passed to generic functions, ignored at the moment
#' @return A graph with the density estimate, the 95\% credible interval.
#' Includes a histogram if the data is non censored.
#' @export
#' @examples
#' \donttest{
#' data(salinity)
#' fit <- multMixNRMI2cens(salinity$left, salinity$right, parallel = TRUE, Nit = 10, ncores = 2)
#' plot(fit)
#' }
plot.multNRMI <- function(x, ...) {
  # This assumes that chains have the same length and can be given equal weight when combining
  res <- x[[1]]
  nchains <- length(x)
  m <- ncol(res$qx)
  res$qx[, 1] <- 1 / nchains * Reduce(f = add, lapply(X = x, FUN = function(x) x$qx[, 1]))
  res$qx[, 2] <- 1 / nchains * Reduce(f = add, lapply(X = x, FUN = function(x) x$qx[, 2]))
  res$qx[, m] <- 1 / nchains * Reduce(f = add, lapply(X = x, FUN = function(x) x$qx[, m]))
  plot(res)
}

#' S3 method for class 'multNRMI'
#'
#' @param x An object of class multNRMI
#' @param ... Further arguments to be passed to generic functions, ignored at the moment
#'
#' @return A visualization of the important information about the object
#' @export
#'
#' @examples
#' \donttest{
#' data(salinity)
#' out <- multMixNRMI2cens(salinity$left, salinity$right, parallel = TRUE, Nit = 10, ncores = 2)
#' print(out)
#' }
print.multNRMI <- function(x, ...) {
  print(x[[1]])
  writeLines(paste(length(x), "independent MCMC chains were run in parallel"))
}

#' S3 method for class 'multNRMI'
#'
#' @param object A fitted object of class NRMI1cens
#' @param number_of_clusters Whether to compute the optimal number of clusters, which can be a time-consuming operation (see \code{\link{compute_optimal_clustering}})
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return Prints out the text for the summary S3 methods
#' @export
#'
#' @examples
#' \donttest{
#' data(salinity)
#' out <- multMixNRMI2cens(salinity$left, salinity$right, parallel = TRUE, Nit = 10, ncores = 2)
#' summary(out)
#' }
summary.multNRMI <- function(object, number_of_clusters = FALSE, ...) {
  kernel_name <- tolower(give_kernel_name(object[[1]]$distr.k))
  NRMI_comment <- paste("Density estimation using a", comment_on_NRMI_type(object[[1]]$NRMI_params))
  kernel_comment <- paste("A nonparametric", kernel_name, "mixture model was used.")
  ndata <- ifelse(is_censored(object[[1]]$data), nrow(object[[1]]$data), length(object[[1]]$data))
  data_comment <- paste("There were", ndata, "data points.")
  n_chains <- length(object)
  MCMC_comment <- paste(n_chains, " MCMC chains were run for ", object[[1]]$Nit, " iterations with ", 100 * object[[1]]$Pbi, "% discarded for burn-in.", sep = "")
  if (number_of_clusters) {
    collected_allocs <- list("Allocs" = Reduce(c, lapply(object, function(x) x$Allocs)))
    estimated_clustering <- compute_optimal_clustering(collected_allocs)
    clustering_comment <- paste("The estimated number of clusters in the data is ", length(unique(estimated_clustering)), ".", sep = "")
  } else {
    clustering_comment <- "To obtain information on the estimated number of clusters, please use summary(object, number_of_clusters = TRUE)."
  }
  writeLines(paste(NRMI_comment, "\n", kernel_comment, "\n", data_comment, "\n", MCMC_comment, "\n", clustering_comment, sep = ""))
}


#' Extract the Conditional Predictive Ordinates (CPOs) from a list of fitted objects
#'
#' This function assumes that all chains have the same size. To allow for different chain sizes, care should be paid to proper weighting.
#'
#' @param object A fit obtained through from the functions MixNRMI1/MixNRMI1cens
#' @param ... Further arguments to be passed to generic function, ignored at the moment
#'
#' @return A vector of Conditional Predictive Ordinates (CPOs)
#' @export
#'
#' @examples
#' data(acidity)
#' out <- multMixNRMI1(acidity, parallel = TRUE, Nit = 10, ncores = 2)
#' cpo(out)
cpo.multNRMI <- function(object, ...) {
  nchains <- length(object)
  inv_cpos_by_chain <- lapply(object, function(x) 1 / x$cpo)
  inv_cpos <- 1 / nchains * Reduce(add, inv_cpos_by_chain)
  return(1 / inv_cpos)
}
