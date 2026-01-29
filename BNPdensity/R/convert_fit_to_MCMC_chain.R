Compute_log_likelihood_given_params <- function(fit_, it_retained, parallel, ncores) {
  if (is_censored(fit_$data)) {
    censor_code <- censor_code_rl(fit_$data$left, fit_$data$right)
    censor_code_filters <- lapply(0:3, FUN = function(x) censor_code == x)
    names(censor_code_filters) <- 0:3

    dpred <- function(iter) {
      log(dmixcens(
        xlefts = fit_$data$left,
        xrights = fit_$data$right,
        c_code_filters = censor_code_filters,
        locations = fit_$means[[iter]],
        scales = fit_$sigmas[[iter]],
        weights = fit_$weights[[iter]],
        distr.k = fit_$distr.k
      ))
    }
  } else {
    dpred <- function(iter) {
      log(dmix(fit_$data,
        locations = fit_$means[[iter]],
        scales = fit_$sigmas[[iter]],
        weights = fit_$weights[[iter]],
        distr.k = fit_$distr.k
      ))
    }
  }
  unlist(parallel::mclapply(
    X = it_retained,
    FUN = function(it) sum(dpred(it)),
    mc.cores = ifelse(test = parallel, yes = ncores, no = 1)
  ))
}

Convert_to_matrix_list <- function(fitlist, thinning_to = 1000, parallel = TRUE, ncores = parallel::detectCores()) {
  # number of iterations * number of parameters
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE

  if (is_semiparametric(fitlist[[1]])) {
    fitlist <- lapply(fitlist, function(fit) {
      fit$sigmas <- fill_sigmas(fit)
      fit
    })
  }

  Nit <- length(fitlist[[1]]$means)
  it_retained <- compute_thinning_grid(Nit, thinning_to = thinning_to)

  if (is_semiparametric(fitlist[[1]])) {
    lapply(X = fitlist, function(fit_i) {
      cbind(
        ncomp = fit_i$R[it_retained],
        Sigma = fit_i$S[it_retained],
        Latent_variable = fit_i$U[it_retained],
        log_likelihood = Compute_log_likelihood_given_params(fit_i, it_retained, parallel, ncores)
      )
    })
  } else {
    lapply(X = fitlist, function(fit_i) {
      cbind(
        ncomp = fit_i$R[it_retained],
        Latent_variable = fit_i$U[it_retained],
        log_likelihood = Compute_log_likelihood_given_params(fit_i, it_retained, parallel, ncores)
      )
    })
  }
}

#' Convert the output of multMixNRMI into a coda mcmc object
#'
#' @param fitlist Output of multMixNRMI.
#' @param thinning_to Final length of the chain after thinning.
#' @param ncores Specify the number of cores to use in the conversion
#' @return a coda::mcmc object
convert_to_mcmc <- function(fitlist, thinning_to = 1000, ncores = parallel::detectCores()) {
  coda::as.mcmc.list(coda::as.mcmc(lapply(Convert_to_matrix_list(fitlist, thinning_to = thinning_to, ncores = ncores), coda::mcmc)))
}
