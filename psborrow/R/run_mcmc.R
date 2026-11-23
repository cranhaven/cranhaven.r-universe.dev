# S4 class, constructors and methods for running simulations for multiple scenarios

`%notin%` <- Negate(`%in%`)


#' Run MCMC for multiple scenarios with provided data
#'
#' @param dt a list of \code{matrix} containing simulated time-to-events information
#' @param priorObj an object of class \code{.priorClass} generated in \code{\link{set_prior}}
#' @param n.chains number of parallel chains for the model
#' @param n.adapt number of iterations for adaptation
#' @param n.burn number of iterations discarded as burn-in
#' @param n.iter number of iterations to monitor
#' @param seed the seed of random number generator. Default is the first element of .Random.seed
#' @param path file name for saving the output including folder path
#' @return a \code{data.frame} containing summary statistics of the posterior distribution for each simulation
#'
#' @examples
#' # examples in vignette
#'
#' @export
#' @keywords simulator
run_mcmc <- function(dt, priorObj, n.chains, n.adapt, n.burn, n.iter, seed, path) {

  if (missing(dt)) {
    stop("Please provide a list of simulated time (dt).")
  }

  if (missing(priorObj)) {
    stop("Please provide .priorObj (priorObj).")
  }

  n_mcmc <- valid_mcmc(n.chains, n.adapt, n.burn, n.iter)

  if (missing(seed)) {
    ps_message("Setting up Bayes model... Set seed to ",.Random.seed[1])
    seed = .Random.seed[1]
  } else set.seed(seed)
  seed_list <- seq(seed, seed + length(dt), by = 1)

  flog.debug(cat("seed_list:", seed_list, "number of dataset", length(dt), "\n"))

  res_list <- sapply(seq(1, length(dt), by = 1), function(i){
    seed_i = seed_list[i]

    ps_message(
      "------------------- Running MCMC: #",
      i,
      " of ",
      length(dt),
      " simulated dataset with seed = ",
      seed_i
    )
    add_mcmc(dt = dt[[i]], priorObj = priorObj,
             n.chains = n.chains, n.adapt = n.adapt,
             n.burn = n.burn, n.iter = n.iter, seed = seed_i)
  }) #loop foreach

  sum_list <- lapply(res_list, function(i) {
    cbind(
      "HR" = i[["HR"]],
      "driftHR" = i[["driftHR"]],
      "prior" = i[["prior"]],
      "pred" = i[["pred"]],
      "reject" = i[["summary"]]["reject"],
      "mean_HR" = i[["summary"]]["mean_HR"],
      "sd_HR" = i[["summary"]]["sd_HR"],
      "mean_driftHR" = i[["summary"]]["mean_driftHR"],
      "sd_driftHR" = i[["summary"]]["sd_driftHR"]
    )
  })

  sum_dt <- as.data.frame(
    do.call(rbind, sum_list),
    stringsAsFactors = FALSE
  )

  rownames(sum_dt) <- NULL

  sum_dt$HR <- as.numeric(sum_dt$HR)
  sum_dt$driftHR <- as.numeric(sum_dt$driftHR)
  sum_dt$reject <- as.numeric(sum_dt$reject)
  sum_dt$mean_HR <- as.numeric(sum_dt$mean_HR)
  sum_dt$sd_HR <- as.numeric(sum_dt$sd_HR)
  sum_dt$mean_driftHR <- as.numeric(sum_dt$mean_driftHR)
  sum_dt$sd_driftHR <- as.numeric(sum_dt$sd_driftHR)

  if (missing(path)) {
    ps_message("Samples from the posterior distribution from MCMC are not saved.")
  } else {
    save(sum_dt, file = path)
    ps_message("Results the posterior distribution from MCMC are saved as ", path)
  }

  sum_dt
}



#' Run MCMC for multiple scenarios with provided data with parallel processing
#'
#' @param dt a list of \code{matrix} containing simulated time-to-events information
#' @param priorObj an object of class \code{.priorClass} generated in \code{\link{set_prior}}
#' @param n.chains number of parallel chains for the model
#' @param n.adapt number of iterations for adaptation
#' @param n.burn number of iterations discarded as burn-in
#' @param n.iter number of iterations to monitor
#' @param seed the seed of random number generator. Default is the first element of .Random.seed
#' @param path file name for saving the output including folder path
#' @param n.cores number of processes to parallelize over (default = 2)
#' @return a \code{data.frame} containing summary statistics of the posterior distribution for each simulation
#'
#' @examples
#' # similar to run_mcmc
#'
#'
#' @export
#' @keywords simulator
run_mcmc_p <- function(dt, priorObj, n.chains, n.adapt, n.burn, n.iter, seed, path, n.cores = 2){

  if (missing(dt)) stop("Please provide a list of simulated time (dt).")
  if (missing(priorObj)) stop("Please provide .priorObj (priorObj).")
  n_mcmc <- valid_mcmc(n.chains, n.adapt, n.burn, n.iter)


  if (missing(seed)) {
    ps_message("Running MCMC... Set seed to ", .Random.seed[1])
    seed <- .Random.seed[1]
  } else {
    set.seed(seed)
  }
  seed_list <- seq(seed, seed + length(dt), by = 1)

  flog.debug(
    cat("seed_list:", seed_list, "number of dataset", length(dt), "\n")
  )

  ps_message(n.cores, " clusters are being used")
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)

  # Load psborrow on the clusters
  ## Flag for global options to pass to clusters
  local.psborrow.quiet <- getOption('psborrow.quiet')
  parallel::clusterExport(cl, varlist =  c('local.psborrow.quiet','is_psborrow_dev'), envir = environment())
  parallel::clusterEvalQ(cl, {
    if(is_psborrow_dev()) {
      pkgload::load_all()
    } else {
      library(psborrow)
    }
    options('psborrow.quiet' = local.psborrow.quiet)
  })

  # set i to NULL to avoid CRAN warnings
  i <- NULL

  res_list <- foreach(
    i = seq_len(length(dt)),
    .combine = "c",
    .multicombine = TRUE,
    .packages = c("psborrow"),
    # .export = c("format_number", "format_date_time", "add_direction_data"),
    # .packages = c("tidyverse", "data.table", "dplyr", "rjags"),
    .verbose = FALSE
  ) %dopar% {

    seed_i <- seed_list[i]

    ps_message(
      "------------------- Running MCMC: #",
      i, " of ", length(dt),
      " simulated dataset with seed = ",
      seed_i
    )

    add_mcmc(dt = dt[[i]], priorObj = priorObj,
              n.chains = n.chains, n.adapt = n.adapt,
              n.burn = n.burn, n.iter = n.iter, seed = seed_i)
  }

  parallel::stopCluster(cl)
  sum_list <- lapply(res_list, function(i) {
    cbind(
      "HR" = i[["HR"]],
      "driftHR" = i[["driftHR"]],
      "prior" = i[["prior"]],
      "pred" = i[["pred"]],
      "reject" = i[["summary"]]["reject"],
      "mean_HR" = i[["summary"]]["mean_HR"],
      "sd_HR" = i[["summary"]]["sd_HR"],
      "mean_driftHR" = i[["summary"]]["mean_driftHR"],
      "sd_driftHR" = i[["summary"]]["sd_driftHR"]
    )
  })
  sum_dt <- as.data.frame(do.call(rbind, sum_list), stringsAsFactors = FALSE)
  rownames(sum_dt) <- NULL
  sum_dt$HR <- as.numeric(sum_dt$HR)
  sum_dt$driftHR <- as.numeric(sum_dt$driftHR)
  sum_dt$reject <- as.numeric(sum_dt$reject)
  sum_dt$mean_HR <- as.numeric(sum_dt$mean_HR)
  sum_dt$sd_HR <- as.numeric(sum_dt$sd_HR)
  sum_dt$mean_driftHR <- as.numeric(sum_dt$mean_driftHR)
  sum_dt$sd_driftHR <- as.numeric(sum_dt$sd_driftHR)

  if (missing(path)) {
    ps_message("Samples from the posterior distribution from MCMC are not saved.")
  } else {
    save(sum_dt, file = path)
    ps_message("Results the posterior distribution from MCMC are saved as ", path)
  }
  sum_dt
}
