#' Estimate a ruin probability for a finite time horizon
#'
#' \code{ruin_probability} simulates paths for a given risk \code{model} and
#' returns a crude Monte-Carlo estimate of the ruin probability for the finite
#' time horizon.
#'
#' The function uses a parallel computing from the package
#' \code{\link{parallel}} (if \code{parallel} is TRUE). The package sets up
#' \code{\link{RNGkind}} to \code{"L'Ecuyer-CMRG"} for a safe seeding (see
#' \code{\link{nextRNGStream}}) when it is loaded, so that user does not have
#' to take care of seeds / RNGs. Further, the function allows computing the
#' confidence interval, assuming the normal distribution of the ruin
#' probability (thanks to CLT).
#'
#' @param model an S4 object indicating a risk model (e.g.,
#' \linkS4class{CramerLundberg}).
#' @param time_horizon a length one numeric finite vector specifying the time at
#' which the ruin probability should be estimated.
#' @param simulation_number a length one numeric vector giving the number of
#' simulations that should be performed. Default: \code{10000}.
#' @param ci_level a length one numeric vector between 0 and 1 indicating the
#' level of the confidence interval of the ruin probability. Default:
#' \code{0.95}.
#' @param parallel a length one logical vector indicating whether the parallel
#' computing should be used. Default: \code{TRUE}.
#' @param return_paths a length one logical vector indicating whether a list of
#' simulated paths should be returned. Default: \code{FALSE}.
#'
#' @return A list of two elements: a numeric vector of lower bound of CI,
#' estimate, and upper bound of CI of the ruin probability; and optionally the
#' list of simulated paths.
#'
#' @examples
#' model <- CramerLundberg(initial_capital = 0,
#'                         premium_rate = 1,
#'                         claim_poisson_arrival_rate = 1,
#'                         claim_size_generator = rexp,
#'                         claim_size_parameters = list(rate = 1))
#' ruin_probability(model = model,
#'                  time_horizon = 10,
#'                  simulation_number = 100,
#'                  return_paths = TRUE,
#'                  parallel = FALSE)
#'
#' @export
ruin_probability <- function(model,
                             time_horizon,
                             simulation_number = NULL,
                             ci_level = NULL,
                             parallel = NULL,
                             return_paths = NULL) {

  # set default arguments
  #---------------------------------------------------------------------------

  if(is.null(simulation_number))
    simulation_number <- 10000

  if(is.null(ci_level))
    ci_level <- 0.95

  if(is.null(parallel))
    parallel <- TRUE

  if(is.null(return_paths))
    return_paths <- FALSE

  # validate arguments
  #---------------------------------------------------------------------------

  stopifnot(

    isS4(model),

    is.numeric(time_horizon) &&
      length(time_horizon) == 1 &&
      isFALSE(is.na(time_horizon)) &&
      time_horizon > 0 &&
      isFALSE(is.infinite(time_horizon)),

    is.numeric(simulation_number) &&
      length(simulation_number) == 1 &&
      isFALSE(is.na(simulation_number)) &&
      simulation_number > 0,

    is.numeric(ci_level) &&
      length(ci_level) == 1 &&
      isFALSE(is.na(ci_level)) &&
      ci_level >= 0 &&
      ci_level <= 1,

    is.logical(parallel) &&
      isFALSE(is.na(parallel)) &&
      length(parallel) == 1,

    is.logical(return_paths) &&
      isFALSE(is.na(return_paths)) &&
      length(return_paths) == 1

  )

  # simulate
  #---------------------------------------------------------------------------

  if(parallel) {

    # detect the number of cores
    ncores <- parallel::detectCores()

    # parallelize for Windows
    if(.Platform[["OS.type"]] == "windows") {

      # set up a cluster
      cluster <- parallel::makePSOCKcluster(ncores)

      # set a RNG stream
      parallel::clusterSetRNGStream(cl = cluster)

      # export "model" variable to cluster workers
      parallel::clusterExport(cl = cluster,
                              varlist = "model",
                              envir = environment())

      # run simulate_path simulation_number times
      processes <- parallel::parLapply(
        cl = cluster,
        X = rep(time_horizon, simulation_number),
        fun = function(x) simulate_path(
          model = model,
          max_time_horizon = x
        )
      )

      # stop cluster
      parallel::stopCluster(cl = cluster)

      # parallelize for Unix
    } else if(.Platform[["OS.type"]] == "unix") {

      processes <- parallel::mclapply(
        X = rep(time_horizon, simulation_number),
        FUN = function(x) simulate_path(
          model = model,
          max_time_horizon = x
        ),
        mc.set.seed = TRUE,
        mc.cores = ncores
      )

    }

    # change .Random.seed in Global enviroment
    seed <- parallel::nextRNGStream(.Random.seed)
    for(i in seq_len(simulation_number))
      seed <- parallel::nextRNGStream(seed)

    assign(x = ".Random.seed",
           value = seed,
           envir = .GlobalEnv)


  } else {

    processes <- replicate(
      n = simulation_number,
      expr = do.call(simulate_path,
                     list(model = model,
                          max_time_horizon = time_horizon)),
      simplify = FALSE
    )

  }

  # define which processes have been ruined
  ruined <- sapply(processes, function(prc) prc@is_ruined)

  p <- mean(ruined)
  std <- stats::sd(ruined)

  z <- stats::qnorm(0.5 + ci_level / 2)

  rval <- list()

  rval[["ruin_probability"]] <- c(
    lower_bound = p - z * std / sqrt(simulation_number),
    estimate = p,
    upper_bound = p + z * std / sqrt(simulation_number)
  )

  if(return_paths)
    rval[["simulated_paths"]] <- processes

  return(rval)

}
