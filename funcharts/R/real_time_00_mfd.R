#' Get a list of functional data objects each evolving up to
#' an intermediate domain point.
#'
#' @description This function produces a list functional data objects,
#' each evolving up to an intermediate domain point, that can be used to
#' estimate models that allow real-time predictions of incomplete functions,
#' from the current functional domain up to the end of the observation,
#' and to build control charts for real-time  monitoring.
#'
#' It calls the function \code{\link{get_mfd_df}} for each domain point.
#'
#' @param dt
#' See \code{\link{get_mfd_df}}.
#' @param domain
#' See \code{\link{get_mfd_df}}.
#' @param arg
#' See \code{\link{get_mfd_df}}.
#' @param id
#' See \code{\link{get_mfd_df}}.
#' @param variables
#' See \code{\link{get_mfd_df}}.
#' @param n_basis
#' See \code{\link{get_mfd_df}}.
#' @param n_order
#' See \code{\link{get_mfd_df}}.
#' @param basisobj
#' See \code{\link{get_mfd_df}}.
#' @param Lfdobj
#' See \code{\link{get_mfd_df}}.
#' @param lambda
#' See \code{\link{get_mfd_df}}.
#' @param lambda_grid
#' See \code{\link{get_mfd_df}}.
#' @param k_seq
#' A vector of values between 0 and 1, containing the domain points
#' over which functional data are to be evaluated in real time.
#' If the domain is the interval (a,b), for each instant k in the sequence,
#' functions are evaluated in (a,k(b-a)).
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating mfd objects separately for different instants.
#'
#' @return
#' A list of \code{mfd} objects as produced by
#' \code{\link{get_mfd_df}},
#' corresponding to a given instant.
#' @export
#'
#' @seealso \code{\link{get_mfd_df}}
#' @examples
#' library(funcharts)
#'
#' x <- seq(1, 10, length = 25)
#' y11 <- cos(x)
#' y21 <- cos(2 * x)
#' y12 <- sin(x)
#' y22 <- sin(2 * x)
#' df <- data.frame(id = factor(rep(1:2, each = length(x))),
#'                  x = rep(x, times = 2),
#'                  y1 = c(y11, y21),
#'                  y2 = c(y12, y22))
#'
#' mfdobj_list <- get_mfd_df_real_time(dt = df,
#'                                     domain = c(1, 10),
#'                                     arg = "x",
#'                                     id = "id",
#'                                     variables = c("y1", "y2"),
#'                                     lambda = 1e-2)
#'
get_mfd_df_real_time <- function(
  dt,
  domain,
  arg,
  id,
  variables,
  n_basis = 30,
  n_order = 4,
  basisobj = NULL,
  Lfdobj = 2,
  lambda = NULL,
  lambda_grid = 10^seq(-10, 1, length.out = 10),
  k_seq = seq(from = 0.25,
              to = 1,
              length.out = 10),
  ncores = 1) {


  if (min(k_seq) < 0 | max(k_seq) > 1) {
    stop("k_seq values must be between 0 and 1.")
  }

  kk_seq <- domain[1] + k_seq * (domain[2] - domain[1])

  single_k <- function(ii) {
    kk <- kk_seq[ii]
    domain_ii <- c(domain[1], kk)
    dt_ii <- dplyr::filter(dt, get(arg) <= kk)
    nbasis <- max(15, round(n_basis * diff(domain_ii) / diff(domain)))
    get_mfd_df(dt = dt_ii,
               domain = domain_ii,
               arg = arg,
               id = id,
               variables = variables,
               n_basis = nbasis,
               n_order = n_order,
               basisobj = basisobj,
               Lfdobj = Lfdobj,
               lambda = lambda,
               lambda_grid = lambda_grid,
               ncores = 1)
  }
  if (ncores == 1) {
    mfd_list <- lapply(seq_along(kk_seq), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mfd_list <- parallel::mclapply(seq_along(kk_seq), single_k, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, c("kk_seq",
                                    "domain",
                                    "dt",
                                    "arg",
                                    "id",
                                    "variables",
                                    "n_basis",
                                    "n_order",
                                    "basisobj",
                                    "Lfdobj",
                                    "lambda",
                                    "lambda_grid"),
                              envir = environment())
      mfd_list <- parallel::parLapply(cl, seq_along(kk_seq), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mfd_list) <- kk_seq

  mfd_list

}

#' Get a list of functional data objects each evolving up to
#' an intermediate domain point.
#'
#' @description
#' This function produces a list functional data objects,
#' each evolving up to an intermediate domain point, that can be used to
#' estimate models that allow real-time predictions of incomplete functions,
#' from the current functional domain up to the end of the observation,
#' and to build control charts for real-time  monitoring.
#'
#' It calls the function \code{\link{get_mfd_list}} for each domain point.
#'
#' @param data_list
#' See \code{\link{get_mfd_list}}.
#' @param grid
#' See \code{\link{get_mfd_list}}.
#' @param n_basis
#' See \code{\link{get_mfd_list}}.
#' @param n_order
#' See \code{\link{get_mfd_list}}.
#' @param basisobj
#' See \code{\link{get_mfd_list}}.
#' @param Lfdobj
#' See \code{\link{get_mfd_list}}.
#' @param lambda
#' See \code{\link{get_mfd_list}}.
#' @param lambda_grid
#' See \code{\link{get_mfd_df}}.
#' @param k_seq
#' A vector of values between 0 and 1, containing the domain points
#' over which functional data are to be evaluated in real time.
#' If the domain is the interval (a,b), for each instant k in the sequence,
#' functions are evaluated in (a,a+k(b-a)).
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating mfd objects separately for different instants.
#'
#' @return
#' A list of \code{mfd} objects as produced by
#' \code{\link{get_mfd_list}}.
#' @export
#'
#' @seealso \code{\link{get_mfd_list}}
#' @examples
#' library(funcharts)
#' data("air")
#' # Only take first 5 multivariate functional observations from air
#' air_small <- lapply(air, function(x) x[1:5, ])
#' # Consider only 3 domain points: 0.5, 0.75, 1
#' mfdobj <- get_mfd_list_real_time(data_list = air_small,
#'                                  lambda = 1e-2,
#'                                  k_seq = c(0.5, 0.75, 1))
#'
get_mfd_list_real_time <- function(
  data_list,
  grid = NULL,
  n_basis = 30,
  n_order = 4,
  basisobj = NULL,
  Lfdobj = 2,
  lambda = NULL,
  lambda_grid = 10^seq(-10, 1, length.out = 10),
  k_seq = seq(from = 0.2, to = 1, by = 0.1),
  ncores = 1) {

  if (!(is.list(data_list))) {
    stop("data_list must be a list of matrices")
  }
  if (is.null(names(data_list))) {
    stop("data_list must be a named list")
  }
  if (length(unique(lapply(data_list, dim))) > 1) {
    stop("data_list must be a list of matrices all of the same dimensions")
  }
  n_args <- ncol(data_list[[1]])
  if (!is.null(grid) & (length(grid) != n_args)) {
    stop(paste0(
      "grid length, ", length(grid),
      " has not the same length as number of observed ",
      "data per functional observation, ",
      ncol(data_list[[1]])))
  }

  if (is.null(grid)) grid <- seq(0, 1, l = n_args)
  domain <- range(grid)

  if (min(k_seq) < 0 | max(k_seq) > 1) {
    stop("k_seq values must be between 0 and 1.")
  }

  kk_seq <- domain[1] + k_seq * (domain[2] - domain[1])

  single_k <- function(ii) {
    kk <- kk_seq[ii]
    domain_ii <- c(domain[1], kk)
    data_list_ii <- lapply(data_list, function(x) x[, grid <= kk])
    nbasis <- max(15, round(n_basis * diff(domain_ii) / diff(domain)))
    get_mfd_list(data_list_ii,
                 grid = grid[grid <= kk],
                 n_basis = nbasis,
                 n_order = n_order,
                 basisobj = basisobj,
                 Lfdobj = Lfdobj,
                 lambda = lambda,
                 lambda_grid = lambda_grid)
  }

  if (ncores == 1) {
    mfd_list <- lapply(seq_along(kk_seq), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mfd_list <- parallel::mclapply(seq_along(kk_seq), single_k, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, c("kk_seq",
                                    "domain",
                                    "data_list",
                                    "n_basis",
                                    "n_order",
                                    "basisobj",
                                    "Lfdobj",
                                    "lambda",
                                    "lambda_grid",
                                    "grid"),
                              envir = environment())
      mfd_list <- parallel::parLapply(cl, seq_along(kk_seq), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mfd_list) <- kk_seq

  mfd_list

}



#' Get a list of functional data objects each evolving up to
#' an intermediate domain point.
#'
#' @description
#' This function produces a list functional data objects,
#' each evolving up to an intermediate domain point, that can be used to
#' estimate models that allow real-time predictions of incomplete functions,
#' from the current functional domain up to the end of the observation,
#' and to build control charts for real-time  monitoring.
#'
#' It calls the function \code{\link{get_mfd_array}} for each domain point.
#'
#' @param data_array
#' See \code{\link{get_mfd_array}}.
#' @param grid
#' See \code{\link{get_mfd_array}}.
#' @param n_basis
#' See \code{\link{get_mfd_array}}.
#' @param n_order
#' See \code{\link{get_mfd_array}}.
#' @param basisobj
#' See \code{\link{get_mfd_array}}.
#' @param Lfdobj
#' See \code{\link{get_mfd_array}}.
#' @param lambda
#' See \code{\link{get_mfd_array}}.
#' @param lambda_grid
#' See \code{\link{get_mfd_array}}.
#' @param k_seq
#' A vector of values between 0 and 1, containing the domain points
#' over which functional data are to be evaluated in real time.
#' If the domain is the interval (a,b), for each instant k in the sequence,
#' functions are evaluated in (a,k(b-a)).
#' @param ncores
#' If you want parallelization, give the number of cores/threads
#' to be used when creating mfd objects separately for different instants.
#'
#' @return
#' A list of \code{mfd} objects as produced by
#' \code{\link{get_mfd_array}}.
#' @export
#'
#' @seealso \code{\link{get_mfd_array}}
#' @examples
#' library(funcharts)
#' library(fda)
#' data("CanadianWeather")
#' fdobj <- get_mfd_array_real_time(CanadianWeather$dailyAv[, 1:5, 1:2],
#'                                  lambda = 1e-2)
#'
get_mfd_array_real_time <- function(
  data_array,
  grid = NULL,
  n_basis = 30,
  n_order = 4,
  basisobj = NULL,
  Lfdobj = 2,
  lambda = NULL,
  lambda_grid = 10^seq(-10, 1, length.out = 10),
  k_seq = seq(from = 0.25,
              to = 1,
              length.out = 10),
  ncores = 1) {

  if (!(is.array(data_array)) |
      (is.array(data_array) & length(dim(data_array)) != 3)) {
    stop("data_array must be a list of three-dimensional arrays.")
  }

  n_args <- dim(data_array)[1]
  if (!is.null(grid) & (length(grid) != n_args)) {
    stop(paste0(
      "grid length, ", length(grid),
      " has not the same length as number of observed ",
      "data per functional observation, ",
      dim(data_array)[1]))
  }

  if (is.null(grid)) grid <- seq(0, 1, length.out = n_args)
  domain <- range(grid)

  if (min(k_seq) < 0 | max(k_seq) > 1) {
    stop("k_seq values must be between 0 and 1.")
  }

  kk_seq <- domain[1] + k_seq * (domain[2] - domain[1])

  single_k <- function(ii) {
    kk <- kk_seq[ii]
    domain_ii <- c(domain[1], kk)
    nbasis <- max(15, round(n_basis * diff(domain_ii) / diff(domain)))
    data_array_ii <- data_array[grid <= kk, , , drop = FALSE]
    get_mfd_array(data_array_ii,
                  grid = grid[grid <= kk],
                  n_basis = nbasis,
                  n_order = n_order,
                  basisobj = basisobj,
                  Lfdobj = Lfdobj,
                  lambda = lambda,
                  lambda_grid = lambda_grid)
  }

  if (ncores == 1) {
    mfd_list <- lapply(seq_along(kk_seq), single_k)
  } else {
    if (.Platform$OS.type == "unix") {
      mfd_list <- parallel::mclapply(seq_along(kk_seq), single_k, mc.cores = ncores)
    } else {
      cl <- parallel::makeCluster(ncores)
      parallel::clusterExport(cl, c("kk_seq",
                                    "domain",
                                    "data_array",
                                    "n_basis",
                                    "n_order",
                                    "basisobj",
                                    "Lfdobj",
                                    "lambda",
                                    "lambda_grid",
                                    "grid"),
                              envir = environment())
      mfd_list <- parallel::parLapply(cl, seq_along(kk_seq), single_k)
      parallel::stopCluster(cl)
    }
  }

  names(mfd_list) <- kk_seq

  mfd_list

}
