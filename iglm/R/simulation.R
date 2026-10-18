#' @title Simulate Responses and Connections
#'
#' @description
#' Simulate responses and connections.
#'
#' @param formula A model `formula` object. The left-hand side should be the
#'   name of a `iglm.data` object available in the calling environment.
#'   See \code{\link{iglm-terms}} for details on specifying the right-hand side terms.
#' @param coef Numeric vector containing the coefficient values for the structural
#'   (non-degrees) terms defined in the `formula`.
#' @param coef_degrees Numeric vector specifying the degrees coefficient
#'   values (expansiveness/attractiveness). This is required \strong{only if} the
#'   `formula` includes degrees terms. Its length must be `n_units` (for
#'   undirected networks) or `2 * n_units` (for directed networks), where
#'   `n_units` is determined from the `iglm.data` object in the formula.
#' @param sampler An object of class `sampler.iglm` (created by
#'   `sampler.iglm()`) specifying the MCMC sampling parameters. This includes
#'   the number of simulations (`n_simulation`), burn-in iterations (`n_burn_in`),
#'   initialization settings (`init_empty`), and component sampler settings
#'   (`sampler_x`, `sampler_y`, etc.).
#'   If `NULL` (default), default settings from `sampler.iglm()` are used.
#' @param only_stats (logical). If \code{TRUE} (default, consistent with the usage signature), the
#'   function returns only the matrix of features calculated
#'   for each simulation. The full simulated \code{iglm.data} objects are discarded to minimize
#'   memory usage. If \code{FALSE}, the complete simulated \code{iglm.data} objects are created
#'   and returned within the \code{samples} component of the output list.
#' @param display_progress Logical. If `TRUE`, progress messages or a progress
#'   bar (depending on the backend implementation) are displayed during simulation.
#'   Default is `FALSE`.
#' @param basis An optional \code{\link{iglm.data}} object to serve as the basis (initial state)
#'   for the simulation. If provided, the simulation starts from the state (\code{x_attribute},
#'   \code{y_attribute}, \code{z_network}) defined in this object.
#'   The `basis` object must be consistent with the model data referenced in `formula`
#'   (same number of units, directedness, and attribute types). All structural specifications
#'   (terms, neighborhood, overlap, and fixed variable flags) are taken from the `formula` data object.
#'   If `NULL` (default), the initial state is taken from the `iglm.data` object
#'   referenced in the `formula`.
#' @param offset_nonoverlap Numeric scalar value passed to the C++ simulator.
#'   This value is typically added to the linear predictor for dyads that are
#'   \strong{not} part of the 'overlap' set defined in the `iglm.data` object, potentially
#'   modifying tie probabilities outside the primary neighborhood. Default is `0`.
#' @param cluster Optional parallel cluster object created, for example, by
#'   \code{parallel::makeCluster}. If provided and valid, the function performs a
#'   single burn-in simulation on the main R process, then distributes the
#'   remaining `n_simulation` tasks across the cluster workers using
#'   \code{parallel::parLapply}. The master seed is offset
#'   for each worker to ensure different random streams. If `NULL` (default),
#'   all simulations are run sequentially in the main R process.
#' @param fix_x Optional logical override indicating if `x_attribute` should be held fixed during simulation.
#'   If `NULL` (default), the setting from the model `iglm.data` object is used.
#' @param fix_z Optional logical override indicating if `z_network` should be held fixed during simulation.
#'   If `NULL` (default), the setting from the model `iglm.data` object is used.
#'
#' @details
#' \strong{Parallel Execution:} When a `cluster` object is provided, the simulation
#' process is adapted:
#' \enumerate{
#'   \item A single simulation run (including burn-in specified by `sampler$n_burn_in`)
#'     is performed on the master node to obtain a starting state for the parallel chains.
#'   \item The total number of requested simulations (`sampler$n_simulation`) is divided
#'     among the cluster workers.
#'   \item \code{parallel::parLapply} is used to run simulations on each worker.
#'     Each worker starts from the state obtained after the initial burn-in, performs
#'     \strong{zero} additional burn-in (`n_burn_in = 0` passed to workers), and generates
#'     its assigned share of the simulations. Component sampler seeds are offset
#'     based on the worker ID to ensure pseudo-independent random number streams.
#'   \item Results (simulated objects or statistics) from all workers are collected
#'     and combined.
#' }
#' This approach ensures that the initial burn-in phase happens only once, saving time.
#'
#' @return A list containing one or two components (depending on `only_stats`):
#' \describe{
#'   \item{`samples`}{If `only_stats = FALSE`, this is a list of length
#'     `sampler$n_simulation` where each element is an \code{\link{iglm.data}} object
#'     representing one simulated draw from the model. The list has the S3 class
#'     `"iglm.data.list"`. If `only_stats = TRUE`, this component is omitted.}
#'   \item{`stats`}{A numeric matrix with `sampler$n_simulation` rows and
#'     `length(coef)` columns. Each row contains the features
#'     (corresponding to the model terms in `formula`) calculated for one
#'     simulation draw. Column names are set to match the term names.}
#' }
#'
#' @section Errors:
#' The function stops with an error if:
#' \itemize{
#'   \item The length of `coef` does not match the number of terms derived from `formula`.
#'   \item Formula preprocessing fails.
#'   \item The `sampler` object is not of class \code{\link{sampler.iglm}}.
#'   \item The C++ backend `xyz_simulate_cpp` encounters an error.
#' }
#'
#' @seealso \code{\link{iglm}} for creating the model object,
#'   \code{\link{sampler.iglm}} for creating the sampler object,
#'   \code{\link{iglm.data}} for the data object structure.
#'
#' @export
#' @importFrom parallel parLapply
simulate_iglm <- function(formula,
                          basis = NULL,
                          coef, coef_degrees = NULL,
                          sampler = NULL,
                          only_stats = TRUE,
                          display_progress = FALSE,
                          offset_nonoverlap = 0,
                          cluster = NULL,
                          fix_x = NULL,
                          fix_z = NULL) {
  if (is.null(sampler)) {
    sampler <- sampler.iglm()
    # if no specifications of the sampler are provided use the default one
  }


  if (!inherits(sampler, "sampler.iglm")) {
    sampler <- sampler.iglm()
  }
  # Search for all things in the environment of the formula
  # attr(formula, ".Environment")
  preprocessed <- formula_preprocess(formula)
  data_obj <- preprocessed$data_object

  if (!is.null(basis)) {
    if (!inherits(basis, "iglm.data")) {
      stop("basis must be an object of class iglm.data")
    }
    if (basis$directed != data_obj$directed) {
      stop(sprintf(
        "The 'basis' object must have the same directedness as the model data in 'formula' (expected directed = %s, got directed = %s).",
        data_obj$directed, basis$directed
      ))
    }
    if (basis$n_units != data_obj$n_units) {
      stop(sprintf(
        "The 'basis' object must have the same number of units as the model data in 'formula' (expected n_units = %d, got n_units = %d).",
        data_obj$n_units, basis$n_units
      ))
    }
    if (basis$family_x != data_obj$family_x) {
      stop(sprintf(
        "The 'basis' object must have the same family_x as the model data in 'formula' (expected family_x = '%s', got family_x = '%s').",
        data_obj$family_x, basis$family_x
      ))
    }
    if (basis$family_y != data_obj$family_y) {
      stop(sprintf(
        "The 'basis' object must have the same family_y as the model data in 'formula' (expected family_y = '%s', got family_y = '%s').",
        data_obj$family_y, basis$family_y
      ))
    }
  }

  init_x <- if (!is.null(basis)) basis$x_attribute else data_obj$x_attribute
  init_y <- if (!is.null(basis)) basis$y_attribute else data_obj$y_attribute
  init_z <- if (!is.null(basis)) basis$z_network else data_obj$z_network
  sim_fix_x <- if (!is.null(fix_x)) fix_x else data_obj$fix_x
  sim_fix_z <- if (!is.null(fix_z)) fix_z else data_obj$fix_z

  if (!is.logical(sim_fix_x) || length(sim_fix_x) != 1 || is.na(sim_fix_x)) {
    stop("'fix_x' must be a single non-missing logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is.logical(sim_fix_z) || length(sim_fix_z) != 1 || is.na(sim_fix_z)) {
    stop("'fix_z' must be a single non-missing logical value (TRUE or FALSE).", call. = FALSE)
  }

  degrees <- preprocessed$includes_degrees
  n_units <- data_obj$n_units
  if (length(coef) != length(preprocessed$term_names)) {
    stop("Wrong number of coefficients for the wanted terms.", call. = FALSE)
  }

  if (is.null(coef_degrees)) {
    coef_degrees <- numeric(0)
  }

  if (!is_cluster_active(cluster)) {
    cluster <- NULL
  }
  if (is.null(cluster)) {
    res <- xyz_simulate_cpp(
      coef = coef, coef_degrees = coef_degrees,
      terms = preprocessed$term_names,
      n_units = n_units,
      x_attribute = init_x,
      y_attribute = init_y,
      z_network = init_z,
      family_x = data_obj$family_x,
      family_y = data_obj$family_y,
      attr_x_scale = data_obj$scale_x,
      attr_y_scale = data_obj$scale_y,
      init_empty = sampler$init_empty,
      nonoverlap_random = !data_obj$fix_z_alocal,
      neighborhood = data_obj$neighborhood,
      overlap = data_obj$overlap,
      directed = data_obj$directed,
      data_list = preprocessed$data_list,
      type_list = preprocessed$type_list,
      n_burn_in = sampler$n_burn_in,
      seed = sampler$seed,
      n_proposals_x = sampler$sampler_x$n_proposals,
      n_proposals_y = sampler$sampler_y$n_proposals,
      n_proposals_z = sampler$sampler_z$n_proposals,
      n_simulation = sampler$n_simulation,
      only_stats = only_stats,
      display_progress = display_progress,
      degrees = degrees,
      offset_nonoverlap = offset_nonoverlap,
      fix_x = sim_fix_x,
      fix_z = sim_fix_z,
      tnt = sampler$sampler_z$tnt
    )
  } else {
    if (display_progress) {
      cat("Starting with burn-in\n")
    }

    res_burn_in <- xyz_simulate_cpp(
      coef = coef,
      coef_degrees = coef_degrees,
      terms = preprocessed$term_names,
      n_units = n_units,
      x_attribute = init_x,
      y_attribute = init_y,
      z_network = init_z,
      init_empty = sampler$init_empty,
      neighborhood = data_obj$neighborhood,
      family_x = data_obj$family_x,
      family_y = data_obj$family_y,
      attr_x_scale = data_obj$scale_x,
      attr_y_scale = data_obj$scale_y,
      overlap = data_obj$overlap,
      directed = data_obj$directed,
      data_list = preprocessed$data_list,
      type_list = preprocessed$type_list,
      n_burn_in = sampler$n_burn_in,
      seed = sampler$seed,
      n_proposals_x = sampler$sampler_x$n_proposals,
      n_proposals_y = sampler$sampler_y$n_proposals,
      n_proposals_z = sampler$sampler_z$n_proposals,
      n_simulation = 1,
      only_stats = FALSE,
      nonoverlap_random = !data_obj$fix_z_alocal,
      display_progress = display_progress,
      degrees = degrees,
      offset_nonoverlap = offset_nonoverlap,
      fix_x = sim_fix_x,
      fix_z = sim_fix_z,
      tnt = sampler$sampler_z$tnt
    )
    res_burnin <- XYZ_to_R(
      x_attribute = res_burn_in$simulation_attributes_x[[1]],
      y_attribute = res_burn_in$simulation_attributes_y[[1]],
      z_network = res_burn_in$simulation_networks_z[[1]],
      n_units = n_units, return_adj_mat = FALSE
    )
    # tmp_split = split(1:(round(sampler$n_simulation/length(cluster))*length(cluster)), 1:length(cluster))
    tmp_split <- suppressWarnings(split(1:sampler$n_simulation, seq_along(cluster)))

    if (display_progress) {
      cat("Starting with parallel simulations")
    }

    res_parallel <- parLapply(
      cl = cluster, X = tmp_split, fun = function(x, preprocessed, n_units, coef,
                                                  coef_degrees, degrees, sampler,
                                                  res_burnin, offset_nonoverlap,
                                                  data_obj, sim_fix_x, sim_fix_z) {
        xyz_simulate_cpp(
          coef = coef, coef_degrees = coef_degrees,
          terms = preprocessed$term_names,
          n_units = n_units,
          family_x = data_obj$family_x,
          family_y = data_obj$family_y,
          attr_x_scale = data_obj$scale_x,
          attr_y_scale = data_obj$scale_y,
          x_attribute = res_burnin$x_attribute,
          y_attribute = res_burnin$y_attribute,
          z_network = res_burnin$z_network,
          init_empty = FALSE,
          neighborhood = data_obj$neighborhood,
          overlap = data_obj$overlap,
          nonoverlap_random = !data_obj$fix_z_alocal,
          directed = data_obj$directed,
          data_list = preprocessed$data_list,
          type_list = preprocessed$type_list,
          n_burn_in = 0L,
          seed = sampler$seed + min(x),
          n_proposals_x = sampler$sampler_x$n_proposals,
          n_proposals_y = sampler$sampler_y$n_proposals,
          n_proposals_z = sampler$sampler_z$n_proposals,
          n_simulation = length(x),
          only_stats = only_stats,
          display_progress = FALSE,
          degrees = degrees,
          fix_x = sim_fix_x,
          fix_z = sim_fix_z,
          offset_nonoverlap = offset_nonoverlap,
          tnt = sampler$sampler_z$tnt
        )
      }, preprocessed = preprocessed, n_units = n_units, coef = coef,
      coef_degrees = coef_degrees, degrees = degrees,
      sampler = sampler, res_burnin = res_burnin, offset_nonoverlap = offset_nonoverlap,
      data_obj = data_obj, sim_fix_x = sim_fix_x, sim_fix_z = sim_fix_z
    )

    res <- list()
    res$simulation_attributes_x <- unlist(lapply(res_parallel, function(x) {
      x$simulation_attributes_x
    }), recursive = F)
    res$simulation_attributes_y <- unlist(lapply(res_parallel, function(x) {
      x$simulation_attributes_y
    }), recursive = F)
    res$simulation_networks <- unlist(lapply(res_parallel, function(x) {
      x$simulation_networks_z
    }), recursive = F)
    res$stats <- do.call(rbind, lapply(res_parallel, function(x) {
      x$stats
    }))
  }

  if (only_stats) {
    colnames(res$stats) <- preprocessed$coef_names
    return(list(stats = res$stats))
  }

  tmp <- lapply(
    seq_along(res$simulation_networks),
    function(x) {
      XYZ_to_R(
        x_attribute = res$simulation_attributes_x[[x]],
        y_attribute = res$simulation_attributes_y[[x]],
        z_network = res$simulation_networks[[x]],
        n_units = n_units, return_adj_mat = FALSE
      )
    }
  )
  # browser(  )
  tmp <- lapply(
    seq_along(res$simulation_networks),
    function(x) {
      iglm.data(
        x_attribute = tmp[[x]]$x_attribute,
        y_attribute = tmp[[x]]$y_attribute,
        z_network = tmp[[x]]$z_network,
        directed = data_obj$directed,
        n_units = length(tmp[[x]]$x_attribute),
        family_x = data_obj$family_x,
        family_y = data_obj$family_y,
        scale_x = data_obj$scale_x,
        scale_y = data_obj$scale_y,
        fix_x = data_obj$fix_x,
        fix_z = data_obj$fix_z,
        fix_z_alocal = data_obj$fix_z_alocal,
        label_x = data_obj$label_x,
        label_y = data_obj$label_y,
        label_z = data_obj$label_z,
        return_neighborhood = FALSE
      )
    }
  )
  # browser()
  class(tmp) <- "iglm.data.list"
  attr(tmp, "neighborhood") <- iglm.data.neighborhood(data_obj$neighborhood)
  colnames(res$stats) <- preprocessed$coef_names
  return(list(samples = tmp, stats = res$stats))
}
