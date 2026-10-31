#' Simulate and Compare Index Scale on Structured vs Noisy Data
#'
#' Performs simulations to compute a projection pursuit index on structured
#' (sampled) data and on random noise, allowing a comparison of index scale across contexts.
#'
#' @param data A data frame or tibble with at least two numeric columns.
#' @param index_fun A function returning a numeric scalar index. It may accept
#' either a 2-column matrix/data frame, or two numeric vectors (`x`, `y`).
#' @param noise_fun Optional function for generating noise data. It should take
#' `n_obs` as input and return a 2-column matrix or data frame. By default,
#' standard bivariate Gaussian noise is used.
#' @param n_sim Integer. Number of simulations. Default is 100.
#' @param n_obs Integer. Number of observations per simulation. Default is 500.
#' @param index_args A named list of additional arguments passed to `index_fun`.
#' @param seed Optional integer seed for reproducibility.
#' @param workers Integer. Number of parallel workers to use. Default is 1.
#'
#' @return A tibble with columns:
#' - `simulation`: simulation number
#' - `var_i`, `var_j`: variable names
#' - `var_pair`: pair name as a string
#' - `sigma`: `"structure"` for structured data, `"noise"` for noisy data
#' - `index`: index value returned by `index_fun`
#'
#' @examples
#' ppi_scale(
#'   data_gen("polynomial", degree = 3),
#'   cassowaryr::sc_stringy,
#'   n_sim = 2,
#'   index_args = list(binner = "hex"),
#'   workers = 2
#' )
#'
#' ppi_scale(
#'   data_gen("polynomial", degree = 3),
#'   scag_index("stringy"),
#'   n_sim = 2
#' )
#' 
#' @export
ppi_scale <- function(data,
                      index_fun,
                      noise_fun = NULL,
                      n_sim = 100,
                      n_obs = 500,
                      index_args = list(),
                      seed = NULL,
                      workers = 1) {
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  
  stopifnot(
    ncol(data) >= 2, nrow(data) >= 2,
    is.function(index_fun),
    is.list(index_args),
    is.numeric(workers), workers >= 1
  )
  
  if (is.null(noise_fun)) {
    noise_fun <- function(n_obs) {
      cbind(
        scale(stats::rnorm(n_obs))[, 1],
        scale(stats::rnorm(n_obs))[, 1]
      )
    }
  }
  
  stopifnot(is.function(noise_fun))
  
  workers <- as.integer(workers)
  if (!is.null(seed)) set.seed(seed)
  
  call_index_fun <- function(mat) {
    mat <- as.matrix(mat)
    
    for (args in list(list(mat), list(mat[, 1], mat[, 2]))) {
      out <- tryCatch(do.call(index_fun, c(args, index_args)),
                      error = function(e) NULL)
      if (is.numeric(out) && length(out) == 1) return(out)
    }
    
    NA_real_
  }
  
  col_pairs <- utils::combn(seq_along(data), 2, simplify = FALSE)
  
  with(future::plan(future::multisession, workers = workers), local = TRUE)
  
  results <- furrr::future_map_dfr(seq_len(n_sim), function(sim) {
    
    purrr::map_dfr(col_pairs, function(pair) {
      i <- pair[1]
      j <- pair[2]
      
      x_struct <- data[[i]]
      y_struct <- data[[j]]
      mat1 <- cbind(x_struct, y_struct)
      index_0 <- call_index_fun(mat1)
      
      mat2 <- as.matrix(noise_fun(n_obs))
      index_1 <- call_index_fun(mat2)
      
      var_names <- names(data)[pair]
      
      tibble::tibble(
        simulation = sim,
        var_i = var_names[1],
        var_j = var_names[2],
        var_pair = paste0(var_names[1], "-", var_names[2]),
        sigma = c("structure", "noise"),
        index = c(index_0, index_1)
      )
    })
  }, .options = furrr::furrr_options(seed = TRUE), .progress = TRUE)
  
  results
}
