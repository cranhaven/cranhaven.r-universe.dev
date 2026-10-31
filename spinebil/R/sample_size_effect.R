#' Simulate Effect of Sample Size on a Projection Pursuit Index
#'
#' For a given index function, simulates how the index behaves across a range
#' of sample sizes. By default, standard bivariate Gaussian noise is used,
#' but the user may instead supply a 2-column dataset to sample from.
#'
#' @param index_fun A function returning a numeric scalar index. It may accept
#' either a 2-column matrix/data frame, or two numeric vectors (`x`, `y`).
#' @param sample_sizes Numeric vector (e.g., 100, c(50,100,200), seq(30,200,5)). Default is 100.
#' @param n_sim Integer. Number of simulations per sample size.
#' @param data Optional 2-column data frame, tibble, or matrix to sample from.
#' If `NULL`, standard bivariate Gaussian noise is used.
#' @param csv_file Optional file path to save results as CSV.
#' @param index_args A named list of additional arguments passed to `index_fun`.
#' @param workers Integer. Number of parallel workers to use. Default is 1.
#'
#' @return A tibble with:
#' - `sample_size`: sample size used for each simulation block
#' - `percentile95`: 95th percentile of the index values over simulations
#'
#' @examples
#' \dontrun{
#' ppi_samplesize_effect(
#'   cassowaryr::sc_stringy,
#'   sample_sizes = c(100, 300),
#'   n_sim = 3,
#'   index_args = list(binner = "hex"),
#'   workers = 2
#' )
#'
#' ppi_samplesize_effect(
#'   cassowaryr::sc_skinny,
#'   sample_sizes = c(100, 300),
#'   n_sim = 3,
#'   data = data_gen("polynomial", degree = 2),
#'   workers = 2
#' )
#' }
#' @export
ppi_samplesize_effect <- function(index_fun,
                                  sample_sizes = 100,
                                  n_sim = 100,
                                  data = NULL,
                                  csv_file = NULL,
                                  index_args = list(),
                                  workers = 1) {
  
  if (!is.null(data)) data <- as.data.frame(data)
  workers <- as.integer(workers)
  
  call_index_fun <- function(mat) {
    mat <- as.matrix(mat)
    
    for (args in list(list(mat), list(mat[, 1], mat[, 2]))) {
      out <- tryCatch(do.call(index_fun, c(args, index_args)),
                      error = function(e) NULL)
      if (is.numeric(out) && length(out) == 1) return(out)
    }
    
    NA_real_
  }
  
  sample_data <- function(n) {
    if (is.null(data)) {
      cbind(stats::rnorm(n), stats::rnorm(n))
    } else {
      data[sample(nrow(data), n), 1:2, drop = FALSE]
    }
  }
  
  sims <- expand.grid(
    sample_size = sample_sizes,
    simulation = seq_len(n_sim)
  )
  
  with(future::plan(future::multisession, workers = workers), local = TRUE)
  
  sim_results <- furrr::future_map_dfr(seq_len(nrow(sims)), function(i) {
    n <- sims$sample_size[i]
    mat <- sample_data(n)
    
    tibble::tibble(
      sample_size = n,
      index = call_index_fun(mat)
    )
  }, .options = furrr::furrr_options(seed = TRUE), .progress = TRUE)
  
  results <- sim_results |>
    dplyr::group_by(.data$sample_size) |>
    dplyr::summarise(
      percentile95 = stats::quantile(
        .data$index, 0.95, na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  if (!is.null(csv_file)) {
    dir.create(dirname(csv_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, csv_file, row.names = FALSE)
  }
  
  results
}
