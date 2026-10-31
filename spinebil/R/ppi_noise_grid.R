#' Estimate Noise Thresholds Across Sample Sizes
#'
#' Simulates two-dimensional noise data over a sequence of sample sizes and
#' estimates the 95th percentile of one or more projection pursuit index
#' functions under the noise distribution.
#' 
#' @param index_funs An index function or a list of index functions. Each index
#'   function should take two numeric vectors as its first two arguments, for
#'   example `function(x, y) ...`.
#' @param n_sim Number of noise simulations to run for each sample size.
#' @param n_start Smallest sample size.
#' @param n_end Largest sample size.
#' @param step Step size between sample sizes. Default is `25`.
#' @param settings A named list of settings. Each setting should be a list of
#'   arguments passed to the index functions. Default is
#'   `list(default = list())`.
#' @param seed Random seed used for reproducibility. Default is `1028`.
#' @param workers Number of parallel workers to use. Default is `1`.
#' @param output_file Optional file path for saving the summarized 95th percentile
#'   results as a CSV file. If `NULL`, results are not written to disk.
#' @param index_args Optional named list of index-specific arguments. Each element
#'   should have the same name as an index in `index_funs`.
#' @param noise_fun Optional function for generating noise data. The function
#'   should take the current sample size as input and return a numeric object
#'   with at least two columns. If `NULL`, independent Gaussian noise is used.
#' @param save_raw Logical. If `TRUE`, saves the raw simulated index values before
#'   calculating the 95th percentile. Default is `FALSE`.
#' @param raw_output_file File path for saving raw simulated values when
#'   `save_raw = TRUE`. Default is `"raw_noise_values.csv"`.
#' @param ... Additional arguments passed to all index functions.
#' 
#' @return A tibble with one row for each sample size, setting, and index. The
#'   output contains:
#'   \describe{
#'     \item{sample_size}{The sample size used in the simulation.}
#'     \item{setting}{The setting name.}
#'     \item{index}{The index name.}
#'     \item{p95}{The estimated 95th percentile of the simulated noise values.}
#'     \item{n_valid}{The number of non-missing index values.}
#'     \item{n_sim}{The number of simulations requested.}
#'   }
#'
#' @details
#' The default noise generator produces two independent standard normal variables.
#' Users can provide their own noise generator through `noise_fun`. This is useful
#' for comparing the behaviour of an index under different null distributions,
#' such as uniform noise, heavy-tailed noise, or correlated noise.
#'
#' If `output_file` is supplied, the summarized results are saved after each
#' sample size. If `save_raw = TRUE`, the raw simulation results are also saved
#' before summarisation.
#'
#' @examples
#' \dontrun{
#' # Estimate the 95th percentile for stringy05 under Gaussian noise
#' res <- ppi_noise_grid(
#'   index_funs = cassowaryr::sc_stringy05,
#'   n_sim = 100,
#'   n_start = 50,
#'   n_end = 150,
#'   step = 25
#' )
#'
#'  Named list of index functions
#' res_named <- ppi_noise_grid(
#'   index_funs = list(
#'     stringy05 = cassowaryr::sc_stringy05,
#'     skinny = cassowaryr::sc_skinny
#'   ),
#'   n_sim = 100,
#'   n_start = 50,
#'   n_end = 150,
#'   step = 25
#' )
#' 
#' Custom uniform noise
#' uniform_noise <- function(n) {
#'   cbind(
#'     runif(n, -1, 1),
#'     runif(n, -1, 1)
#'   )
#' }
#'
#' res_uniform <- ppi_noise_grid(
#'   index_funs = list(stringy05 = cassowaryr::sc_stringy05),
#'   n_sim = 100,
#'   n_start = 50,
#'   n_end = 150,
#'   noise_fun = uniform_noise
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
ppi_noise_grid <- function(
    index_funs,
    n_sim,
    n_start,
    n_end,
    step = 25,
    settings = list(default = list()),
    seed = 1028,
    workers = 1,
    output_file = NULL,
    index_args = list(),
    noise_fun = NULL,
    save_raw = FALSE,
    raw_output_file = "raw_noise_values.csv",
    ...
) {
  
  extra_args <- list(...)
  
  if (is.function(index_funs)) {
    index_funs <- list(index = index_funs)
  }
  
  if (!is.list(index_funs)) {
    stop("index_funs should be a function or a list of functions.")
  }
  
  if (is.null(names(index_funs))) {
    names(index_funs) <- paste0("index", seq_along(index_funs))
  }
  
  
  if (is.null(noise_fun)) {
    noise_fun <- function(n_obs) {
      cbind(
        stats::rnorm(n_obs),
        stats::rnorm(n_obs)
      )
    }
  }
  
  sample_sizes <- seq(n_start, n_end, by = step)
  
  if (workers > 1) {
    with(future::plan(future::multisession, workers = workers), local = TRUE)
  } else {
    with(future::plan(future::sequential), local = TRUE)
  }
  
  if (!is.null(output_file)) {
    readr::write_csv(
      tibble::tibble(
        sample_size = numeric(),
        setting = character(),
        index = character(),
        p95 = numeric(),
        n_valid = numeric(),
        n_sim = numeric()
      ),
      output_file
    )
  }
  
  if (save_raw) {
    readr::write_csv(
      tibble::tibble(
        sample_size = numeric(),
        simulation = numeric(),
        seed = numeric(),
        setting = character(),
        index = character(),
        value = numeric()
      ),
      raw_output_file
    )
  }
  
  final_results <- list()
  
  for (n_obs in sample_sizes) {
    
    cat("Running sample size:", n_obs, "\n")
    
    raw_results <- furrr::future_map_dfr(
      seq_len(n_sim),
      function(i) {
        
        seed_i <- seed + i 
        set.seed(seed_i)
        
        mat <- noise_fun(n_obs)
        
        purrr::map_dfr(names(settings), function(setting_name) {
          
          setting_args <- settings[[setting_name]]
          
          purrr::map_dfr(names(index_funs), function(index_name) {
            
            index_fun <- index_funs[[index_name]]
            
            all_args <- c(
              setting_args,
              extra_args,
              index_args[[index_name]]
            )
            
            value <- tryCatch(
              {
                as.numeric(
                  do.call(
                    index_fun,
                    c(list(mat[, 1], mat[, 2]), all_args)
                  )
                )[1]
              },
              error = function(e) NA
            )
            
            tibble::tibble(
              sample_size = n_obs,
              simulation = i,
              seed = seed_i,
              setting = setting_name,
              index = index_name,
              value = value
            )
          })
        })
      },
      .options = furrr::furrr_options(seed = TRUE),
      .progress = TRUE
    )
    
    if (save_raw) {
      readr::write_csv(raw_results, raw_output_file, append = TRUE)
    }
    
    p95_results <- raw_results |>
      dplyr::group_by(
        .data$sample_size,
        .data$setting,
        .data$index
      ) |>
      dplyr::summarise(
        p95 = as.numeric(
          stats::quantile(.data$value, 0.95, na.rm = TRUE)
        ),
        n_valid = sum(!is.na(.data$value)),
        n_sim = n_sim,
        .groups = "drop"
      )
    
    if (!is.null(output_file)) {
      readr::write_csv(p95_results, output_file, append = TRUE)
    }
    
    final_results[[as.character(n_obs)]] <- p95_results
    
    cat("Finished sample size:", n_obs, "\n")
  }
  
  dplyr::bind_rows(final_results)
}
