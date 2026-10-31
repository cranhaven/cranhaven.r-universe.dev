#' Simulate and Summarize Projection Pursuit Index (PPI) Values
#' 
#' @param data A data frame or matrix. Must have at least two columns.
#' @param index_fun A function taking two numeric vectors (`x`, `y`) and returning a scalar index.
#' @param n_sim Integer. Number of simulations. Default is 100.
#' @param n_obs Integer. Number of observations to sample in each simulation. Default is 300.
#' 
#' @return A tibble with:
#' - `var_i`, `var_j`: Names of variable pairs
#' - `mean_index`: Mean index value over simulations
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' data <- as.data.frame(data_gen(type = "polynomial", degree = 2))
#' ppi_mean(data, scag_index("stringy"), n_sim = 10)
ppi_mean <- function(data,
                     index_fun,
                     n_sim = 100,
                     n_obs = 300) {
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  stopifnot(ncol(data) >= 2, nrow(data) >= 2)
  
  col_pairs <- utils::combn(seq_along(data), 2, simplify = FALSE)
  
  with(future::plan(future::multisession), local = TRUE)
  
  # Simulate and compute index values across all variable pairs
  all_results <- furrr::future_map_dfr(seq_len(n_sim), function(sim) {
    purrr::map_dfr(col_pairs, function(pair) {
      i <- pair[1]
      j <- pair[2]
      x <- data[[i]]
      y <- data[[j]]
      mat = cbind(x,y)
      
      result <- tryCatch(index_fun(mat), error = function(e) NA_real_)
      
      tibble::tibble(
        sim = sim,
        var_i = names(data)[i],
        var_j = names(data)[j],
        value = result
      )
    })
  }, .options = furrr::furrr_options(seed = TRUE), .progress = TRUE)
  
  # Aggregate mean value for each pair
  all_results |>
    dplyr::group_by(.data$var_i, .data$var_j) |>
    dplyr::summarise(mean_index = mean(.data$value, na.rm = TRUE), .groups = "drop")
}
