#' Estimate the 95th Percentile of a Projection Pursuit Index Under Noise
#'
#' This function estimates the 95th percentile of a projection pursuit index
#' under synthetic noise data.
#'
#' @param index_fun A function that takes either a 2-column matrix or two numeric vectors and returns a scalar index.
#' @param n_sim Integer. Number of index evaluations to simulate. Default is 100.
#' @param n_obs Integer. Number of observations per noise sample. Default is 500.
#' @param noise_type Character. Type of noise to use (e.g., "gaussian", "t_distributed", etc.). Default is "gaussian".
#' @param noise_level Numeric. Controls the scale/spread of the generated noise. Default is 0.01.
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return A single numeric value: the estimated 95th percentile of the index under noise.
#'
#' @examples
#' ppi_noise_threshold(
#'   index_fun = scag_index("stringy"),
#'   noise_type = "cauchy",
#'   noise_level = 0.1,
#'   n_sim = 10,
#'   n_obs = 100
#' )
#' 
#' @export
ppi_noise_threshold <- function(index_fun,
                                 n_sim = 100,
                                 n_obs = 500,
                                 noise_type = "gaussian",
                                 noise_level = 0.01,
                                 seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  
  results <- purrr::map_dbl(seq_len(n_sim), function(i) {
    
    noise1 <- noise_gen(n = n_obs, type = noise_type, level = noise_level)$value
    noise2 <- noise_gen(n = n_obs, type = noise_type, level = noise_level)$value
    mat <- cbind(noise1, noise2)
      
       tryCatch(
        index_fun(mat),
        error = function(e1) {
          tryCatch(
            index_fun(noise1, noise2),
            error = function(e2) {
              warning(sprintf("Index computation failed at sim %d: %s", i, e2$message))
              NA_real_
            }
          )
        }
      )
    }, .progress = TRUE)
  
  stats::quantile(results, 0.95, na.rm = TRUE)
  
  
}
