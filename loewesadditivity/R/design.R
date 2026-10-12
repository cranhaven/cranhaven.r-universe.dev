#' Helper function to generate code to run an experiment
#'
#' @param levels_A levels of A used in the combination
#' @param levels_B levels of B used in the combination
#' @param par named vector of model parameters
#' @param n_rep number of total repetitions of experiment
#' @param n_sims number of simulations to run
#' @param noise_par named vector with 'a0' and 'a1' which are used to generate noise for the GIA.
#' @return NULL
#' @details prints out code to copy and paste into \code{R} to simulate the expected coverage of your experiment under your designed hypothesis
#' @export
design_experiment <- function(levels_A = c(0, 1 * 2^(-4:2)),
                              levels_B = c(0, 2 * 2^(-4:2)),
                              par = c("beta_A" = 1,
                                      "beta_B" = 2,
                                      "gamma_A" = .5,
                                      "gamma_B" = .5,
                                      "tau_1" = 3,
                                      "tau_2" = .05),
                              n_rep = 1,
                              n_sims = 100,
                              noise_par = c("a0" = 3,
                                            "a1" = .01)){

  cat(
    paste0("\nlibrary(loewesadditivity)\nlevels_A <- c(",
           paste(levels_A, collapse = ", "),
           ")\n",
           "levels_B <- c(",
           paste(levels_B, collapse = ", "),
           ")\n",
           "par <- c(",
           paste(paste0("  '", names(par), "' = ", par), collapse = ", \n"),
           ")\n",
           "my_grid <- design_grid(levels_A = levels_A, \n  levels_B = levels_B, \n",
           paste0("  n_rep = ", n_rep, ")\n"),
           "## SIMULATE COVERAGE\n",
           "sim_results <- simulate_coverage(",
           paste0("n_sims = ", n_sims, ",\n  "),
           paste0("n_boot = ", 100, ",\n  "),
           "experimental_grid = my_grid, \n  ",
           "model_par = par,\n  ",
           "alpha = .05,\n  ",
           paste0("noise_par = c(", paste0("'", names(noise_par), "' = ", noise_par, collapse = ", \n  "), "))\n"),
           "## LOOK AT RESULTS\n",
           "sim_results\n",
           "## Uncomment below to write the grid to a .csv file you can open in Excel or google spreadsheets\n",
           "#write.csv(sim_results, 'coverage_results.csv')"
    ))



  return(NULL)
}

#' Function to design an experimental grid of combinations
#'
#' @param levels_A levels of A used in the combination
#' @param levels_B levels of B used in the combination
#' @param n_rep number of total repetitions of experiment
#' @return data frame with columns dose_A, dose_B, and GIA for all possible combinations
design_grid <- function(levels_A = c(0, 1 * 2^(-4:2)),
                        levels_B = c(0, 2 * 2^(-4:2)),
                        n_rep = 1){

  base_grid <- expand.grid(dose_A = levels_A,
                           dose_B = levels_B)
  A_seq <- rep(base_grid$dose_A, each = n_rep)
  B_seq <- rep(base_grid$dose_B, each = n_rep)
  df <- data.frame(dose_A = A_seq,
                   dose_B = B_seq,
                   GIA = 0)
  return(df)
}
