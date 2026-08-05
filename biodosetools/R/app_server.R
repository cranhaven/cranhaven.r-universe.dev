#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Dicentrics Modules ----

  # Fitting
  mod_fitting_counts_hot_server("fitting_dicent_ui", "dicentrics")
  mod_fitting_results_server("fitting_dicent_ui", "dicentrics")

  ###################

  mod_limits_fit_curve_hot_server("limits_dicent_ui")
  mod_limits_fit_curve_server("limits_dicent_ui", "dicentrics")
  mod_limits_results_server("limits_dicent_ui", "dicentrics")


  # Dose Estimation
  mod_mixed_hot_server("mixed_ui", "dicentrics")
  mod_mixed_case_table_hot_server("mixed_ui", "dicentrics")
  mod_estimation_case_hot_server("estimation_dicent_ui", "dicentrics")
  mod_estimation_fit_curve_hot_server("estimation_dicent_ui")
  mod_estimation_fit_curve_server("estimation_dicent_ui", "dicentrics")
  mod_estimation_results_server("estimation_dicent_ui", "dicentrics")

  #Interlab comparison
  mod_interlab_server("interlab_ui", "dicentrics")
  #Comparison
  #mod_compare_server("compare_ui")

  # Translocations Modules ----

  # Help
  mod_help_chromosome_hot_server("fitting_trans_ui")
  mod_help_chromosome_hot_server("estimation_trans_ui")

  # Fitting
  mod_trans_chromosome_hot_server("fitting_trans_ui")
  genome_factor_fit <- mod_trans_fraction_to_full_genome_server("fitting_trans_ui")
  mod_fitting_counts_hot_server("fitting_trans_ui", "translocations")
  mod_fitting_results_server("fitting_trans_ui", "translocations", genome_factor_fit)

  # Dose Estimation
  mod_trans_chromosome_hot_server("estimation_trans_ui")
  genome_factor_estimate <- mod_trans_fraction_to_full_genome_server("estimation_trans_ui")
  mod_estimation_case_hot_server("estimation_trans_ui", "translocations", genome_factor_estimate)
  mod_estimation_fit_curve_hot_server("estimation_trans_ui")
  mod_estimation_fit_curve_server("estimation_trans_ui", "translocations")
  mod_estimation_results_server("estimation_trans_ui", "translocations", genome_factor_estimate)

  # Micronuclei Modules ----

  #if (golem::app_dev()) {
    # Fitting
    mod_fitting_counts_hot_server("fitting_micro_ui", "micronuclei")
    mod_fitting_results_server("fitting_micro_ui", "micronuclei")

    # Dose Estimation
    mod_estimation_case_hot_server("estimation_micro_ui", "micronuclei")
    mod_estimation_fit_curve_hot_server("estimation_micro_ui")
    mod_estimation_fit_curve_server("estimation_micro_ui", "micronuclei")
    mod_estimation_results_server("estimation_micro_ui", "micronuclei")
  #}
}
