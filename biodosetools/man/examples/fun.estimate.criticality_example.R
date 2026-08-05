#Results from the fitting module
fit_results_gamma <- system.file("extdata",
                                 "gamma_dicentrics-fitting-results.rds",
                                 package = "biodosetools") %>%
  readRDS()

fit_results_neutrons <- system.file("extdata",
                                    "neutrons-mixed-dicentrics-fitting-results.rds",
                                    package = "biodosetools") %>%
  readRDS()

#FUNCTION TO ESTIMATE DOSE IN CRITICALITY ACCIDENTS
fun.estimate.criticality(num_cases = 2,
                         dics = c(380,456),
                         cells = c(218,567),
                         coef_gamma = fit_results_gamma[["fit_coeffs"]][,1],
                         cov_gamma = fit_results_gamma[["fit_var_cov_mat"]],
                         coef_neutron = fit_results_neutrons[["fit_coeffs"]][,1],
                         cov_neutron = fit_results_neutrons[["fit_var_cov_mat"]],
                         ratio = 1.2,
                         p = 0)
