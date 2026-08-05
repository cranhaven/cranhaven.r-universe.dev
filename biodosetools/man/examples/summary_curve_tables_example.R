#fit_results_X is the output from the Estimation module

fit_results_A1 <- system.file("extdata", "A1_Estimation_results.rds", package = "biodosetools") %>%
  readRDS()

fit_results_A2 <- system.file("extdata", "A2_Estimation_results.rds", package = "biodosetools") %>%
  readRDS()



summary_curve_tables(num_labs = 2,
                     list_lab_names = c("A", "B"),
                     all_rds = list(fit_results_A1, fit_results_A2)
                     )
