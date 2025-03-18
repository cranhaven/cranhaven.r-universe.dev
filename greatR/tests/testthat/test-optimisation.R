brapa_sample_data <- data.table::fread(system.file("extdata/brapa_arabidopsis_data.csv", package = "greatR"))
reference <- "Ro18"
query <- "Col0"
gene_data <- brapa_sample_data[gene_id == "BRAA03G051930.3C"]
all_data <- suppressMessages(preprocess_data(gene_data, reference, query, scaling_method = "z-score"))

test_that("objective_fun works", {
  # Expected outputs
  expect_equal(objective_fun(all_data, 3.10, -12.58, 0.5), -18.82, tolerance = 1e-2)
  expect_equal(objective_fun(all_data, 3.10, 12.58, 0.5), -999)
  expect_equal(objective_fun(all_data), -999)
})

test_that("optimise works", {
  optimisation_config <- list(num_iterations = 1, num_fun_evals = 1)
  results_sa <- optimise(all_data, optimisation_config = optimisation_config, optimise_fun = optimise_using_sa)
  results_nm <- optimise(all_data, optimisation_config = optimisation_config, optimise_fun = optimise_using_nm)

  # Expected outputs
  expect_equal(names(results_sa), c("stretch", "shift", "loglik_score"))
  expect_equal(names(results_nm), c("stretch", "shift", "loglik_score"))
  expect_equal(unname(unlist(results_nm)), c(2.02, -6.84, -41.89), tolerance = 1e-1)
  expect_equal(unname(unlist(results_sa)), c(2.67, -7.67, -19.51), tolerance = 1e-1)
})
