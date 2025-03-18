brapa_sample_data <- data.table::fread(system.file("extdata/brapa_arabidopsis_data.csv", package = "greatR"))
reference <- "Ro18"
query <- "Col0"
gene_data <- brapa_sample_data[gene_id == "BRAA03G051930.3C"]

test_that("calc_loglik_H1 and calc_loglik_H2 work", {
  all_data <- suppressMessages(preprocess_data(gene_data, reference, query, scaling_method = "z-score"))
  loglik_H2 <- calc_loglik_H2(all_data)
  loglik_H1 <- calc_loglik_H1(all_data)

  # Expected outputs
  expect_no_error(calc_loglik_H2(all_data))
  expect_no_error(calc_loglik_H1(all_data))
  expect_error(calc_loglik_H2(gene_data))
  expect_error(calc_loglik_H1(gene_data))
  expect_equal(loglik_H2, -18.635, tolerance = 1e-2)
  expect_equal(loglik_H1, -42.519, tolerance = 1e-2)
})
