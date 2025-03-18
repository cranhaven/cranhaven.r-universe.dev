brapa_sample_data <- data.table::fread(system.file("extdata/brapa_arabidopsis_data.csv", package = "greatR"))
reference <- "Ro18"
query <- "Col0"
gene_data <- brapa_sample_data[gene_id == "BRAA03G051930.3C"]

# Preprocessing and intermediate functions ----

test_that("transform_input.data.frame works", {
  df <- gene_data
  df_tf <- transform_input(df)

  # Expected outputs
  expect_true("data.frame" %in% class(df_tf))
  expect_equal(colnames(df), colnames(df_tf))
  expect_equal(nrow(df), nrow(df_tf))
  expect_error(transform_input(1:10))
})

test_that("transform_input.list (numeric) works", {
  list_num <- list(
    reference = c(1, 2, 3, 4, 5, 6, 7, 8),
    query = c(0, 1, 2, 3, 4)
  )
  df_tf <- transform_input(list_num, reference = reference, query = query)

  # Expected outputs
  expect_true("data.frame" %in% class(df_tf))
  expect_setequal(colnames(df_tf), c("gene_id", "accession", "timepoint", "replicate", "expression_value"))
  expect_equal(length(intersect(unique(df_tf$accession), c(reference, query))), 2)
  expect_error(transform_input(unname(list_num)))
})

test_that("transform_input.list (df) works", {
  ara_data <- data.table::fread(system.file("extdata/arabidopsis_SOC1_data.csv", package = "greatR"))
  rapa_data <- data.table::fread(system.file("extdata/brapa_SOC1_data.csv", package = "greatR"))

  list_df <- list(
    reference = rapa_data,
    query = ara_data
  )

  df_tf <- transform_input(list_df, reference = reference, query = query)

  # Expected outputs
  expect_true("data.frame" %in% class(df_tf))
  expect_setequal(colnames(df_tf), c("gene_id", "accession", "timepoint", "replicate", "expression_value"))
  expect_equal(length(intersect(unique(df_tf$accession), c(reference, query))), 2)
  expect_setequal(paste0(unique(rapa_data$gene_id), "_", unique(ara_data$gene_id)), unique(df_tf$gene_id))
  expect_error(transform_input(unname(list_df)))
})

test_that("preprocess_data works", {
  all_data <- suppressMessages(preprocess_data(brapa_sample_data, reference, query))
  all_data_norm <- suppressMessages(preprocess_data(brapa_sample_data, reference, query, scaling_method = "z-score"))
  all_data_minmax <- suppressMessages(preprocess_data(brapa_sample_data, reference, query, scaling_method = "min-max"))

  # Expected outputs
  expect_equal(class(all_data)[1], "data.table")
  expect_equal(class(all_data_norm)[1], "data.table")
  expect_equal(class(all_data_minmax)[1], "data.table")
  expect_gte(mean(all_data$expression_value), mean(all_data_norm$expression_value))
  expect_equal(colnames(all_data), c(colnames(brapa_sample_data), "var"))
  expect_equal(levels(unique(all_data$accession)), c("ref", "query"))
  expect_equal(nrow(all_data), nrow(brapa_sample_data))
})

test_that("register_manually works", {
  gene_data <- suppressMessages(preprocess_data(gene_data, reference, query, scaling_method = "z-score"))
  stretch <- 3.10
  shift <- -12.58
  loglik_separate <- -18.635
  results <- register_manually(gene_data, stretch, shift, loglik_separate)
  results_simple <- register_manually(gene_data, stretch, shift, loglik_separate, return_data_reg = FALSE)

  # Expected outputs
  expect_equal(names(results), c("data_reg", "model_comparison"))
  expect_equal(names(results_simple), "model_comparison")
  expect_equal(colnames(results$data_reg), c("gene_id", "accession", "timepoint", "replicate", "expression_value", "var"))
  expect_equal(colnames(results$model_comparison), c("gene_id", "stretch", "shift", "BIC_diff", "registered"))
  expect_equal(results$model_comparison$stretch, stretch)
  expect_equal(results$model_comparison$shift, shift)
  expect_equal(results$model_comparison$BIC_diff, -8.096, tolerance = 1e-2)
  expect_equal(results$model_comparison$registered, TRUE)
  expect_equal(results$model_comparison, results_simple$model_comparison)
})

# Full pipeline ----

test_that("register (with no optimisation) works", {
  stretch <- 3.10
  shift <- -12.58
  registration_results <- suppressMessages(register(
    gene_data,
    reference = "Ro18",
    query = "Col0",
    stretches = stretch,
    shifts = shift,
    use_optimisation = FALSE,
    scaling_method = "z-score"
  ))

  data_reg <- registration_results$data
  model_comparison <- registration_results$model_comparison

  # Expected outputs
  expect_s3_class(registration_results, "res_greatR")
  expect_equal(capture_output(print(registration_results$model_comparison)), capture_output(print(registration_results)))
  expect_equal(names(registration_results), c("data", "model_comparison", "fun_args"))
  expect_equal(colnames(data_reg), c("gene_id", "accession", "expression_value", "replicate", "timepoint", "timepoint_reg"))
  expect_equal(colnames(model_comparison), c("gene_id", "stretch", "shift", "BIC_diff", "registered"))
  expect_equal(model_comparison$registered, TRUE)
  expect_error(suppressMessages(register(gene_data, reference = "Ro18", query = "Col0", stretches = stretch, shifts = NA, use_optimisation = FALSE)))
})

test_that("register (with optimisation) works", {
  registration_results <- suppressMessages(register(
    gene_data,
    reference = "Ro18",
    query = "Col0",
    optimisation_method = "lbfgsb",
    scaling_method = "z-score",
    optimisation_config = list(num_iterations = 10, num_fun_evals = 10)
  ))

  data_reg <- registration_results$data
  model_comparison <- registration_results$model_comparison

  # Expected outputs
  expect_s3_class(registration_results, "res_greatR")
  expect_equal(names(registration_results), c("data", "model_comparison", "fun_args"))
  expect_equal(colnames(data_reg), c("gene_id", "accession", "expression_value", "replicate", "timepoint", "timepoint_reg"))
  expect_equal(colnames(model_comparison), c("gene_id", "stretch", "shift", "BIC_diff", "registered"))
  expect_equal(model_comparison$stretch, 3.10, tolerance = 1e-2)
  expect_equal(model_comparison$shift, -12.58, tolerance = 1e-2)
  expect_equal(model_comparison$BIC_diff, -8.097, tolerance = 1e-2)
  expect_equal(model_comparison$registered, TRUE)
  expect_error(suppressMessages(register(gene_data, reference = "Ro19", query = "Col0")))
})
