library(testthat)
# Load example data (pastikan data tersedia dalam package atau buat dummy data dulu)
data(df_svy_A)
data(df_svy_B)

# Define predictor columns
x_predictors <- names(df_svy_A)[5:19]

test_that("projection_randomforest runs without error and returns expected output", {

  # Run function
  result <- projection_randomforest(
    data_model = df_svy_A,
    target_column = "Y",
    predictor_cols = x_predictors,
    data_proj = df_svy_B,
    domain1 = "province",
    domain2 = "regency",
    psu = "num",
    ssu = NULL,
    strata = NULL,
    weights = "weight",
    feature_selection = TRUE,
    bias_correction = TRUE
  )

  # Cek output tipe list
  expect_type(result, "list")

  # Pastikan list mengandung komponen-komponen penting (ganti sesuai outputmu)
  expect_true(all(c("Domain1_corrected_bias", "Domain2_corrected_bias", "model") %in% names(result)))

  # Pastikan Estimation_Domain2 adalah data.frame
  expect_s3_class(result$Domain2_corrected_bias, "data.frame")

  # Pastikan ada kolom "Estimation"
  expect_true("Estimation_Domain1" %in% names(result$Domain1_corrected_bias))
  expect_true("Est_corrected" %in% names(result$Domain2_corrected_bias))

  # Pastikan jumlah baris di Estimation_Domain1 hanya 1
  expect_equal(nrow(result$Domain1_corrected_bias), 1)

  # (Opsional) Pastikan hasilnya bukan NA
  expect_false(any(is.na(result$Domain1_corrected_bias$Estimation_Domain1)))

  # Pastikan ada lebih dari 0 baris hasil estimasi domain2
  expect_gt(nrow(result$Domain2_corrected_bias), 0)

})

test_that("projection_randomforest works without feature selection", {
  result <- projection_randomforest(
    data_model = df_svy_A,
    target_column = "Y",
    predictor_cols = x_predictors,
    data_proj = df_svy_B,
    domain1 = "province",
    domain2 = "regency",
    psu = "num",
    weights = "weight",
    feature_selection = FALSE,
    bias_correction = TRUE
  )

  expect_type(result, "list")
  expect_true(all(c("Domain1_corrected_bias", "Domain2_corrected_bias") %in% names(result)))
})

test_that("projection_randomforest works without bias correction", {
  result <- projection_randomforest(
    data_model = df_svy_A,
    target_column = "Y",
    predictor_cols = x_predictors,
    data_proj = df_svy_B,
    domain1 = "province",
    domain2 = "regency",
    psu = "num",
    weights = "weight",
    feature_selection = TRUE,
    bias_correction = FALSE
  )

  expect_type(result, "list")
  expect_true(all(c("Domain1", "Domain2") %in% names(result)))
})


test_that("projection_randomforest returns non-NA and positive estimations", {
  result <- projection_randomforest(
    data_model = df_svy_A,
    target_column = "Y",
    predictor_cols = x_predictors,
    data_proj = df_svy_B,
    domain1 = "province",
    domain2 = "regency",
    psu = "num",
    weights = "weight",
    feature_selection = TRUE,
    bias_correction = TRUE
  )

  expect_false(any(is.na(result$Domain1_corrected_bias$Estimation)))
  expect_true(all(result$Domain1_corrected_bias$Estimation >= 0))
})

test_that("projection_randomforest handles empty data input gracefully", {
  expect_error(
    projection_randomforest(
      data_model = df_svy_A,
      target_column = "Y",
      predictor_cols = x_predictors,
      data_proj = df_svy_B,
      domain1 = "province",
      domain2 = "regency",
      psu = NULL,
      weights = "weight"
    ),
    regexp = "PSU cannot be NULL"
  )
})

