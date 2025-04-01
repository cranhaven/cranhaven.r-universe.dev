context("Parameter validation")
library(clusterability)

test_that("validate_data", {
  expect_error(validate_data(NULL), NULL, info = "Null data")
  expect_error(validate_data(rep(0, 0)), NULL, info = "0 rows")
  expect_error(validate_data(t(rep(0, 0))), NULL, info = "0 cols")
})

test_that("validate_test", {
  expect_match(validate_test("dip"), "DIP")
  expect_match(validate_test("silverman"), "SILVERMAN")

  expect_error(validate_test(dip), "dip", info = "Forgot quotes")
  expect_error(validate_test(NULL), "dip", info = "NULL test")
  expect_error(validate_test("blah"), "dip", info = "Invalid test")
})

test_that("validate_metric", {
  # Setup
  testdata <- cbind(rnorm(100), rnorm(100, 5, 1))
  test1d <- rnorm(100)

  # Tests
  expect_match(validate_metric("EUCLIDEAN", testdata), "euclidean", fixed = TRUE)
  expect_match(validate_metric("Maximum", testdata), "maximum", fixed = TRUE)
  expect_match(validate_metric("mAnhATTaN", testdata), "manhattan", fixed = TRUE)
  expect_match(validate_metric("canberra", testdata), "canberra", fixed = TRUE)
  expect_match(validate_metric("binary", testdata), "binary", fixed = TRUE)
  expect_match(validate_metric("mINKOWSKI(4.2)", testdata), "minkowski(4.2)", fixed = TRUE)
  expect_match(validate_metric("minkowski(.21112)", testdata), "minkowski(.21112)", fixed = TRUE)
  expect_match(validate_metric("mINKOWSKI(9963)", testdata), "minkowski(9963)", fixed = TRUE)
  expect_match(validate_metric("COV", testdata), "cov", fixed = TRUE)
  expect_match(validate_metric("sQeUC", testdata), "sqeuc", fixed = TRUE)
  expect_match(validate_metric("cORr", testdata), "corr", fixed = TRUE)
  expect_match(validate_metric("sQCOrr", testdata), "sqcorr", fixed = TRUE)

  expect_warning(validate_metric(NULL, testdata), "euclidean")
  expect_warning(validate_metric(blah, testdata), "euclidean")
  expect_warning(validate_metric(55, testdata), "euclidean")
  expect_warning(validate_metric("minkowski", testdata), "euclidean")
  expect_warning(validate_metric("minkowski(0)", testdata), "positive")

  expect_error(validate_metric("coV", test1d), "dimensional")
  expect_error(validate_metric("coRR", test1d), "dimensional")
})

test_that("validate_reduction", {
  expect_match(validate_reduction("pca"), "PCA", fixed = TRUE)
  expect_match(validate_reduction("distance"), "DISTANCE", fixed = TRUE)
  expect_match(validate_reduction("nOnE"), "NONE", fixed = TRUE)

  expect_warning(validate_reduction("jalks"), "PCA")
  expect_warning(validate_reduction(NULL), "PCA")
  expect_warning(validate_reduction(blah), "PCA")

})

test_that("validate_isdistmatrix", {
  data1 <- dist(rnorm(10))
  data2 <- cbind(c(1, 2, 3), c(3, 8, 7), c(6, 1, 9))
  data3 <- cbind(c(0, 2, 3), c(2, 0, 4), c(3, 4, 0))
  expect_false(validate_isdistmatrix(FALSE, "PCA", data1))
  expect_false(validate_isdistmatrix(FALSE, "NONE", data1))
  expect_false(validate_isdistmatrix(FALSE, "DISTANCE", data1))

  expect_true(validate_isdistmatrix(TRUE, "NONE", data1))
  expect_true(validate_isdistmatrix(TRUE, "NONE", data3))

  expect_error(validate_isdistmatrix(TRUE, "PCA", data1))
  expect_error(validate_isdistmatrix(TRUE, "DiStAnCE", data1))
  expect_error(validate_isdistmatrix(TRUE, "NONE", data2))

})

test_that("validate_standardize", {
  expect_match(validate_standardize("nOnE"), "NONE", fixed = TRUE)
  expect_match(validate_standardize("STd"), "STD", fixed = TRUE)
  expect_match(validate_standardize("MEan"), "MEAN", fixed = TRUE)
  expect_match(validate_standardize("meDIaN"), "MEDIAN", fixed = TRUE)

  expect_warning(validate_standardize("blah"), "standardization")
  expect_warning(validate_standardize(NULL), "standardization")
  expect_warning(validate_standardize(blah), "standardization")
})

test_that("validate_pca_center", {
  expect_true(validate_pca_center(T))
  expect_true(validate_pca_center(TRUE))
  expect_false(validate_pca_center(F))
  expect_false(validate_pca_center(FALSE))

  expect_true(suppressWarnings(validate_pca_center(blah)))  # Default value is TRUE

  expect_warning(validate_pca_center(true))
  expect_warning(validate_pca_center("TRUE"))
  expect_warning(validate_pca_center(false))
  expect_warning(validate_pca_center("FALSE"))
})

test_that("validate_pca_scale", {
  expect_true(validate_pca_scale(T))
  expect_true(validate_pca_scale(TRUE))
  expect_false(validate_pca_scale(F))
  expect_false(validate_pca_scale(FALSE))

  # Default value is TRUE
  expect_true(suppressWarnings(validate_pca_scale(blah)))

  expect_warning(validate_pca_scale(true))
  expect_warning(validate_pca_scale("TRUE"))
  expect_warning(validate_pca_scale(false))
  expect_warning(validate_pca_scale("FALSE"))
})

test_that("validate_completecase", {
  # Valid inputs
  expect_true(validate_completecase(T))
  expect_true(validate_completecase(TRUE))
  expect_false(validate_completecase(F))
  expect_false(validate_completecase(FALSE))

  # Invalid input - does it return default?
  expect_false(suppressWarnings(validate_completecase(blah)))

  # Invalid input - does it warn the user?
  expect_warning(validate_completecase(true))
  expect_warning(validate_completecase("TRUE"))
  expect_warning(validate_completecase(false))
  expect_warning(validate_completecase("FALSE"))
})

test_that("validate_dsimulatepvalue", {
  # Valid inputs
  expect_true(validate_dsimulatepvalue(T))
  expect_true(validate_dsimulatepvalue(TRUE))
  expect_false(validate_dsimulatepvalue(F))
  expect_false(validate_dsimulatepvalue(FALSE))

  # Invalid input - does it return default?
  expect_false(suppressWarnings(validate_dsimulatepvalue(blah)))

  # Invalid input - does it warn the user?
  expect_warning(validate_dsimulatepvalue(true))
  expect_warning(validate_dsimulatepvalue("TRUE"))
  expect_warning(validate_dsimulatepvalue(false))
  expect_warning(validate_dsimulatepvalue("FALSE"))
})

test_that("validate_sadjust", {
  # Valid inputs
  expect_true(validate_sadjust(T))
  expect_true(validate_sadjust(TRUE))
  expect_false(validate_sadjust(F))
  expect_false(validate_sadjust(FALSE))

  # Invalid input - does it return default?
  expect_true(suppressWarnings(validate_sadjust(blah)))

  # Invalid input - does it warn the user?
  expect_warning(validate_sadjust(true))
  expect_warning(validate_sadjust("TRUE"))
  expect_warning(validate_sadjust(false))
  expect_warning(validate_sadjust("FALSE"))
})

test_that("validate_soutseed", {
  # Valid inputs
  expect_true(validate_soutseed(T))
  expect_true(validate_soutseed(TRUE))
  expect_false(validate_soutseed(F))
  expect_false(validate_soutseed(FALSE))

  # Invalid input - does it return default?
  expect_false(suppressWarnings(validate_soutseed(blah)))

  # Invalid input - does it warn the user?
  expect_warning(validate_soutseed(true))
  expect_warning(validate_soutseed("TRUE"))
  expect_warning(validate_soutseed(false))
  expect_warning(validate_soutseed("FALSE"))
})

test_that("validate_dreps", {
  # Valid inputs
  expect_equal(validate_dreps(700), 700)
  expect_equal(validate_dreps(1), 1)

  # Invalid input - does it return default?
  expect_equal(suppressWarnings(validate_dreps(0)), 2000)

  # Invalid inputs
  expect_warning(validate_dreps(0), "d_reps", info = "d_reps - 0 is invalid")
  expect_warning(validate_dreps(4.5), "d_reps", info = "d_reps - non-integer is invalid")
  expect_warning(validate_dreps(NULL), "d_reps", info = "d_reps - NULL is invalid")
  expect_warning(validate_dreps("55"), "d_reps", info = "d_reps - non-numeric is invalid")
  expect_warning(validate_dreps(blah), "d_reps", info = "d_reps - invalid object is invalid")
})

test_that("validate_sk", {
  # Valid inputs
  expect_equal(validate_sk(1), 1)
  expect_equal(validate_sk(2), 2)

  # Invalid input - does it return the default?
  expect_equal(suppressWarnings(validate_sk(0)), 1)

  # Invalid inputs
  expect_warning(validate_sk(1.5), "s_k")
  expect_warning(validate_sk(0), "s_k")
  expect_warning(validate_sk(blah), "s_k")
  expect_warning(validate_sk(NULL), "s_k")
  expect_warning(validate_sk("1"), "s_k")
})

test_that("validate_sm", {
  # Valid inputs
  expect_equal(validate_sm(1000), 1000)
  expect_equal(validate_sm(1), 1)

  # Invalid input - does it return the default?
  expect_equal(suppressWarnings(validate_sm(0)), 999)

  # Invalid inputs
  expect_warning(validate_sm(1.5), "s_m")
  expect_warning(validate_sm(0), "s_m")
  expect_warning(validate_sm(blah), "s_m")
  expect_warning(validate_sm(NULL), "s_m")
  expect_warning(validate_sm("1"), "s_m")
})

test_that("validate_sdigits", {
  # Valid inputs
  expect_equal(validate_sdigits(9), 9)
  expect_equal(validate_sdigits(1), 1)

  # Invalid input - does it return the default?
  expect_equal(suppressWarnings(validate_sdigits(0)), 6)

  # Invalid inputs
  expect_warning(validate_sdigits(1.5), "s_digits")
  expect_warning(validate_sdigits(0), "s_digits")
  expect_warning(validate_sdigits(blah), "s_digits")
  expect_warning(validate_sdigits(NULL), "s_digits")
  expect_warning(validate_sdigits("1"), "s_digits")
})

test_that("validate_ssetseed", {
  # Valid inputs
  expect_equal(validate_ssetseed(9), 9)
  expect_equal(validate_ssetseed(123), 123)
  expect_equal(validate_ssetseed(-1), -1)
  expect_null(validate_ssetseed(NULL))

  # Invalid input - does it return the default?
  expect_null(suppressWarnings(validate_ssetseed(9932.12)))

  # Invalid inputs
  expect_warning(validate_ssetseed(1.5), "s_setseed")
  expect_warning(validate_ssetseed(blah), "s_setseed")
  expect_warning(validate_ssetseed("1"), "s_setseed")
})

