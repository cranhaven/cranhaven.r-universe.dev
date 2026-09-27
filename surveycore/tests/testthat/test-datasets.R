test_that("datasets load and have expected dimensions", {
  expect_equal(nrow(nhanes_2017), 9254)
  expect_equal(ncol(nhanes_2017), 14)

  expect_equal(nrow(acs_pums_wy), 5962)
  expect_equal(ncol(acs_pums_wy), 96)

  expect_equal(nrow(anes_2024), 5521)
  expect_equal(ncol(anes_2024), 19)

  expect_equal(nrow(gss_2024), 3309)
  expect_equal(ncol(gss_2024), 27)

  expect_equal(nrow(pew_npors_2025), 5022)

  expect_equal(nrow(pew_jewish_2020), 5881)
  expect_equal(ncol(pew_jewish_2020), 130)
})

test_that("nhanes_2017 carries variable labels", {
  # NHANES XPT files embed variable labels but not value labels (value codes
  # are documented in external NHANES codebooks, not as SAS formats in the
  # XPT files themselves).
  expect_equal(attr(nhanes_2017$riagendr, "label", exact = TRUE), "Gender")
})

test_that("acs_pums_wy has 80 replicate weights", {
  n_rep <- sum(grepl("^pwgtp[0-9]", names(acs_pums_wy)))
  expect_equal(n_rep, 80)
})

test_that("pew_jewish_2020 has 100 replicate weights", {
  n_rep <- sum(grepl("^extweight[0-9]", names(pew_jewish_2020)))
  expect_equal(n_rep, 100)
})

test_that("pew_npors_2025 smuse_* variables carry question_preface", {
  qp <- attr(pew_npors_2025$smuse_fb, "question_preface")
  expect_false(is.null(qp))
  expect_true(nchar(qp) > 0)
})

test_that("pew_jewish_2020 battery variables carry question_preface", {
  qp <- attr(pew_jewish_2020$discrim_a, "question_preface")
  expect_false(is.null(qp))
  expect_true(nchar(qp) > 0)
})
