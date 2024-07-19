test_that("LES classic House data", {
  hr_classic <- get_les("house", les_2 = FALSE)

  # data checks
  expect_s3_class(hr_classic, "tbl_df")
  expect_length(hr_classic, 60)
  expect_equal(nrow(hr_classic), 11158)
  expect_equal(unique(hr_classic$congress), 93:117)
  expect_equal(unique(hr_classic$year), seq(1973, 2021, 2))

  # chamber argument
  expect_equal(hr_classic, get_les("hr", les_2 = FALSE))
})

test_that("LES classic Senate data", {
  s_classic <- get_les("senate", les_2 = FALSE)

  # data checks
  expect_s3_class(s_classic, "tbl_df")
  expect_length(s_classic, 60)
  expect_equal(nrow(s_classic), 2533)
  expect_equal(unique(s_classic$congress), 93:117)
  expect_equal(unique(s_classic$year), seq(1972, 2020, 2))

  # chamber argument
  expect_equal(s_classic, get_les("s", les_2 = FALSE))
})

test_that("LES 2.0 House data", {
  hr_2 <- get_les("hr", les_2 = TRUE)

  # data checks
  expect_s3_class(hr_2, "tbl_df")
  expect_length(hr_2, 60)
  expect_equal(nrow(hr_2), 454)
  expect_equal(unique(hr_2$congress), 117)
  expect_equal(unique(hr_2$year), 2021)

  # chamber argument
  expect_equal(hr_2, get_les("h", les_2 = TRUE))
})

test_that("LES 2.0 Senate data", {
  s_2 <- get_les("sen", les_2 = TRUE)

  # data checks
  expect_s3_class(s_2, "tbl_df")
  expect_length(s_2, 60)
  expect_equal(nrow(s_2), 100)
  expect_equal(unique(s_2$congress), 117)
  expect_equal(unique(s_2$year), 2020)

  # chamber argument
  expect_equal(s_2, get_les("s", les_2 = TRUE))
})
