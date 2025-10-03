context("get_decon")

test_that("get_decon produces an opinionated dataset", {

  skip_if_offline()
  skip_on_cran()

  cesR::get_decon()
  expect_true(exists("decon"))
})

test_that("get_decon produces an error message if the decon object already exists", {

  skip_if_offline()
  skip_on_cran()

  expect_error(cesR::get_decon())
})
