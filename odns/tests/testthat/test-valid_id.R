testthat::test_that("char string of nchar 36 returns TRUE", {
  testthat::expect_true(
    valid_id(paste0(rep("a", 36), collapse = ""))
  )
})

testthat::test_that("char string of nchar < 36 returns FALSE", {
  testthat::expect_false(
    valid_id(paste0(rep("a", 35), collapse = ""))
  )
})

testthat::test_that("char string of nchar > 36 returns FALSE", {
  testthat::expect_false(
    valid_id(paste0(rep("a", 37), collapse = ""))
  )
})
