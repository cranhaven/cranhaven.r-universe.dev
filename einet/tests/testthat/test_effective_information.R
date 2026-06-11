context("Testing Effective Information")

MAX_ERROR <- 0.001

### Testing Effective Information ---------------------------------------------
test_that("effective_information is correctly calculated", {
  copy_copy <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 1.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  and_and <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  or_or <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  or_copy <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 1.0, 0.0),
      c(0.0, 0.0, 0.0, 1.0),
      c(0.0, 0.0, 0.0, 1.0)
    ),
    nrow = 4
  )

  star <- matrix(
    rbind(
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0),
      c(1.0, 0.0, 0.0, 0.0)
    ),
    nrow = 4
  )

  example <- matrix(
    rbind(
      c(0.0, 2.0, 1.0),
      c(0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.0)
    ),
    nrow = 3
  )

  expect_equal(effective_information(copy_copy), 2.0)
  expect_lte(effective_information(and_and) - 0.8112781, MAX_ERROR)
  expect_lte(effective_information(or_or) - 0.8112781, MAX_ERROR)
  expect_equal(effective_information(or_copy), 1.5)
  expect_equal(effective_information(star), 0)
  expect_equal(effective_information(example), 0)
})

### Testing Effective Information Internal Data -------------------------------
test_that("effective_information is calculated with internal data", {
  expect_lte(effective_information(karate) - 2.35, MAX_ERROR)
})
