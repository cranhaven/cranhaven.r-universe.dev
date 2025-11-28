data(Arthritis)

tab_two <- table(Arthritis[["Sex"]], Arthritis[["Treatment"]])
tab_more <- table(Arthritis[["Sex"]], Arthritis[["Improved"]])

test_that("output class is correct", {
  skip_on_cran()

  expect_type(paired_test_categorical(tab_two), "list")
  expect_type(paired_test_categorical(tab_more), "list")
})

test_that("output structure is correct", {
  skip_on_cran()

  expect_true(all(
    c("P", "stat", "df", "testname", "statname", "namefun") %in%
      names(paired_test_categorical(tab_two))
  ))
  expect_true(all(
    c("P", "stat", "df", "testname", "statname", "namefun") %in%
      names(paired_test_categorical(tab_more))
  ))
})

test_that("wrong input return NA list", {
  expect_warning(
    out <- paired_test_categorical(c(1L, 2L)),
    "not a proper matrix"
  )
  expect_type(out, "list")
  expect_true(is.na(out[["P"]]))

  expect_warning(
    out <- paired_test_categorical(matrix(c(1L, 2L))),
    "not a proper matrix"
  )
  expect_type(out, "list")
  expect_true(is.na(out[["P"]]))

  expect_warning(
    out <- paired_test_categorical(matrix(c(1L, 2L), ncol = 2L)),
    "not a proper matrix"
  )
  expect_type(out, "list")
  expect_true(is.na(out[["P"]]))
})


test_that("singular matrix were managed", {
  expect_warning(expect_warning(
    out <- paired_test_categorical(matrix(c(1L, 0L, 2L, 0L), ncol = 2L)),
    "is not a table"), "is not a proper matrix")

  expect_type(out, "list")
  expect_true(is.na(out[["P"]]))
})


test_that("matrices are converted to table with a warning", {
  mat_test <- matrix(1L:9L, nrow = 3L, dimnames = list(a = 1L:3L, b = 1L:3L))
  expect_warning(paired_test_categorical(mat_test))
})


test_that("input without names do not throw an error", {
  expect_success(
   expect_warning(paired_test_categorical(matrix(1L:9L, nrow = 3L)))
  )
})


test_that("paired_test_categorical works with 0 data", {
  mat_1 <- as.table(matrix(
    c(0L, 45L, 19L, 29L, 53L, 3L, 5L, 0L), ncol = 2L, nrow = 4L,
    byrow = TRUE,
    dimnames = list(
      "outcome" = c("1", "2", "3", "4"),
      "time" = c("baseline", "fup")
    )
  ))

  expect_false(
    is.nan(depigner::paired_test_categorical(mat_1)[["P"]])
  )
})
