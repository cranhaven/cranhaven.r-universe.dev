test_that("assert_is_single_h_desc works", {
  skip_on_cran()
  test_desc <- Hmisc::describe(mtcars)

  expect_error(
    assert_is_single_h_desc(test_desc),
    "must be a single|is not TRUE"
  )
  expect_true(assert_is_h_desc(test_desc))

  expect_true(assert_is_single_h_desc(test_desc[[1L]]))
  expect_true(assert_is_h_desc(test_desc[[1L]]))
})


test_that("is_val_freq_list works", {
  expect_false(is_val_freq_list(integer()))
  expect_false(is_val_freq_list(integer(1L)))
  expect_false(is_val_freq_list(list(a = 1L, b = 2L)))

  expect_true(
    is_val_freq_list(list(value = 1L, frequency = 2L))
  )
})


test_that("is_proper_matrix works", {
  expect_false(is_proper_matrix(1L))
  expect_false(is_proper_matrix(matrix(c(1L, 2L))))
  expect_false(is_proper_matrix(matrix(c(1L, 2L), nrow = 1L)))

  expect_true(is_proper_matrix(matrix(1L:4L, nrow = 2L)))
})


test_that("empty_h_test works", {
  expect_warning(
    out <- empty_h_test(),
    "not a proper matrix"
  )
  expect_type(out, "list")
  expect_equal(out[["P"]], NA)
})

test_that("fake_h_group_test works", {
  expect_warning(
    out <- fake_h_group_test(),
    "Only one group"
  )
  expect_type(out, "list")
  expect_equal(out[["P"]], c(P = 1L))
})
