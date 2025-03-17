test_that("utils", {
  
  expect_true(is_int(1L))
  expect_false(is_int(1.5))
  expect_false(is_int(1:5L))
  expect_false(is_int(NULL))
  expect_false(is_int(NA_integer_))
  
  expect_true(is_string('a'))
  expect_true(is_string(NULL, null_ok = TRUE))
  expect_false(is_string(NULL, null_ok = FALSE))
  expect_false(is_string(1L))
  expect_false(is_string(c('a', 'b')))
  expect_false(is_string(NA_character_))
  expect_false(is_string(''))
  
  expect_false(valid_string(''))
  
  expect_true(is_bool(TRUE))
  expect_true(is_bool(FALSE))
  expect_false(is_bool(1L))
  expect_false(is_bool(c(TRUE, FALSE)))
  expect_false(is_bool(NULL))
  expect_false(is_bool(as.logical(NA)))
  
  expect_true(all_named(c(x = 1, y = 2)))
  expect_true(all_named(NULL))
  expect_false(all_named(c(1, 2)))
  expect_false(all_named(c(x = 1, 2)))
})
