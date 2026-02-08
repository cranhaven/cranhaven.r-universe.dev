expect_equal_dictionary <- function(x, y) {
  expect_true(inherits(x, "Dictionary"))
  expect_true(inherits(y, "Dictionary"))
  expect_type(private(x)$.items, "environment")
  expect_type(private(y)$.items, "environment")
  expect_identical(x$typed, y$typed)
  if (x$typed) {
    expect_setequal(x$types, y$types)
  }
  expect_mapequal(x$items, y$items)
}