test_that("test.is_32_bit.any_version_of_r.returns_pointer_size_equals_4", {
  expected <- .Machine$sizeof.pointer == 4
  actual <- is_32_bit()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(sprintf("R is %d bit.", 8 * .Machine$sizeof.pointer))
    )
  }
})

test_that("test.is_64_bit.any_version_of_r.returns_pointer_size_equals_8", {
  expected <- .Machine$sizeof.pointer == 8
  actual <- is_64_bit()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(
      cause(actual), 
      noquote(sprintf("R is %d bit.", 8 * .Machine$sizeof.pointer))
    )
  }
})
