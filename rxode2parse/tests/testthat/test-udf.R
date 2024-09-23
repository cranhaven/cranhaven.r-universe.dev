test_that("test udf", {

  udf <- function(x, y, ...) {
    x + y
  }

  expect_error(rxode2parse("b <- udf(x, y)"))

  udf <- function(x, y) {
    x + y
  }

  expect_error(rxode2parse("b <- udf(x, y)"), NA)

  expect_error(rxode2parse("b <- udf(x, y, z)"))

  rxode2parse("b <- udf(x, y)", code="udf.c")

  expect_true(file.exists("udf.c"))

  if (file.exists("udf.c")) {
    lines <- readLines("udf.c")
    unlink("udf.c")
    expect_false(file.exists("udf.c"))
  }

  .w <- which(grepl("b =_udf(\"udf\",", lines, fixed=TRUE))
  expect_true(length(.w) > 0)

  .w <- which(grepl("double __udf[2]", lines, fixed=TRUE))
  expect_true(length(.w) > 0)

})
