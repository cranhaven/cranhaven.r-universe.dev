
test_that("Utils instance works (default new)", {
  ## more parameters than needed
  expect_error(Utils$new("a"))

  u <- Utils$new()

  expect_equal(class(u)[[1]], "Utils")
  expect_equal(class(u)[[2]], "R6")

})

test_that("Utils convertTra method", {
  u <- Utils$new()

  ## not all mandatory parameters
  expect_error(u$convertTra())

  ## more parameters than needed
  expect_error(u$convertTra(M1, 1))

  ## invalid parameter: not a data frame
  expect_error(u$convertTra(1))
  expect_error(u$convertTra(0L))
  expect_error(u$convertTra("a"))
  expect_error(u$convertTra(6.59))
  expect_error(u$convertTra(Inf))
  expect_error(u$convertTra(-Inf))
  expect_error(u$convertTra(NA))
  expect_error(u$convertTra(NaN))
  expect_error(u$convertTra(NULL))
  expect_error(u$convertTra(list()))
  expect_error(u$convertTra(c()))

  ## invalid parameter: data frame not only contains double values
  m <- as.data.frame(matrix(c(
    "a", "b", 1L, NA, 3, 2, 2, NA, 1, 3, 3.333, 3, 6, 4, 0, 0.0000
  ), ncol = 4))
  expect_error(u$convertTra(m))

  m <- as.data.frame(matrix(c(
    NA, NA, NA, NA, 3, 2, 2, NA, 1, 3, NA, NA, 6, 4, NA, NA
  ), ncol = 4))
  expect_equal(u$convertTra(m), NA)

  ## valid parameter
  list <- u$convertTra(M1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 68)

  list <- u$convertTra(M2)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 68)

  list <- u$convertTra(M3)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 69)

  list <- u$convertTra(S1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 67)

  list <-
    u$convertTra(as.data.frame(matrix(
      c(NA, 1, 2, NA, 3, 2, 2, NA, 1, 3, NA, NA, 6, 4, NA, NA), ncol = 4
    )))
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 1)
  expect_equal(list$getDimension(1L)$getInf0(), 1)
  expect_equal(list$getDimension(1L)$getInf1(), 2)
  expect_equal(list$getDimension(1L)$getSup1(), 3)
  expect_equal(list$getDimension(1L)$getSup0(), 4)

  list <-
    u$convertTra(as.data.frame(matrix(c(
      NA, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3
    ), ncol = 4)))
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 2)
  expect_equal(list$getDimension(1L)$getInf0(), 1)
  expect_equal(list$getDimension(1L)$getInf1(), 2)
  expect_equal(list$getDimension(1L)$getSup1(), 2)
  expect_equal(list$getDimension(1L)$getSup0(), 3)
  expect_equal(list$getDimension(2L)$getInf0(), 1)
  expect_equal(list$getDimension(2L)$getInf1(), 2)
  expect_equal(list$getDimension(2L)$getSup1(), 3)
  expect_equal(list$getDimension(2L)$getSup0(), 3)

})
