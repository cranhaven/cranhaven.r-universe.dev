
test_that("TrapezoidalFuzzyNumber instance works (initialize)", {
  ## CONSTRUCTOR ERRORS
  ## not enough parameters
  expect_error(TrapezoidalFuzzyNumber$new())
  expect_error(TrapezoidalFuzzyNumber$new(3))
  expect_error(TrapezoidalFuzzyNumber$new(3, 3))
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, 3))
  ## more parameters than needed
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, 3, 3, 3))
  ## invalid parameter: not all are numbers
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, 4, "a"))
  expect_error(TrapezoidalFuzzyNumber$new(Inf, 3, 4, "a"))
  expect_error(TrapezoidalFuzzyNumber$new(3,-Inf, 4, "a"))
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, NULL, "a"))
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, 4, NaN))
  expect_error(TrapezoidalFuzzyNumber$new(3, 3, 4, NA))
  expect_error(TrapezoidalFuzzyNumber$new(Inf, NaN, NA, NULL))
  ## valid parameters in constructor
  ## invalid TrapezoidalFuzzyNumber
  expect_error(
    TrapezoidalFuzzyNumber$new(1, 0, 2, 3),
    "The TrapezoidalFuzzyNumber is not valid as inf0, inf1, sup1 and sup0 are not non-decreasing."
  )

  ## inf1 = sup0
  expect_error(
    TrapezoidalFuzzyNumber$new(-1, 0, -0.5, 0),
    "The TrapezoidalFuzzyNumber is not valid as inf0, inf1, sup1 and sup0 are not non-decreasing."
  )

  expect_error(
    TrapezoidalFuzzyNumber$new(-2, -4, -6, -8),
    "The TrapezoidalFuzzyNumber is not valid as inf0, inf1, sup1 and sup0 are not non-decreasing."
  )

  ## valid parameter
  ## correct called to checkValidity
  ## all attributes are saved correctly
  tra <- TrapezoidalFuzzyNumber$new(1, 2, 3, 4)

  expect_equal(class(tra$getInf0())[1], "numeric")
  expect_equal(class(tra$getInf1())[1], "numeric")
  expect_equal(class(tra$getSup1())[1], "numeric")
  expect_equal(class(tra$getSup0())[1], "numeric")
  expect_equal(class(tra$is_positive()), "logical")

})

test_that("TrapezoidalFuzzyNumber is_positive method",
          {
            ## valid parameters in constructor
            ## valid TrapezoidalFuzzyNumber
            tra <- TrapezoidalFuzzyNumber$new(1, 2, 3, 4)

            ## more parameters than needed
            expect_error(tra$is_positive(1))

            expect_equal(tra$is_positive(), TRUE)

            ## valid parameters in constructor
            ## valid TrapezoidalFuzzyNumber
            tra <- TrapezoidalFuzzyNumber$new(-8, -6, -4, -2)

            expect_equal(tra$is_positive(), FALSE)

            ## valid parameters in constructor
            ## valid TrapezoidalFuzzyNumber
            tra <- TrapezoidalFuzzyNumber$new(-1, -1, 2, 3)

            expect_equal(tra$is_positive(), FALSE)

            ## valid parameters in constructor
            ## valid TrapezoidalFuzzyNumber
            ## sup1 = sup0
            tra <- TrapezoidalFuzzyNumber$new(1, 2, 3, 3)

            expect_equal(tra$is_positive(), TRUE)
          })

test_that("FuzzyNumber getInf0, getInf1, getSup1, getSup0 methods", {
  ## Example 1
  tra <- TrapezoidalFuzzyNumber$new(-1, 2.5, 3, 4.4)

  ## more parameters than needed
  expect_error(tra$getInf0(1))
  expect_error(tra$getInf1(1))
  expect_error(tra$getSup1(1))
  expect_error(tra$getSup0(1))

  expect_equal(tra$getInf0(),-1)
  expect_equal(tra$getInf1(), 2.5)
  expect_equal(tra$getSup1(), 3)
  expect_equal(tra$getSup0(), 4.4)

  ## Example 2
  tra <- TrapezoidalFuzzyNumber$new(-8.5, -6.9, -4.0, 2)

  expect_equal(tra$getInf0(),-8.5)
  expect_equal(tra$getInf1(),-6.9)
  expect_equal(tra$getSup1(),-4.0)
  expect_equal(tra$getSup0(), 2)

})

test_that("FuzzyNumber plot method", {
  p1 <- TrapezoidalFuzzyNumber$new(1, 2, 3, 4)$plot(palette()[7:9])
  vdiffr::expect_doppelganger("TrapezoidalFuzzyNumber", p1)
})
