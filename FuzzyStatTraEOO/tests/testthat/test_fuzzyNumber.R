
test_that("FuzzyNumber instance works (initialize)", {
  ## CONSTRUCTOR ERRORS
  ## invalid parameter: not an array
  expect_error(FuzzyNumber$new())
  expect_error(FuzzyNumber$new(3))
  expect_error(FuzzyNumber$new("a"))
  expect_error(FuzzyNumber$new(matrix()))
  ## more parameters than needed
  expect_error(FuzzyNumber$new(c(), 1))
  ## invalid parameter: array of number that are not numeric
  array <- array(c(0, 5, 1,-1,-1,-1, 2, 1, "a"), dim = c(3, 3))
  expect_error(FuzzyNumber$new(array))
  array <- array(c(NULL, Inf, -Inf, NA, NaN, NULL), dim = c(2, 3))
  expect_error(FuzzyNumber$new(array))

  ## valid parameter
  ## array saved correctly in the attribute fnAlphaLevels
  ## correct called to checkValidity
  array <-
    array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  expect_equal(class(fuzzy$getAlphaLevels())[2], "array")
  expect_equal(class(fuzzy$getInfimums())[2], "array")
  expect_equal(class(fuzzy$getSupremums())[2], "array")

})

test_that("FuzzyNumber checkValidity method", {
  ## valid array
  ## valid FuzzyNumber
  array <-
    array(c(0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0), dim = c(3, 3))
  fuzzy <- FuzzyNumber$new(array)

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(1, 2, 3, 4), dim = c(2, 1, 2))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.1, 0.5, 1, 2, 3), dim = c(3, 2))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(1, 2, 3, 4, 5, 6), dim = c(2, 3))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0, 1, 2, 3, 4, 5, 0, 1), dim = c(3, 3))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 2, 5, 6, 7), dim = c(3, 3))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 4, 5, 0, 1), dim = c(3, 3))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

  ## invalid array
  ## invalid FuzzyNumber
  array <- array(c(0, 0.5, 1, 2, 3, 4, 6, 5, 3), dim = c(3, 3))

  expect_error(FuzzyNumber$new(array),
               "An invalid FuzzyNumber cannot be created")

})

test_that("FuzzyNumber getAlphalevels, getInfimums and getSupremums methods",
          {
            ## checks the dimension
            ## checks random values
            fuzzy <-
              FuzzyNumber$new(array(c(
                0.0, 0.5, 1.0,-1.5,-1.0,-1.0, 2.0, 1.5, 1.0
              ), dim = c(3, 3)))

            ## more parameters than needed
            expect_error(fuzzy$getAlphaLevels(1))

            expect_equal(dim(fuzzy$getAlphaLevels())[1], 3)
            expect_equal(dim(fuzzy$getAlphaLevels())[2], 1)
            expect_equal(dim(fuzzy$getInfimums())[1], 3)
            expect_equal(dim(fuzzy$getInfimums())[2], 1)
            expect_equal(dim(fuzzy$getSupremums())[1], 3)
            expect_equal(dim(fuzzy$getSupremums())[2], 1)
            expect_equal(fuzzy$getAlphaLevels()[2], 0.5)
            expect_equal(fuzzy$getInfimums()[1], -1.5)
            expect_equal(fuzzy$getSupremums()[3], 1)

          })

test_that("FuzzyNumber plot method", {
  p1 <-
    FuzzyNumber$new(array(c(0.0, 1.0, 1, 1.5, 2, 1.7), dim = c(2, 3)))$plot("blue")
  vdiffr::expect_doppelganger("FuzzyNumber", p1)

})
