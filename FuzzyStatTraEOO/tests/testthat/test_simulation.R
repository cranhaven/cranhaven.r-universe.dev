
test_that("Simulation instance works (default new)", {
  simul <- Simulation$new()

  expect_equal(class(simul)[[1]], "Simulation")
  expect_equal(class(simul)[[2]], "R6")

})

test_that("Simulation simulCase1 method", {
  simul <- Simulation$new()

  ## not all mandatory parameters
  expect_error(simul$simulCase1())

  ## more parameters than needed
  expect_error(simul$simulCase1(1, 1))

  ## invalid parameter: not positive integer
  expect_error(simul$simulCase1(1))
  expect_error(simul$simulCase1(0L))
  expect_error(simul$simulCase1("a"))
  expect_error(simul$simulCase1(6.59))

  ## valid parameter
  list <- simul$simulCase1(6L)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 6)

})

test_that("Simulation simulCase2 method", {
  simul <- Simulation$new()

  ## not all mandatory parameters
  expect_error(simul$simulCase2())

  ## more parameters than needed
  expect_error(simul$simulCase2(1, 1))

  ## invalid parameter: not positive integer
  expect_error(simul$simulCase2(6))
  expect_error(simul$simulCase2(-2L))
  expect_error(simul$simulCase2("b"))
  expect_error(simul$simulCase2(10.0))

  ## valid parameter
  list <- simul$simulCase2(10L)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 10)
})

test_that("Simulation simulCase3 method", {
  simul <- Simulation$new()

  ## not all mandatory parameters
  expect_error(simul$simulCase3())

  ## more parameters than needed
  expect_error(simul$simulCase3(1, 1))

  ## invalid parameter: not positive integer
  expect_error(simul$simulCase3(5))
  expect_error(simul$simulCase3(-10L))
  expect_error(simul$simulCase3("c"))
  expect_error(simul$simulCase3(15.0001))

  ## valid parameter
  list <- simul$simulCase3(3L)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 3)
})

test_that("Simulation simulCase4 method", {
  simul <- Simulation$new()

  ## not all mandatory parameters
  expect_error(simul$simulCase4())

  ## more parameters than needed
  expect_error(simul$simulCase4(1, 1))

  ## invalid parameter: not positive integer
  expect_error(simul$simulCase4(4))
  expect_error(simul$simulCase4(-4L))
  expect_error(simul$simulCase4("d"))
  expect_error(simul$simulCase4(0.0001))

  ## valid parameter
  list <- simul$simulCase4(4L)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 4)
})

test_that("Simulation simulFRSTra method", {
  simul <- Simulation$new()

  ## not all mandatory parameters
  expect_error(simul$simulFRSTra())

  ## more parameters than needed
  expect_error(simul$simulFRSTra(1, 1))

  ## invalid parameter: first parameter is not a positive integer
  expect_error(simul$simulFRSTra(4, 0.05, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(0L, 0.05, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra("a", 0.05, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4.44, 0.05, 0.35, 0.6, 2, 1))

  ## invalid parameter: second parameter is not a double between 0 and 1
  expect_error(simul$simulFRSTra(4L,-1, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L,-1.0, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 1.01, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 1.01, 0.35, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, "a", 0.35, 0.6, 2, 1))

  ## invalid parameter: third parameter is not a double between 0 and 1
  expect_error(simul$simulFRSTra(4L, 0.05,-1, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05,-1.0, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 1.01, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 1.1, 0.6, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, "a", 0.6, 2, 1))

  ## invalid parameter: fourth parameter is not a double between 0 and 1
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35,-1, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35,-1.0, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 1.01, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 1.1, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, "a", 2, 1))

  ## invalid parameter: fifth parameter is not a positive double
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6,-2, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6,-2L, 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, "a", 1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6,-Inf, 1))

  ## invalid parameter: sixth parameter is not a positive double
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2,-1))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2,-1L))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2, "a"))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2,-Inf))

  ## invalid parameters: fifth and/or sixth parameters are infinite
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, Inf, Inf))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2, Inf))
  expect_error(simul$simulFRSTra(4L, 0.05, 0.35, 0.6, Inf, 1))

  ## not fulfill the condition: second, third and fourth parameter must sum 1
  expect_error(simul$simulFRSTra(4L, 0, 0, 0, 2, 1))
  expect_error(simul$simulFRSTra(4L, 1, 1, 1, 2, 1))
  expect_error(simul$simulFRSTra(4L, 0.0, 1.0, 0.5, 2, 1))

  ## valid parameters and condition fulfilled
  list <- simul$simulFRSTra(4L, 0.05, 0.35, 0.6, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 4)

  list <- simul$simulFRSTra(9L, 0.5, 0, 0.5, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 9)

  list <- simul$simulFRSTra(9L, 0.4, 0.6, 0, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 9)

  list <- simul$simulFRSTra(9L, 1, 0, 0, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 9)

  list <- simul$simulFRSTra(9L, 0, 1, 0, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 9)

  list <- simul$simulFRSTra(9L, 0, 0, 1, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 9)

  list <- simul$simulFRSTra(10L, 0.15, 0.4, 0.45, 2, 1)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 10)

  list <- simul$simulFRSTra(100L, 0.05, 0.35, 0.6, 1, 100)
  expect_equal(class(list)[[1]], "TrapezoidalFuzzyNumberList")
  expect_equal(class(list)[[2]], "StatList")
  expect_equal(class(list)[[3]], "R6")

  expect_equal(list$getLength(), 100)
})
