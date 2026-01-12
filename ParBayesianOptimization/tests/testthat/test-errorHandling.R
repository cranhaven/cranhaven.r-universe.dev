context('errorHandling')

testthat::test_that(

  "continue"

  , {

    skip_on_cran()
    set.seed(10)
    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      if (runif(1) > 0.5) stop("You foo'd when you should have bar'd.")
      return(list(Score = sf(x,y)))
    }

    bounds = list(
      x = c(0,15)
      , y = c(-20,100)
    )

    optObj <- bayesOpt(
      FUN
      , bounds
      , initPoints = 3
      , iters.n = 6
      , errorHandling = "continue"
      , verbose = 1
    )

    expect_equal(
      optObj$stopStatus
      , "OK"
    )

  }

)

testthat::test_that(

  "Error Limit"

  , {

    skip_on_cran()
    set.seed(10)
    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      if (runif(1) > 0.5) stop("You foo'd when you should have bar'd.")
      return(list(Score = sf(x,y)))
    }

    bounds = list(
      x = c(0,15)
      , y = c(-20,100)
    )

    optObj <- bayesOpt(
      FUN
      , bounds
      , initPoints = 3
      , iters.n = 8
      , errorHandling = 2
      , verbose = 1
    )

    expect_equal(
      optObj$stopStatus
      , ParBayesianOptimization:::makeStopEarlyMessage("Errors from FUN exceeded errorHandling limit")
    )

  }

)

testthat::test_that(

  "1D Error Handling"

  , {

    skip_on_cran()
    set.seed(14)
    sf <- function(x) 1000 - x^2

    FUN <- function(x) {
      if (runif(1) > 0.5) stop("You foo'd when you should have bar'd.")
      return(list(Score = sf(x)))
    }

    bounds = list(
      x = c(-1000,1000)
    )

    optObj <- bayesOpt(
      FUN
      , bounds
      , initPoints = 3
      , iters.n = 8
      , errorHandling = 2
      , verbose = 1
    )

    optObj$scoreSummary

    expect_equal(
      optObj$stopStatus
      , ParBayesianOptimization:::makeStopEarlyMessage("Errors from FUN exceeded errorHandling limit")
    )

  }

)

testthat::test_that(

  "Malformed FUN Return"

  , {

    skip_on_cran()
    set.seed(14)
    sf <- function(x) 1000 - x^2

    FUN <- function(x) {
      ot <- if (runif(1) > 0.75) c(0,1) else 1
      return(list(Score = sf(x), ot = ot))
    }

    bounds = list(
      x = c(-1000,1000)
    )

    expect_error(
      bayesOpt(
        FUN
        , bounds
        , initPoints = 3
        , iters.n = 8
        , errorHandling = 2
        , verbose = 1
      )
    )

  }

)
