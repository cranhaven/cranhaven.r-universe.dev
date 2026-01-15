test_that("Macdonald at q=0 is Hall-Littlewood - P", {
  n <- 4
  lambda <- c(2, 2)
  hlPoly <- HallLittlewoodPol(n, lambda, "P")
  macPoly <- MacdonaldPol(n, lambda, "P")
  coeffs <- lapply(macPoly@coeffs, function(rOQ) {
    swapVariables(
      substituteRatioOfQsprays(rOQ, values = c(0, NA)),
      1, 2
    )
  })
  obtained <- new(
    "symbolicQspray",
    powers = macPoly@powers,
    coeffs = coeffs
  )
  expect_true(obtained == hlPoly)
})

test_that("Macdonald at q=0 is Hall-Littlewood - Q", {
  n <- 3
  lambda <- c(2, 1)
  hlPoly <- HallLittlewoodPol(n, lambda, "Q")
  macPoly <- MacdonaldPol(n, lambda, "Q")
  coeffs <- lapply(macPoly@coeffs, function(rOQ) {
    swapVariables(
      substituteRatioOfQsprays(rOQ, values = c(0, NA)),
      1, 2
    )
  })
  obtained <- new(
    "symbolicQspray",
    powers = macPoly@powers,
    coeffs = coeffs
  )
  expect_true(obtained == hlPoly)
})
