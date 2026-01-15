test_that("Skew Macdonald at q=0 is skew Hall-Littlewood - P", {
  n <- 4
  lambda <- c(3, 2)
  mu <- c(1)
  hlPoly <- SkewHallLittlewoodPol(n, lambda, mu, "P")
  macPoly <- SkewMacdonaldPol(n, lambda, mu, "P")
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

test_that("Skew Macdonald at q=0 is skew Hall-Littlewood - Q", {
  n <- 3
  lambda <- c(3, 2)
  mu <- c(1, 1)
  hlPoly <- SkewHallLittlewoodPol(n, lambda, mu, "Q")
  macPoly <- SkewMacdonaldPol(n, lambda, mu, "Q")
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
