context("Old style powPar object")

library(testthat)
library(sse)

test_that("theta.name", {
  psi <- powPar(delta = seq(from = 0.5, to = 1.5, by = 0.05),
                n = seq(from = 20, to = 60, by = 2),
                theta.name = "delta")
  expect_equal(theta(psi), 0.5)

  ## theta.name and theta
  expect_warning(powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                        n = seq(from = 20, to = 60, by = 2),
                        delta = 1:10,
                        theta.name = "delta"))
})



test_that("xi.name", {
  ## a xi.name without corresponding entry with xi
  expect_warning(
      powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
             n = seq(from = 20, to = 60, by = 2),
             xi = 1:3,
             xi.name = "delta")
  )
  ## a xi.name without corresponding entry without xi
  expect_error(
      powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
             n = seq(from = 20, to = 60, by = 2),
             xi.name = "delta")
  )
  
  psi.xi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                   delta = seq(from = 0.5, to = 1.5, by = 0.05),
                   n = seq(from = 20, to = 60, by = 2),
                   xi.name = "delta")
  expect_equal(xi(psi.xi), 0.5)
  expect_equal(psi.xi@xi.name, "delta")
  
  
})
