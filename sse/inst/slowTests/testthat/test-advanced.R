context("Advanced applications")

library(sse)
library(testthat)
### ------------------------------------------------------------------
### with pilot data and several endpoints
pilot.data <- rnorm(1000)
#
psi <- powPar(F.hat = pilot.data,
              delta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 50, by = 2),
              theta.name = "delta")

powFun <- function(psi){
   a <- sample(pp(psi, "F.hat"), size = n(psi) / 2, replace = TRUE)
   b <- sample(pp(psi, "F.hat"), size = n(psi) / 2, replace = TRUE) + theta(psi)
   w <- wilcox.test(a, b)$p.value < 0.05
   t <- t.test(a, b)$p.value < 0.05
   return(c(w = w, t = t))
   }

calc <- powCalc(psi, statistic = powFun, n.iter = 99)

pow.w <- powEx(calc, theta = 1, drop = 0.1, endpoint = "w")

plot(pow.w, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "Wilcoxon Test")

pow.t <- powEx(calc, theta = 1, drop = 0.1, endpoint = "t")

plot(pow.t, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "T- Test",
     ylim = c(30, 40))

## parametric resampling:
psi.parametric <- powPar(delta = seq(from = 0.5, to = 1.5, by = 0.05),
                         xi = seq(from = 0.5, to = 1.5, by = 0.5),
                         n = seq(from = 20, to = 50, by = 2),
                         theta.name = "delta")


powFun.parametric <- function(psi){
  a <- rnorm(n(psi), mean = 0, sd = xi(psi))
  b <- rnorm(n(psi), mean = theta(psi), sd = xi(psi))
  w <- wilcox.test(a, b)$p.value < 0.05
  t <- t.test(a, b)$p.value < 0.05
  ##  cat(paste("xi:", xi(psi), "theta:", theta(psi), "\n"))    ## UNCOMMENT to see what is done!
  return(c(w = w, t = t))
}

calc.parametric <- powCalc(psi.parametric,
                           statistic = powFun.parametric,
                           n.iter = 99)

pow.t.parametric <- powEx(calc.parametric,
                          xi = 1,
                          theta = 1,
                          drop = 0.1,
                          endpoint = "t")

pow.w.parametric <- powEx(calc.parametric,
                          xi = 1,
                          theta = 1,
                          drop = 0.1,
                          endpoint = "w")

pow.t.parametric.xi05 <- powEx(calc.parametric,
                               xi = 0.5,
                               theta = 1,
                               drop = 0.1,
                               endpoint = "t")

plot(pow.t.parametric)

show(pow.t.parametric)

### --------------------------------- TESTS
test_that("endpoints", {
#
  ## selecting an endpoint that does not exist
  expect_error(
      powEx(calc, theta = 1, drop = 0.1, endpoint = "T")  ## READ ERROR MESSAGE
  )
  expect_equal(pow.t.parametric@endpoint.example, "t")
})


test_that("powPar with data", {
#
  ## data extracted from psi is like the original
  expect_equal(pp(psi, "F.hat"), pilot.data)
})


test_that("refine", {
  pow.w.rf <- refine(pow.w)
  expect_equal(pow.w.rf@iter, pow.w@iter)
  expect_equal(pow.w.rf@iter.example, pow.w@iter * 10)
})
test_that("refine with xi", {
  pow.w.parametric.rf <- refine(pow.w.parametric) # check the values for xi and theta that are evaluated (cat())
  expect_equal(pow.w.parametric.rf@iter, pow.w.parametric@iter)
  expect_equal(pow.w.parametric.rf@iter.example, pow.w.parametric@iter * 10)
})

test_that("powEx", {
  ## choosing and endpoint for example that is not part of the calc-object
  expect_error(powEx(calc.power, theta = 2, endpoint = "s"))  ## READ MESSAGE
})

test_that("plot", {
  ## power is constant at 1
  expect_error(
      plot(pow.t.parametric.xi05)
  )
})

test_that("List as return", {
  powFun.list <- function(psi){
    a <- sample(pp(psi, "F.hat"), size = n(psi) / 2, replace = TRUE)
    b <- sample(pp(psi, "F.hat"),
                size = n(psi) / 2, replace = TRUE) + theta(psi)
    w <- wilcox.test(a, b)$p.value < 0.05
    t <- t.test(a, b)$p.value < 0.05
    length(c(a,b) %% 2)
    return(list(power = c(w = w, t = t), size = sum(c(a,b) %/% 2)))
}
expect_error(    
    calc.power <- powCalc(psi, statistic = powFun.list)
)
})
