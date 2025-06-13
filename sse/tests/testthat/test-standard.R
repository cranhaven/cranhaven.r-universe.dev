context("Standard use of sse")

library(testthat)
library(sse)
set.seed(123)
## as used in the vignette(examples)
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 2))
psi2 <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 2),
              xi = c(0.05, 0.1))
##
powFun.power <- function(psi){
  return(power.t.test(n = n(psi) / 2,
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}
powFun.power2 <- function(psi){
  return(power.t.test(n = n(psi) / 2,
                      delta = theta(psi),
                      sig.level = xi(psi2))$power)
}
powFun.resample <- function(psi){
  x <- rnorm(n(psi) / 2)
  y <- rnorm(n(psi) / 2) + theta(psi)
  return(wilcox.test(x = x, y = y)$p.value < 0.05)
}


##
calc.power <- powCalc(psi, statistic = powFun.power)
calc.power2 <- powCalc(psi2, statistic = powFun.power2)
calc.resample <- powCalc(psi, statistic = powFun.resample, n.iter = 33)
# calc.resample <- powCalc(psi, statistic = powFun.resample, n.iter = -1) ## FIXME catch error by power.R

#
calc.resample2 <- update(calc.resample, theta = 1, n.iter = 99)


##
pow.power <- powEx(calc.power, theta = 1, power = 0.9)
pow.power2 <- powEx(calc.power2, theta = 1, xi = 0.05, power = 0.9)
pow.resample <- powEx(calc.resample, theta = 1, power = 0.9)
pow.resample <- powEx(pow.resample, theta = 0.8, power = 0.9)
pow.resample2 <- powEx(calc.resample2, theta = 1, power = 0.9)
##
inspect(pow.resample)
inspect(pow.resample2)
##

plot(pow.resample)

pow.refined <- refine(pow.resample)


inspect(pow.refined)
plot(pow.refined)
#pow.refined@iter <- as.integer(33)
update(pow.refined, n = seq(from = 30, to = 60, by = 5))

pow.refined.newEx <- powEx(pow.refined, theta = 1, power = 0.9)

pow.refined2 <- refine(pow.resample, factor = 99)
inspect(pow.refined2)
plot(pow.refined2)
### --------------------------------- TEST PLOTS  -->>  CHECK THEM VISUALLY
##



plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     )

## default is without smooting ?
plot(pow.resample,
     xlab = "Effect size",
     ylab = "Total sample size")

## default is with smooting
plot(pow.resample,
     xlab = "Effect size",
     ylab = "Total sample size",
     smooth = TRUE)

## sequence of "at" elements
## is the right line emph (power = 0.9)?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     at = seq(from = 0.8, to = 0.95, by = 0.05))

## is the right line emph (power = 0.8)?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     at = seq(from = 0.8, to = 0.95, by = 0.05),
     example = FALSE)

## xlim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     xlim = c(0.79, 1.21))

## ylim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     ylim = c(30, 50))

## xlim and ylim, correct range and overall look of labels etc?
plot(pow.power,
     xlab = "Effect size",
     ylab = "Total sample size",
     xlim = c(0.79, 1.21),
     ylim = c(30, 50))

## default method for resampling is "lm" -> check: is method lm is used?
inspect(pow.resample)
inspect(pow.resample2)


## method = "step" is possible for resampling -> check: is method step is used?
# it is possible that by chance these lines fail because power is to small!
 inspect(powEx(calc.resample, theta = 1, power = 0.8, method = "step"))
 inspect(powEx(calc.resample2, theta = 1, power = 0.8, method = "step"))

inspect(powEx(calc.resample2,
              theta = 1,
              power = 0.9,
              method = "step",
              forceDivisor = 2))
inspect(
    powEx(calc.resample2,
          theta = 1,
          power = 0.9,
          method = "step",
          forceDivisor = TRUE)
)




### --------------------------------- TESTS
test_that("powPar", {
  # and not an object of class powCalc
  expect_error( # no theta
      powPar(n = seq(from = 20, to = 60, by = 2))  ## READ ERROR MESSAGE
  )
  expect_error( # an n that can not be coerced to an integer
      powPar(n  = calc.power, theta = seq(from = 0.5, to = 1.5, by = 0.05))
      ## READ ERROR MESSAGE
  )
  ## n as integer is dangerous but allowed
  ## expect_error(
  ##    powPar(n = c(TRUE, FALSE), theta = seq(from = 0.5, to = 1.5, by = 0.05))
  ## )
  expect_error(
      powPar(used.as.theta = seq(from = 0.5, to = 1.5, by = 0.05),
             n = seq(from = 20, to = 60, by = 2))
  )
  expect_error(
      powPar(used.as.theta = seq(from = 0.5, to = 1.5, by = 0.05),
             n = seq(from = 20, to = 60, by = 2),
             theta.name = "as.theta")
  )
  expect_error(
      powPar(n = c(10, 15, 15, 20),
             theta = seq(from = 0.5, to = 1.5, by = 0.05))
  )
  expect_error(
      powPar(n = c(10, 15, 20),
             theta = c(0.5, 0.6, 0.6))
  )
  expect_error(
      powPar(n = c(10, 15, 20),
             theta = c(0.5, 0.6),
             xi = c(1, 1, 2))
  )
  
})



test_that("powCalc", {
  ## expects an object of class powPar and not an object of class powCalc
  expect_error(
      powCalc(seq(from = 0, to = 100, by = 10), powFun1)
  )
  ## n.iter of length > 1
  expect_warning(
      powCalc(psi, statistic = powFun.resample, n.iter = c(4, 33))
  )
  ## ## default is using cluster
  ## expect_message(
  ##     calc.resample <- powCalc(psi,
  ##               statistic = powFun.resample, n.iter = 3, cluster = TRUE)
  ##    , "using cluster"
  ## )
  ## ## if cluster = FALSE
  ## expect_message(
  ##     calc.resample <- powCalc(psi,
  ##              statistic = powFun.resample, n.iter = 3, cluster = FALSE)
  ##     , "not using cluster"
  ## )
})



test_that("powEx", {

  ## power out of observed range
  pow.r1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.1, to = 0.5, by = 0.1),
                 n = seq(from = 20, to = 60, by = 2)),
          statistic = powFun.power),
      theta = 0.1, power = 0.9)
  expect_error(
      plot(pow.r1)  
  )

  ## power is a constant
  pow.c1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.5, to = 1.5, by = 0.1),
                 n = seq(from = 20, to = 60, by = 2)),
          statistic = function(psi){return(0.9)}),
      theta = 1, power = 0.9)
  expect_error(
      plot(pow.c1)  
  )

  ## a wrong method, e.g. linear, is not allowed
  expect_error(
      powEx(calc.power, theta = 1, method = "linear")  ## READ ERROR MESSAGE
  )

  ## theta is not in the range used
  expect_error(
      powEx(calc.power, theta = 2)  ## READ ERROR MESSAGE
  )
  ## 
  expect_error(
      powEx(calc.power, theta = 2, forceDivisor = -2)  ## READ ERROR MESSAGE
  )
  expect_error(
      powEx(calc.power, theta = 2, endpoint = "ep2")  ## READ ERROR MESSAGE
  )
  ## calc object does not use xi but example for xi provided
  expect_warning(
      powEx(calc.power, theta = 1, xi = 7)  ## READ WARNING MESSAGE
  )

  ## from the internal usage of construct.pwEx
  expect_warning(
      powEx(calc.power, theta = 1, endpoint = c("ep1", "ep2"))
  )
  expect_error(
      powEx(calc.power, theta = 1, power = 1.1)
  )
  expect_warning(
      powEx(calc.power, theta = 1, power = c(0.8, 0.9))
  )
  expect_warning(
      powEx(calc.power, theta = 1, drop = c(0.1, 0.15))
  )
  expect_warning(
      powEx(calc.power, theta = c(1, 2))
  )
  expect_warning(
      powEx(calc.power2, theta = 1, xi = c(0.05, 0.1))
  )
  expect_warning(
      powEx(calc.power, theta = 1, method = "step", lm.range = 0.2)
  )
  expect_error(
      powEx(calc.power, theta = 1, method = "lm", lm.range = 1.2)
  )
    expect_warning(
      powEx(calc.power, theta = 1, lm.range = c(0.2, 0.3))
  )
    expect_warning(
        powEx(calc.power, theta = 1, forceDivisor = 2.000001)
    )
    expect_warning(
        powEx(calc.power, theta = 1, forceDivisor = 2.000001, method = "lm")
    )
    expect_error(# FIXME
        powEx(calc.power, theta = 1, forceDivisor = list(1:2))
    )

  expect_error(
      powEx(calc.power, theta = 1, drop = 1.5)
    )

  
})


test_that("refine", {

  ## only if
  expect_error(
      refine(pow.power),
      strwrap(
          "Additional iterations for the chosen example are only
            meaningful if the object was created using resampling.",
          prefix = " ", initial = ""))
  ## does not work for objects of class calc
  expect_error(
      refine(calc.resample, factor = 0.5)  ## READ WARNING MESSAGE
  )
  ## factor must be larger than 1
  expect_error(
      refine(pow.resample, factor = 0.5)  ## READ WARNING MESSAGE
  )
  ## factor must be larger than 1
  expect_error(
      refine(pow.resample, factor = 1)  ## READ WARNING MESSAGE
  )
})


### ------------------------------------------------------------------ plot
test_that("plot", {

  ##
  expect_warning(
      plot(pow.power, ylim = c(30, 40))  ## READ WARNING MESSAGE
  )

  ## auto detect best divisor
  pow.d0 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                 n = c(13, 26)),
          statistic = powFun.power),
      theta = 1, power = 0.5, forceDivisor = TRUE)
  expect_equal(pow.d0@divisor, as.integer(13))


  ## divisior changes number return
  pow.d1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                 n = seq(from = 20, to = 60, by = 1)),
          statistic = powFun.power),
      theta = 1, power = 0.7, forceDivisor = 4)#, method = "lm")
  expect_message(
      plot(pow.d1)
  )
  expect_message(
      inspect(pow.d1)
  )
  ## divisior changes number return
  pow.d1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                 n = seq(from = 20, to = 60, by = 1)),
          statistic = powFun.power),
      theta = 1, power = 0.7, forceDivisor = 4, method = "lm")
  expect_message(
      plot(pow.d1)
  )
  expect_message(
      inspect(pow.d1)
  )

  ## with length of theta == 1, plotting is not possible
  pow.t1 <- powEx(
      powCalc(
          powPar(theta = 0.5,
                 n = seq(from = 20, to = 60, by = 2)),
          statistic = powFun.power),
      theta = 0.5, power = 0.9)
  expect_error(
  plot(pow.t1)
  )

  ## with length of n == 1, plotting is not possible
  pow.n1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
                 n = 20),
          statistic = powFun.power),
      theta = 0.5, power = 0.9)
  expect_error(
      plot(pow.n1)
  )
  
  ## example outside of plotting range
  pow.o1 <- powEx(calc.power, theta = 0.5, power = 0.9)
  expect_warning(
      plot(pow.o1)
  )
  expect_warning(
      inspect(pow.o1)
  )

  ## argument smooth
  expect_warning(
      plot(pow.power, smooth = c(1, 2))
    )
  expect_error(
      plot(pow.power, smooth = -1)
  )

  ## argument at
  expect_error(
      plot(pow.power, at = FALSE)
  )
  expect_error(
      plot(pow.power, at = -0.5)
  )

  expect_warning(
      plot(pow.power, xlim = c(0.1, 0.8))
  )
  
})


## a helper function for latex strings
## prep.tex <- function(string){
##   new.string <- gsub("\\", "", string, fixed = TRUE)
##   new.string <- gsub("{", "", new.string, fixed = TRUE)
##   new.string <- gsub("}", "", new.string, fixed = TRUE)
##   return(new.string)
## }


### ------------------------------------------------------------------ tex
test_that("tex methods", {
  expect_match(tex(pow.power, "drop"), "0~\\\\%", fixed = TRUE)
  expect_match(tex(pow.power, "sampling"),
               "$n_{i=1,...,21} = 20, ..., 60$", fixed = TRUE)
  expect_equal(tex(pow.power, "nRec"), 46)
  expect_equal(tex(pow.power, "nEval"), 46)
  expect_equal(tex(pow.power, "theta"), 1)
  expect_equal(tex(pow.power, "xi"), as.numeric(NA))
  expect_equal(tex(pow.power, "power"), 0.9)
  expect_equal(tex(pow.power, "n.iter"), as.numeric(NA))
  expect_error(tex(pow.power, "hallo"))
})



### ------------------------------------------------------------------ show
test_that("show", {
  show(pow.power)                                           # READ CAREFULLY

  ## information about resampling
  show(pow.resample)                                           # READ CAREFULLY
  
  ## with additional parameters in powPar
  pow.s1 <- powEx(
      powCalc(
          powPar(theta = seq(from = 0.1, to = 0.5, by = 0.1),
                 n = seq(from = 20, to = 60, by = 2),
                 extra = 1:8),
          statistic = powFun.power),
      theta = 0.1, power = 0.9)

  show(pow.s1)
})




### ---------------------------------------------------------------- powFunGen
test_that("powFunGen", {
  
powFun.matrix <- function(psi){
  return(matrix(1:16))
}
expect_error(
    calc.power <- powCalc(psi, statistic = powFun.matrix)
)



})
