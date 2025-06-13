### R code from vignette source 'examples.Rnw'

###################################################
### code chunk number 1: misc
###################################################

library(sse)


###################################################
### code chunk number 2: pPmini
###################################################

psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 60, by = 10))


###################################################
### code chunk number 3: pFmini
###################################################

powFun <- function(psi)
{
  return(power.t.test(n = n(psi)/2, 
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}


###################################################
### code chunk number 4: pCmini
###################################################

calc <- powCalc(psi, statistic = powFun)


###################################################
### code chunk number 5: pmini
###################################################

pow <- powEx(x = calc, theta = 1, power = 0.9)


###################################################
### code chunk number 6: inspectmini
###################################################

inspect(pow)


###################################################
### code chunk number 7: examples.Rnw:102-104
###################################################

pow <- update(pow, n = seq(from = 20, to = 60, by = 2))


###################################################
### code chunk number 8: plotmini
###################################################

plot(pow,
     xlab = "Effect size",
     ylab = "Total sample size")


###################################################
### code chunk number 9: texmini
###################################################

tex(pow, type = "nEval")


###################################################
### code chunk number 10: pFresample
###################################################

powFun.resample <- function(psi)
{
  x <- rnorm(n(psi)/2)
  y <- rnorm(n(psi)/2) + theta(psi)
  return(wilcox.test(x = x, y = y)$p.value < 0.05)
}


###################################################
### code chunk number 11: pCresample
###################################################

calc.resample <- powCalc(psi,
                         statistic = powFun.resample,
                         n.iter = 99)


###################################################
### code chunk number 12: examples.Rnw:166-168 (eval = FALSE)
###################################################
## 
## save(calc.resample, file = "dat/calc.resample.rda")


###################################################
### code chunk number 13: examples.Rnw:171-173 (eval = FALSE)
###################################################
## 
## load(file = "dat/calc.resample.rda")


###################################################
### code chunk number 14: pMresample
###################################################

pow.resample <- powEx(calc.resample, theta = 1, power = 0.9)


###################################################
### code chunk number 15: inspectresample
###################################################

inspect(pow.resample)


###################################################
### code chunk number 16: refine
###################################################

pow.resample <- refine(pow.resample, factor = 5)


###################################################
### code chunk number 17: examples.Rnw:221-222 (eval = FALSE)
###################################################
## tex(pow.resample, type = "sampling")


###################################################
### code chunk number 18: examples.Rnw:227-228 (eval = FALSE)
###################################################
## tex(pow.resample, type = "n.iter")


###################################################
### code chunk number 19: createCalc
###################################################
##
pilot.data <- rnorm(1000)

##
psi <- powPar(F.hat = pilot.data,
              delta = seq(from = 0.5, to = 1.5, by = 0.05),
              n = seq(from = 20, to = 50, by = 2),
              theta.name = "delta")
##
powFun <- function(psi){
   a <- sample(pp(psi, "F.hat"), size = n(psi)/2, replace = TRUE)
   b <- sample(pp(psi, "F.hat"), size = n(psi)/2, replace = TRUE) + theta(psi)
   w <- wilcox.test(a, b)$p.value < 0.05
   t <- t.test(a, b)$p.value < 0.05
   return(c(w = w, t = t))
   }


calc <- powCalc(psi, statistic = powFun, n.iter = 99)
pow.t <- powEx(calc, theta = 1, drop = 0.1, endpoint = "t")
pow.w <- powEx(calc, theta = 1, drop = 0.1, endpoint = "w")


###################################################
### code chunk number 20: wilcox
###################################################
plot(pow.w, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "Wilcoxon Test")


###################################################
### code chunk number 21: t
###################################################
plot(pow.t, smooth = 0.5,
     xlab = expression(paste("Delta, ", delta)),
     ylab = "Total sample size",
     main = "t Test")


###################################################
### code chunk number 22: coarse
###################################################
## as used in the vignette(examples)
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.5),
              n = seq(from = 20, to = 200, by = 20))
##
powFun.power <- function(psi)
{
  return(power.t.test(n = n(psi)/2, 
                      delta = theta(psi),
                      sig.level = 0.05)$power)
}
##
calc.power <- powCalc(psi, statistic = powFun.power)
##
pow.power <- powEx(calc.power, theta = 1, power = 0.9)
##
plot(pow.power)
##
calc.power.fine <- update(calc.power, 
                     n = c(seq(from = 20, to = 60, by = 2), 
                           seq(from = 80, to = 200, by = 20)),
                     theta = seq(from = 0.5, to = 1.5, by = 0.05))
##
pow.power.fine <- powEx(calc.power.fine, theta = 1, power = 0.9)
##
plot(pow.power.fine,
     xlim = c(0.79, 1.21),
     ylim = c(28, 62)
     )


