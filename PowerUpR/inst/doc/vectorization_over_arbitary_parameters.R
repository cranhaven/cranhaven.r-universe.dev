## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(PowerUpR)

## ----  message=FALSE, eval=FALSE-----------------------------------------
#  install.packages("PowerUpR")
#  library(PowerUpR)

## ---- message=FALSE, fig.width=7, fig.height=5, results = FALSE----------
custom_fun <- function(x) {
  parms <- list(rho3 = x,
                power = .80, rho2 = .06,
                g3 = 1, r21 = .55, r22 = .50, r23 = .45,
                p = .40, n = 10, J = 2, K = 83)
  design <- do.call("mdes.cra3", parms)
  design$mdes[1]
}

x = seq(.10,.90,.01)
mdes <- mapply(custom_fun, x)
plot(x, mdes, type = "l", xlab = "rho3")


## ---- message=FALSE, fig.width=7, fig.height=5, results = FALSE----------
custom_fun <- function(x, y) {
  parms <- list(K = x, r23 = y, 
                es = .23, rho2 = .06, rho3 = .18,
                g3 = 1, r21 = .55, r22 = .50,
                p = .40, n = 10, J = 2)
  design <- do.call("power.cra3", parms)
  design$power
}

x = seq(10,100,5)
power.r23.30 <- mapply(custom_fun, x, .30)
power.r23.40 <- mapply(custom_fun, x, .40)
power.r23.50 <- mapply(custom_fun, x, .50)
power.r23.60 <- mapply(custom_fun, x, .60)

# plot
plot(x, power.r23.30, pch = 18, type = "b",
     ylim = c(0,1),  xlab = "K", ylab = "Power")
lines(x, power.r23.40, col = 2, pch = 19, type = "b")
lines(x, power.r23.50, col = 3, pch = 20, type = "b")
lines(x, power.r23.60, col = 4, pch = 21, type = "b")
legend("bottomright",  bty = "n",
       legend = c("r23=.30", "r23=.40", "r23=.50", "r23=.60"), 
       col = c(1, 2, 3, 4), lty = c(1, 1, 1, 1), pch = c(18, 19, 20, 21))
grid(nx = 20, ny = 18)

## ---- message=FALSE, fig.width=7, fig.height=5, results=FALSE------------
custom_fun <- function(x) {
  parms <- list(es = x, power = .80, rho2 = .06, rho3 = .18,
                g3 = 1, r21 = .55, r22 = .50, r23 = .45,
                p = .40, n = 10, J = 2)
  design <- do.call("mrss.cra3", parms)
  design$K
}

x = seq(.10,.50,.05)
K <- mapply(custom_fun, x)
table <- data.frame(es = x, K = K)

## ---- message=FALSE------------------------------------------------------
print(table)

## ---- message=FALSE, fig.width=7, fig.height=5, results=FALSE------------
custom_fun <- function(x1,x2) {
  parms <- list(es = x1, r23 = x2,
                power = .80, rho2 = .06, rho3 = .18,
                g3 = 1, r21 = .55, r22 = .50,
                p = .40, n = 10, J = 2)
  design <- do.call("mrss.cra3", parms)
  design$K
}

vec.custom_fun <- Vectorize(custom_fun, c("x1", "x2"))

x1 = seq(.10,.50,.05)
x2 = seq(.20,.70,.10)
  
table.K <- outer(x1, x2, vec.custom_fun)
rownames(table.K) <- paste0("es=",x1)
colnames(table.K) <- paste0("r23=",x2)

## ------------------------------------------------------------------------
print(table.K)

