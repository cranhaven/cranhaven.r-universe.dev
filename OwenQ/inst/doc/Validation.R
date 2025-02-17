## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE)
library(OwenQ)

## -----------------------------------------------------------------------------
testData <- data.frame(
  h = c(0.0625, 6.5, 7, 4.78125, 2, 1), 
  a = c(0.25, 0.4375, 0.96875, 0.0625, 0.5, 0.9999975),
  Patefield = c(3.8911930234701e-02, 2.0005773048508e-11, 6.3990627193899e-13, 1.0632974804687e-07, 8.6250779855215e-03, 6.6741808978229e-02),
  OwenT = numeric(6)
)
for(i in 1:nrow(testData)){
  testData$OwenT[i] <- OwenT(testData$h[i], testData$a[i])
}
print(testData, digits=14)

## -----------------------------------------------------------------------------
# wolfram: Integrate[(1+Erf[(3*x/Sqrt[3]-2)/Sqrt[2]])*x^(3-1)*Exp[-x^2/2],{x,0,5}]/2/Gamma[3/2]/2^((3-2)/2)
OwenQ1(3, 3, 2, 5)

## -----------------------------------------------------------------------------
# wolfram: Integrate[(1+Erf[(3*x/Sqrt[1000]-2)/Sqrt[2]])*x^(1000-1)*Exp[-x^2/2],{x,0,30}]/2/Gamma[1000/2]/2^((1000-2)/2)
print(OwenQ1(1000, 3, 2, 30), digits=16)

## -----------------------------------------------------------------------------
# wolfram: Integrate[(1+Erf[(3*x/Sqrt[3]-2)/Sqrt[2]])*x^(3-1)*Exp[-x^2/2],{x,5,Infinity}]/2/Gamma[3/2]/2^((3-2)/2)
OwenQ2(3, 3, 2, 5)

## -----------------------------------------------------------------------------
# wolfram: Integrate[(1+Erf[(3*x/Sqrt[1000]-2)/Sqrt[2]])*x^(1000-1)*Exp[-x^2/2],{x,5,Infinity}]/2/Gamma[1000/2]/2^((1000-2)/2)
print(OwenQ2(1000, 3, 2, 5), digits=16)

## -----------------------------------------------------------------------------
powerTOST <- function(alpha, delta0, Delta, sigma, n1, n2, algo=2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  powen4(dof, q, -q, delta1, delta2, algo=algo)
}

## ----echo=FALSE---------------------------------------------------------------
SAS <- structure(list(alpha = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.01, 0.01, 0.01, 0.01, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 
0.05, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
0.05, 0.05, 0.01, 0.01, 0.05, 0.05), delta0 = c(0, 0, 0, 0, 0, 
0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 
0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 
0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 
0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 
0.5, 0.5, 0, 1, 2, 2.5, 0, 1, 2, 2.5, 0, 1, 2, 3, 0, 1, 2, 3, 
0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 4, 0, 4, 0, 
4, 0, 4), Delta = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 
5, 5), sigma = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 7, 7, 7, 7, 7, 7, 7, 7, 4, 4, 
4, 4, 8, 8, 8, 8, 10, 10, 10, 10, 14, 14, 14, 14, 35, 35, 30, 
50, 6, 6, 9, 9), n1 = c(10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 
10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 
15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 
20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 20, 
25, 30, 50, 50, 50, 50, 10, 10, 10, 10, 100, 100, 100, 100, 100, 
100, 100, 100, 185, 185, 185, 185, 185, 185, 185, 185, 250, 250, 
250, 250, 500, 500, 500, 500, 600, 600, 600, 600, 1190, 1190, 
1190, 1190), n2 = c(10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 
15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 20, 25, 30, 10, 15, 
20, 25, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 
30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 
30, 50, 50, 50, 50, 90, 90, 90, 90, 100, 100, 100, 100, 100, 
100, 100, 100, 10, 10, 10, 10, 100, 100, 100, 100, 250, 250, 
250, 250, 500, 500, 500, 500, 600, 600, 600, 600, 10, 10, 10, 
10), powerPASS = c(0.3909, 0.6954, 0.8558, 0.9343, 0.9709, 0.3827, 
0.6784, 0.8366, 0.9178, 0.9589, 0.3591, 0.6295, 0.7807, 0.868, 
0.9202, 0.3229, 0.5554, 0.6932, 0.7847, 0.8491, 0.2782, 0.4653, 
0.5831, 0.6717, 0.7426, 0.2297, 0.3697, 0.4621, 0.5388, 0.606, 
0.7037, 0.8576, 0.9232, 0.9545, 0.9709, 0.6865, 0.8385, 0.906, 
0.94, 0.9589, 0.637, 0.7826, 0.8544, 0.895, 0.9202, 0.5619, 0.695, 
0.7694, 0.8168, 0.8491, 0.4706, 0.5847, 0.6561, 0.7059, 0.7426, 
0.3736, 0.4634, 0.5247, 0.5705, 0.606, 0.9624, 0.7985, 0.3433, 
0.1529, 0.8179, 0.7035, 0.436, 0.2982, 0.9828, 0.9151, 0.6438, 
0.2617, 0.9083, 0.7492, 0.3744, 0.0929, 0.8457, 0.7303, 0.4546, 
0.19, 0.9824, 0.9142, 0.6423, 0.261, 0.9671, 0.8452, 0.4616, 
0.1129, 0.9714, 0.8552, 0.4733, 0.1161, 0.1179, 0.0169, 0.7856, 
0.0268, 0.2343, 0.0277, 0.0836, 0.0315), powerSAS = c(0.39094, 
0.69541, 0.8558, 0.93426, 0.97092, 0.38272, 0.67836, 0.83661, 
0.91781, 0.95889, 0.35908, 0.62954, 0.78069, 0.86796, 0.92017, 
0.32287, 0.5554, 0.69322, 0.78471, 0.84909, 0.2782, 0.46531, 
0.58306, 0.67174, 0.7426, 0.2297, 0.36967, 0.46208, 0.53883, 
0.606, 0.70373, 0.85764, 0.92324, 0.95452, 0.97092, 0.68648, 
0.83846, 0.90604, 0.94003, 0.95889, 0.63703, 0.78255, 0.85439, 
0.89501, 0.92017, 0.56192, 0.69503, 0.76944, 0.81676, 0.84909, 
0.47061, 0.5847, 0.6561, 0.70594, 0.7426, 0.37365, 0.46343, 0.52475, 
0.57052, 0.606, 0.96239, 0.79849, 0.34329, 0.15288, 0.81791, 
0.70346, 0.43595, 0.29819, 0.98278, 0.91512, 0.64376, 0.26169, 
0.90831, 0.74924, 0.37443, 0.0929, 0.84568, 0.73025, 0.45459, 
0.19001, 0.9824, 0.91417, 0.64226, 0.26103, 0.96713, 0.84523, 
0.46161, 0.11288, 0.97112, 0.85433, 0.47184, 0.11536, 0.11547, 
0.01658, 0.78512, 0.02642, 0.23194, 0.02739, 0.08256, 0.03115
)), .Names = c("alpha", "delta0", "Delta", "sigma", "n1", "n2", 
"powerPASS", "powerSAS"), class = "data.frame", row.names = c(NA, 
-100L))
SAS <- SAS[,-7]

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(SAS, row.names = TRUE)

## -----------------------------------------------------------------------------
power <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  power[i] <-
    powerTOST(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}

## -----------------------------------------------------------------------------
identical(round(power,5), SAS$powerSAS)

## -----------------------------------------------------------------------------
power <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  power[i] <-
    powerTOST(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i],
      algo = 1
    )
}
identical(round(power,5), SAS$powerSAS)

## -----------------------------------------------------------------------------
ipowerTOST <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  OwenQ:::ipowen4(dof, q, -q, delta1, delta2)
}

## ---- echo=FALSE, fig.width=8, fig.height=4-----------------------------------
oldpar <- par(mar=c(4,4,0.4,0.4))
layout(t(c(1,2)))
sigma <- seq(65,69,len=100)
n1 <- 1000; n2 <- 1000
plot(sigma, powerTOST(0.05, 0, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.05, 0, 5, sigma, n1, n2))
lines(sigma, y, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
abline(h=0, col="green", lty=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
sigma <- seq(15,69,len=100)
plot(sigma, powerTOST(0.05, 0, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.05, 0, 5, sigma, n1, n2))
lines(sigma, y, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
abline(h=0, col="green", lty=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
par(oldpar)

## ---- echo=FALSE, fig.width=4, fig.height=4-----------------------------------
oldpar <- par(mar=c(4,4,0.4,0.4))
n1 <- n2 <- 720
sigma <- seq(56,57,len=100)
plot(sigma, powerTOST(0.05, 0, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.05, 0, 5, sigma, n1, n2))
lines(sigma, y, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
par(oldpar)

## ---- echo=FALSE, fig.width=8, fig.height=4-----------------------------------
oldpar <- par(mar=c(4, 4, 0.2, 0.2))
layout(t(c(1,2)))
n1 <- n2 <- 700
sigma <- seq(35,45,len=100)
plot(sigma, powerTOST(0.01, 1, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.01, 1, 5, sigma, n1, n2))
lines(sigma, y, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
n1 <- n2 <- 700
sigma <- seq(38.5,39,len=100)
plot(sigma, powerTOST(0.01, 1, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.01, 1, 5, sigma, n1, n2))
lines(sigma, y, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
par(oldpar)

## ---- echo=FALSE, fig.width=4, fig.height=4-----------------------------------
oldpar <- par(mar=c(4, 4, 0.7, 0.2))
n1 <- n2 <- 600
sigma <- seq(30,36,len=100)
plot(sigma, powerTOST(0.005, 0, 5, sigma, n1, n2), type="l", lwd=2, 
     xlab=expression(sigma), ylab="power")
y <- sapply(sigma, function(sigma) ipowerTOST(0.005, 0, 5, sigma, n1, n2))
lines(sigma, y, pch=19, col="blue", lwd=2)
y <- sapply(sigma, function(sigma) powerTOST(0.05, 0, 5, sigma, n1, n2, algo=1))
lines(sigma, y, col="red", lwd=2)
legend("topright", c("powen4 - 1", "powen4 - 2", "ipowen4"), 
       lty=c(1,1,1), col=c("red", "black", "blue"))
par(oldpar)

## -----------------------------------------------------------------------------
alpha <- 0.05; delta0 <- 0; Delta <- 5
sigma <- 110
n1 <- n2 <- 2500
powerTOST(alpha, delta0, Delta, sigma, n1, n2)
ipowerTOST(alpha, delta0, Delta, sigma, n1, n2)

## -----------------------------------------------------------------------------
sigma <- 152
n1 <- n2 <- 5000
powerTOST(alpha, delta0, Delta, sigma, n1, n2)
ipowerTOST(alpha, delta0, Delta, sigma, n1, n2)

## -----------------------------------------------------------------------------
power <- ipower <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  power[i] <-
    powerTOST(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
  ipower[i] <-
    ipowerTOST(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}
identical(round(power, 10), round(ipower, 10))

## -----------------------------------------------------------------------------
power <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  power[i] <-
    powerTOST(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i], 
      algo = 1
    )
}
identical(round(power, 10), round(ipower, 10))

## -----------------------------------------------------------------------------
f <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  powen1(dof, q,-q, delta1, delta2) + powen2(dof, q,-q, delta1, delta2) + 
    powen3(dof, q,-q, delta1, delta2) + powen4(dof, q,-q, delta1, delta2)
}
test <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  test[i] <-
    f(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}
all(abs(test-1) < 1e-14)

## -----------------------------------------------------------------------------
powerTOST2 <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  R <- sqrt(dof)*(delta1 - delta2)/q/2
  OwenQ1(dof, -q, delta2, R) - OwenQ1(dof, q, delta1, R)
}
power2 <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  power2[i] <-
    powerTOST2(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}
all(abs(power - power2) < 1e-9)

## -----------------------------------------------------------------------------
g <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  R <- sqrt(dof)*(delta1 - delta2)/q/2
  x <- OwenQ2(dof, q, delta1, R) - OwenQ2(dof, -q, delta2, R)
  y <- powen2(dof, q, -q, delta1, delta2)
  x - y
}
test <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  test[i] <-
    g(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}
all(abs(test) < 1e-15)

## -----------------------------------------------------------------------------
h <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  x <- ptOwen(q, dof, delta1)
  y <- powen1(dof, q, -q, delta1, delta2) + powen2(dof, q, -q, delta1, delta2)
  x - y
}
test <- numeric(nrow(SAS))
for (i in 1:nrow(SAS)) {
  test[i] <-
    h(
      alpha = SAS$alpha[i],
      delta0 = SAS$delta0[i],
      Delta = SAS$Delta[i],
      sigma = SAS$sigma[i],
      n1 = SAS$n1[i],
      n2 = SAS$n2[i]
    )
}
all(abs(test) < 1e-15)

