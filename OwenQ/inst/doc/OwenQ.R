## ----setup, include=FALSE-----------------------------------------------------
library(OwenQ)
knitr::opts_chunk$set(collapse=TRUE)

## -----------------------------------------------------------------------------
ptOwen(q=1, nu=3, delta=2)
pt(q=1, df=3, ncp=2)

## -----------------------------------------------------------------------------
p1 <- pt(q=80, df=4, ncp=70)
p2 <- ptOwen(q=80, nu=4, delta=70)
wolfram <- 0.54742763380700947685
p1 - wolfram
p2 - wolfram

## ----ptOwen_fails-------------------------------------------------------------
ptOwen(q=50, nu=3500, delta=50)
ptOwen(q=50, nu=3600, delta=50)
ptOwen(q=50, nu=3650, delta=50)
ptOwen(q=50, nu=3660, delta=50)
ptOwen(q=50, nu=3670, delta=50)
ptOwen(q=50, nu=3680, delta=50)

## ----pt_boost-----------------------------------------------------------------
OwenQ:::pt_boost(q=50, nu=3500, delta=50)
OwenQ:::pt_boost(q=50, nu=3600, delta=50)
OwenQ:::pt_boost(q=50, nu=3650, delta=50)
OwenQ:::pt_boost(q=50, nu=3660, delta=50)
OwenQ:::pt_boost(q=50, nu=3670, delta=50)
OwenQ:::pt_boost(q=50, nu=3680, delta=50)

## -----------------------------------------------------------------------------
Delta1 <- -2; Delta2 <- 2 
mu <- 1; sigma <- 6; n <- 30L
alpha <- 0.05
nsims <- 1e6L
equivalence <- inconclusive <- numeric(nsims)
for (i in 1L:nsims) {
  y <- rnorm(n, mu, sigma)
  CI <- t.test(x = y, conf.level = 1-2*alpha)$conf.int
  equivalence[i] <- (CI[1] > Delta1) && (CI[2] < Delta2)
  inconclusive[i] <- ((CI[1] < Delta1) && (CI[2] > Delta1)) ||
    ((CI[1] < Delta2) && (CI[2] > Delta2))
}

## -----------------------------------------------------------------------------
dof <- n-1
q <- qt(1-alpha, dof)
se <- sqrt(1/n)*sigma
delta1 <- (mu-Delta1)/se; delta2 <- (mu-Delta2)/se
# probability to get equivalence
mean(equivalence)
powen4(dof, q, -q, delta1, delta2)
# probability to get inconclusive
mean(inconclusive)
ptOwen(q, dof, delta2) - ptOwen(-q, dof, delta1) - powen4(dof, q, -q, delta1, delta2)

## -----------------------------------------------------------------------------
powerTOST <- function(alpha, delta0, Delta, sigma, n1, n2) {
  se <- sqrt(1/n1 + 1/n2) * sigma
  delta1 <- (delta0 + Delta) / se
  delta2 <- (delta0 - Delta) / se
  dof <- n1 + n2 - 2
  q <- qt(1 - alpha, dof)
  powen4(dof, q, -q, delta1, delta2)
}

## ---- echo=FALSE, fig.width=5, fig.height=5-----------------------------------
h <- seq(-3, 3, length.out=30)
a <- seq(-5, 5, length.out=30)
z <- outer(h, a, Vectorize(OwenT))
oldpar <- par(mar=c(0,2,0,0))
persp(h, a, z, theta=30, phi=30, expand=0.5, zlab="Owen T", ticktype = "detailed", col = "lightblue")
par(oldpar)

## -----------------------------------------------------------------------------
p <- 0.9; alpha <- 0.05
n <- 100
delta <- sqrt(n)*qnorm((1+p)/2)
uniroot(function(ke) spowen2(n-1, ke*sqrt(n), delta) - (1-alpha), 
        lower=qnorm(1-alpha), upper=5, extendInt = "upX", tol=1e-9)$root

