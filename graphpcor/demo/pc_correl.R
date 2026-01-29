
library(graphpcor)

## A base correlation matrix
c0 <- matrix(c(1,.8,-.625, 0.8,1,-.5, -0.625,-.5,1), 3)
c0
p <- ncol(c0)

## The (dense) cgeneric model for correlation matrix
## considering the CPC parametrization
M1 <- cgeneric("pc_correl", n = p,
               base = c0, lambda = 1,
               useINLAprecomp = FALSE)
M5 <- cgeneric("pc_correl", n = p,
               base = c0, lambda = 5,
               useINLAprecomp = FALSE)

M1

dataf0 <- data.frame(
    i = 1:p,
    y = NA
)

library(INLA)

fh <- list(prec = list(initial = 20, fixed = TRUE))

fit0 <- inla(
    y ~ 0 + f(i, model = c0model),
    data = dataf0,
    control.family = list(hyper = fh)
)

th0 <- fit0$mode$theta

cholcor(th0)
tcrossprod(cholcor(th0))

basecor(base = th0, p = p)

thpost <- inla.hyperpar.sample(n = 3000, result = fit0)
summary(thpost)

iil <- which(lower.tri(c0))
iil

corpost <- t(sapply(1:nrow(thpost), function(i)
    tcrossprod(cholcor(thpost[i, ]))[iil]))
summary(corpost)

thlabs <- lapply(1:3, function(i) as.expression(bquote(theta[.(i)])))
clabs <- list(expression(rho[1~","~2]),
              expression(rho[1~","~3]),
              expression(rho[2~","~3]))

k1 <- k2 <- 0
par(mfrow = c(3, 3), mar = c(3,3,0.5,0.5), mgp = c(2,0.5,0), bty = "n")
for(i in 1:p) {
    for(j in 1:p) {
        if (i==j) {
            plot(0, type = "n", xlab = "", ylab = "", axes = FALSE)
        }
        if(i<j) {
            k1 <- k1 + 1
            hist(corpost[,k1], 100, main = '', xlab = clabs[[k1]], freq = FALSE)
        }
        if(i>j) {
            k2 <- k2 + 1
            hist(thpost[,k2], 100, main = '', xlab = thlabs[[k2]], freq = FALSE)
        }
    }
}
