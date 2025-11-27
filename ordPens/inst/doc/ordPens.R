## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",  
  out.extra = 'style="border:0; margin: auto"', 
fig.width=5,
fig.height=5,
out.width="400px",  
out.height="400px"  
)

## ----setup, results="hide", warning=FALSE,message=FALSE-----------------------
library(ordPens) 

## -----------------------------------------------------------------------------
set.seed(123)

# generate (ordinal) predictors
x1 <- sample(1:8, 100, replace = TRUE)
x2 <- sample(1:6, 100, replace = TRUE)
x3 <- sample(1:7, 100, replace = TRUE)

# the response
y <- -1 + log(x1) + sin(3*(x2-1)/pi) + rnorm(100)

# x matrix
x <- cbind(x1,x2,x3)

# lambda values
lambda <- c(1000, 500, 200, 100, 70, 50, 30, 20, 10, 1) 

## ---- figures-side, fig.show="hold", fig.width = 3, fig.height = 3, out.width="31%",out.height="30%"----
osm1 <- ordSmooth(x = x, y = y, lambda = lambda)
osl <- ordSelect(x = x, y = y, lambda = lambda)
ofu <- ordFusion(x = x, y = y, lambda = lambda) 

par(mar = c(4.1, 4.1, 3.1, 1.1)) 
plot(osm1)
plot(osl, main = "")
plot(ofu, main = "")

## ---- results = "hide"--------------------------------------------------------
round(osm1$coefficients, digits = 3)
round(osl$coefficients, digits = 3)
round(ofu$coefficients, digits = 3)

## ----coef_path2, figures-side, fig.show="hold"--------------------------------
matplot(log(lambda), t(osm1$coefficients), type = "l", xlab = expression(log(lambda)),
        ylab = "Coefficients", col = c(1, rep(2,8), rep(3,6), rep(4,7)), cex.main = 1,
        main = "Smoothing", xlim = c(max(log(lambda)), min(log(lambda))))
axis(4, at = osm1$coefficients[,ncol(osm1$coefficients)], label = rownames(osm1$coefficients), 
     line = -.5, las = 1, tick = FALSE, cex.axis = 0.75)

matplot(log(lambda), t(osl$coefficients), type = "l", xlab = expression(log(lambda)), 
        ylab = "Coefficients", col = c(1, rep(2,8), rep(3,6), rep(4,7)), cex.main = 1, 
        main = "Selection",  xlim = c(max(log(lambda)), min(log(lambda))))
axis(4, at = osl$coefficients[,ncol(osl$coefficients)], label = rownames(osl$coefficients), 
     line = -.5, las = 1, tick = FALSE, cex.axis = 0.75)

matplot(log(lambda), t(ofu$coefficients), type = "l", xlab = expression(log(lambda)), 
        ylab = "Coefficients", col = c(1, rep(2,8), rep(3,6), rep(4,7)), cex.main = 1, 
        main = "Fusion", xlim = c(max(log(lambda)), min(log(lambda))))
axis(4, at = ofu$coefficients[,ncol(ofu$coefficients)], label = rownames(ofu$coefficients), 
     line = -.5, las = 1, tick = FALSE, cex.axis = 0.75)

## -----------------------------------------------------------------------------
x1 <- as.ordered(x1)
x2 <- as.ordered(x2)
x3 <- as.ordered(x3)

u1 <- sample(1:8, 100, replace = TRUE)
u <- cbind(u1)
osm2 <- ordSmooth(x = x, y = y, u = u, lambda = lambda)

## -----------------------------------------------------------------------------
gom1 <- gam(y ~ s(x1, bs = "ordinal", m = 1) + s(x2, bs = "ordinal", m = 1) + 
              s(x3, bs = "ordinal", m = 1) + factor(u1), method = "REML")

## -----------------------------------------------------------------------------
gom2 <- gam(y ~ s(x1, bs = "ordinal", m = 2) + s(x2, bs = "ordinal", m = 2) + 
              s(x3, bs = "ordinal", m = 2) + factor(u1), method = "REML")

## -----------------------------------------------------------------------------
summary(gom2)

## ----test2, figures-side, fig.show="hold", fig.cap="Top: `ordSmooth`; middle row: gam with first-order penalty; bottom: gam with second-order penalty", fig.width = 3, fig.height = 3, out.width="31%",out.height="30%"----
par(mar = c(4.1, 4.1, 3.1, 1.1)) 
plot(osm2)
plot(gom1)
plot(gom2)

## -----------------------------------------------------------------------------
x1 <- sample(1:8, 10, replace = TRUE)
x2 <- sample(1:6, 10, replace = TRUE)
x3 <- sample(1:7, 10, replace = TRUE)
newx <- cbind(x1, x2, x3)

## -----------------------------------------------------------------------------
round(predict(osm1, newx), digits = 3)
round(predict(osl, newx), digits = 3)
round(predict(ofu, newx), digits = 3)

## -----------------------------------------------------------------------------
data(ICFCoreSetCWP)
head(ICFCoreSetCWP)

## -----------------------------------------------------------------------------
y <- ICFCoreSetCWP$phcs
x <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1, 50), rep(5, 16), 1),
                                    nrow(ICFCoreSetCWP), 67,
                                    byrow = TRUE)
xnames <- names(x)
head(x)

## -----------------------------------------------------------------------------
rbind(apply(x, 2, min), apply(x, 2, max))

## -----------------------------------------------------------------------------
x <- rbind(x, rep(1,67))
x <- rbind(x, c(rep(5, 50), rep(9,16), 5))
y <- c(y, NA, NA)

osm_icf <- ordSmooth(x = x, y = y, lambda = lambda)
osl_icf <- ordSelect(x = x, y = y, lambda = lambda)
ofu_icf <- ordFusion(x = x, y = y, lambda = lambda) 

## ----test, figures-side, fig.show="hold", fig.cap="Left column: smoothing; middle column: selection; right column: fusion.", fig.width = 3, fig.height = 3, out.width="31%",out.height="30%"----
wx <- which(xnames=="b1602"|xnames=="d230"|xnames=="d430"|xnames=="d455"|xnames=="e1101")
xmain <- c()
xmain[wx] <- c("Content of thought",
               "Carrying out daily routine",
               "Lifting and carrying objects",
               "Moving around",
               "Drugs")

par(mar = c(4.1, 4.1, 3.1, 1.1))  
for(i in wx){
  plot(osm_icf, whx = i, main = "", xaxt = "n")
  axis(1, at = 1:length(osm_icf$xlevels), 
       labels = ((1:length(osm_icf$xlevels)) - c(rep(1,50), rep(5,16), 1)[i]))   

  plot(osl_icf, whx = i, main = xmain[i], xaxt = "n")
  axis(1, at = 1:length(osm_icf$xlevels), 
       labels = ((1:length(osm_icf$xlevels)) - c(rep(1,50), rep(5,16), 1)[i]))   

  plot(ofu_icf, whx = i, main = "", xaxt = "n")
  axis(1, at = 1:length(osm_icf$xlevels), 
       labels = ((1:length(osm_icf$xlevels)) - c(rep(1,50), rep(5,16), 1)[i]))   
}

## -----------------------------------------------------------------------------
xgrp <- rep(1:67, apply(x, 2, max))

osm_coefs <- osm_icf$coef[2:(length(xgrp) + 1),, drop = FALSE]
osl_coefs <- osl_icf$coef[2:(length(xgrp) + 1),, drop = FALSE]
ofu_coefs <- ofu_icf$coef[2:(length(xgrp) + 1),, drop = FALSE]

round(osm_coefs[xgrp == wx[1],, drop = FALSE] ,3)
round(osl_coefs[xgrp == wx[1],, drop = FALSE] ,3)
round(ofu_coefs[xgrp == wx[1],, drop = FALSE] ,3)

## -----------------------------------------------------------------------------
y <- ICFCoreSetCWP$phcs

## -----------------------------------------------------------------------------
x <- ICFCoreSetCWP[, which(xnames == "d455")]

## -----------------------------------------------------------------------------
x <- as.integer(x - min(x) + 1)

## ---- fig.align="center"------------------------------------------------------
boxplot(y~factor(x, levels = 1:5, labels = 0:4), varwidth = TRUE, col = "white", 
        xlab = "level", ylab = "physical health summary")
 
x.mean <- tapply(as.numeric(y)[order(x)], as.factor(x[order(x)]), mean)
lines(x.mean, type = "b", col = 2, pch = 17)

## -----------------------------------------------------------------------------
ordAOV(x, y, type = "RLRT", nsim = 1000000)

## -----------------------------------------------------------------------------
ordAOV(x, y, type = "LRT", nsim = 1000000)
anova(lm(y ~factor(x)))

## -----------------------------------------------------------------------------
set.seed(321) 
ni <- 5
n <- sum(5*ni)
xpr <- matrix(NA, ncol = n, nrow = 100)
mu_lin <- 3:7  
mu_sq2 <- (-2:2)^2 * 0.5 + 3   
a <- seq(0.75, 1.25, length.out = 10)

for(i in 1:10){ 
  xpr[i,] <- a[i] * rep(mu_lin, each = ni) + rnorm(n)
  xpr[i+10,] <- a[i] * rep(mu_sq2, each = ni) + rnorm(n) 
} 
for(i in 21:100) xpr[i,] <- 3 + rnorm(n)

## -----------------------------------------------------------------------------
dose <- rep(c(0, 0.01, 0.05, 0.2, 1.5), each = ni)

## ----genes, figures-side, fig.show="hold", fig.width = 5, fig.height = 5, out.width="45%",out.height="50%"----
plot(dose, xpr[4,], col = as.factor(dose), lwd = 2, ylab = "expression", main = "gene 4") 
lines(sort(unique(dose)), mu_lin * a[4], lty = 1, col = 1) 

plot(dose, xpr[14,], col = as.factor(dose), lwd = 2, ylab = "expression", main = "gene 14") 
lines(sort(unique(dose)), mu_sq2 * a[4], lty = 1, col = 1) 

## ----genes2, figures-side, fig.show="hold", fig.width = 5, fig.height = 5, out.width="45%",out.height="50%"----
plot(1:length(sort(unique(dose))), ylim = range(xpr[4,]), pch = "", ylab = "expression", 
     main = "gene 4", xlab = "levels", xaxt = "n")
axis(1, at = 1:length(sort(unique(dose))) ) 
points(as.factor(dose), xpr[4,], col = as.factor(dose), lwd = 2) 
lines(1:length(sort(unique(dose))), mu_lin * a[4], lty = 1)

plot(1:length(sort(unique(dose))), ylim = range(xpr[14,]), pch = "", ylab = "expression", 
     main = "gene 14", xlab = "levels", xaxt = "n")
axis(1, at = 1:length(sort(unique(dose))) ) 
points(as.factor(dose), xpr[14,], col = as.factor(dose), lwd = 2) 
lines(1:length(sort(unique(dose))), mu_sq2 * a[4], lty = 1)

## -----------------------------------------------------------------------------
pvals <- ordGene(xpr = xpr, lvs = dose, nsim = 1e6)

## ---- fig.align="center"------------------------------------------------------
plot(ecdf(pvals[,1]), xlim = c(0,0.05), ylim = c(0, 0.25),
     main = "", xlab = "p-value", ylab = "F(p-value)")
plot(ecdf(pvals[,2]), xlim = c(0, 0.05), add = TRUE, col = 2)
plot(ecdf(pvals[,3]), xlim = c(0, 0.05), add = TRUE, col = 3)
legend('topleft', colnames(pvals), col = 1:3, lwd = 2, lty = 1) 

