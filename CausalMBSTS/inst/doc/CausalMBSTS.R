## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.height = 5, fig.width = 7-------------------------------------------
library(CausalMBSTS)

# Set seed & random data generation
set.seed(1)
t <- seq(from = 0,to = 4*pi, length.out=300)
y <- cbind(3*sin(2*t)+rnorm(300), 2*cos(2*t) + rnorm(300))
dates <- seq.Date(from = as.Date("2015-01-01"), by = "week", length.out=300)
int.date <- as.Date("2020-02-27")
y[dates >= int.date,] <- y[dates >= int.date,]+2

# Some plots
plot(y = y[,1], x=dates, type="l", col="cadetblue")
lines(y = y[,2], x = dates, col = "orange")
abline(v=int.date, col="red")

## ---- fig.height = 3, fig.width = 7-------------------------------------------
# Causal effect estimation
causal.2 <- CausalMBSTS(y, components = c("trend", "cycle"), cycle.period = 75,
                        dates = dates, int.date = int.date, s0.r = 0.01*diag(2),
                        s0.eps = 0.1*diag(2), niter = 100, burn = 10)
summary(causal.2)

# Causal effect plot
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
plot(causal.2, int.date = int.date, type = "impact")

# Observed sales vs counterfactual sales plot
plot(causal.2, int.date = int.date, type = "forecast")

## ---- fig.height = 3, fig.width = 8-------------------------------------------
# Posterior predictive checks
par(mar = c(2,2,2,2)) ; par(mfrow = c(2,4))
plot(causal.2, int.date = int.date, type = "ppchecks")

## ---- fig.height = 3, fig.width = 8-------------------------------------------
# Trace plots
par(mar = c(2,2,2,2)) ; par(mfrow = c(2,3))
mcmc <- causal.2$mcmc
plot(mcmc$Sigma.eps[1,1,], type = "l", main = "Variance of obs residuals Y1")
plot(mcmc$Sigma.eps[2,2,], type = "l", main = "Variance of obs residuals Y2")
plot(mcmc$Sigma.eps[1,2,], type = "l", main = "Covariance of obs residuals")
plot(mcmc$Sigma.r[1,1,], type = "l", main = "Variance of trend residuals Y1")
plot(mcmc$Sigma.r[2,2,], type = "l", main = "Variance of trend residuals Y2")
plot(mcmc$Sigma.r[1,2,], type = "l", main = "Covariance of trend residuals")

## ---- fig.height = 5, fig.width = 7-------------------------------------------
# Set seed & random data generation
set.seed(1)
t <- seq(from = 0,to = 4*pi, length.out=222)
y <- cbind(3*sin(2*t)+rnorm(222), 2*cos(2*t) + rnorm(222))
dates <- seq.Date(from = as.Date("2015-06-01"), by = "week", length.out=222)

# MBSTS model definition
sales_model <- as.mbsts(y, components = c("trend", "cycle"), cycle.period = 75,
                        s0.r = 0.01*diag(2), s0.eps = 0.1*diag(2), niter = 100, burn = 10) 

# Prediction step
pred <- predict(sales_model, steps.ahead = 26)

# Some plots
par(mfrow = c(1,1))
y.mean <- apply(pred$post.pred,c(1,2),mean)
new.dates <- seq.Date(from = as.Date("2015-06-01"), by = "week", length.out=248)
plot(y = y.mean[,1], x = new.dates, type="l", col="cadetblue")
lines(y = y.mean[,2], x = new.dates, col = "orange")
abline( v = dates[222], col = "red")
par(oldpar)

