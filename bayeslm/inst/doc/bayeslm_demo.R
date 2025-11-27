## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bayeslm)

## ----data_generate------------------------------------------------------------
set.seed(200)
p = 20
n = 100
kappa = 1.25
beta_true = c(c(1,2,3),rnorm(p-3,0,0.01))
sig_true = kappa*sqrt(sum(beta_true^2))
x = matrix(rnorm(p*n),n,p)
y = x %*% beta_true + sig_true * rnorm(n)
x = as.matrix(x)
y = as.matrix(y)
data = data.frame(x = x, y = y)

## ----ols----------------------------------------------------------------------
fitOLS = lm(y~x-1)
coef(fitOLS)

## ----model_setup--------------------------------------------------------------
block_vec = rep(1, p)

## ----prior_comparison, results = 'hide', warning=F, error=F-------------------
# Horseshoe prior
fit1 = bayeslm(y, x, prior = 'horseshoe', icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est1 = colMeans(fit1$beta)

# Laplace prior
fit2 = bayeslm(y, x, prior = 'laplace', icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est2 = colMeans(fit2$beta)

# Ridge prior
fit3 = bayeslm(y, x, prior = 'ridge', icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est3 = colMeans(fit3$beta)

# "Sharkfin" prior
fit4 = bayeslm(y, x, prior = 'sharkfin', icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est4 = colMeans(fit4$beta)

# "Non-local" prior
fit5 = bayeslm(y, x, prior = 'nonlocal', icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est5 = colMeans(fit5$beta)

# Inverse laplace prior
fit6 = bayeslm(y, x, prior = 'inverselaplace', lambda = 0.01, icept = FALSE, 
               block_vec = block_vec, N = 10000, burnin=2000)
beta_est6 = colMeans(fit6$beta)

## ----comparison_plot, fig.height=5, fig.width=7-------------------------------
plot(NULL,xlim=range(beta_true),ylim=range(beta_true), 
     xlab = "beta true", ylab = "estimation", )
points(beta_true,beta_est1,pch=20)
points(beta_true,fitOLS$coef,col='red')
points(beta_true,beta_est2,pch=20,col='cyan')
points(beta_true,beta_est3,pch=20,col='orange')
points(beta_true,beta_est4,pch=20,col='pink')
points(beta_true,beta_est5,pch=20,col='lightgreen')
points(beta_true,beta_est6,pch=20,col='grey')
legend("topleft", c("OLS", "horseshoe", "laplace", "ridge", "sharkfin", 
  "nonlocal", "inverselaplace"), col = c("red", "black", "cyan", "orange", 
    "pink", "lightgreen", "grey"), pch = rep(1, 7))
abline(0,1,col='red')

## ----rmse_comparison----------------------------------------------------------
rmseOLS = sqrt(sum((fitOLS$coef-beta_true)^2))
rmse1 = sqrt(sum((beta_est1-beta_true)^2))
rmse2 = sqrt(sum((beta_est2-beta_true)^2))
rmse3 = sqrt(sum((beta_est3-beta_true)^2))
rmse4 = sqrt(sum((beta_est4-beta_true)^2))
rmse5 = sqrt(sum((beta_est5-beta_true)^2))
rmse6 = sqrt(sum((beta_est6-beta_true)^2))
print(cbind(ols = rmseOLS, hs = rmse1,laplace = rmse2, ridge = rmse3, 
            sharkfin = rmse4,nonlocal = rmse5, inverselaplace = rmse6))

## ----block_sampling-----------------------------------------------------------
# Put the first two coefficients in one elliptical sampling block
block_vec2 = c(2, rep(1, p-2))
fitb = bayeslm(y ~ x - 1, data = data, prior = 'horseshoe', 
               block_vec = block_vec2, N = 10000, burnin = 2000)
summary(fitb)

