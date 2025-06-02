library(gorica)
library(MASS)
# Generate data
n <- 1000 # choice does not impact message of our story
k <- 10 # 10 # 3 # k predictors, so x_1 to x_k
#Sigma <- matrix(0.25, nrow = k, ncol = k)
#diag(Sigma) <- 1
Sigma_lowk <- diag(k) # so, predictors now uncorrelated
set.seed(123) # to make code reproducable
X_lowk <- mvrnorm(n = n, mu = rep(0, k), Sigma_lowk, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
# Standardize data - since parameters for continuous variables will be compared
X_s_lowk <- scale(X_lowk) # now, e.g. colMeans(X_s) are all 0
colnames(X_s_lowk) <- paste0("x", 1:k)
error <- rnorm(n)
intercept <- 1
y_lowk <- intercept + rowSums(X_s_lowk) + error # rowSums(X) = x_1 + x_2 + ... + x_k
#y_lowk_scale <- scale(y_lowk)
#
# Fit regression model
fit.lm_lowk <- lm(y_lowk ~ X_s_lowk)
#fit.lm_lowk <- lm(y_lowk_scale ~ X_s_lowk - 1)
#
# Check names of  parameters to use in hypotheses
coef(fit.lm_lowk)

# gorica()
H1_lowk_g <- "X_s_lowkx1 > X_s_lowkx2 &  X_s_lowkx2 > X_s_lowkx3 &  X_s_lowkx3 > X_s_lowkx4 &  X_s_lowkx4 > X_s_lowkx5 &
X_s_lowkx5 > X_s_lowkx6 &  X_s_lowkx6 > X_s_lowkx7 &  X_s_lowkx7 > X_s_lowkx8 &  X_s_lowkx8 > X_s_lowkx9 &
X_s_lowkx9 > X_s_lowkx10"

# Call gorica
out_gorica_c_lowk_1_g <- gorica(fit.lm_lowk, H1_lowk_g, comparison = "complement")
expect_equivalent(out_gorica_c_lowk_1_g$fit[,2], c(2.92093, 9.9999999), tolerance = .001)

# if k = 3
H1_lowk_g <- "X_s_lowkx1 > X_s_lowkx2 > X_s_lowkx3"
out_gorica_c_lowk_1_g <- gorica(fit.lm_lowk, H1_lowk_g, comparison = "complement")
expect_equivalent(out_gorica_c_lowk_1_g$fit[,2], c(1.84, 2.68), tolerance = .03)

# Test that complement doesnt work with >1 hyp
H1_lowk_g <- "X_s_lowkx1 > X_s_lowkx2 > X_s_lowkx3; X_s_lowkx1 = X_s_lowkx2 > X_s_lowkx3"
expect_warning(gorica(fit.lm_lowk, H1_lowk_g, comparison = "complement"))



# 12-2-2020 ---------------------------------------------------------------

# Generate data
n <- 1000 # choice does not impact message of our story
k <- 3 # 10 # 3 # k predictors, so x_1 to x_k
#Sigma <- matrix(0.25, nrow = k, ncol = k)
#diag(Sigma) <- 1
Sigma_lowk <- diag(k) # so, predictors now uncorrelated
set.seed(123) # to make code reproducable
X_lowk <- mvrnorm(n = n, mu = rep(0, k), Sigma_lowk, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
# Standardize data - since parameters for continuous variables will be compared
X_s_lowk <- scale(X_lowk) # now, e.g. colMeans(X_s) are all 0
colnames(X_s_lowk) <- paste0("x", 1:k)
error <- rnorm(n)
intercept <- 1
y_lowk <- intercept + rowSums(X_s_lowk) + error # rowSums(X) = x_1 + x_2 + ... + x_k
#y_lowk_scale <- scale(y_lowk)
#
# Fit regression model
fit.lm_lowk <- lm(y_lowk ~ X_s_lowk)

#H1: x1 < x2 < x3 geeft een PT van 1+½+1/3 = 1+5/6
#H0: x1 = x2 = x3 geeft een PT van 1.

# gorica()
H1 <- "X_s_lowkx1 < X_s_lowkx2 <  X_s_lowkx3"
# Call gorica
set.seed(2637)
out <- gorica(fit.lm_lowk, H1, comparison = "complement")

expect_equivalent(out$fit$penalty[1], 1+5/6, tolerance = .02)

H1 <- "X_s_lowkx1 = X_s_lowkx2 =  X_s_lowkx3"
set.seed(63882)
out <- gorica(fit.lm_lowk, H1, comparison = "complement")

expect_equivalent(out$fit$penalty[1], 1)
