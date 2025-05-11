# This file verifies that under the null hypothesis, the observed statistics
# and simulated statistics seem to be coming from the same distribution.

library(distfreereg)
n <- 100


# Function

set.seed(202409020)
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,1)
true_func <- function(theta, X) theta[1]*X[,1] + theta[2]^(0.2*X[,2]) + theta[3]*X[,3]
test_func <- true_func
true_X <- matrix(rexp(length(theta)*n), nrow = n)
X <- true_X

set.seed(202409021)
cdfr_1 <- compare(true_mean = true_func,
                  test_mean = test_func,
                  true_X = true_X,
                  X = X,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  theta = theta,
                  theta_init = rep(1, length(theta)),
                  reps = 1000,
                  prog = 100)

ks_1 <- ks.test(cdfr_1)
ks_1
plot(cdfr_1)




# Formula (lm)

rm(list = setdiff(ls(), c("n", paste0("cdfr_", 1), paste0("ks_", 1))))
set.seed(202409022)
theta <- c(2,5,1)
true_form <- c ~ a + a:b
test_form <- true_form
a <- rexp(n, rate = 1)
b <- rnorm(n)
c <- a + a*b + rnorm(n)
true_data <- data.frame(a = a, b = b, c = c)
data <- true_data

set.seed(202409023)
cdfr_2 <- compare(true_mean = true_form,
                  true_method = "lm",
                  test_mean = test_form,
                  true_data = true_data,
                  data = data,
                  true_covariance = list(Sigma = 1),
                  theta = theta,
                  reps = 1000,
                  prog = 100)

ks_2 <- ks.test(cdfr_2)
ks_2
plot(cdfr_2)




# Formula (nls)

rm(list = setdiff(ls(), c("n", paste0("cdfr_", 1:2), paste0("ks_", 1:2))))
set.seed(202409024)
theta <- c(j = 2, k = 5)
true_form <- w ~ j*x + k*y
test_form <- true_form
x <- rnorm(n)
y <- rexp(n, rate = 1)
z <- rnorm(n)
w <- theta[1]*x + theta[2]*y + rnorm(n)
true_data <- data.frame(x = x, y = y, w = w)
data <- true_data

set.seed(202409025)
cdfr_3 <- compare(true_mean = true_form,
                  true_method = "nls",
                  test_mean = test_form,
                  method = "nls",
                  true_data = true_data,
                  data = data,
                  true_covariance = list(Sigma = 1),
                  theta = theta,
                  theta_init = c(j = 1, k = 1),
                  reps = 1000,
                  prog = 100)

ks_3 <- ks.test(cdfr_3)
ks_3
plot(cdfr_3)





# lm

rm(list = setdiff(ls(), c("n", paste0("cdfr_", 1:3), paste0("ks_", 1:3))))
set.seed(202409026)
theta <- c(2,5,1)
true_form <- c ~ a + a:b
test_form <- true_form
a <- rexp(n, rate = 1)
b <- rnorm(n)
c <- a + a*b + rnorm(n)
true_data <- data.frame(a = a, b = b, c = c)
data <- true_data
true_m <- lm(true_form, data = true_data)
test_m <- lm(test_form, data = data)

set.seed(202409027)
cdfr_4 <- compare(true_mean = true_m,
                  test_mean = test_m,
                  true_covariance = list(Sigma = 1),
                  theta = theta,
                  reps = 1000,
                  prog = 100)

ks_4 <- ks.test(cdfr_4)
ks_4
plot(cdfr_4)




# nls

rm(list = setdiff(ls(), c("n", paste0("cdfr_", 1:4), paste0("ks_", 1:4))))
set.seed(202409028)
theta <- c(j = 2, k = 5, l = 1)
true_form <- w ~ j*x + y^k + l*z
test_form <- true_form
x <- rnorm(n)
y <- rexp(n, rate = 1)
z <- rnorm(n)
w <- theta[1]*x + y^theta[2] + theta[3]*z + rnorm(n)
true_data <- data.frame(x = x, y = y, z = z, w = w)
data <- true_data
true_m <- nls(true_form, data = true_data)
test_m <- nls(test_form, data = data)

set.seed(202409029)
cdfr_5 <- compare(true_mean = true_m,
                  test_mean = test_m,
                  true_covariance = list(Sigma = 1),
                  theta = theta,
                  reps = 1000,
                  prog = 100)

ks_5 <- ks.test(cdfr_5)
ks_5
plot(cdfr_5)


ks.test(c(ks_1$p.value, ks_2$p.value, ks_3$p.value, ks_4$p.value, ks_5$p.value),
        punif)
