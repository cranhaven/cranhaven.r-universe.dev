library(distfreereg)

# First, test a linear model.
set.seed(20240816)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rexp(n) + 1
theta <- c(2,5,-1)
X <- matrix(rexp(2*n, rate = 1), nrow = n)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) + rnorm(n, sd = sqrt(Sig))

set.seed(20240816)
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = rep(1, length(theta)), verbose = FALSE)

vcov_1 <- vcov(dfr_1)
signif(vcov_1, digits = 4)

# Now compare to results from lm().
df <- data.frame(a = X[,1], b = X[,2], c = Y)
m <- lm(c ~ a + b, data = df, weights = dfr_1[["covariance"]][["P"]])
dfr_2 <- distfreereg(test_mean = m, verbose = FALSE)
all.equal(dfr_1[["theta_hat"]], dfr_2[["theta_hat"]], check.attributes = FALSE)

vcov_2 <- vcov(dfr_2)
signif(vcov_2, digits = 4)

all.equal(vcov_1, vcov_2, check.attributes = FALSE, tolerance = 1e-4)


# Compare to results from using a matrix form for Sigma
set.seed(20240816)
dfr_3 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = diag(Sig)),
                     theta_init = rep(1, length(theta)), verbose = FALSE)
vcov_3 <- vcov(dfr_3)
all.equal(vcov_1, vcov_3, tolerance = 1e-4)



# Next, try a non-linear model
set.seed(20240816)
n <- 1e2
func <- function(x, theta) theta[1] + x[1]^theta[2] + theta[3]*x[2]
Sig <- rexp(n) + 1
theta <- c(5,1,-1)
X <- matrix(rexp(2*n, rate = 1), nrow = n)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) + rnorm(n, sd = sqrt(Sig))

set.seed(20240816)
dfr_4 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = rep(1, length(theta)), verbose = FALSE)

vcov_4 <- vcov(dfr_4)
signif(vcov_4, digits = 4)

# Next, compare to results from nls().
df <- data.frame(x1 = X[,1], x2 = X[,2], y = Y)
m <- nls(y ~ k + x1^a + b*x2, data = df, weights = dfr_1[["covariance"]][["P"]])

dfr_5 <- distfreereg(test_mean = m, verbose = FALSE)
all.equal(dfr_4[["theta_hat"]], dfr_5[["theta_hat"]], check.attributes = FALSE,
          tolerance = 1e-4)

vcov_5 <- vcov(dfr_5)
signif(vcov_5, digits = 4)

all.equal(vcov_4, vcov_5, check.attributes = FALSE, tolerance = 1e-4)

# Compare to results from using a matrix form for Sigma
set.seed(20240816)
dfr_6 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = diag(Sig)),
                     theta_init = rep(1, length(theta)), verbose = FALSE)
vcov_6 <- vcov(dfr_6)
all.equal(vcov_4, vcov_6, tolerance = 1e-4)
