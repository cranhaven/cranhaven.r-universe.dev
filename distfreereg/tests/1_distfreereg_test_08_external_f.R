library(distfreereg)
set.seed(20240123)

# Verify that distfreereg() can deal with an external mean function (that is, a
# function that is not defined in R).

n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1), verbose = FALSE)
dfr_2 <- distfreereg(Y = Y, X = X, covariance = list(Sigma = Sig), verbose = FALSE,
                     fitted_values = dfr_1[["fitted_values"]], J = dfr_1[["J"]])

message('identical(dfr_1[["epsp"]], dfr_2[["epsp"]]) (should be TRUE): ', identical(dfr_1[["epsp"]], dfr_2[["epsp"]]))

dfr_1
dfr_2
