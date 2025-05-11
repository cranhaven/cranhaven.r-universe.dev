library(distfreereg)
set.seed(20240317)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1), verbose = FALSE,
                     group = FALSE,
                     1:5,
                     ordering = "simplex",
                     stat = c("KS", "CvM"), B = 1e3, control = NULL,
                     override = NULL),
         error = function(e) warning(e))
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1), verbose = FALSE, extra_stuff = 1:5),
         error = function(e) warning(e))
