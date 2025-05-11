library(distfreereg)
set.seed(20240123)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2] + theta[4]*x[3]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2, 5, 1, -3)
X <- matrix(replicate(3, sample(1:10, size = n, replace = TRUE)), ncol = 3)
colnames(X) <- c("a", "b", "c")
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1,1),
                     verbose = TRUE, ordering = "asi"),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1,1),
                     verbose = TRUE, ordering = c("asis", "natural")),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1,1),
                     verbose = TRUE, ordering = c(1,3)),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1,1),
                     verbose = TRUE, ordering = list(1,4)),
         error = function(e) warning(e))

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1,1,1),
                     verbose = TRUE, ordering = list(1,"a")),
         error = function(e) warning(e))
