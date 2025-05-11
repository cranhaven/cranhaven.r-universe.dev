library(distfreereg)
set.seed(20240123)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
X <- matrix(replicate(3, sample(1:10, size = n, replace = TRUE)), ncol = 3)
Y <- rnorm(n)

tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = c(1,1),
                     verbose = TRUE),
         error = function(e) warning(e))

func <- function(x, theta) theta[1] + sqrt(x[1] - 2)
tryCatch(distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig), theta_init = 1,
                     verbose = TRUE),
         error = function(e) warning(e))
warnings()