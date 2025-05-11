library(distfreereg)
set.seed(20240419)

n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2] + theta[4]*x[3] + theta[5]*x[4]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5,-1,7,3)
X <- matrix(rexp(4*n, rate = 1), ncol = 4)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
df <- as.data.frame(cbind(Y, X))
colnames(df) <- c("y", "a", "b", "c", "d")
nls_form <- y ~ e + f*a

tryCatch(distfreereg(test_mean = nls_form, data = df, covariance = list(Sigma = Sig),
                     method = "nls"),
         error = function(e) warning(e))
