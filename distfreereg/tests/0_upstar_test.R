library(distfreereg)
set.seed(20240215)
n <- 1e2
p <- 6
func <- function(x, theta) sum(x*theta)
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- runif(p, min = -10, max = 10)
X <- matrix(rexp(n*p, rate = 1), nrow = n)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n),
                                  SqrtSigma = distfreereg:::matsqrt(Sig)))
dfr <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                   theta_init = rep(1,p), verbose = FALSE)

mu <- dfr[["mu"]]
r <- dfr[["r"]]

# All of the following should be TRUE
for(i in seq_len(p)){
  print(all.equal(r[,i],
                  distfreereg:::calc_k2_resid(mu[,i], r_tilde = dfr[["r_tilde"]],
                                              mu = mu, k2_tol = sqrt(.Machine[["double.eps"]]))))
}
