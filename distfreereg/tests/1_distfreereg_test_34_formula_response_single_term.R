library(distfreereg)
set.seed(20240419)

n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2] + theta[4]*x[3] + theta[5]*x[4]
Sig <- diag(rexp(n))
w <- 1/diag(Sig)
theta <- c(2,5,-1,7,3)
X <- matrix(rexp(4*n, rate = 1), ncol = 4)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
df <- as.data.frame(cbind(Y, X))
colnames(df) <- c("y", "a", "b", "c", "d")
lm_form <- y + c ~ a + b
nls_form <- y + c ~ e + f*a
m_lm <- lm(lm_form, data = df, weights = w)
m_nls <- nls(nls_form, data = df, weights = w)

tryCatch(distfreereg(test_mean = lm_form, data = df, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1)),
         error = function(e) warning(e))
tryCatch(distfreereg(test_mean = m_lm),
         error = function(e) warning(e))
tryCatch(distfreereg(test_mean = nls_form, data = df, covariance = list(Sigma = Sig),
                     method = "nls"),
         error = function(e) warning(e))
tryCatch(distfreereg(test_mean = m_nls),
         error = function(e) warning(e))
