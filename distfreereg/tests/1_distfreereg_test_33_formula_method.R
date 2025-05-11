library(distfreereg)
set.seed(20240303)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- diag(rexp(n))
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
data <- data.frame(a = X, b = Y)
lm_form <- b ~ a
nls_form <- b ~ d + e*a
m_lm <- lm(lm_form, data = data)
m_nls <- nls(nls_form, data = data, weights = 1/diag(Sig))

dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1), verbose = FALSE)
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = NULL, covariance = list(Sigma = Sig),
                     verbose = FALSE, J = dfr_1[["J"]],
                     fitted_values = dfr_1[["fitted_values"]])
dfr_3 <- distfreereg(test_mean = lm_form, data = data, covariance = list(Sigma = Sig),
                     verbose = FALSE)
dfr_4 <- distfreereg(test_mean = m_lm, verbose = FALSE)
dfr_5 <- distfreereg(test_mean = nls_form, data = data, covariance = list(Sigma = Sig),
                     method = "nls", verbose = FALSE)
dfr_6 <- distfreereg(test_mean = m_nls, verbose = FALSE)

is.null(formula(dfr_1))# TRUE
is.null(formula(dfr_2))# TRUE
message('identical(formula(dfr_3), dfr_3[["test_mean"]]) (should be TRUE): ', identical(formula(dfr_3), dfr_3[["test_mean"]]))
message('identical(formula(dfr_4), formula(dfr_4[["test_mean"]])) (should be TRUE): ', identical(formula(dfr_4), formula(dfr_4[["test_mean"]])))
message('identical(formula(dfr_5), dfr_5[["test_mean"]]) (should be TRUE): ', identical(formula(dfr_5), dfr_5[["test_mean"]]))
message('identical(formula(dfr_6), formula(dfr_6[["test_mean"]])) (should be TRUE): ', identical(formula(dfr_6), formula(dfr_6[["test_mean"]])))
