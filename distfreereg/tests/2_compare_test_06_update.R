library(distfreereg)
set.seed(20240123)
n <- 20
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))

cdfr <- compare(reps = 10, B = 10, prog = Inf, theta = theta, true_mean = func, true_X = X,
                X = X, test_mean = func, covariance = list(Sigma = Sig),
                true_covariance = list(Sigma = Sig), theta_init = rep(1, length(theta)))

signif(cdfr[["observed_stats"]][["KS"]], digits = 4)
signif(cdfr[["observed_stats"]][["CvM"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["KS"]], digits = 4)
signif(cdfr[["mcsim_stats"]][["CvM"]], digits = 4)

cdfr_2 <- update(cdfr, true_covariance = list(Sigma = 1))

signif(cdfr_2[["observed_stats"]][["KS"]], digits = 4)
signif(cdfr_2[["observed_stats"]][["CvM"]], digits = 4)
signif(cdfr_2[["mcsim_stats"]][["KS"]], digits = 4)
signif(cdfr_2[["mcsim_stats"]][["CvM"]], digits = 4)

ks.test(cdfr)

rejection(cdfr, alpha = seq(from = 0, to = 1, by = 0.1))
