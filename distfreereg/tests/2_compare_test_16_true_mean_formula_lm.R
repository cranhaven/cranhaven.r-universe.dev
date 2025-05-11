library(distfreereg)
set.seed(20240305)
n <- 20
Sig <- diag(rexp(n))
theta <- c(2,5)
form <- c ~ a + b - 1
data <- data.frame(a = rexp(n, rate = 1),
                   b = rnorm(n))
data$c <- 4*data$a - 3*data$b
m <- lm(c ~ a + b - 1, data = data, weights = 1/diag(Sig))

# true formula, test formula
set.seed(20240305)
cdfr_1 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = form,
                  true_method = "lm",
                  data = data,
                  true_data = data, test_mean = form,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig))

# true lm, test formula
set.seed(20240305)
cdfr_2 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = m,
                  data = data,
                  test_mean = form,
                  true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig))

# true formula, test lm
set.seed(20240305)
cdfr_3 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = form,
                  true_method = "lm",
                  true_data = data, test_mean = m,
                  true_covariance = list(Sigma = Sig))

# true lm, test lm
set.seed(20240305)
cdfr_4 <- compare(reps = 10, prog = Inf, theta = theta, true_mean = m,
                  test_mean = m,
                  true_covariance = list(Sigma = Sig))

message('identical(cdfr_1[["observed_stats"]], cdfr_2[["observed_stats"]]) (should be TRUE): ', identical(cdfr_1[["observed_stats"]], cdfr_2[["observed_stats"]]))
message('identical(cdfr_1[["observed_stats"]], cdfr_3[["observed_stats"]]) (should be TRUE): ', identical(cdfr_1[["observed_stats"]], cdfr_3[["observed_stats"]]))
message('identical(cdfr_1[["observed_stats"]], cdfr_4[["observed_stats"]]) (should be TRUE): ', identical(cdfr_1[["observed_stats"]], cdfr_4[["observed_stats"]]))
message('identical(cdfr_1[["mcsim_stats"]], cdfr_2[["mcsim_stats"]]) (should be TRUE): ', identical(cdfr_1[["mcsim_stats"]], cdfr_2[["mcsim_stats"]]))
message('identical(cdfr_1[["mcsim_stats"]], cdfr_3[["mcsim_stats"]]) (should be TRUE): ', identical(cdfr_1[["mcsim_stats"]], cdfr_3[["mcsim_stats"]]))
message('identical(cdfr_1[["mcsim_stats"]], cdfr_4[["mcsim_stats"]]) (should be TRUE): ', identical(cdfr_1[["mcsim_stats"]], cdfr_4[["mcsim_stats"]]))
