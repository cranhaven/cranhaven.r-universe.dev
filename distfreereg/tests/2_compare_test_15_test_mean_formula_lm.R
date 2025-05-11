library(distfreereg)
set.seed(20240305)
n <- 20
Sig <- diag(rexp(n))
theta <- c(2,5)
func <- function(x, theta) sum(theta * x)

# test formula
data <- data.frame(a = rexp(n, rate = 1),
                   b = rnorm(n))
data$c <- 4*data$a - 3*data$b

set.seed(20240305)
cdfr_1 <- compare(true_mean = func, true_covariance = list(Sigma = Sig),
                  covariance = list(Sigma = Sig),
                  true_X = as.matrix(data[,-3]), test_mean = c ~ a + b - 1,
                  data = data, reps = 10, B = 10, prog = Inf, theta = theta)
cdfr_1



# test lm
m <- lm(c ~ a + b - 1, data = data, weights = 1/diag(Sig))

set.seed(20240305)
cdfr_2 <- compare(true_mean = func, true_covariance = list(Sigma = Sig),
                  true_X = as.matrix(data[,-3]), test_mean = m,
                  reps = 10, B = 10, prog = Inf, theta = theta)

message('identical(cdfr_1[["observed_stats"]], cdfr_2[["observed_stats"]]) (should be TRUE): ', identical(cdfr_1[["observed_stats"]], cdfr_2[["observed_stats"]]))
message('identical(cdfr_1[["mcsim_stats"]], cdfr_2[["mcsim_stats"]]) (should be TRUE): ', identical(cdfr_1[["mcsim_stats"]], cdfr_2[["mcsim_stats"]]))
message('identical(cdfr_1[["p"]], cdfr_2[["p"]]) (should be TRUE): ', identical(cdfr_1[["p"]], cdfr_2[["p"]]))
