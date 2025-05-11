library(distfreereg)
set.seed(20240418)

n <- 1e2
test_func <- function(x, theta) x[1]^theta[1] + x[2]^theta[2]
test_form <- c ~ a^f + b^g
theta <- c(5,2)
Sig <- diag(rexp(n, rate = 1/10))
df <- data.frame(a = rexp(n), b = rexp(n))
df$c <- df$a^theta[1] + df$b^theta[2] + rnorm(n)

set.seed(20240418)
dfr_0 <- distfreereg(test_mean = test_func, theta_init = c(1,1), Y = df$c, 
                     X = as.matrix(df[,c("a", "b")]), covariance = list(Sigma = Sig))

dfr_1 <- distfreereg(test_mean = test_form, data = df, covariance = list(Sigma = Sig),
                     method = "nls")

message('identical(dfr_0[["data"]][["Y"]], dfr_1[["data"]][["Y"]]) (should be TRUE): ', identical(dfr_0[["data"]][["Y"]], dfr_1[["data"]][["Y"]]))
message('identical(dfr_0[["data"]][["X"]], dfr_1[["data"]][["X"]]) (should be TRUE): ', identical(dfr_0[["data"]][["X"]], dfr_1[["data"]][["X"]]))

all.equal(dfr_0[["J"]], dfr_1[["J"]], check.attributes = FALSE, tolerance = 1e-5)
all.equal(dfr_0[["fitted_values"]], dfr_1[["fitted_values"]], check.attributes = FALSE, tolerance = 1e-5)
all.equal(dfr_0[["theta_hat"]], dfr_1[["theta_hat"]], check.attributes = FALSE, tolerance = 1e-5)
all.equal(dfr_0[["observed_stats"]], dfr_1[["observed_stats"]], check.attributes = FALSE, tolerance = 1e-3)
