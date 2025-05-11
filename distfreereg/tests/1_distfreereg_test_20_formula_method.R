library(distfreereg)
set.seed(20240303)
n <- 1e1

df <- data.frame(
  a = rnorm(n),
  b = sample(1:3, size = n, replace = TRUE)
)

Sig <- 7
df$c <- 5*df[["a"]] + df[["b"]] + rnorm(n, sd = sqrt(Sig))
df$b <- factor(LETTERS[df[["b"]]])
test_mean_formula <- c ~ a + b

set.seed(20240303)
dfr_1 <- distfreereg(test_mean = test_mean_formula, data = df,
                     covariance = list(Sigma = Sig), verbose = FALSE)
dfr_1

# Note: using sum() leads to slightly difference results from an explicit sum of
# the form theta[1]*X[1] + theta[2]*X[2] + theta[3]*X[3] + theta[4]*X[4].
test_mean_function <- function(x, theta) sum(x*theta)

X <- cbind(`(Intercept)` = 1,# avoid warning about single value in a column
           df$a,
           ifelse(df[["b"]] == "B", 1, 0),
           ifelse(df[["b"]] == "C", 1, 0))
Y <- df[["c"]]

set.seed(20240303)
dfr_2 <- distfreereg(test_mean = test_mean_function, X = X, Y = Y, covariance = list(Sigma = Sig),
                     theta_init = rep(1,4), verbose = FALSE)

message('all.equal(dfr_1[["epsp"]], dfr_2[["epsp"]], check.attributes = FALSE, tolerance = 1e-5) (should be TRUE): ', all.equal(dfr_1[["epsp"]], dfr_2[["epsp"]], check.attributes = FALSE, tolerance = 1e-5))
message('identical(dfr_1[["mcsim_stats"]], dfr_2[["mcsim_stats"]]) (should be TRUE): ', identical(dfr_1[["mcsim_stats"]], dfr_2[["mcsim_stats"]]))
