library(distfreereg)
set.seed(20240319)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(2,5)
X <- matrix(rexp(n, rate = 1))
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
data <- data.frame(a = X, b = Y)
m <- lm(b ~ a, data = data)

# Basic tests.
dfr_01a <- distfreereg(test_mean = m, verbose = FALSE)

# Tests for res_order
dfr_04a <- distfreereg(test_mean = m,
                       verbose = FALSE, ordering = "optimal")
dfr_04b <- update(dfr_01a, ordering = "optimal")
message('identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]) (should be FALSE): ', identical(dfr_01a[["res_order"]], dfr_04a[["res_order"]]))
message('identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]) (should be TRUE): ', identical(dfr_04a[["res_order"]], dfr_04b[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04b[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04b[["r"]]))

dfr_04c <- distfreereg(test_mean = m,
                       verbose = FALSE, override = list(res_order = dfr_04a[["res_order"]]))
message('identical(dfr_04a[["r"]], dfr_04c[["r"]]) (should be TRUE): ', identical(dfr_04a[["r"]], dfr_04c[["r"]]))

dfr_05a <- distfreereg(test_mean = m,
                       verbose = FALSE, ordering = "asis")
dfr_05b <- update(dfr_04c, ordering = "asis")# presence of "asis" should clear override from dfr_04c
message('identical(dfr_05a[["r"]], dfr_05b[["r"]]) (should be TRUE): ', identical(dfr_05a[["r"]], dfr_05b[["r"]]))
