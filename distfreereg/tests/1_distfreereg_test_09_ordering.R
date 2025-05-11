library(distfreereg)
set.seed(20240123)
n <- 1e2
func <- function(x, theta) theta[1]*x[1] + theta[2]*x[2] + theta[3]*x[3]
Sig <- rWishart(1, df = n, Sigma = diag(n))[,,1]
theta <- c(5, 1, -3)
X <- cbind(1, matrix(replicate(2, sample(1:10, size = n, replace = TRUE)), ncol = 2))
colnames(X) <- c("a", "b", "c")
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

dfr_1 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = "asis")
dfr_2 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = "optimal")
dfr_3 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = "simplex")
dfr_4 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = "natural")
dfr_5 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = list(1:ncol(X)))
dfr_6 <- distfreereg(Y = Y, X = X, test_mean = func, covariance = list(Sigma = Sig),
                     theta_init = c(1,1,1), verbose = TRUE, ordering = list("b", "c"))

dfr_1[["res_order"]]
dfr_2[["res_order"]]
dfr_3[["res_order"]]
dfr_4[["res_order"]]
dfr_5[["res_order"]]
dfr_6[["res_order"]]
message('identical(dfr_4[["res_order"]], dfr_5[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_5[["res_order"]]))
message('identical(dfr_4[["res_order"]], dfr_6[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_6[["res_order"]]))


# Compare the results above with what should be the identical results with the
# formula method with "lm" method.

df <- as.data.frame(cbind(Y, X[,2:3]))
colnames(df) <- c("y", "b", "c")
test_form_lm <- y ~ b + c
Sig <- diag(rexp(n))

dfr_form_lm_1 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = "asis")
dfr_form_lm_2 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = "optimal")
dfr_form_lm_3 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = "simplex")
dfr_form_lm_4 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = "natural")
dfr_form_lm_5 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = list(1:2))
dfr_form_lm_6 <- distfreereg(test_mean = test_form_lm, data = df,
                             covariance = list(Sigma = Sig),
                             verbose = TRUE, ordering = list("b", "c"))

message('identical(dfr_1[["res_order"]], dfr_form_lm_1[["res_order"]]) (should be TRUE): ', identical(dfr_1[["res_order"]], dfr_form_lm_1[["res_order"]]))
message('identical(dfr_2[["res_order"]], dfr_form_lm_2[["res_order"]]) (should be TRUE): ', identical(dfr_2[["res_order"]], dfr_form_lm_2[["res_order"]]))
message('identical(dfr_3[["res_order"]], dfr_form_lm_3[["res_order"]]) (should be TRUE): ', identical(dfr_3[["res_order"]], dfr_form_lm_3[["res_order"]]))
message('identical(dfr_4[["res_order"]], dfr_form_lm_4[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_form_lm_4[["res_order"]]))
message('identical(dfr_5[["res_order"]], dfr_form_lm_5[["res_order"]]) (should be TRUE): ', identical(dfr_5[["res_order"]], dfr_form_lm_5[["res_order"]]))
message('identical(dfr_6[["res_order"]], dfr_form_lm_6[["res_order"]]) (should be TRUE): ', identical(dfr_6[["res_order"]], dfr_form_lm_6[["res_order"]]))





# Compare the results above with what should be the identical results with the
# formula method with "nls" method.

test_form_nls <- y ~ u + v*b + w*c

dfr_form_nls_1 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = "asis")
dfr_form_nls_2 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = "optimal")
dfr_form_nls_3 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = "simplex")
dfr_form_nls_4 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = "natural")
dfr_form_nls_5 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = list(1:2))
dfr_form_nls_6 <- distfreereg(test_mean = test_form_nls, data = df,
                             covariance = list(Sigma = Sig), method = "nls",
                             verbose = TRUE, ordering = list("b", "c"))

message('identical(dfr_1[["res_order"]], dfr_form_nls_1[["res_order"]]) (should be TRUE): ', identical(dfr_1[["res_order"]], dfr_form_nls_1[["res_order"]]))
message('identical(dfr_2[["res_order"]], dfr_form_nls_2[["res_order"]]) (should be FALSE): ', identical(dfr_2[["res_order"]], dfr_form_nls_2[["res_order"]]))
message('identical(dfr_3[["res_order"]], dfr_form_nls_3[["res_order"]]) (should be TRUE): ', identical(dfr_3[["res_order"]], dfr_form_nls_3[["res_order"]]))
message('identical(dfr_4[["res_order"]], dfr_form_nls_4[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_form_nls_4[["res_order"]]))
message('identical(dfr_5[["res_order"]], dfr_form_nls_5[["res_order"]]) (should be TRUE): ', identical(dfr_5[["res_order"]], dfr_form_nls_5[["res_order"]]))
message('identical(dfr_6[["res_order"]], dfr_form_nls_6[["res_order"]]) (should be TRUE): ', identical(dfr_6[["res_order"]], dfr_form_nls_6[["res_order"]]))






# Now verify distfreereg.lm

test_lm <- lm(test_form_lm, data = df)

dfr_lm_1 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = "asis")
dfr_lm_2 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = "optimal")
dfr_lm_3 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = "simplex")
dfr_lm_4 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = "natural")
dfr_lm_5 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = list(1:2))
dfr_lm_6 <- distfreereg(test_mean = test_lm,
                        verbose = TRUE, ordering = list("b", "c"))

message('identical(dfr_1[["res_order"]], dfr_lm_1[["res_order"]]) (should be TRUE): ', identical(dfr_1[["res_order"]], dfr_lm_1[["res_order"]]))
message('identical(dfr_2[["res_order"]], dfr_lm_2[["res_order"]]) (should be TRUE): ', identical(dfr_2[["res_order"]], dfr_lm_2[["res_order"]]))
message('identical(dfr_3[["res_order"]], dfr_lm_3[["res_order"]]) (should be TRUE): ', identical(dfr_3[["res_order"]], dfr_lm_3[["res_order"]]))
message('identical(dfr_4[["res_order"]], dfr_lm_4[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_lm_4[["res_order"]]))
message('identical(dfr_5[["res_order"]], dfr_lm_5[["res_order"]]) (should be TRUE): ', identical(dfr_5[["res_order"]], dfr_lm_5[["res_order"]]))
message('identical(dfr_6[["res_order"]], dfr_lm_6[["res_order"]]) (should be TRUE): ', identical(dfr_6[["res_order"]], dfr_lm_6[["res_order"]]))







# Now verify distfreereg.nls

test_nls <- nls(test_form_nls, data = df)

dfr_nls_1 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = "asis")
dfr_nls_2 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = "optimal")
dfr_nls_3 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = "simplex")
dfr_nls_4 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = "natural")
dfr_nls_5 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = list(1:2))
dfr_nls_6 <- distfreereg(test_mean = test_nls,
                        verbose = TRUE, ordering = list("b", "c"))

message('identical(dfr_1[["res_order"]], dfr_nls_1[["res_order"]]) (should be TRUE): ', identical(dfr_1[["res_order"]], dfr_nls_1[["res_order"]]))
message('identical(dfr_2[["res_order"]], dfr_nls_2[["res_order"]]) (should be FALSE): ', identical(dfr_2[["res_order"]], dfr_nls_2[["res_order"]]))
message('identical(dfr_3[["res_order"]], dfr_nls_3[["res_order"]]) (should be TRUE): ', identical(dfr_3[["res_order"]], dfr_nls_3[["res_order"]]))
message('identical(dfr_4[["res_order"]], dfr_nls_4[["res_order"]]) (should be TRUE): ', identical(dfr_4[["res_order"]], dfr_nls_4[["res_order"]]))
message('identical(dfr_5[["res_order"]], dfr_nls_5[["res_order"]]) (should be TRUE): ', identical(dfr_5[["res_order"]], dfr_nls_5[["res_order"]]))
message('identical(dfr_6[["res_order"]], dfr_nls_6[["res_order"]]) (should be TRUE): ', identical(dfr_6[["res_order"]], dfr_nls_6[["res_order"]]))
