library(distfreereg)
set.seed(20240206)
n <- 1e2
func_1 <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
func_2 <- function(theta) theta
Sig <- diag(rexp(n))
w <- 1/diag(Sig)
theta_1 <- c(2,5,1)
theta_2 <- 7
X_1 <- matrix(rexp(2*n, rate = 1), nrow = n)
Y_1 <- distfreereg:::f2ftheta(f = func_1, X = X_1)(theta_1) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
Y_2 <- distfreereg:::f2ftheta(f = func_2, X = NULL, n = n)(theta_2) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))
dfr_1 <- distfreereg(Y = Y_1, X = X_1, test_mean = func_1,
                     covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE)
dfr_2 <- distfreereg(Y = Y_2, test_mean = func_2,
                     covariance = list(Sigma = Sig), theta_init = 1,
                     verbose = FALSE)

predict(dfr_1)
predict(dfr_2)
predict(dfr_2, newdata = 4)
predict(dfr_2, newdata = c(4,5))

df <- as.data.frame(cbind(X_1, Y_1))
colnames(df) <- c("a", "b", "c")
test_form <- c ~ a + b
dfr_3 <- distfreereg(test_mean = test_form, data = df,
                     covariance = list(Sigma = Sig),
                     verbose = FALSE)
message('all.equal(dfr_1$theta_hat, dfr_3$theta_hat, check.attributes = FALSE) (should be TRUE): ', all.equal(dfr_1$theta_hat, dfr_3$theta_hat, check.attributes = FALSE))

n_new <- 10
X_new <- matrix(rnorm(2*n_new), ncol = 2)
df_new <- as.data.frame(X_new)
colnames(df_new) <- c("a", "b")

predict(dfr_3)
predict(dfr_3, newdata = df_new)

all.equal(predict(dfr_3, newdata = df_new),
          predict(dfr_1, newdata = X_new), check.attributes = FALSE)# TRUE


m <- lm(test_form, data = df, weights = w)

dfr_4 <- distfreereg(test_mean = m, verbose = FALSE)

predict(dfr_4)
predict(dfr_4, newdata = df_new)

all.equal(predict(dfr_4, newdata = df_new),
          predict(dfr_1, newdata = X_new), check.attributes = FALSE)# TRUE





# nls

dfr_5 <- distfreereg(Y = Y_1, X = X_1, test_mean = func_1,
                     covariance = list(Sigma = Sig), theta_init = c(1,1,1),
                     verbose = FALSE)
form <- c ~ f + g*a + h*b
dfr_6 <- distfreereg(test_mean = form, data = df, method = "nls",
                     covariance = list(Sigma = Sig), verbose = FALSE)

message('all.equal(predict(dfr_5), predict(dfr_6), check.attributes = FALSE) (should be TRUE): ', all.equal(predict(dfr_5), predict(dfr_6), check.attributes = FALSE))

m_nls <- nls(form, data = df, weights = w)
dfr_7 <- distfreereg(m_nls)

message('all.equal(predict(dfr_6), predict(dfr_7), check.attributes = FALSE) (should be TRUE): ', all.equal(predict(dfr_6), predict(dfr_7), check.attributes = FALSE))
new_data <- data.frame(a = 1:2, b = 4:5)
message('identical(predict(dfr_6, newdata = new_data), predict(dfr_7, newdata = new_data)) (should be TRUE): ', identical(predict(dfr_6, newdata = new_data), predict(dfr_7, newdata = new_data)))
