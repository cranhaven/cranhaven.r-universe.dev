library(mgss)

n <- 100
P <- 2
data <- generate_test_data(n, P)
X_train <- data$X_train
y_train <- data$y_train

G <- 5
m <- rep(2^G-1, P)
q <- rep(3, P)
tol <- 0.1

#####--------------------------------------------
##### regularization parameter
lambda <- 0.0001


#####--------------------------------------------
##### Provided algorithms
penalty_type <- "curve"
model <- CG_smooth(m, q, lambda, X_train, y_train, pen_type = penalty_type, tolerance = tol)
penalty_type <- "diff"
model <- PCG_smooth(m, q, lambda, X_train, y_train, pen_type = penalty_type, tolerance = tol)
model <- MGCG_smooth(G, q, lambda, X_train, y_train, tolerance = tol)


#####--------------------------------------------
##### Model performance on training data
model$rmse
model$R_squared
n <- length(y_train)
res <- model$residuals
RSS <- sum(res^2)
df <- estimate_trace(m, q, lambda, X_train, pen_type = penalty_type)
AIC <- log(RSS) + 2*df/n
AIC_C <- log(RSS) + 2*(df+1) / (n-df-2)


#####--------------------------------------------
##### New predictions on test data and model validation
X_test <- data$X_test
y_test <- data$y_test

y_pred <- predict_smooth(model, X_test)
res <- y_pred - y_test
RSS <- sum(res^2)
mean_res <- mean(res)
sd_res <- sd(res)
rmse <- sqrt( mean( res^2 ) )
R_squared <- 1 - ( sum(res^2) / sum( (y_test-mean(y_test))^2 ) )

test_that("MGCG method conveges", {
  expect_true(RSS < 10.0)
})
