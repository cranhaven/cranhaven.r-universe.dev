## ----message=FALSE, echo=FALSE------------------------------------------------
library("gbm3")

## -----------------------------------------------------------------------------
# create some binomial response data - N observations 
N <- 1000
X1 <- runif(N)
X2 <- runif(N)
X3 <- factor(sample(letters[1:4],N,replace=T))
mu <- c(-1,0,1,2)[as.numeric(X3)]

p <- 1/(1+exp(-(sin(3*X1) - 4*X2 + mu)))
Y <- rbinom(N,1,p) # response

# Normalized weights for each observation
w <- rexp(N)
w <- N*w/sum(w)

# Offset is set to 0
offset <- rep(0, N)

# Collate data
data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3)

## -----------------------------------------------------------------------------
available_distributions()

## -----------------------------------------------------------------------------
# Create a default Bernoulli 
bern_dist <- gbm_dist("Bernoulli")

## -----------------------------------------------------------------------------
# Creating an appropriate training parameters object
train_params <- training_params(num_trees = 2000, interaction_depth = 3, 
                                num_train=nrow(data), num_features = 3)

## -----------------------------------------------------------------------------
# Example of a gbmParallel with more threads
par_detail <- gbmParallel(num_threads = 10) # Pass to par_details in gbmt

## ----message=FALSE------------------------------------------------------------
# Create a gbm fit
fit <- gbmt(Y ~ X1 + X2 + X3, distribution=bern_dist, data=data, weights = w, 
            offset=offset, train_params = train_params,
            cv_folds=5, keep_gbm_data=TRUE)

## ----message=FALSE------------------------------------------------------------
# A default fit to gaussian data
# Create data
N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- factor(sample(letters[1:4],N,replace=T))
X4 <- ordered(sample(letters[1:6],N,replace=T))
X5 <- factor(sample(letters[1:3],N,replace=T))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]

SNR <- 10 # signal-to-noise ratio
Y <- X1**1.5 + 2 * (X2**.5) + mu
sigma <- sqrt(var(Y)/SNR)
Y <- Y + rnorm(N,0,sigma)

# create a bunch of missing values
X1[sample(1:N,size=100)] <- NA
X3[sample(1:N,size=300)] <- NA

# Gaussian data
gauss_data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

# Perform a cross-validated fit
gauss_fit <- gbmt(Y ~ X1 + X2 + X3 + X4 + X5 + X6, 
                  data=gauss_data, cv_folds =5, keep_gbm_data = TRUE)

## ----message=FALSE------------------------------------------------------------
# Use gaussian fit and determine the performance
# red line is testing error
# green line is cv error
# blue line is OOB error
best_iter_test <- gbmt_performance(gauss_fit, method='test')
best_iter_cv <- gbmt_performance(gauss_fit, method='cv')
best_iter_oob <- gbmt_performance(gauss_fit, method='OOB')

## ----message=FALSE------------------------------------------------------------
# Fit additional trees to gaussian fit and recalculate optimal number of iterations
gauss_fit_more <- gbm_more(gauss_fit, num_new_trees = 5000) # This is a large number of 
                                                            #extra trees!
best_iter_test_more <- gbmt_performance(gauss_fit_more, method='test')

## ----message=FALSE------------------------------------------------------------
# Given additional data - of the same shape as our gaussian example
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- factor(sample(letters[1:4],N,replace=TRUE))
X4 <- ordered(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]

Y <- X1**1.5 + 2 * (X2**.5) + mu
Y <- Y + rnorm(N,0,sigma)

data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

# Then predictions can be made
preds <- predict(gauss_fit_more, newdata=data2, n.trees=best_iter_test_more)

## -----------------------------------------------------------------------------
# Relative influence of predictors - both examples
print(relative_influence(gauss_fit_more, best_iter_test_more))
print(relative_influence(fit, gbmt_performance(fit, method='cv')))

## -----------------------------------------------------------------------------
# Examine the marginal effects of the first two variables - gaussian example
# Shows the fitted model predictions as a function of 1st two predictors
# with all the others integrated out
plot(gauss_fit_more, var_index = 1:2)

