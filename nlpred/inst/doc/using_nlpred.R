## ------------------------------------------------------------------------
# load package
library(nlpred)

# turn off messages from np package
options(np.messages=FALSE)

# simulate data
n <- 200
p <- 10
X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))

# get cv auc estimates for logistic regression
logistic_cv_auc_ests <- cv_auc(Y = Y, X = X, K = 5, 
                               learner = "glm_wrapper",
                               nested_cv = FALSE)
logistic_cv_auc_ests

## ------------------------------------------------------------------------
# print a 90% CI
print(logistic_cv_auc_ests, ci_level = 0.9)

## ------------------------------------------------------------------------
# load the ranger package
library(ranger)

# set a seed (reason to be made clear)
set.seed(123)

# get cv auc estimates for random forest
# using nested cross-validation for nuisance parameter estimation
rf_cv_auc_ests <- cv_auc(Y = Y, X = X, K = 5, 
              			 learner = "ranger_wrapper", 
              			 nested_cv = TRUE)
rf_cv_auc_ests

## ------------------------------------------------------------------------
glm_wrapper

## ------------------------------------------------------------------------
# get cv scrnp estimates for logistic regression
logistic_cv_scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, 
                               learner = "glm_wrapper",
                               nested_cv = FALSE)
logistic_cv_scrnp_ests

# get cv scrnp estimates for random forest
# using nested cross-validation for nuisance parameter estimation
rf_cv_scrnp_ests <- cv_scrnp(Y = Y, X = X, K = 5, 
              			 learner = "ranger_wrapper", 
              			 nested_cv = TRUE)
rf_cv_scrnp_ests

## ------------------------------------------------------------------------
# get bootstrap estimated auc of logistic regression
boot_auc_est <- boot_auc(Y = Y, X = X, learner = "glm_wrapper", 
                         correct632 = FALSE)
boot_auc_est

# with 0.632 correction 
boot632_auc_est <- boot_auc(Y = Y, X = X, learner = "glm_wrapper", 
                         correct632 = TRUE)
boot632_auc_est

# get bootstrap estimated scrnp of logistic regression
boot_scrnp_est <- boot_scrnp(Y = Y, X = X, learner = "glm_wrapper", 
                             correct632 = FALSE)
boot_scrnp_est

# with 0.632 correction 
boot632_scrnp_est <- boot_scrnp(Y = Y, X = X, learner = "glm_wrapper", 
                         correct632 = TRUE)
boot632_scrnp_est

## ------------------------------------------------------------------------
# leave out at most 250 pairs
lpo_cv_auc_est <- lpo_auc(Y = Y, X = X, learner = "glm_wrapper",
                          max_pairs = 250)
lpo_cv_auc_est

