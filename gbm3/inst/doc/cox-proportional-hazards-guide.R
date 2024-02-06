## ----message=FALSE, echo=FALSE------------------------------------------------
library("gbm3")

## ----message=FALSE------------------------------------------------------------
## Install package
require(survival)

# get datasets
right_cens <- cgd[cgd$enum==1, ]
start_stop <- cgd

# Set up GBMDist objects
right_cens_dist <- gbm_dist("CoxPH", strata=right_cens$hos.cat)
start_stop_dist <- gbm_dist("CoxPH", strata=start_stop$hos.cat)

## ----messagee=FALSE-----------------------------------------------------------
# Set-up training parameters
params_right_cens <- training_params(num_trees = 2000, interaction_depth = 3, 
                                     id=right_cens$id,
                                     num_train=round(0.5 * length(unique(right_cens$id))) )
params_start_stop <- training_params(num_trees = 2000, interaction_depth = 3, 
                                     id=start_stop$id,
                                     num_train=round(0.5 * length(unique(start_stop$id))) )

# Call to gbmt
fit_right_cens <- gbmt(Surv(tstop, status)~ age + sex + inherit +
                     steroids + propylac, data=right_cens, 
                     distribution = right_cens_dist,
                     train_params = params_right_cens, cv_folds=10,
                     keep_gbm_data = TRUE)
fit_start_stop <- gbmt(Surv(tstart, tstop, status)~ age + sex + inherit +
                     steroids + propylac, data=start_stop, 
                     distribution = start_stop_dist,
                     train_params = params_start_stop, cv_folds=10, 
                     keep_gbm_data = TRUE)

# Plot performance
best_iter_right <- gbmt_performance(fit_right_cens, method='test')
best_iter_stop_start <- gbmt_performance(fit_start_stop, method='test')

## ----message=FALSE, fig.height=6, fig.width=6---------------------------------
# Example using Breslow and Efron tie-breaking

# Create data
require(survival)

set.seed(1)
N <- 3000
X1 <- runif(N)
X2 <- runif(N)
X3 <- factor(sample(letters[1:4],N,replace=T))
mu <- c(-1,0,1,2)[as.numeric(X3)]

f <- 0.5*sin(3*X1 + 5*X2^2 + mu/10)
tt.surv <- rexp(N,exp(f))
tt.cens <- rexp(N,0.5)
delta <- as.numeric(tt.surv <= tt.cens)
tt <- apply(cbind(tt.surv,tt.cens),1,min)

# throw in some missing values
X1[sample(1:N,size=100)] <- NA
X3[sample(1:N,size=300)] <- NA

# random weights if you want to experiment with them
w <- rep(1,N)
data <- data.frame(tt=tt,delta=delta,X1=X1,X2=X2,X3=X3)

# Set up distribution objects
cox_breslow <- gbm_dist("CoxPH", ties="breslow", prior_node_coeff_var=100)
cox_efron <- gbm_dist("CoxPH", ties="efron", prior_node_coeff_var=100)

# Define training parameters
params <- training_params(num_trees=3000, interaction_depth=3, min_num_obs_in_node=10, 
                          shrinkage=0.001, bag_fraction=0.5, id=seq(nrow(data)), 
                          num_train=N/2, num_features=3)

# Fit gbm 
fit_breslow <- gbmt(Surv(tt, delta)~X1+X2+X3, data=data, distribution=cox_breslow, 
                    weights=w, train_params=params, var_monotone=c(0, 0, 0), 
                    keep_gbm_data=TRUE, cv_folds=5, is_verbose = FALSE)

fit_efron <- gbmt(Surv(tt, delta)~X1+X2+X3, data=data, distribution=cox_efron,
                  weights=w, train_params=params, var_monotone=c(0, 0, 0), 
                  keep_gbm_data=TRUE, cv_folds=5, is_verbose = FALSE)


# Evaluate fit 
plot(gbmt_performance(fit_breslow, method='test'))
legend("topleft", c("training error", "test error", "optimal iteration"),
       lty=c(1, 1, 2), col=c("black", "red", "blue"))
plot(gbmt_performance(fit_efron, method='test'))
legend("topleft", c("training error", "test error", "optimal iteration"),
       lty=c(1, 1, 2), col=c("black", "red", "blue"))

