## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libraries, message=FALSE-------------------------------------------------
library(gesso)
library(glmnet)
library(ggplot2)
library(bigmemory)

## -----------------------------------------------------------------------------
family = "gaussian"
sample_size = 180; p = 400; n_g_non_zero = 10; n_gxe_non_zero = 5

data = data.gen(seed=1, sample_size=sample_size, p=p, 
                n_g_non_zero=n_g_non_zero, 
                n_gxe_non_zero=n_gxe_non_zero, 
                mode="strong_hierarchical",
                family=family)

## -----------------------------------------------------------------------------
dim(data$G_train)
head(data$G_train[,1:5])
data$E_train[1:10]
head(data$Y_train)

## -----------------------------------------------------------------------------
c(sum(data$Beta_G != 0), sum(data$Beta_GxE != 0))

## -----------------------------------------------------------------------------
cbind(data$Beta_G[data$Beta_G != 0], data$Beta_GxE[data$Beta_G != 0])

## -----------------------------------------------------------------------------
start = Sys.time()
tune_model = gesso.cv(G=data$G_train, E=data$E_train, Y=data$Y_train, 
                      family=family, grid_size=20, tolerance=1e-4,
                      grid_min_ratio=1e-2,
                      parallel=TRUE, nfolds=3,
                      normalize=TRUE,
                      normalize_response=TRUE,
                      seed=1,
                      max_iterations=10000)
Sys.time() - start


## -----------------------------------------------------------------------------
coefficients = gesso.coef(fit=tune_model$fit, lambda=tune_model$lambda_min)
gxe_coefficients = coefficients$beta_gxe                      
g_coefficients = coefficients$beta_g   

## -----------------------------------------------------------------------------
cbind(data$Beta_GxE[data$Beta_GxE != 0], gxe_coefficients[data$Beta_GxE != 0])

## -----------------------------------------------------------------------------
(data$Beta_GxE[order(abs(gxe_coefficients), decreasing=TRUE)])[1:10]

## -----------------------------------------------------------------------------
selection_gesso = selection.metrics(true_b_g=data$Beta_G, true_b_gxe=data$Beta_GxE,
                                    estimated_b_g=g_coefficients,
                                    estimated_b_gxe=gxe_coefficients)
cbind(selection_gesso)

## -----------------------------------------------------------------------------
set.seed(1)
tune_model_glmnet = cv.glmnet(x=cbind(data$E_train, data$G_train, 
                                      data$G_train * data$E_train),
                              y=data$Y_train,
                              nfolds=3,
                              family=family)

coef_glmnet = coef(tune_model_glmnet, s=tune_model_glmnet$lambda.min)
g_glmnet = coef_glmnet[3: (p + 2)]
gxe_glmnet = coef_glmnet[(p + 3): (2 * p + 2)]   

cbind(data$Beta_GxE[data$Beta_GxE != 0], gxe_glmnet[data$Beta_GxE != 0])
(data$Beta_GxE[order(abs(gxe_glmnet), decreasing=TRUE)])[1:10]

selection_glmnet = selection.metrics(data$Beta_G, data$Beta_GxE, g_glmnet, gxe_glmnet)
cbind(selection_gesso, selection_glmnet)

## -----------------------------------------------------------------------------
coefficients = gesso.coefnum(cv_model=tune_model, target_b_gxe_non_zero=5)
gxe_coefficients = coefficients$beta_gxe                      
g_coefficients = coefficients$beta_g   

## -----------------------------------------------------------------------------
selection_gesso = selection.metrics(data$Beta_G, data$Beta_GxE, g_coefficients,
                                    gxe_coefficients)
cbind(selection_gesso, selection_glmnet)

## -----------------------------------------------------------------------------
coefficients = gesso.coef(tune_model$fit, tune_model$lambda_min)
beta_0 = coefficients$beta_0; beta_e = coefficients$beta_e                   
beta_g = coefficients$beta_g; beta_gxe = coefficients$beta_gxe     

new_G = data$G_test; new_E = data$E_test
new_Y = gesso.predict(beta_0, beta_e, beta_g, beta_gxe, new_G, new_E)
test_R2_gesso = cor(new_Y, data$Y_test)^2

## -----------------------------------------------------------------------------
new_Y_glmnet = predict(tune_model_glmnet, newx=cbind(new_E, new_G, new_G * new_E), 
                       s=tune_model_glmnet$lambda.min)
test_R2_glmnet = cor(new_Y_glmnet[,1], data$Y_test)^2
cbind(test_R2_gesso, test_R2_glmnet)

## -----------------------------------------------------------------------------
family = "gaussian"
sample_size = 180; p = 400; n_g_non_zero = 10; n_gxe_non_zero = 5
n_confounders = 2

grid = 10^seq(-3, log10(1), length.out = 20)

data = data.gen(seed=1, sample_size=sample_size, p=p, 
                n_g_non_zero=n_g_non_zero, 
                n_gxe_non_zero=n_gxe_non_zero, 
                mode="strong_hierarchical",
                family=family,
                n_confounders=n_confounders)

tune_model = gesso.cv(G=data$G_train, E=data$E_train, Y=data$Y_train, 
                      C=data$C_train,
                      family=family, grid=grid, tolerance=1e-4,
                      parallel=TRUE, nfolds=3,
                      normalize=TRUE,
                      normalize_response=TRUE,
                      verbose=FALSE,
                      seed=1)


## -----------------------------------------------------------------------------
G_train_sparse = as(data$G_train, "dgCMatrix")

start = Sys.time()
fit = gesso.fit(G=G_train_sparse, E=data$E_train, Y=data$Y_train, 
                tolerance=1e-4,
                grid_size=20, grid_min_ratio=1e-1,
                normalize=TRUE,
                normalize_response=TRUE)
time_sparse = difftime(Sys.time(), start, units="secs"); time_sparse

## -----------------------------------------------------------------------------
hist(fit$working_set_size, breaks = 100, col="blue")

## ----fig.height = 3, fig.width = 4, fig.align = "center"----------------------
df = data.frame(lambda_1_factor = factor(fit$lambda_1),
                lambda_2_factor = factor(fit$lambda_2),
                ws = fit$working_set_size)

log_0 = function(x){
  return(ifelse(x == 0, 0, log10(x)))
}

ggplot(df, aes(lambda_1_factor, lambda_2_factor, fill=log_0(ws))) + 
  scale_fill_distiller(palette = "RdBu") +
  scale_x_discrete("lambda_1", breaks=c(1)) +
  scale_y_discrete("lambda_2", breaks=c(1)) +
  labs(fill='log WS') +
  geom_tile()

