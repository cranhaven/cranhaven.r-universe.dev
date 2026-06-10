## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(nestedcv)
library(mlbench)  # Boston housing dataset

data(BostonHousing2)
dat <- BostonHousing2
y <- dat$cmedv
x <- subset(dat, select = -c(cmedv, medv, town, chas))

# Fit a glmnet model using nested CV
set.seed(1, "L'Ecuyer-CMRG")
fit <- nestcv.glmnet(y, x, family = "gaussian",
                     min_1se = 1, alphaSet = 1, cv.cores = 2)
vs <- var_stability(fit)
vs

## ----fig.dim = c(10, 5)-------------------------------------------------------
p1 <- plot_var_stability(fit)

# overlay directionality using colour
p2 <- plot_var_stability(fit, final = FALSE, direction = 1)

# or show directionality with the sign of the variable importance
# plot_var_stability(fit, final = FALSE, percent = F)

ggpubr::ggarrange(p1, p2, ncol=2)

## ----eval=FALSE---------------------------------------------------------------
# # change bubble colour scheme
# p1 + scale_fill_manual(values=c("orange", "green3"))

## ----fig.dim = c(10, 5)-------------------------------------------------------
# beeswarm plot of variable ranks 
p1 <- plot_var_ranks(fit)

# histogram of variable ranks
p2 <- hist_var_ranks(fit)

ggpubr::ggarrange(p1, p2, ncol=2)

## -----------------------------------------------------------------------------
library(fastshap)

# Generate SHAP values using fastshap::explain
# Only using 5 repeats here for speed, but recommend higher values of nsim
sh <- explain(fit, X=x, pred_wrapper = pred_nestcv_glmnet, nsim = 5)

# Plot overall variable importance
plot_shap_bar(sh, x)

## -----------------------------------------------------------------------------
# Plot beeswarm plot
plot_shap_beeswarm(sh, x, size = 1)

## ----eval=FALSE---------------------------------------------------------------
# # Only 3 outer folds to speed up process
# fit <- nestcv.train(y, x,
#                     method = "gbm",
#                     n_outer_folds = 3, cv.cores = 2)
# 
# # Only using 5 repeats here for speed, but recommend higher values of nsim
# sh <- explain(fit, X=x, pred_wrapper = pred_train, nsim = 5)
# plot_shap_beeswarm(sh, x, size = 1)

## ----fig.width = 9, fig.height = 3.5------------------------------------------
library(ggplot2)
data("iris")
dat <- iris
y <- dat$Species
x <- dat[, 1:4]

# Only 3 outer folds to speed up process
fit <- nestcv.glmnet(y, x, family = "multinomial", n_outer_folds = 3, alphaSet = 0.6)

# SHAP values for each of the 3 classes
sh1 <- explain(fit, X=x, pred_wrapper = pred_nestcv_glmnet_class(1), nsim = 5)
sh2 <- explain(fit, X=x, pred_wrapper = pred_nestcv_glmnet_class(2), nsim = 5)
sh3 <- explain(fit, X=x, pred_wrapper = pred_nestcv_glmnet_class(3), nsim = 5)

s1 <- plot_shap_bar(sh1, x, sort = FALSE) +
  ggtitle("Setosa")
s2 <- plot_shap_bar(sh2, x, sort = FALSE) +
  ggtitle("Versicolor")
s3 <- plot_shap_bar(sh3, x, sort = FALSE) +
  ggtitle("Virginica")

ggpubr::ggarrange(s1, s2, s3, ncol=3, legend = "bottom", common.legend = TRUE)

## ----fig.width = 9.5, fig.height = 3.5----------------------------------------
s1 <- plot_shap_beeswarm(sh1, x, sort = FALSE, cex = 0.7) +
  ggtitle("Setosa")
s2 <- plot_shap_beeswarm(sh2, x, sort = FALSE, cex = 0.7) +
  ggtitle("Versicolor")
s3 <- plot_shap_beeswarm(sh3, x, sort = FALSE, cex = 0.7) +
  ggtitle("Virginica")

ggpubr::ggarrange(s1, s2, s3, ncol=3, legend = "right", common.legend = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# sh <- explain(fit, X = fit$xsub, pred_wrapper = pred_nestcv_glmnet, nsim = 5)
# plot_shap_bar(sh, fit$xsub)
# 
# sh <- explain(fit, X = x[, fit$final_vars], pred_wrapper = pred_nestcv_glmnet, nsim = 5)
# plot_shap_bar(sh, x[, fit$final_vars])

