## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)
library(marginaleffects)

## ----basic--------------------------------------------------------------------
data(mroz)
mroz$incthou <- mroz$faminc / 1000

fit <- ml_lm(incthou ~ age + I(age^2) + educ + kidslt6, data = mroz)

# Default: expected value (response)
pred <- predict(fit)
head(pred$fit)

# With standard errors
pred_se <- predict(fit, se.fit = TRUE)
head(data.frame(fit = pred_se$fit, se = pred_se$se.fit))

## ----several-preds------------------------------------------------------------
# Linear model
pred_lm <- predict(fit)
head(pred_lm$fit)

# With standard errors (robust)
pred_lm_se <- predict(fit, vcov.type = "robust", se.fit = TRUE)
head(data.frame(fit = pred_lm_se$fit, se = pred_lm_se$se.fit))

# Poisson model
pois <- ml_poisson(docvis ~ age + educyr + totchr, data = docvis)

pred_pois <- predict(pois)
head(pred_pois$fit)

pred_pois_prob <- predict(pois, type = "P(0)")   # Probability of zero
head(pred_pois_prob$fit)

# Negative Binomial (NB2)
nb2 <- ml_negbin(docvis ~ age + educyr + totchr, data = docvis)

pred_nb <- predict(nb2)
head(pred_nb$fit)

pred_nb_var <- predict(nb2, type = "variance")
head(pred_nb_var$fit)

# Beta regression (fractional response)
beta <- ml_beta(prate ~ mrate + age,
                scale = ~ sole + totemp,
                data = pw401k,
                subset = prate < 1)

pred_beta <- predict(beta)
head(pred_beta$fit)

pred_beta_phi <- predict(beta, type = "phi")     # Precision parameter
head(pred_beta_phi$fit)

## ----outofsample--------------------------------------------------------------
pred_beta <- predict(beta, newdata = pw401k)
head(pred_beta$fit)

## ----help---------------------------------------------------------------------
?predict.mlmodel

## ----basic-comp---------------------------------------------------------------
# Using our predict() function
our_pred <- predict(fit, 
                    vcov.type = "robust", 
                    se.fit = TRUE)

# Using marginaleffects::predictions()
me_pred <- predictions(fit, 
                       vcov = "robust")

# Compare first 8 observations
comp <- data.frame(
  obs         = 1:8,
  our_fit     = our_pred$fit[1:8],
  our_se      = our_pred$se.fit[1:8],
  me_fit      = me_pred$estimate[1:8],
  me_se       = me_pred$std.error[1:8]
)

comp

## ----default-sample-comp------------------------------------------------------
# Beta model fitted on a subset (prate < 1)
beta <- ml_beta(prate ~ mrate + age, 
                scale = ~ sole + totemp, 
                data = pw401k, 
                subset = prate < 1)

# Our predict() - returns full length with NAs
our_beta <- predict(beta, se.fit = TRUE, vcov.type = "robust")

# marginaleffects::predictions() - returns reduced dataset
me_beta <- predictions(beta, vcov = "robust")

# Compare frst 8 observations
head(data.frame(Estimate = our_beta$fit, Std.Error = our_beta$se.fit), 8)
head(me_beta[, c("estimate", "std.error")], 8)


## ----out-of-sample------------------------------------------------------------
# Out-of-sample with our predict()
our_full <- predict(beta, newdata = pw401k, se.fit = TRUE, vcov.type = "robust")

# Out-of-sample with marginaleffects
me_full <- predictions(beta, newdata = pw401k, vcov = "robust")

# Compare first 8 observations
comp <- data.frame(
  obs         = 1:8,
  our_fit     = our_full$fit[1:8],
  our_se      = our_full$se.fit[1:8],
  me_fit      = me_full$estimate[1:8],
  me_se       = me_full$std.error[1:8]
)

comp

## ----marg-align---------------------------------------------------------------
# predict as if out-of-sample
me_outin <- predictions(beta, newdata = pw401k, vcov = "robust")

# replace the values for the observations that weren't used with NA
me_outin[!beta$model$sample, ] <- NA

# Compare first 8 observations with our in-sample
comp <- data.frame(
  obs         = 1:8,
  our_fit     = our_beta$fit[1:8],
  our_se      = our_beta$se.fit[1:8],
  me_fit      = me_outin$estimate[1:8],
  me_se       = me_outin$std.error[1:8]
)

comp

## ----bootstrap-pred-----------------------------------------------------------
## 1st approach (Bootstrapped variance)
# pre-compute the variance on the linear model (low number of repetitions to make it fast)
v_boot <- vcov(fit, type = "boot", repetitions = 20, progress = FALSE)
# use it with predictions()
boot_delta <- predictions(fit, vcov = v_boot)

## 2nd approach (Bootstrapped prediction)
boot_pred <- predictions(fit) |>
  inferences(method = "boot", R = 20) # Inferences allows you to set the repetitions.

# The estimate is the same in both methods
comp <- data.frame(
  obs = 1:8,
  Estimate = boot_delta$estimate[1:8],
  Delta_Low = boot_delta$conf.low[1:8],
  Delta_High = boot_delta$conf.high[1:8],
  Pred_Low = boot_pred$conf.low[1:8],
  Pred_High = boot_pred$conf.high[1:8]
)

comp

## ----interaction-model--------------------------------------------------------
# Create binary variable
mroz$minors <- factor(mroz$kidslt6 > 0,
                      levels = c(FALSE, TRUE),
                      labels = c("No young children", "Has young children"))

# Gamma regression with interaction
fit_gamma <- ml_gamma(incthou ~ age * minors + educ + hushrs, 
                      data = mroz)

summary(fit_gamma, vcov.type = "robust")

## ----interaction-plot-pred, fig.width=10, fig.height=8, dpi=72----------------
# Load ggplot2 to plot with marginaleffects
library(ggplot2)
plot_predictions(fit_gamma, 
                 condition = c("age", "minors"),
                 vcov = "robust") +
  labs(title = "Predicted Family Income",
       subtitle = "Gamma regression model with interaction",
       color = "", 
       fill = "") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

## ----interaction-slopes, fig.width=10, fig.height=8, dpi=72-------------------
plot_comparisons(fit_gamma, 
            variables = "minors",
            condition = "age",
            vcov = "robust") +
  labs(title = "Contrast of Having Small Children on Family Income",
       subtitle = "By age (Gamma model)") +
  theme_minimal(base_size = 15)

