## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mlmodels)
library(marginaleffects)
library(dplyr)
library(ggplot2)

## ----qmle-pw------------------------------------------------------------------
data("pw401k")

# Store formula for multiple use
form_full <- prate ~ mrate + I(mrate^2) + 
                    log(totemp) + I(log(totemp)^2) + 
                    age + I(age^2) + sole

# Estimation
logit_full  <- ml_logit(form_full, data = pw401k)
probit_full <- ml_probit(form_full, data = pw401k)

# Display the results
summary(logit_full,  vcov.type = "robust")
summary(probit_full, vcov.type = "robust")

## ----grid-predictions---------------------------------------------------------
# Grid focused on the main mass of the data (covers up to ~Q3 + a bit)
# You can pass either fitted model in `model`.
newdata <- datagrid(model = logit_full,
                    age = seq(4, 25, length.out = 80),
                    FUN = mean)

# Predictions (participation rate)
pred_logit  <- predictions(logit_full,  newdata = newdata, vcov = "robust")
pred_probit <- predictions(probit_full, newdata = newdata, vcov = "robust")

# Marginal effects of age
me_logit  <- slopes(logit_full,  variables = "age", newdata = newdata, vcov = "robust")
me_probit <- slopes(probit_full, variables = "age", newdata = newdata, vcov = "robust")

## ----plot-qmle-prob, fig.width=10, fig.height=8, dpi=72-----------------------
# Form Long dataset with both predictions and factor variable Model.
probs <- bind_rows(
  pred_logit |> mutate(Model = "Logit"),
  pred_probit  |> mutate(Model = "Probit")
)

ggplot(probs, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Predicted Participation Rate by Age",
       subtitle = "Logit vs Probit models",
       x = "Age", 
       y = "Participation Rate",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

## ----plot-qmle-mefss, fig.width=10, fig.height=8, dpi=72----------------------
mes <- bind_rows(
  me_logit |> mutate(Model = "Logit"),
  me_probit  |> mutate(Model = "Probit")
)

ggplot(mes, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Marginal Effects of Age on Participation Rate",
       subtitle = "Logit vs Probit models",
       x = "Age", 
       y = "Marginal Effect",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

## ----strict-frac--------------------------------------------------------------
# There are no outcomes at 0, only at 1. Subset accordingly

logit_frac <- ml_logit(form_full, data = pw401k, subset = prate < 1)
beta_frac  <- ml_beta(form_full,  data = pw401k, subset = prate < 1)

summary(logit_frac, vcov.type = "robust")
summary(beta_frac,  vcov.type = "robust")


## ----pred-log-beta------------------------------------------------------------
# New grid to recalculate the means to the reuced sample.
newdata <- datagrid(model = logit_frac,
                    age = seq(4, 25, length.out = 80),
                    FUN = mean)

# Predictions (participation rate)
pred_logit  <- predictions(logit_frac,  newdata = newdata, vcov = "robust")
pred_beta <- predictions(beta_frac, newdata = newdata, vcov = "robust")

# Marginal effects of age
me_logit  <- slopes(logit_frac,  variables = "age", newdata = newdata, vcov = "robust")
me_beta <- slopes(beta_frac, variables = "age", newdata = newdata, vcov = "robust")


## ----plot-logbeta-prob, fig.width=10, fig.height=8, dpi=72--------------------
probs <- bind_rows(
  pred_logit |> mutate(Model = "Logit"),
  pred_beta  |> mutate(Model = "Beta")
)

ggplot(probs, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Predicted Participation Rate by Age",
       subtitle = "Logit vs Beta models",
       x = "Age", 
       y = "Participation Rate",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

## ----plot-logbeta-mefss, fig.width=10, fig.height=8, dpi=72-------------------
mes <- bind_rows(
  me_logit |> mutate(Model = "Logit"),
  me_beta  |> mutate(Model = "Beta")
)

ggplot(mes, aes(x = age, y = estimate, color = Model, fill = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Marginal Effects of Age on Participation Rate",
       subtitle = "Logit vs Beta models",
       x = "Age", 
       y = "Marginal Effect",
       color = "",
       fill = "") +
  theme_minimal(base_size = 15) + 
  theme(legend.position = "bottom")

