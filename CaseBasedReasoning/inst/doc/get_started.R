## ----setup, include=TRUE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(CaseBasedReasoning)
library(survival)

## ----LinearBetaModel----------------------------------------------------------
cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)

# linear_model <- LinearBetaModel$new(y ~ x1 + x2 + x3)

# logistic_model <- LogisticBetaModel$new(y ~ x1 + x2 + x3)

## -----------------------------------------------------------------------------
cph_model$fit()

## ----initialization, warning=FALSE, message=FALSE-----------------------------
ovarian$resid.ds <- factor(ovarian$resid.ds)
ovarian$rx <- factor(ovarian$rx)
ovarian$ecog.ps <- factor(ovarian$ecog.ps)

# initialize R6 object
cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)

## ----similarity---------------------------------------------------------------
n <- nrow(ovarian)
trainID <- sample(1:n, floor(0.8 * n), F)
testID <- (1:n)[-trainID]

cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian[trainID, ])

# fit model 
cph_model$fit()

# get similar cases
matched_data_tbl = cph_model$get_similar_cases(query = ovarian[testID, ], k = 3) 
knitr::kable(head(matched_data_tbl))

## ----proportional hazard, warning=FALSE, message=FALSE, fig.width=8, fig.height=8----
cph_model$check_ph()

## ----distance_matrix, fig.width=8, fig.height=8-------------------------------
distance_matrix = cph_model$calc_distance_matrix()
heatmap(distance_matrix)

