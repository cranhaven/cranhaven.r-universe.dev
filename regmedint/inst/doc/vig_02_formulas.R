## ----message = FALSE, tidy = FALSE, echo = F----------------------------------
## knitr configuration: http://yihui.name/knitr/options#chunk_options
library(knitr)
showMessage <- FALSE
showWarning <- TRUE
set_alias(w = "fig.width", h = "fig.height", res = "results")
opts_chunk$set(comment = "##", error= TRUE, warning = showWarning, message = showMessage,
               tidy = FALSE, cache = FALSE, echo = TRUE,
               fig.width = 7, fig.height = 7,
               fig.path = "man/figures")

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_linear_yreg_linear_est

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_linear_yreg_linear_se

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_linear_yreg_logistic_est

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_linear_yreg_logistic_se

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_logistic_yreg_linear_est

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_logistic_yreg_linear_se

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_logistic_yreg_logistic_est

## -----------------------------------------------------------------------------
regmedint:::calc_myreg_mreg_logistic_yreg_logistic_se

