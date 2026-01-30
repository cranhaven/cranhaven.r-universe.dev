## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(pryr)

## ----install_github, eval = FALSE----------------------------------------
#  remotes::install_github("smaakage85/trimmer")

## ----install_cran, eval = FALSE------------------------------------------
#  install.packages("trimmer")

## ------------------------------------------------------------------------
library(trimmer)

## ------------------------------------------------------------------------
# load training data.
trn <- datasets::mtcars

# estimate model.
mdl <- lm(mpg ~ ., data = trn)

## ------------------------------------------------------------------------
mdl_trim <- trim(obj = mdl,
                 obj_arg_name = "object",
                 fun = predict,
                 newdata = trn)

## ------------------------------------------------------------------------
mdl_trim <- trim(obj = mdl,
                 obj_arg_name = "object",
                 fun = predict,
                 newdata = trn,
                 size_target = 0.015)

## ------------------------------------------------------------------------
mdl_trim <- trim(obj = mdl,
                 obj_arg_name = "object",
                 fun = summary)

