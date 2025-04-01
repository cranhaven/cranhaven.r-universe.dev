## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F-------------------------------------------------------
library(MMRcaseselection)

## -----------------------------------------------------------------------------
df <- lm(mpg ~ disp + wt, data = mtcars)
most_typical(df)
most_deviant(df)

## -----------------------------------------------------------------------------
# largest positive residual
most_underpredicted(df)
# largest negative residual
most_overpredicted(df)

