## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MMRcaseselection)

## -----------------------------------------------------------------------------
df <- lm(mpg ~ disp + wt, data = mtcars)
extreme_on_x(df, "wt")

## -----------------------------------------------------------------------------
df <- lm(mpg ~ disp + wt, data = mtcars)
Y_extreme <- extreme_on_y(df)
head(Y_extreme)

