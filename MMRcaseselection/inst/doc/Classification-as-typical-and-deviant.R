## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MMRcaseselection)
library(ggplot2) # for illustration of plot customization

## -----------------------------------------------------------------------------
df <- lm(mpg ~ disp + wt, data = mtcars)
pi_df <- predint(df, piwidth = 0.9)
head(pi_df)

## ---- fig.height = 6, fig.width = 6-------------------------------------------
predint_plot(pi_df)
# Using scale_color_brewer() instead of the viridis palette.
predint_plot(pi_df) + scale_color_brewer()

## -----------------------------------------------------------------------------
resid_df <- residstd(df, stdshare = 1.5)
head(resid_df)

## ---- fig.width = 6, fig.height = 6-------------------------------------------
residstd_plot(resid_df)

