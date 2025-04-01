## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MMRcaseselection)

## -----------------------------------------------------------------------------
df_full <- lm(mpg ~ disp + wt, data = mtcars) # full model
df_reduced <- lm(mpg ~ wt, data = mtcars) # reduced model dropp 'disp' as pathway variable
pw_out <- pathway(df_full, df_reduced) # calculation of pathway variables
head(pw_out)

## ---- fig.height = 6, fig.width = 6-------------------------------------------
pathway_xvr(df_full, df_reduced, pathway_type = "pathway_wb")

## ---- fig.height = 6, fig.width = 6-------------------------------------------
pathway_xvr(df_full, df_reduced, pathway_type = "pathway_gvalue")

