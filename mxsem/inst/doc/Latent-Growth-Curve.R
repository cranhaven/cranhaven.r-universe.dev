## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
library(mxsem)

## ---- echo=FALSE--------------------------------------------------------------
set.seed(123)
head(mxsem::simulate_latent_growth_curve(N = 100)[,paste0("y",1:5)])

## -----------------------------------------------------------------------------
model <- "
  # specify latent intercept
     I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  # specify latent slope
     S =~ 0 * y1 + 1 * y2 + 5 * y3 + 7 * y4 + 11 * y5
    
  # specify means of latent intercept and slope
     I ~ int*1
     S ~ slp*1
  
  # set intercepts of manifest variables to zero
     y1 ~ 0*1; y2 ~ 0*1; y3 ~ 0*1; y4 ~ 0*1; y5 ~ 0*1;
  "

## -----------------------------------------------------------------------------
library(mxsem)
lgc_dat <- simulate_latent_growth_curve(N = 100)
head(lgc_dat)

## -----------------------------------------------------------------------------
model <- "
  # specify latent intercept
     I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
  # specify latent slope
     S =~ data.t_1 * y1 + data.t_2 * y2 + data.t_3 * y3 + data.t_4 * y4 + data.t_5 * y5
    
  # specify means of latent intercept and slope
     I ~ int*1
     S ~ slp*1
  
  # set intercepts of manifest variables to zero
     y1 ~ 0*1; y2 ~ 0*1; y3 ~ 0*1; y4 ~ 0*1; y5 ~ 0*1;
  "

## -----------------------------------------------------------------------------
# set up model
lgc_mod <- mxsem(model = model, 
                 data = lgc_dat, 
                 # we set scale_loadings to FALSE because the 
                 # loadings were already fixed to specific values.
                 # This just avoids a warning from mxsem
                 scale_loadings = FALSE)
# fit 
lgc_fit <- mxRun(model = lgc_mod)

summary(lgc_fit)

