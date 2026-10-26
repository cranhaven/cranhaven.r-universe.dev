## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stxplore)
library(dplyr)
library(tidyr)
library(cubelyr)
library(stars)

## ----loaddata-----------------------------------------------------------------
data("aerosol_australia")
aerosol_australia

## ----ssnap1-------------------------------------------------------------------
aerosol_australia4 <- aerosol_australia %>% 
  slice(date, 1:4)
spatial_snapshots(aerosol_australia4)

## ----tempsnap-----------------------------------------------------------------
xvals <- c(120, 150)
yvals <- c(-20, -35)
temporal_snapshots(aerosol_australia,
                   xvals = xvals,
                   yvals = yvals)


## ----spmeans------------------------------------------------------------------
spmeans <- spatial_means(aerosol_australia)
autoplot(spmeans)



## ----tempmeans----------------------------------------------------------------
tem <- temporal_means(aerosol_australia)
autoplot(tem)

## ----hovmoller1---------------------------------------------------------------
hov <- hovmoller(aerosol_australia, lat_or_lon = 'lon')
autoplot(hov)

## ----hovmoller2---------------------------------------------------------------
hov <- hovmoller(aerosol_australia, lat_or_lon = 'lat')
autoplot(hov)

## ----ridgeline----------------------------------------------------------------
ridgeline(aerosol_australia, group_dim = 1)


## ----empcov1------------------------------------------------------------------
aerosol_region <- aerosol_australia %>%
  filter(x > 150, x < 170, y < -20, y> -40  )

# longitudinal strips
emp <- emp_spatial_cov(aerosol_region, num_strips = 2)
autoplot(emp)

## ----empcov2------------------------------------------------------------------
# latitude strips
emp <- emp_spatial_cov(aerosol_region, num_strips = 2, lat_or_lon_strips = 'lat')
autoplot(emp)

## ----semivariogram1-----------------------------------------------------------
semi <- semivariogram(aerosol_region)
autoplot(semi)

## ----eof----------------------------------------------------------------------

eoff <- emp_orth_fun(aerosol_australia)
autoplot(eoff, EOF_num = 1)
autoplot(eoff, EOF_num = 2)
autoplot(eoff, EOF_num = 3)

## ----eof2---------------------------------------------------------------------
pc1 <- eoff$pcts %>%
  filter(EOF == 'X1') %>%
  pull(nPC)

pc2 <- eoff$pcts %>%
  filter(EOF == 'X2') %>%
  pull(nPC)

cor(pc1, pc2)

## ----eof3---------------------------------------------------------------------
pcs <- eoff$pcts %>% 
  select(t, EOF, nPC) %>% 
  pivot_wider(names_from = EOF, values_from = nPC) %>%
  select(-t)

cormat <- cor(pcs)[1:3, 1:3]
cormat

## ----cancor-------------------------------------------------------------------
cc1 <- cancor_eof(aerosol_australia, lag = 6, n_eof = 4)
autoplot(cc1)

