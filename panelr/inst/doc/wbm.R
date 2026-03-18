## ----setup, include = FALSE---------------------------------------------------
required <- c("clubSandwich", "geepack")
do_eval <- all(sapply(required, requireNamespace, quietly = TRUE))
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  message = FALSE,
  eval = do_eval
)

## ----include = FALSE----------------------------------------------------------
library(panelr)
data("teen_poverty")
teen_poverty

## ----echo = FALSE-------------------------------------------------------------
library(panelr)
data("teen_poverty")
teen_poverty

## -----------------------------------------------------------------------------
teen <- long_panel(teen_poverty, begin = 1, end = 5, label_location = "end")
teen

## -----------------------------------------------------------------------------
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

## ----eval = do_eval-----------------------------------------------------------
library(panelr)
library(splines)
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

mod_spline <- wbm(lwage ~ ns(exp, df = 3) | blk, data = wages)
mod_spline

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages)
summary(model)

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + lag(ms) + occ | blk + fem, data = wages)
summary(model)

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages, use.wave = TRUE)
summary(model)

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem | (t | id), use.wave = TRUE, data = wages)
summary(model)

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages, model = "contextual")
summary(model)

## -----------------------------------------------------------------------------
model <- wbm(lwage ~ wks + union + ms + occ, data = wages, model = "within")
summary(model)

## -----------------------------------------------------------------------------
model <- wbgee(lwage ~ wks + union + ms + occ | blk + fem, data = wages)
summary(model)

## -----------------------------------------------------------------------------
model <- asym(lwage ~ ms + occ + union + wks, data = wages)
summary(model)

## -----------------------------------------------------------------------------
summary(asym(hours ~ spouse + inschool, data = teen))

## ----message = TRUE-----------------------------------------------------------
model <- asym_gee(pov ~ mother + spouse + inschool + hours, data = teen, family = binomial(link = "logit"), 
                  use.wave = TRUE, wave.factor = TRUE)
summary(model)

