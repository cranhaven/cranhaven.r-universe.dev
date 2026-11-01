## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>")
library(baggr)
library(ggplot2)
baggr_schools <- baggr(schools, model = "rubin", pooling = "partial")
# baggr_plot(baggr_schools)
# baggr_compare(schools)
# my_baggr_plot <- baggr_compare(schools)

## ----eval=FALSE---------------------------------------------------------------
# # Auto-detect binary summary input (a, b, c, d columns)
# baggr(df_binary_summary)
# 
# # Auto-detect individual-level input with a multi-arm treatment factor
# baggr(df_multiarm_individual,
#       outcome = "outcome",
#       treatment = "treatment_arm",
#       group = "study")

## -----------------------------------------------------------------------------
prepare_ma(microcredit_simplified, outcome = "consumption")

## -----------------------------------------------------------------------------
schools

## ----eval=FALSE---------------------------------------------------------------
# baggr_schools <- baggr(schools, model = "rubin", pooling = "partial")

## -----------------------------------------------------------------------------
print(baggr_schools)

## -----------------------------------------------------------------------------
hypermean(baggr_schools)
hypersd(baggr_schools)

## -----------------------------------------------------------------------------
head(group_effects(baggr_schools, summary = TRUE))
treatment_effect(baggr_schools, summary = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# baggr(schools, "rubin",
#       prior_hypermean = normal(-5, 10),
#       prior_hypersd   = uniform(0, 5))

## ----eval=FALSE---------------------------------------------------------------
# custom_priors <- list( hypermean = cauchy(0,25), hypersd = normal(0,30))
# baggr(schools, "rubin", pooling = "partial", prior = custom_priors)

## ----eval=FALSE---------------------------------------------------------------
# baggr_schools <- baggr(schools, model = "rubin", pooling = "partial",
#                        iter = 10000, chains = 8)

## ----eval=FALSE---------------------------------------------------------------
# baggr_schools <- baggr(schools, model = "rubin", pooling = "partial",
#                        iter = 10000, chains = 8,
#                        control = list(adapt_delta = 0.99)) #see rstan documentation for details

## -----------------------------------------------------------------------------
pooling(baggr_schools)

## ----fig.width=4--------------------------------------------------------------
plot(baggr_schools, order = FALSE)

## ----fig.width = 4------------------------------------------------------------
effect_plot(baggr_schools)

## -----------------------------------------------------------------------------
effect_draw(baggr_schools, draws = 1)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# my_baggr_comparison <- baggr_compare(schools)

## ----echo=FALSE, eval = TRUE, include = FALSE---------------------------------
my_baggr_comparison <- baggr_compare(schools)

## ----fig.width=5, fig.height=4, echo = TRUE-----------------------------------
plot(my_baggr_comparison) + 
  ggtitle("8 schools: model comparison")

## ----eval=F, echo=T-----------------------------------------------------------
# baggr_schools_v2 <- baggr(schools, prior_hypermean = normal(10, 2.5))

## ----eval=T, include=F--------------------------------------------------------
baggr_schools_v2 <- baggr(schools, prior_hypermean = normal(10, 2.5))

## ----fig.width=6, fig.height=5------------------------------------------------
effect_plot("Default model" = baggr_schools, "normal(10, 2.5)" = baggr_schools_v2) +
  coord_cartesian(xlim = c(-10, 30)) + theme(legend.position = "top")
baggr_compare("Default model" = baggr_schools, "normal(10, 2.5)" = baggr_schools_v2)

## ----fig.width=8, fig.height = 5.5, out.width="90%"---------------------------
forest_plot(baggr_schools, show = "both")

## ----echo=TRUE, fig.width=8, fig.height = 5, out.width="90%"------------------
plot(baggr_schools, style = "forest")

## ----loocv, echo = T, results = 'hide', warning = F, message = F--------------
loocv_res <- loocv(schools, return_models = FALSE, 
                   iter = 1000, #just to make it a bit faster -- don't try it at home!
                   model = "rubin", pooling = "partial")

## -----------------------------------------------------------------------------
loocv_res

## -----------------------------------------------------------------------------
names(attributes(loocv_res))
attr(loocv_res, "df")

## ----echo = FALSE, include = FALSE, results = 'hide'--------------------------
fit1 <- baggr(data = schools[1:7,], test_data = schools[8,], 
              model = "rubin", pooling = "partial")
fit2 <- baggr(data = schools[1:7,], test_data = schools[8,], 
              model = "rubin", pooling = "full")

## -----------------------------------------------------------------------------
fit1$mean_lpd
fit2$mean_lpd

## ----results = 'hide'---------------------------------------------------------
loocv_full <- loocv(data = schools, 
                    model = "rubin", 
                    pooling = "full")

## -----------------------------------------------------------------------------
loo_compare(loocv_res, loocv_full)

