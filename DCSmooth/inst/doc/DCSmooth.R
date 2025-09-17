## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, warning = FALSE-------------------------------------------
library(DCSmooth)
set.seed(123)
summary(set.options(type = "KR"))

## ---- echo = FALSE, warning = FALSE-------------------------------------------
summary(set.options(type = "LP"))

## -----------------------------------------------------------------------------
# surface.dcs(y.norm1)
# surface.dcs(y.norm2)
# surface.dcs(y.norm3)

## -----------------------------------------------------------------------------
# surface.dcs(temp.nunn)
# surface.dcs(temp.yuma)
# surface.dcs(wind.nunn)
# surface.dcs(wind.yuma)

## -----------------------------------------------------------------------------
# surface.dcs(returns.alv)
# surface.dcs(volumes.alv)

## -----------------------------------------------------------------------------
opt1 = set.options(type = "KR", kerns = c("M_220", "M_422"), drv = c(0, 2),
                   var_model = "sarma_RSS",
                   IPI_options = list(trim = c(0.1, 0.1), infl_par = c(1, 1),
                                      infl_exp = c(0.7, 0.7), 
                                      const_window = TRUE),
                   model_order = list(ar = c(1, 1), ma = c(0, 0)))
summary(opt1)
class(opt1)

## -----------------------------------------------------------------------------
opt2 = set.options(type = "KR", kerns = c("M_220", "M_422"), drv = c(0, 2),
                   var_model = "sarma_RSS", trim = c(0.1, 0.1),
                   infl_par = c(1, 1), infl_exp = c(0.7, 0.7), 
                   const_window = TRUE,
                   model_order = list(ar = c(1, 1), ma = c(0, 0)))
summary(opt2)
class(opt2)

## -----------------------------------------------------------------------------
opt3 = set.options(var_model = "sarma_sep", model_order = "bic",
                   order_max = list(ar = c(0, 1), ma = c(2, 2)))
summary(opt3)

## -----------------------------------------------------------------------------
# surface.dcs(y.norm1)

y_iid = y.norm1 + matrix(rnorm(101^2), nrow = 101, ncol = 101)
# surface.dcs(y_iid)

## -----------------------------------------------------------------------------
opt_iid_KR = set.options(type = "KR")
dcs_iid_KR = dcs(y_iid, opt_iid_KR)

# print results
dcs_iid_KR

# print options used for DCS procedure
dcs_iid_KR$dcs_options

# plot regression surface
# surface.dcs(dcs_iid_KR, plot_choice = 2)

## -----------------------------------------------------------------------------
summary(dcs_iid_KR)

## -----------------------------------------------------------------------------
dcs_LP_iid = dcs(y_iid)
dcs_LP_iid

summary(dcs_LP_iid)

# plot regression surface
# surface.dcs(dcs_LP_iid, plot_choice = 2)

## -----------------------------------------------------------------------------
ar_mat = matrix(c(1, -0.3, 0.4, 0.12), nrow = 2, ncol = 2)
ma_mat = matrix(c(1, -0.2, -0.5, 0.1), nrow = 2, ncol = 2)
sigma  = sqrt(0.25)
model_list = list(ar = ar_mat, ma = ma_mat, sigma = sigma)
sim_sarma = sarma.sim(n_x = 101, n_t = 101, model = model_list)

# SARMA observations
y_sarma = y.norm1 + sim_sarma$Y
# surface.dcs(y_sarma)

## -----------------------------------------------------------------------------
est_sarma = sarma.est(sim_sarma$Y, method = "HR",
                      model_order = list(ar = c(1, 1), ma = c(1, 1)))

summary(est_sarma)

## -----------------------------------------------------------------------------
# SARMA((1, 1), (1, 1))
opt_sarma_1 = set.options(var_model = "sarma_sep")
dcs_sarma_1 = dcs(y_sarma, opt_sarma_1)

summary(dcs_sarma_1$var_est)

# SARMA((1, 1), (0, 0))
# Use "sarma_HR" as it includes fast Yule-Walker estimation
opt_sarma_2 = set.options(var_model = "sarma_HR",
                          model_order = list(ar = c(1, 1), ma = c(0, 0)))
dcs_sarma_2 = dcs(y_sarma, opt_sarma_2)

summary(dcs_sarma_2$var_est)

## -----------------------------------------------------------------------------
# BIC
opt_sarma_3 = set.options(var_model = "sarma_HR", model_order = "bic",
                          order_max = list(ar = c(2, 2), ma = c(2, 2)))
dcs_sarma_3 = dcs(y_sarma, opt_sarma_3)

summary(dcs_sarma_3$var_est)

# gpac
opt_sarma_4 = set.options(var_model = "sarma_HR", model_order = "gpac",
                          order_max = list(ar = c(2, 2), ma = c(2, 2)))
dcs_sarma_4 = dcs(y_sarma, opt_sarma_4)

summary(dcs_sarma_4$var_est)

## -----------------------------------------------------------------------------
ar_mat = matrix(c(1, -0.3, 0.4, 0.12), nrow = 2, ncol = 2)
ma_mat = matrix(c(1, -0.2, -0.5, 0.1), nrow = 2, ncol = 2)
d = c(0.3, 0.1)
sigma  = sqrt(0.25)

model_list = list(ar = ar_mat, ma = ma_mat, d = d, sigma = sigma)
sim_sfarima = sfarima.sim(n_x = 101, n_t = 101, model = model_list)

# SFARIMA surface observations
y_sfarima = y.norm1 + sim_sfarima$Y

# surface.dcs(y_sfarima)

opt_sfarima = set.options(var_model = "sfarima_RSS")
dcs_sfarima = dcs(y_sfarima, opt_sfarima)

summary(dcs_sfarima$var_est)

## -----------------------------------------------------------------------------
opt_drv_1 = set.options(drv = c(1, 0), kerns = c("MW_321", "MW_220"))
opt_drv_1$IPI_options$trim = c(0.1, 0.1)
dcs_drv_1 = dcs(y_iid, opt_drv_1)

dcs_drv_1
# surface.dcs(dcs_drv_1, trim = c(0.1, 0.1), plot_choice = 2)

## -----------------------------------------------------------------------------
opt_drv_2 = set.options(drv = c(0, 2), kerns = c("MW_220", "MW_422"))
opt_drv_2$IPI_options$trim = c(0.1, 0.1)

dcs_drv_2 = dcs(y_iid, opt_drv_2)

dcs_drv_2

# surface.dcs(dcs_drv_2, trim = c(0.1, 0.1), plot_choice = 2)

