## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)
library(loewesadditivity)

## ----eval = FALSE-------------------------------------------------------------
#  if(!require(loewesadditivity)){
#    library(devtools)
#    devtools::install_github("shannong19/loewesadditivity")
#  }
#  library(loewesadditivity)

## ----echo = F, message = F, warning = F---------------------------------------
# hidden load currently
#library(devtools)
devtools::load_all()

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

results <- rh5_ama1ron2 %>% 
  rename(dose_A = "RH5", dose_B = "AMA1RON2")  %>% 
  fortify_gia_data() %>%
  estimate_params() 

results$params_est



## ----data, message = FALSE, warning = FALSE-----------------------------------
data(rh5_ama1ron2)

rh5_ama1ron2 %>% head() %>% knitr::kable() %>%
  kableExtra::kable_styling(full_width = T, font_size = 7)


## ----data_f, message = FALSE, warning = FALSE---------------------------------

rh5_ama1ron2_f <-
  rh5_ama1ron2 %>% rename(dose_A = 'RH5', dose_B = 'AMA1RON2') %>%
  fortify_gia_data()
  
  rh5_ama1ron2_f %>% head() %>%  kable(align = "c") %>%
  kableExtra::kable_styling(full_width = F)

## ----est, message = FALSE, warning = FALSE------------------------------------
## Set up initial guesses and parameters
model_params <- c(
  "beta_A" = .5,
  "beta_B" = .5,
  "gamma_A" = .5,
  "gamma_B" = .5,
  "tau_1" = 0,
  "tau_2" = 0
  )
  n_boot <- 10
  fn_list <- NULL
  alpha <- .05
  verbose <- TRUE
  
  
out <- estimate_params(
    data = rh5_ama1ron2_f,
    init_params = model_params,
    n_boot = n_boot,
    alpha = alpha,
    verbose = verbose
  )

## ----S, message = FALSE, warning = FALSE--------------------------------------
out$S_est  %>%  kable(align = "c", digits = 3) %>%
  kableExtra::kable_styling(full_width = F)

## ----par_est, message = FALSE, warning = FALSE--------------------------------
out$params_est %>%  kable(align = "c", digits = 3) %>%
  kableExtra::kable_styling(full_width = F)

## ----GIA_est, message = FALSE, warning = FALSE--------------------------------
out$GIA_est %>% head()  %>%  kable(align = "c", digits = 3) %>%
  kableExtra::kable_styling(full_width = F)

## ---- message = FALSE, warning = FALSE, fig.width=8, fig.height = 6-----------
# Note xlab is dose_A and ylab is dose_B
g1 <- plot_surface(out, xlab = "RH5", ylab = "AMA1RON2")

## ---- message = FALSE, warning = FALSE, fig.width=8, fig.height = 12----------
g2 <- plot_curves(out, dose_A = "RH5", dose_B = "AMA1RON2" )

## ---- message = FALSE, warning = FALSE, fig.width=8, fig.height = 6-----------
g3 <- plot_isobologram(out, dose_A = "RH5", dose_B = "AMA1RON2" )

## ----coverage, message = FALSE, warning = FALSE-------------------------------
  model_params <- c("beta_A" = .250, "beta_B" = .146,
                    "gamma_A" = .527, "gamma_B" = .921,
                    "tau_1" = -.058, "tau_2" = -.289)
  experimental_grid <- make_grid(par = model_params, 
                                 n = 5)
  n_boot <- 3
  n_sims <- 5
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- TRUE
  out <- simulate_coverage(n_sims = n_sims,
                         n_boot = n_boot,
                         verbose = FALSE,
                         experimental_grid = experimental_grid,
                         model_par = model_params,
                         alpha = .05,
                         noise_par = c("a0" = 3, "a1" = .01),
                         GIA_fn = base_GIA,
                         fn_list = NULL)
  
  out


## -----------------------------------------------------------------------------
 out <- design_experiment(n_rep = 2)

