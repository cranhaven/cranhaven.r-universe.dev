## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library("devtools")
#  install_github("NSAPH-Software/GPCERF", ref = "main")

## -----------------------------------------------------------------------------
library("GPCERF")

## -----------------------------------------------------------------------------
mydata <- generate_synthetic_data()
gps_m <- estimate_gps(cov_mt = mydata[, c("cf1", "cf2", "cf3", "cf4",
                                         "cf5", "cf6")],
                  w_all = mydata$treat,
                  sl_lib = c("SL.xgboost"),
                  dnorm_log = FALSE)

## -----------------------------------------------------------------------------
set.seed(129)
sim_data <- generate_synthetic_data(sample_size = 400, gps_spec = 3)

# Estimate GPS function
gps_m <- estimate_gps(cov_mt = sim_data[, -(1:2)],
                      w_all = sim_data$treat,
                      sl_lib = c("SL.xgboost"),
                      dnorm_log = TRUE)

# exposure values
q1 <- stats::quantile(sim_data$treat, 0.05)
q2 <- stats::quantile(sim_data$treat, 0.95)

w_all <- seq(q1, q2, 1)

params_lst <- list(alpha = c(0.05),
                   beta = c(12),
                   g_sigma = c(0.1),
                   tune_app = "all")
cerf_gp_obj <- estimate_cerf_gp(sim_data,
                               w_all,
                               gps_m,
                               params = params_lst,
                               outcome_col = "Y",
                               treatment_col = "treat",
                               covariates_col = paste0("cf", seq(1,6)),
                               nthread = 1)
summary(cerf_gp_obj)
plot(cerf_gp_obj)

## -----------------------------------------------------------------------------
set.seed(19)
sim_data <- generate_synthetic_data(sample_size = 1000, gps_spec = 3)

# Estimate GPS function
gps_m <- estimate_gps(cov_mt = sim_data[, -(1:2)],
                      w_all = sim_data$treat,
                      sl_lib = c("SL.xgboost"),
                      dnorm_log = TRUE)
# exposure values
q1 <- stats::quantile(sim_data$treat, 0.05)
q2 <- stats::quantile(sim_data$treat, 0.95)

w_all <- seq(q1, q2, 1)

params_lst <- list(alpha = c(0.05),
                   beta = c(12),
                   g_sigma = c(0.1),
                   tune_app = "all",
                   n_neighbor = 20,
                   block_size = 1e4)
cerf_nngp_obj <- estimate_cerf_nngp(sim_data,
                                    w_all,
                                    gps_m,
                                    params = params_lst,
                                    outcome_col = "Y",
                                    treatment_col = "treat",
                                    covariates_col = paste0("cf", seq(1,6)),
                                    nthread = 1)

summary(cerf_nngp_obj)
plot(cerf_nngp_obj)

