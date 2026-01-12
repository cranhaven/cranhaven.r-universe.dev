## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load---------------------------------------------------------------------
library(GPCERF)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(134)
# Generate dataset with a normally distributed exposure given covariates
data_sim_normal <- generate_synthetic_data(sample_size = 400,
                                           outcome_sd = 10,
                                           gps_spec = 1)

# Generate dataset with a t-distributed with 2df exposure given covariates
data_sim_t <- generate_synthetic_data(sample_size = 400,
                                      outcome_sd = 10,
                                      gps_spec = 2)

tru_R <- function(w, sim_data) {
  design_mt <- model.matrix(~cf1 + cf2 + cf3 + cf4 + cf5 + cf6 - 1,
                            data = sim_data)
  mean(apply(design_mt, 1, function(x) {
    -10 - sum(c(2, 2, 3, -1, 2, 2) * x) -
      w * (0.1 - 0.1 * x[1] + 0.1 * x[4] + 0.1 * x[5] + 0.1 * x[3] ^ 2) +
      0.13 ^ 2 * w ^ 3
  }))
}

plot_fun <- function(object, ...) {
    # extract data
  tmp_data <- data.frame(w_vals = object$posterior$w,
                         mean_vals = object$posterior$mean,
                         sd_vals = object$posterior$sd)

  g1 <- ggplot2::ggplot(tmp_data) +
       ggplot2::geom_ribbon(ggplot2::aes(.data$w_vals,
                                y = .data$mean_vals,
                                ymin = .data$mean_vals - 1.96 * .data$sd_vals,
                                ymax = .data$mean_vals + 1.96 * .data$sd_vals),
                                fill = "blue", alpha = 0.25) +
        ggplot2::geom_line(ggplot2::aes(.data$w_vals, .data$mean_vals),
                           color = "blue", size = 1) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle("Estimated CERF (gp) with credible band (1.96sd)") +
        ggplot2::xlab("Exposure level") +
        ggplot2::ylab("Population average counterfactual outcome")

  return(g1)
}

erf_tru_normal <- sapply(seq(0, 20, 0.1), function(w) tru_R(w, data_sim_normal))
erf_tru_t <- sapply(seq(0, 20, 0.1), function(w) tru_R(w, data_sim_t))

## -----------------------------------------------------------------------------
gps_m_normal <- estimate_gps(cov_mt = data_sim_normal[, -(1:2)],
                             w_all = data_sim_normal$treat,
                             sl_lib = c("SL.xgboost"),
                             dnorm_log = FALSE)

gps_m_t <- estimate_gps(cov_mt = data_sim_t[, -(1:2)],
                        w_all = data_sim_t$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

## -----------------------------------------------------------------------------
w_all <- seq(0, 20, 0.1)

gp_res_normal <- estimate_cerf_gp(data_sim_normal,
                                  w_all,
                                  gps_m_normal,
                                  params = list(alpha = c(0.1),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1)
plot_fun(gp_res_normal) +
  geom_line(data = data.frame(w = w_all, y = erf_tru_normal),
            aes(x = w, y = y, color = "True"), size = 1.5)

## -----------------------------------------------------------------------------
gp_res_t <- estimate_cerf_gp(data_sim_t,
                             w_all,
                             gps_m_t,
                             params = list(alpha = c(0.1),
                                           beta = 0.2,
                                           g_sigma = 1,
                                           tune_app = "all"),
                             outcome_col = "Y",
                             treatment_col = "treat",
                             covariates_col = paste0("cf", seq(1,6)),
                             nthread = 1)
plot_fun(gp_res_t) +
  geom_line(data = data.frame(w = w_all, y = erf_tru_t),
            aes(x = w, y = y, color = "True"), size = 1.5)

