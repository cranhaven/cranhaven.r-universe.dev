## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = FALSE,  # Stan/brms chunks are too slow for automated builds
  fig.width = 7,
  fig.height = 4.5
)

## ----libraries----------------------------------------------------------------
# library(ErrorTracer)
# library(glmnet)      # for elastic net (Suggests)
# library(ggplot2)
# 
# # Load the bundled simulated dataset
# data(et_sim)

## ----demo-settings------------------------------------------------------------
# # Use small MCMC settings for this tutorial.
# # In a real analysis: CHAINS = 4, ITER = 2000 (or more).
# CHAINS <- 2L
# ITER   <- 1000L
# SEED   <- 42L

## ----data-overview------------------------------------------------------------
# str(et_sim, max.level = 2)
# # List of 7
# #  $ train         :'data.frame': 40 obs. of 6 variables:
# #  $ forecast      :'data.frame': 10 obs. of 5 variables:
# #  $ validation    :'data.frame': 10 obs. of 3 variables:
# #  $ true_params   :List of 2
# #  $ env_noise     :List of 3
# #  $ standardization:List of 3
# #  $ description   : chr ...

## ----head-train---------------------------------------------------------------
# head(et_sim$train)
# #   year cluster_id      Tmean        PPT        SWE     z_diff
# # 1 1995          A -0.6775963  0.9119435 -0.1386249 -0.4917161
# # 2 1996          A  0.4866542 -1.7284276 -1.4391959  0.4088745
# # 3 1997          A -0.4935765 -0.3945083 -0.7267905  0.2352018
# # 4 1998          A -0.2397691 -0.9805886 -0.8206722  0.3194194
# # 5 1999          A -0.5193449  1.1304845  1.6453631 -0.2554886
# # 6 2000          A -1.5333598  0.6437605  0.6252043 -0.5864645

## ----true-params--------------------------------------------------------------
# # True coefficients — known because the data are simulated
# et_sim$true_params
# # $A
# #   intercept     Tmean       PPT       SWE     sigma
# #        0.00      0.50     -0.30      0.20      0.40
# #
# # $B
# #   intercept     Tmean       PPT       SWE     sigma
# #        0.10      0.30     -0.20     -0.10      0.50

## ----forecast-predictors------------------------------------------------------
# # Forecast predictors are slightly above training range in Tmean (warming trend)
# et_sim$forecast[et_sim$forecast$cluster_id == "A", ]
# #   year cluster_id     Tmean       PPT        SWE
# # 1 2015          A 0.6918714  0.291...  0.102...
# # 2 2016          A 0.7012... ...
# # ...
# # Forecast Tmean is ~0.7-1.4 SDs above training mean — mild extrapolation.

## ----plot-training-data-------------------------------------------------------
# ggplot(et_sim$train, aes(x = year, y = z_diff, colour = cluster_id)) +
#   geom_line() +
#   geom_point(size = 2) +
#   labs(
#     title   = "Simulated allele-frequency change (training period)",
#     x       = "Year",
#     y       = expression(italic(z)[diff]),
#     colour  = "Cluster"
#   ) +
#   et_theme()

## ----plot-predictors----------------------------------------------------------
# # Climate predictors over the full period — warming trend visible in Tmean
# all_years <- rbind(
#   transform(et_sim$train[et_sim$train$cluster_id == "A",
#                           c("year", "Tmean", "PPT", "SWE")],
#             period = "Training"),
#   transform(et_sim$forecast[et_sim$forecast$cluster_id == "A",
#                              c("year", "Tmean", "PPT", "SWE")],
#             period = "Forecast")
# )
# 
# all_long <- reshape(all_years, direction = "long",
#                     varying = c("Tmean", "PPT", "SWE"),
#                     v.names = "value", timevar = "predictor",
#                     times   = c("Tmean", "PPT", "SWE"))
# 
# ggplot(all_long, aes(x = year, y = value,
#                      colour = period, group = period)) +
#   geom_line() +
#   geom_point(size = 1.8) +
#   facet_wrap(~ predictor, ncol = 3) +
#   labs(
#     title  = "Climate predictors: training and forecast periods",
#     x      = "Year", y = "Standardised value", colour = "Period"
#   ) +
#   et_theme()

## ----enet-fit-----------------------------------------------------------------
# train_A <- et_sim$train[et_sim$train$cluster_id == "A", ]
# 
# x_train <- as.matrix(train_A[, c("Tmean", "PPT", "SWE")])
# y_train <- train_A$z_diff
# 
# set.seed(42)
# cv_fit_A <- glmnet::cv.glmnet(
#   x     = x_train,
#   y     = y_train,
#   alpha = 0.5          # 0 = ridge, 1 = lasso, 0.5 = elastic net
# )
# 
# # Coefficient estimates at lambda.min
# coef(cv_fit_A, s = "lambda.min")
# # 4 x 1 sparse Matrix of class "dgCMatrix"
# #             lambda.min
# # (Intercept)  0.0430
# # Tmean        0.6658
# # PPT         -0.4591
# # SWE          0.3775

## ----enet-plot----------------------------------------------------------------
# plot(cv_fit_A)
# title("Cross-validated elastic net — Cluster A", line = 2.5)

## ----extract-priors-----------------------------------------------------------
# prior_spec_A <- extract_priors(
#   model      = cv_fit_A,
#   lambda     = "lambda.min",
#   multiplier = 2.0,   # prior SD = 2 × |elastic-net coef|
#   min_sd     = 0.10   # floor: avoids spike priors on near-zero coefficients
# )
# 
# print(prior_spec_A)
# # ErrorTracer prior specification
# #   Method      : glmnet
# #   Predictors  : 3
# #   Multiplier  : 2
# #   Min SD      : 0.1
# #   Coefficients:
# #     Tmean                 mean =   0.6658  sd =   1.3316
# #     PPT                   mean =  -0.4591  sd =   0.9182
# #     SWE                   mean =   0.3775  sd =   0.7550

## ----et-fit-------------------------------------------------------------------
# fit_A <- et_fit(
#   formula = z_diff ~ Tmean + PPT + SWE,
#   data    = train_A,
#   priors  = prior_spec_A,
#   chains  = CHAINS,
#   iter    = ITER,
#   warmup  = ITER %/% 2L,
#   cores   = CHAINS,
#   seed    = SEED
# )
# 
# print(fit_A)
# # ErrorTracer model (et_model)
# #   Formula : z_diff ~ Tmean + PPT + SWE
# #   n obs   : 20
# #   Chains  : 2  Iter: 1000  Warmup: 500
# #   Priors  : informed (glmnet, 3 predictors)
# #   Rhat max: 1.004

## ----et-summary---------------------------------------------------------------
# summary(fit_A)
# # === ErrorTracer model summary ===
# #
# # --- Fixed effects ---
# #              Estimate  Est.Error     Q2.5    Q97.5
# # Intercept    0.041      0.104    -0.162    0.246
# # Tmean        0.666      0.113     0.438    0.888
# # PPT         -0.487      0.200    -0.909   -0.123
# # SWE          0.406      0.206     0.023    0.827

## ----et-diagnose--------------------------------------------------------------
# diag_A <- et_diagnose(fit_A, loo = TRUE)
# 
# # Convergence
# diag_A$convergence
# # $rhat_max
# # [1] 1.004         # all chains well below 1.05 threshold
# #
# # $rhat_all_ok
# # [1] TRUE
# #
# # $neff_min
# # [1] 0.275         # lowest effective-sample-size ratio
# #
# # $neff_all_ok
# # [1] TRUE          # all Neff ratios > 0.10
# #
# # $n_divergences
# # [1] 0             # no divergent transitions

## ----loo-results--------------------------------------------------------------
# diag_A$loo[c("elpd_loo", "p_loo", "looic", "n_bad_pareto_k")]
# # $elpd_loo
# # [1] -14.55
# #
# # $p_loo
# # [1] 4.00          # effective parameters ≈ 4 (intercept, 3 betas); sigma
# #                   # is implicit — consistent with a 4-parameter model
# # $looic
# # [1] 29.10
# #
# # $n_bad_pareto_k
# # [1] 0             # no influential observations (Pareto k > 0.7)

## ----et-predict---------------------------------------------------------------
# fcast_A <- et_sim$forecast[et_sim$forecast$cluster_id == "A", ]
# 
# pred_A <- et_predict(
#   model     = fit_A,
#   newdata   = fcast_A,
#   env_noise = et_sim$env_noise,   # list(Tmean = 0.30, PPT = 0.20, SWE = 0.15)
#   n_draws   = 1000L,
#   ci_levels = c(0.50, 0.80, 0.90, 0.95),
#   n_perturb = 300L                # draws for the perturbation step
# )
# 
# print(pred_A)
# # ErrorTracer prediction (et_prediction)
# #   Observations : 5
# #   Draws        : 1000
# #   CI levels    : 0.5, 0.8, 0.9, 0.95
# #   Mean var decomposition (across observations):
# #     Parameter    : 0.0441
# #     Environmental: 0.0579
# #     Residual     : 0.2173
# #     Total        : 0.2673

## ----ci-table-----------------------------------------------------------------
# # 90% credible intervals by forecast year
# ci90 <- pred_A$credible_intervals[pred_A$credible_intervals$ci_level == 0.90, ]
# ci90$year <- fcast_A$year
# ci90[, c("year", "lower", "median", "upper", "width")]
# #   year    lower  median   upper  width
# # 1 2015   -0.507   0.381   1.213  1.720
# # 2 2016    0.009   0.794   1.660  1.651
# # 3 2017   -0.009   0.875   1.631  1.640
# # 4 2018   -0.179   0.649   1.460  1.639
# # 5 2019   -0.767   0.131   1.002  1.769

## ----decompose----------------------------------------------------------------
# decomp_A <- decompose_uncertainty(pred_A)
# decomp_A
# #   obs_id total_var param_var  env_var residual_var
# # 1      1    0.2627    0.0202   0.0541       0.2173
# # 2      2    0.2667    0.0361   0.0588       0.2173
# # 3      3    0.2573    0.0364   0.0541       0.2173
# # 4      4    0.2529    0.0427   0.0653       0.2173
# # 5      5    0.2972    0.0849   0.0574       0.2173

## ----plot-decomp--------------------------------------------------------------
# et_plot_decomposition(decomp_A, proportional = TRUE) +
#   labs(subtitle = "Cluster A, forecast 2015–2019")

## ----shelf-life---------------------------------------------------------------
# # Derive plausible range from training data (arcsin-sqrt scale — unbounded)
# pr_A <- range(train_A$z_diff)   # e.g. c(-1.50, 2.04) => width ~ 3.54
# 
# sl_A <- shelf_life(
#   predictions              = pred_A,
#   plausible_range          = pr_A,
#   ci_level                 = 0.90,
#   threshold                = 1.0,        # uninformative when CI width >= plausible range
#   time_col                 = "year",
#   min_slope_for_projection = 1e-4,       # positive slope required to project
#   max_extrapolation_factor = 10          # cap: project <= last_year + 10 * window_width
# )
# 
# print(sl_A)
# # ErrorTracer shelf life analysis
# #   Observations     : 5
# #   Plausible range  : 3.54
# #   Threshold        : 1
# #   Informative      : 5 / 5
# #   Mean CI/range    : 0.488
# #   Max CI/range     : 0.499
# #   Shelf life       : > 2019 (lower bound — all periods informative, no trend)
# #   Detail           : All 5 forecast periods informative. Linear trend
# #                      (slope = 0.00241 per time unit) projects threshold
# #                      crossing at ~2234.3, but this exceeds the extrapolation
# #                      cap (2059). Shelf life > 2019.

## ----shelf-life-table---------------------------------------------------------
# as.data.frame(sl_A)
# #   obs_id  time ci_width plausible_range  ratio informative
# # 1      1  2015    1.720            3.54  0.486        TRUE
# # 2      2  2016    1.651            3.54  0.466        TRUE
# # 3      3  2017    1.640            3.54  0.463        TRUE
# # 4      4  2018    1.639            3.54  0.463        TRUE
# # 5      5  2019    1.769            3.54  0.499        TRUE

## ----plot-shelf-life----------------------------------------------------------
# et_plot_shelf_life(sl_A, show_ratio = TRUE) +
#   labs(subtitle = "Cluster A — 90% CI width / training range (arcsin-sqrt scale)")

## ----calibrate----------------------------------------------------------------
# valid_A <- et_sim$validation[et_sim$validation$cluster_id == "A", ]
# 
# cal_A <- et_calibrate(
#   predictions  = pred_A,
#   observed     = valid_A,
#   response_col = "z_diff",
#   ci_levels    = c(0.50, 0.80, 0.90, 0.95)
# )
# 
# print(cal_A)
# #   ci_level nominal observed_coverage n_obs calibration_error
# # 1     0.50    0.50              0.60     5              0.10
# # 2     0.80    0.80              0.80     5              0.00
# # 3     0.90    0.90              1.00     5              0.10
# # 4     0.95    0.95              1.00     5              0.05

## ----plot-calibration---------------------------------------------------------
# et_plot_calibration(cal_A) +
#   labs(subtitle = "Cluster A — posterior predictive calibration")

## ----plot-forecast------------------------------------------------------------
# et_plot_forecast(
#   predictions  = pred_A,
#   observed     = valid_A,
#   response_col = "z_diff",
#   time_col     = "year"
# ) +
#   labs(subtitle = "Cluster A — shaded ribbons: 50%, 80%, 90%, 95% CI")

## ----plot-prior-posterior-----------------------------------------------------
# et_plot_prior_posterior(fit_A, max_preds = 3L) +
#   labs(subtitle = "Cluster A — how 20 observations update the elastic-net prior")

## ----plot-coefficients--------------------------------------------------------
# et_plot_coefficients(fit_A) +
#   labs(subtitle = "Cluster A — Bayesian 95% CI (blue) vs. enet coefficient (red ×)")

## ----parameter-recovery-------------------------------------------------------
# post    <- brms::fixef(fit_A$fit)
# true_A  <- et_sim$true_params$A
# 
# post_df <- data.frame(
#   parameter  = c("Intercept", "Tmean", "PPT", "SWE"),
#   true_value = c(true_A["intercept"], true_A["Tmean"],
#                  true_A["PPT"],       true_A["SWE"]),
#   post_mean  = post[, "Estimate"],
#   lower_95   = post[, "Q2.5"],
#   upper_95   = post[, "Q97.5"]
# )
# post_df$covered <- with(post_df,
#                         true_value >= lower_95 & true_value <= upper_95)
# 
# post_df[, c("parameter", "true_value", "post_mean", "lower_95", "upper_95", "covered")]
# #   parameter true_value post_mean lower_95 upper_95 covered
# # 1 Intercept       0.00     0.041   -0.162    0.246    TRUE
# # 2     Tmean       0.50     0.666    0.438    0.888    TRUE
# # 3       PPT      -0.30    -0.487   -0.909   -0.123    TRUE
# # 4       SWE       0.20     0.406    0.023    0.827    TRUE

## ----grouped-priors-----------------------------------------------------------
# # Build one et_prior_spec per cluster from individual elastic nets
# prior_list <- lapply(c("A", "B"), function(cl) {
#   df <- et_sim$train[et_sim$train$cluster_id == cl, ]
#   x  <- as.matrix(df[, c("Tmean", "PPT", "SWE")])
#   set.seed(42)
#   suppressWarnings(cv <- glmnet::cv.glmnet(x, df$z_diff, alpha = 0.5))
#   extract_priors(cv, multiplier = 2.0, min_sd = 0.10)
# })
# names(prior_list) <- c("A", "B")

## ----grouped-fit--------------------------------------------------------------
# # One brms model per cluster, returned as et_model_list
# fit_all <- et_fit(
#   formula  = z_diff ~ Tmean + PPT + SWE,
#   data     = et_sim$train,
#   priors   = prior_list,       # named list: one et_prior_spec per cluster
#   grouping = "cluster_id",
#   chains   = CHAINS,
#   iter     = ITER,
#   warmup   = ITER %/% 2L,
#   cores    = CHAINS,
#   seed     = SEED
# )
# 
# print(fit_all)
# # ErrorTracer grouped model list (et_model_list)
# #   Grouping : cluster_id
# #   Formula  : z_diff ~ Tmean + PPT + SWE
# #   Groups   : 2
# #   Fitted   : 2 / 2
# 
# summary(fit_all)
# # --- Per-group Rhat max ---
# #   A                     Rhat max = 1.004
# #   B                     Rhat max = 1.007

## ----grouped-diagnose---------------------------------------------------------
# diag_all <- et_diagnose(fit_all, loo = TRUE)
# diag_all$summary
# #   group rhat_ok neff_ok n_divergences elpd_loo n_bad_pareto_k
# # 1     A    TRUE    TRUE             0   -14.55              0
# # 2     B    TRUE    TRUE             0   -17.15              1

## ----grouped-predict----------------------------------------------------------
# pred_all <- et_predict(
#   model     = fit_all,
#   newdata   = et_sim$forecast,
#   env_noise = et_sim$env_noise,
#   n_draws   = 1000L,
#   ci_levels = c(0.50, 0.80, 0.90, 0.95),
#   n_perturb = 300L
# )
# 
# print(pred_all)
# # ErrorTracer grouped predictions (et_prediction_list)
# #   Grouping : cluster_id
# #   Groups   : 2 / 2 predicted

## ----grouped-decompose--------------------------------------------------------
# decomp_all <- decompose_uncertainty(pred_all)
# head(decomp_all)
# #   group obs_id total_var param_var  env_var residual_var
# # 1     A      1    0.2627    0.0202   0.0541       0.2173
# # 2     A      2    0.2667    0.0361   0.0588       0.2173
# # ...
# # 6     B      1    0.3374    0.0252   0.0122       0.2999
# # 7     B      2    0.3472    0.0488   0.0180       0.2999

## ----grouped-shelf-life-------------------------------------------------------
# # Use the pooled training range across both clusters (arcsin-sqrt scale)
# pr_all <- range(et_sim$train$z_diff)   # e.g. c(-1.67, 2.04) => width ~ 3.71
# 
# sl_all <- shelf_life(
#   pred_all,
#   plausible_range          = pr_all,
#   ci_level                 = 0.90,
#   threshold                = 1.0,
#   time_col                 = "year",
#   max_extrapolation_factor = 10
# )
# 
# print(sl_all)
# # ErrorTracer shelf life analysis
# #   Observations     : 10
# #   Plausible range  : 3.71
# #   Threshold        : 1
# #   [A]  100.0% informative  mean ratio = 0.465  shelf life > 2019 (lower bound)
# #   [B]  100.0% informative  mean ratio = 0.544  shelf life > 2019 (lower bound)

## ----plot-grouped-shelf-life--------------------------------------------------
# et_plot_shelf_life(sl_all, show_ratio = TRUE) +
#   labs(
#     subtitle = "Both clusters — 90% CI width / plausible range",
#     colour   = "Cluster"
#   )

## ----grouped-calibrate--------------------------------------------------------
# cal_all <- et_calibrate(
#   pred_all,
#   observed     = et_sim$validation,
#   response_col = "z_diff",
#   ci_levels    = c(0.50, 0.80, 0.90, 0.95)
# )
# 
# et_plot_calibration(cal_all) +
#   labs(subtitle = "Both clusters — posterior predictive calibration")

## ----grouped-coef-plot--------------------------------------------------------
# # Forest plot for all groups in a single faceted figure
# et_plot_coefficients(fit_all) +
#   labs(subtitle = "Bayesian 95% CI (blue) vs. enet coefficient (red ×)")

## ----gcm-workflow, eval = FALSE-----------------------------------------------
# # Suppose you have GCM ensemble-mean predictors for 2025-2075,
# # standardised using your training-period statistics.
# gcm_years <- 2025:2075
# 
# gcm_df <- data.frame(
#   year  = gcm_years,
#   Tmean = ...,    # standardised GCM ensemble-mean temperature
#   PPT   = ...,    # standardised GCM ensemble-mean precipitation
#   SWE   = ...     # standardised GCM ensemble-mean snowpack
# )
# 
# # GCM ensemble spread (SD across runs) grows roughly linearly with time.
# # Here we model spread as starting at the validation-period noise level
# # and increasing by ~0.01 SD per year for temperature.
# base_year   <- 2025
# tmean_noise <- 0.30 + 0.01 * (gcm_years - base_year)  # 0.30 → 0.80
# ppt_noise   <- 0.20 + 0.005 * (gcm_years - base_year) # 0.20 → 0.45
# swe_noise   <- 0.15 + 0.005 * (gcm_years - base_year) # 0.15 → 0.40
# 
# # Predict over the 50-year GCM window with time-varying noise
# pred_gcm <- et_predict(
#   model     = fit_A,
#   newdata   = gcm_df,
#   env_noise = list(
#     Tmean = tmean_noise,   # per-row vector, length = nrow(gcm_df)
#     PPT   = ppt_noise,
#     SWE   = swe_noise
#   ),
#   ci_levels = c(0.90, 0.95),
#   n_draws   = 1000L
# )
# 
# # Shelf life over the 50-year horizon
# sl_gcm <- shelf_life(
#   pred_gcm,
#   plausible_range          = range(train_A$z_diff),
#   time_col                 = "year",
#   max_extrapolation_factor = Inf    # allow projection beyond the 50-yr window
# )
# 
# print(sl_gcm)
# # If the 90% CI stays below the plausible range throughout 2025-2075:
# #   Shelf life : > 2075 (lower bound — all periods informative, no trend)
# # If it crosses (say, around 2058):
# #   Shelf life : ~2058 (observed — threshold first exceeded)

## ----gcm-decompose, eval = FALSE----------------------------------------------
# decomp_gcm <- decompose_uncertainty(pred_gcm)
# # env_var should be small in 2025 and growing toward 2075 as GCM spread
# # accumulates; param_var and residual_var remain roughly constant.
# plot(gcm_years, decomp_gcm$env_var,
#      type = "l", xlab = "Year", ylab = "Environmental variance",
#      main = "Growing GCM ensemble uncertainty propagated into forecast")

## ----lm-priors----------------------------------------------------------------
# lm_fit_A <- lm(z_diff ~ Tmean + PPT + SWE, data = train_A)
# 
# prior_lm_A <- extract_priors(
#   model      = lm_fit_A,
#   multiplier = 2.0,
#   min_sd     = 0.10
# )
# 
# print(prior_lm_A)
# # ErrorTracer prior specification
# #   Method      : lm
# #   Predictors  : 3
# #   Multiplier  : 2
# #   Min SD      : 0.1
# #   Coefficients:
# #     Tmean                 mean =   0.6669  sd =   1.3338
# #     PPT                   mean =  -0.4677  sd =   0.9354
# #     SWE                   mean =   0.3859  sd =   0.7719

## ----back-transform-----------------------------------------------------------
# # Standardisation constants used when generating et_sim
# et_sim$standardization
# # $Tmean
# #      mean        sd
# # 15.5422  0.6797
# #
# # $PPT
# #      mean        sd
# # 110.5460  14.6855
# #
# # $SWE
# #      mean        sd
# # 86.9190  15.1885
# 
# # A Tmean of +1 SD corresponds to:
# unstandardize(z = 1.0,
#               mu = et_sim$standardization$Tmean["mean"],
#               s  = et_sim$standardization$Tmean["sd"])
# # [1] 16.22   ← 1 SD above training mean = 16.2 °C

## ----session-info, eval = TRUE, echo = FALSE----------------------------------
sessionInfo()

