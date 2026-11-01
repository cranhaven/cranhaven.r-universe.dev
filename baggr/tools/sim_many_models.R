#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(baggr)
  library(tidyverse)
})

set.seed(20260211)

K_DEFAULT <- 14L

make_covariates <- function(K, covariate_set, covariate_level, n_per_study = 40L) {
  if (covariate_level == "study") {
    xk <- rnorm(K, 0, 1)
    zk <- rbinom(K, 1, 0.5)
    return(list(study = tibble(study = seq_len(K), x = xk, z = zk), within = NULL))
  }

  # within-study variation: generate individual covariates and keep study means too
  within <- map_dfr(seq_len(K), function(k) {
    n_k <- sample(seq(n_per_study - 10L, n_per_study + 10L), 1)
    tibble(
      study = k,
      id = seq_len(n_k),
      x = rnorm(n_k, 0, 1),
      z = rbinom(n_k, 1, 0.5)
    )
  })

  study <- within %>%
    group_by(study) %>%
    summarise(x = mean(x), z = mean(z), .groups = "drop")

  list(study = study, within = within)
}

covariate_names <- function(covariate_set) {
  if (covariate_set == "cont") "x" else c("x", "z")
}

sim_summary_rubin <- function(K, covariate_set, covariate_level) {
  covs <- make_covariates(K, covariate_set, covariate_level)
  b1 <- 0.9
  b2 <- if (covariate_set == "cont_bin") -0.6 else 0
  tau_true <- 0.25 + b1 * covs$study$x + b2 * covs$study$z + rnorm(K, 0, 0.2)
  se <- runif(K, 0.08, 0.15)

  tibble(
    group = paste0("S", seq_len(K)),
    tau = tau_true + rnorm(K, 0, se),
    se = se,
    x = covs$study$x,
    z = covs$study$z
  )
}

sim_summary_mutau <- function(K, covariate_set, covariate_level) {
  covs <- make_covariates(K, covariate_set, covariate_level)
  b1 <- 0.8
  b2 <- if (covariate_set == "cont_bin") -0.5 else 0
  tau_true <- 0.2 + b1 * covs$study$x + b2 * covs$study$z + rnorm(K, 0, 0.2)
  mu_true <- -0.3 + 0.4 * covs$study$z + rnorm(K, 0, 0.2)

  tibble(
    group = paste0("SM", seq_len(K)),
    mu = mu_true + rnorm(K, 0, 0.12),
    se.mu = runif(K, 0.08, 0.15),
    tau = tau_true + rnorm(K, 0, 0.12),
    se.tau = runif(K, 0.08, 0.15),
    x = covs$study$x,
    z = covs$study$z
  )
}

sim_full_rubin <- function(K, covariate_set, covariate_level) {
  covs <- make_covariates(K, covariate_set, covariate_level, n_per_study = 30L)

  if (covariate_level == "study") {
    map_dfr(seq_len(K), function(k) {
      n_k <- sample(45:65, 1)
      trt <- rbinom(n_k, 1, 0.5)
      bsl <- rnorm(1, 0, 0.6)
      tau_k <- 0.3 + 0.9 * covs$study$x[k] + if (covariate_set == "cont_bin") -0.6 * covs$study$z[k] else 0
      y <- bsl + trt * tau_k + rnorm(n_k, 0, 1)
      tibble(group = paste0("FR", k), outcome = y, treatment = trt,
             x = covs$study$x[k], z = covs$study$z[k])
    })
  } else {
    covs$within %>%
      mutate(
        group = paste0("FR", study),
        treatment = rbinom(n(), 1, 0.5),
        bsl = rnorm(K, 0, 0.6)[study],
        tau_row = 0.3 + 0.8 * x + if (covariate_set == "cont_bin") -0.5 * z else 0,
        outcome = bsl + treatment * tau_row + rnorm(n(), 0, 1)
      ) %>%
      select(group, outcome, treatment, x, z)
  }
}

sim_full_mutau <- function(K, covariate_set, covariate_level) {
  covs <- make_covariates(K, covariate_set, covariate_level, n_per_study = 30L)

  if (covariate_level == "study") {
    map_dfr(seq_len(K), function(k) {
      n_k <- sample(45:65, 1)
      trt <- rbinom(n_k, 1, 0.5)
      mu_k <- -0.4 + 0.4 * covs$study$z[k] + rnorm(1, 0, 0.25)
      tau_k <- 0.25 + 0.8 * covs$study$x[k] + if (covariate_set == "cont_bin") -0.5 * covs$study$z[k] else 0
      y <- mu_k + trt * tau_k + rnorm(n_k, 0, 1)
      tibble(group = paste0("FM", k), outcome = y, treatment = trt,
             x = covs$study$x[k], z = covs$study$z[k])
    })
  } else {
    covs$within %>%
      mutate(
        group = paste0("FM", study),
        treatment = rbinom(n(), 1, 0.5),
        mu_k = (-0.4 + 0.4 * as.numeric(runif(K)[study] > 0.5) + rnorm(K, 0, 0.25))[study],
        tau_row = 0.25 + 0.75 * x + if (covariate_set == "cont_bin") -0.45 * z else 0,
        outcome = mu_k + treatment * tau_row + rnorm(n(), 0, 1)
      ) %>%
      select(group, outcome, treatment, x, z)
  }
}

sim_full_logit <- function(K, covariate_set, covariate_level) {
  covs <- make_covariates(K, covariate_set, covariate_level, n_per_study = 35L)

  if (covariate_level == "study") {
    map_dfr(seq_len(K), function(k) {
      n_k <- sample(45:65, 1)
      trt <- rbinom(n_k, 1, 0.5)
      alpha_k <- -1.1 + rnorm(1, 0, 0.2)
      log_or <- 0.5 + 0.8 * covs$study$x[k] + if (covariate_set == "cont_bin") -0.6 * covs$study$z[k] else 0
      p <- plogis(alpha_k + trt * log_or)
      y <- rbinom(n_k, 1, p)
      tibble(group = paste0("LG", k), treatment = trt, outcome = y,
             x = covs$study$x[k], z = covs$study$z[k])
    })
  } else {
    covs$within %>%
      mutate(
        group = paste0("LG", study),
        treatment = rbinom(n(), 1, 0.5),
        alpha = (-1.1 + rnorm(K, 0, 0.2))[study],
        log_or_row = 0.5 + 0.75 * x + if (covariate_set == "cont_bin") -0.55 * z else 0,
        p = plogis(alpha + treatment * log_or_row),
        outcome = rbinom(n(), 1, p)
      ) %>%
      select(group, treatment, outcome, x, z)
  }
}

make_data <- function(model, covariate_set, covariate_level, K = K_DEFAULT) {
  switch(
    model,
    rubin = sim_summary_rubin(K, covariate_set, covariate_level),
    mutau = sim_summary_mutau(K, covariate_set, covariate_level),
    rubin_full = sim_full_rubin(K, covariate_set, covariate_level),
    mutau_full = sim_full_mutau(K, covariate_set, covariate_level),
    logit = sim_full_logit(K, covariate_set, covariate_level),
    stop("Unknown model: ", model)
  )
}

fit_one <- function(model, pooling, covariate_set, covariate_level, pooling_baseline = NA_character_) {
  dat <- make_data(model, covariate_set, covariate_level)
  covs <- covariate_names(covariate_set)

  args <- list(
    data = dat,
    model = model,
    pooling = pooling,
    covariates = covs,
    iter = 300,
    chains = 1,
    refresh = 0
  )

  if (model == "logit") {
    args$show_messages <- FALSE
  }

  if (model %in% c("rubin_full", "mutau_full") && !is.na(pooling_baseline)) {
    args$pooling_control <- pooling_baseline
  }

  runtime <- system.time({
    fit_obj <- tryCatch(
      suppressWarnings(do.call(baggr, args)),
      error = function(e) e
    )
  })["elapsed"]

  is_ok <- inherits(fit_obj, "baggr")
  list(
    fit = if (is_ok) fit_obj else NULL,
    status = if (is_ok) "ok" else "error",
    error = if (is_ok) NA_character_ else conditionMessage(fit_obj),
    runtime_sec = as.numeric(runtime),
    n_rows = nrow(dat),
    n_groups = dplyr::n_distinct(dat$group)
  )
}

grid_nonfull <- tidyr::crossing(
  model = c("rubin", "mutau", "logit"),
  pooling = c("partial", "full"),
  covariate_set = c("cont", "cont_bin"),
  covariate_level = c("study", "within")
) %>%
  mutate(pooling_baseline = NA_character_)

grid_full <- tidyr::crossing(
  model = c("rubin_full", "mutau_full"),
  pooling = c("partial", "full"),
  covariate_set = c("cont", "cont_bin"),
  covariate_level = c("study", "within"),
  pooling_baseline = c("remove", "none")
)

fit_grid <- bind_rows(grid_nonfull, grid_full) %>%
  mutate(
    scenario_id = row_number(),
    label = paste(
      model,
      paste0("pooling=", pooling),
      paste0("covs=", covariate_set),
      paste0("cov_level=", covariate_level),
      ifelse(is.na(pooling_baseline), "", paste0("pooling_baseline=", pooling_baseline)),
      sep = " | "
    )
  )

results <- fit_grid %>%
  mutate(
    fit_result = pmap(
      list(model, pooling, covariate_set, covariate_level, pooling_baseline),
      fit_one
    ),
    fit = map(fit_result, "fit"),
    status = map_chr(fit_result, "status"),
    error = map_chr(fit_result, "error"),
    runtime_sec = map_dbl(fit_result, "runtime_sec"),
    n_rows = map_int(fit_result, "n_rows"),
    n_groups = map_int(fit_result, "n_groups")
  ) %>%
  select(-fit_result)

print(results %>% count(model, status))
print(summary(results$runtime_sec))

saveRDS(results, file = "simulation_fit_results.rds")
