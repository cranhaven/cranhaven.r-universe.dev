## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)
library(couplr)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(42)

# Treatment group: younger, more educated, higher prior earnings
treatment <- tibble(

  id = 1:200,
  age = rnorm(200, mean = 35, sd = 8),
  education = rnorm(200, mean = 14, sd = 2),
  prior_earnings = rnorm(200, mean = 40000, sd = 12000),
  employed = rbinom(200, 1, 0.7),
  group = "treatment"
)

# Control group: older, less educated, lower prior earnings (selection bias)
control <- tibble(
  id = 201:700,
  age = rnorm(500, mean = 45, sd = 12),
  education = rnorm(500, mean = 12, sd = 3),
  prior_earnings = rnorm(500, mean = 32000, sd = 15000),
  employed = rbinom(500, 1, 0.5),
  group = "control"
)

# Combine for packages that expect single data frame
combined <- bind_rows(treatment, control) |>
  mutate(treated = as.integer(group == "treatment"))

cat("Treatment units:", nrow(treatment), "\n")
cat("Control units:", nrow(control), "\n")

# Baseline imbalance
vars <- c("age", "education", "prior_earnings", "employed")
for (v in vars) {
  diff <- mean(treatment[[v]]) - mean(control[[v]])
  pooled_sd <- sqrt((var(treatment[[v]]) + var(control[[v]])) / 2)
  std_diff <- diff / pooled_sd
  cat(sprintf("%s: std diff = %.3f\n", v, std_diff))
}

## ----eval = requireNamespace("MatchIt", quietly = TRUE)-----------------------
if (requireNamespace("MatchIt", quietly = TRUE)) {
  library(MatchIt)

  # MatchIt: Propensity score matching (default)
  m_ps <- matchit(
    treated ~ age + education + prior_earnings + employed,
    data = combined,
    method = "nearest",
    distance = "glm"  # Propensity score via logistic regression
  )

  cat("MatchIt (propensity score, nearest neighbor):\n")
  cat("  Matched pairs:", sum(m_ps$weights[combined$treated == 1] > 0), "\n")

  # Extract matched data
  matched_ps <- match.data(m_ps)
}

## -----------------------------------------------------------------------------
# couplr: Direct covariate matching
result_couplr <- match_couples(
  left = treatment,
  right = control,
  vars = c("age", "education", "prior_earnings", "employed"),
  auto_scale = TRUE,
  scale = "robust"
)

cat("\ncouplr (direct covariate matching):\n")
cat("  Matched pairs:", result_couplr$info$n_matched, "\n")
cat("  Mean distance:", round(mean(result_couplr$pairs$distance), 4), "\n")

## ----eval = requireNamespace("MatchIt", quietly = TRUE), fig.width=9, fig.height=5, fig.alt="Side-by-side comparison of covariate balance achieved by MatchIt and couplr, showing standardized mean differences for age, education, prior_earnings, and employed"----
if (requireNamespace("MatchIt", quietly = TRUE)) {
  # MatchIt balance
  matched_treated_ps <- matched_ps |> filter(treated == 1)
  matched_control_ps <- matched_ps |> filter(treated == 0)

  matchit_balance <- tibble(
    variable = vars,
    std_diff = sapply(vars, function(v) {
      diff <- mean(matched_treated_ps[[v]]) - mean(matched_control_ps[[v]])
      pooled_sd <- sqrt((var(matched_treated_ps[[v]]) + var(matched_control_ps[[v]])) / 2)
      diff / pooled_sd
    }),
    method = "MatchIt"
  )

  # couplr balance
  couplr_balance <- balance_diagnostics(
    result_couplr, treatment, control, vars
  )

  couplr_balance_df <- couplr_balance$var_stats |>
    dplyr::select(variable, std_diff) |>
    mutate(method = "couplr")

  # Combine and plot
  balance_comparison <- bind_rows(matchit_balance, couplr_balance_df)

  ggplot(balance_comparison, aes(x = variable, y = abs(std_diff), fill = method)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "#93c54b") +
    geom_hline(yintercept = 0.25, linetype = "dashed", color = "#f47c3c") +
    labs(
      title = "Covariate Balance: MatchIt vs couplr",
      subtitle = "Green line = 0.1 (excellent), Orange line = 0.25 (acceptable)",
      x = "Variable",
      y = "|Standardized Difference|",
      fill = "Method"
    ) +
    scale_fill_manual(values = c("MatchIt" = "#29abe0", "couplr" = "#93c54b")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom"
    )
}

## ----eval = requireNamespace("optmatch", quietly = TRUE)----------------------
if (requireNamespace("optmatch", quietly = TRUE)) {
  library(optmatch)

  # Create distance matrix
  dist_mat <- match_on(
    treated ~ age + education + prior_earnings + employed,
    data = combined,
    method = "mahalanobis"
  )

  # Optimal pair matching
  m_opt <- pairmatch(dist_mat, data = combined)

  n_matched <- sum(!is.na(m_opt)) / 2
  cat("optmatch (optimal pair matching):\n")
  cat("  Matched pairs:", n_matched, "\n")
}

## -----------------------------------------------------------------------------
# couplr with Mahalanobis-like scaling
result_couplr_maha <- match_couples(
  left = treatment,
  right = control,
  vars = vars,
  auto_scale = TRUE,
  scale = "standardize"  # Similar to Mahalanobis diagonal
)

cat("\ncouplr (optimal pair matching):\n")
cat("  Matched pairs:", result_couplr_maha$info$n_matched, "\n")

## ----eval = requireNamespace("optmatch", quietly = TRUE)----------------------
if (requireNamespace("optmatch", quietly = TRUE)) {
  # Compare total distances
  # (Note: Direct comparison is complex due to different distance scaling)
  cat("\nBoth packages find globally optimal one-to-one assignments.\n")
  cat("Total distance differences arise from distance metric choices.\n")
}

## ----eval = requireNamespace("designmatch", quietly = TRUE)-------------------
# if (requireNamespace("designmatch", quietly = TRUE)) {
#   library(designmatch)
# 
#   # Prepare data for designmatch
#   t_ind <- combined$treated
# 
#   # Distance matrix (Mahalanobis)
#   X <- as.matrix(combined[, vars])
#   dist_mat_dm <- distmat(t_ind, X)
# 
#   # Balance constraints: mean differences
#   mom <- list(
#     covs = X,
#     tols = rep(0.1, ncol(X))  # Tolerance for standardized difference
#   )
# 
#   # Solve with balance constraints
#   tryCatch({
#     m_dm <- bmatch(
#       t_ind = t_ind,
#       dist_mat = dist_mat_dm,
#       mom = mom,
#       solver = list(name = "glpk", approximate = 1)
#     )
# 
#     cat("designmatch (balance-constrained matching):\n")
#     cat("  Matched pairs:", sum(m_dm$t_id > 0), "\n")
#   }, error = function(e) {
#     cat("designmatch: Balance constraints may be infeasible\n")
#     cat("  Try relaxing tolerances or reducing constraint count\n")
#   })
# }

## -----------------------------------------------------------------------------
# couplr: Optimize distance, then check balance
result_couplr_dm <- match_couples(
  left = treatment,
  right = control,
  vars = vars,
  auto_scale = TRUE
  # No caliper - let algorithm find optimal matches
)

balance_dm <- balance_diagnostics(result_couplr_dm, treatment, control, vars)

cat("\ncouplr (distance-minimizing):\n")
cat("  Matched pairs:", result_couplr_dm$info$n_matched, "\n")
cat("  Mean |std diff|:", round(balance_dm$overall$mean_abs_std_diff, 4), "\n")
cat("  Max |std diff|:", round(balance_dm$overall$max_abs_std_diff, 4), "\n")

## ----eval = requireNamespace("Matching", quietly = TRUE)----------------------
# if (requireNamespace("Matching", quietly = TRUE)) {
#   library(Matching)
# 
#   # Genetic matching (finds optimal weights)
#   X <- as.matrix(combined[, vars])
# 
#   set.seed(123)
#   m_gen <- Match(
#     Tr = combined$treated,
#     X = X,
#     M = 1,  # 1:1 matching
#     estimand = "ATT",
#     replace = FALSE
#   )
# 
#   cat("Matching package (multivariate matching):\n")
#   cat("  Matched pairs:", length(m_gen$index.treated), "\n")
# }

## ----eval = requireNamespace("Matching", quietly = TRUE)----------------------
# if (requireNamespace("Matching", quietly = TRUE)) {
#   # Check balance from Matching package
#   mb <- MatchBalance(
#     treated ~ age + education + prior_earnings + employed,
#     data = combined,
#     match.out = m_gen,
#     nboots = 0
#   )
# }

## ----eval = FALSE-------------------------------------------------------------
# # Stage 1: couplr for initial matching
# matched <- match_couples(
#   left = treatment_data,
#   right = control_data,
#   vars = covariates,
#   auto_scale = TRUE
# )
# 
# # Stage 2: Check balance
# balance <- balance_diagnostics(matched, treatment_data, control_data, covariates)
# 
# # Stage 3: If balance insufficient, consider alternatives
# if (balance$overall$max_abs_std_diff > 0.25) {
#   # Try full matching (variable-ratio groups)
#   result_full <- full_match(treatment_data, control_data, vars = covariates,
#                             auto_scale = TRUE)
# }
# 
# # Stage 4: Analysis on matched data
# matched_data <- join_matched(matched, treatment_data, control_data)
# model <- lm(outcome ~ treatment, data = matched_data)

## ----eval = FALSE-------------------------------------------------------------
# # Use couplr for: one-to-one matching, full matching, large-scale matching, distance caching
# # Use MatchIt for: propensity scores, CEM, genetic matching, published protocols
# # Use optmatch for: sparse distance matrices, RItools integration
# # Use designmatch for: guaranteed balance constraints

## ----lalonde-style------------------------------------------------------------
set.seed(1986)

# NSW treatment group (randomized) - smaller sample for CRAN
nsw_treat <- tibble(
  id = 1:100,
  age = pmax(17, rnorm(100, 25, 7)),
  education = pmax(0, pmin(16, rnorm(100, 10, 2))),
  black = rbinom(100, 1, 0.84),
  hispanic = rbinom(100, 1, 0.06),
  married = rbinom(100, 1, 0.19),
  nodegree = rbinom(100, 1, 0.71),
  re74 = pmax(0, rnorm(100, 2100, 5000)),
  re75 = pmax(0, rnorm(100, 1500, 3500)),
  group = "treatment"
)

# CPS comparison group (observational - very different!)
# Reduced from 15,815 to 500 for CRAN timing compliance
cps_control <- tibble(
  id = 101:600,
  age = pmax(17, rnorm(500, 33, 11)),
  education = pmax(0, pmin(16, rnorm(500, 12, 3))),
  black = rbinom(500, 1, 0.07),
  hispanic = rbinom(500, 1, 0.07),
  married = rbinom(500, 1, 0.71),
  nodegree = rbinom(500, 1, 0.30),
  re74 = pmax(0, rnorm(500, 14000, 9000)),
  re75 = pmax(0, rnorm(500, 13500, 9000)),
  group = "control"
)

cat("NSW treatment:", nrow(nsw_treat), "individuals\n")
cat("CPS control:", nrow(cps_control), "individuals\n")
cat("(Note: Real CPS has ~16,000 controls; reduced here for vignette timing)\n")

# Baseline imbalance is severe
vars_lalonde <- c("age", "education", "black", "hispanic", "married",
                  "nodegree", "re74", "re75")
cat("\nBaseline standardized differences:\n")
for (v in vars_lalonde) {
  t_mean <- mean(nsw_treat[[v]])
  c_mean <- mean(cps_control[[v]])
  pooled_sd <- sqrt((var(nsw_treat[[v]]) + var(cps_control[[v]])) / 2)
  std_diff <- (t_mean - c_mean) / pooled_sd
  cat(sprintf("  %s: %.2f\n", v, std_diff))
}

## ----lalonde-matching---------------------------------------------------------
# Greedy matching (fast for large control pools)
result_lalonde <- match_couples(
  left = nsw_treat,
  right = cps_control,
  vars = vars_lalonde,
  strategy = "pq",       # Priority queue - efficient for large pools
  auto_scale = TRUE,
  scale = "robust"
, method = "greedy")

cat("Matched", result_lalonde$info$n_matched, "of", nrow(nsw_treat), "treatment units\n")
cat("Mean distance:", round(mean(result_lalonde$pairs$distance), 4), "\n")

## ----lalonde-balance, fig.width=9, fig.height=5, fig.alt="Balance plot for Lalonde-style matching showing improvement in standardized differences"----
balance_lalonde <- balance_diagnostics(
  result_lalonde, nsw_treat, cps_control, vars_lalonde
)

# Before/after comparison
before_df <- tibble(
  variable = vars_lalonde,
  std_diff = sapply(vars_lalonde, function(v) {
    t_mean <- mean(nsw_treat[[v]])
    c_mean <- mean(cps_control[[v]])
    pooled_sd <- sqrt((var(nsw_treat[[v]]) + var(cps_control[[v]])) / 2)
    (t_mean - c_mean) / pooled_sd
  }),
  stage = "Before"
)

after_df <- balance_lalonde$var_stats |>
  dplyr::select(variable, std_diff) |>
  mutate(stage = "After")

balance_plot_df <- bind_rows(before_df, after_df) |>
  mutate(stage = factor(stage, levels = c("Before", "After")))

ggplot(balance_plot_df, aes(x = reorder(variable, abs(std_diff)),
                             y = std_diff, fill = stage)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "#93c54b") +
  geom_hline(yintercept = c(-0.25, 0.25), linetype = "dashed", color = "#f47c3c") +
  coord_flip() +
  labs(
    title = "Covariate Balance: Before vs After Matching",
    subtitle = "Lalonde-style job training evaluation",
    x = "",
    y = "Standardized Difference",
    fill = ""
  ) +
  scale_fill_manual(values = c("Before" = "#d9534f", "After" = "#93c54b")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

## ----lalonde-summary----------------------------------------------------------
cat("Balance summary:\n")
cat("  Mean |std diff| before:",
    round(mean(abs(before_df$std_diff)), 3), "\n")
cat("  Mean |std diff| after:",
    round(balance_lalonde$overall$mean_abs_std_diff, 3), "\n")
cat("  Max |std diff| after:",
    round(balance_lalonde$overall$max_abs_std_diff, 3), "\n")

if (balance_lalonde$overall$max_abs_std_diff < 0.25) {
  cat("\nYes All variables within acceptable balance threshold (0.25)\n")
} else {
  cat("\n⚠ Some variables exceed 0.25 threshold - consider calipers or blocking\n")
}

