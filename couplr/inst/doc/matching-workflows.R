## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(couplr)
library(dplyr)
library(ggplot2)

## ----basic-example------------------------------------------------------------
# Simulate observational data
set.seed(123)
n_left <- 100
n_right <- 150

# Treatment group (tends to be younger, higher income)
left_data <- tibble(
  id = 1:n_left,
  age = rnorm(n_left, mean = 45, sd = 10),
  income = rnorm(n_left, mean = 60000, sd = 15000),
  education = sample(c("HS", "BA", "MA"), n_left, replace = TRUE),
  group = "treatment"
)

# Control group (older, lower income on average)
right_data <- tibble(
  id = 1:n_right,
  age = rnorm(n_right, mean = 52, sd = 12),
  income = rnorm(n_right, mean = 50000, sd = 18000),
  education = sample(c("HS", "BA", "MA"), n_right, replace = TRUE),
  group = "control"
)

# Perform optimal matching
result <- match_couples(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  auto_scale = TRUE,
  return_diagnostics = TRUE
)

# View matched pairs
head(result$pairs)

# Summary statistics
result$info

## ----inspect-output, fig.alt="Histogram showing the distribution of match distances, with most matches having low distances indicating good match quality"----
# How many matched?
cat("Matched pairs:", result$info$n_matched, "\n")
cat("Unmatched left:", nrow(result$left_unmatched), "\n")
cat("Unmatched right:", nrow(result$right_unmatched), "\n")

# Distribution of match distances
summary(result$pairs$distance)

# Visualize match quality
ggplot(result$pairs, aes(x = distance)) +
  geom_histogram(bins = 30, fill = "#29abe0", alpha = 0.7) +
  labs(
    title = "Distribution of Match Distances",
    x = "Euclidean Distance (scaled)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank())

## ----preprocessing-demo-------------------------------------------------------
# Create data with scaling challenges
set.seed(456)
challenging_data <- tibble(
  id = 1:50,
  age = rnorm(50, 50, 10),                    # Years (reasonable scale)
  income = rnorm(50, 60000, 20000),           # Dollars (large scale)
  bmi = rnorm(50, 25, 5),                     # Ratio (small scale)
  smoker = sample(0:1, 50, replace = TRUE),   # Binary
  education = factor(
    sample(c("HS", "BA", "MA", "PhD"), 50, replace = TRUE),
    ordered = TRUE,
    levels = c("HS", "BA", "MA", "PhD")
  )
)

left_chal <- challenging_data[1:25, ]
right_chal <- challenging_data[26:50, ]

# Match WITHOUT auto-scaling (income dominates)
result_no_scale <- match_couples(
  left_chal, right_chal,
  vars = c("age", "income", "bmi"),
  auto_scale = FALSE
)

# Match WITH auto-scaling (all variables contribute)
result_scaled <- match_couples(
  left_chal, right_chal,
  vars = c("age", "income", "bmi"),
  auto_scale = TRUE,
  scale = "robust"  # Median/MAD scaling (robust to outliers)
)

# Compare match quality
cat("Without scaling - mean distance:", mean(result_no_scale$pairs$distance), "\n")
cat("With scaling - mean distance:", mean(result_scaled$pairs$distance), "\n")

## ----scaling-comparison-------------------------------------------------------
# Demonstrate scaling methods
demo_var <- c(10, 20, 25, 30, 100)  # Contains outlier (100)

# Function to show scaling
show_scaling <- function(x, method) {
  left_demo <- tibble(id = 1:3, var = x[1:3])
  right_demo <- tibble(id = 1:2, var = x[4:5])

  result <- match_couples(
    left_demo, right_demo,
    vars = "var",
    auto_scale = TRUE,
    scale = method,
    return_diagnostics = TRUE
  )

  # Extract scaled values from cost matrix
  cat(method, "scaling:\n")
  cat("  Original:", x, "\n")
  cat("  Distance matrix diagonal:", diag(result$cost_matrix)[1:2], "\n\n")
}

show_scaling(demo_var, "robust")
show_scaling(demo_var, "standardize")
show_scaling(demo_var, "range")

## ----health-checks, eval=FALSE------------------------------------------------
# # Create data with issues
# problematic_data <- tibble(
#   id = 1:100,
#   age = rnorm(100, 50, 10),
#   constant_var = 5,                           # No variation - will warn
#   mostly_missing = c(rnorm(20, 50, 10), rep(NA, 80)),  # >50% missing
#   extreme_skew = rexp(100, rate = 0.1)       # Very skewed
# )
# 
# # Attempt matching - will show warnings
# result <- match_couples(
#   problematic_data[1:50, ],
#   problematic_data[51:100, ],
#   vars = c("age", "constant_var", "mostly_missing", "extreme_skew"),
#   auto_scale = TRUE
# )
# 
# # Warning messages will indicate:
# # - "constant_var excluded (SD = 0)"
# # - "mostly_missing has 80% missing values"
# # - "extreme_skew has high skewness (3.2)"

## ----optimal-vs-greedy--------------------------------------------------------
# Create moderately large dataset
set.seed(789)
n <- 1000
large_left <- tibble(
  id = 1:n,
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
large_right <- tibble(
  id = 1:n,
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

# Optimal matching
time_optimal <- system.time({
  result_optimal <- match_couples(
    large_left, large_right,
    vars = c("x1", "x2", "x3"),
    method = "hungarian"
  )
})

# Greedy matching (row_best strategy)
time_greedy <- system.time({
  result_greedy <- match_couples(
    large_left, large_right,
    vars = c("x1", "x2", "x3"),
    strategy = "row_best"
  , method = "greedy")
})

# Compare
cat("Optimal matching:\n")
cat("  Time:", round(time_optimal["elapsed"], 3), "seconds\n")
cat("  Mean distance:", round(mean(result_optimal$pairs$distance), 4), "\n\n")

cat("Greedy matching:\n")
cat("  Time:", round(time_greedy["elapsed"], 3), "seconds\n")
cat("  Mean distance:", round(mean(result_greedy$pairs$distance), 4), "\n")
cat("  Speedup:", round(time_optimal["elapsed"] / time_greedy["elapsed"], 1), "x\n")

## ----greedy-strategies--------------------------------------------------------
# Compare greedy strategies on same data
set.seed(101)
test_left <- tibble(id = 1:200, x = rnorm(200))
test_right <- tibble(id = 1:200, x = rnorm(200))

strategies <- c("sorted", "row_best", "pq")
results <- list()

for (strat in strategies) {
  time <- system.time({
    greedy_result <- match_couples(
      test_left, test_right,
      vars = "x",
      strategy = strat
    , method = "greedy")
  })

  results[[strat]] <- list(
    time = time["elapsed"],
    mean_dist = mean(greedy_result$pairs$distance),
    total_dist = greedy_result$info$total_distance
  )
}

# Display comparison
comparison <- do.call(rbind, lapply(names(results), function(s) {
  data.frame(
    strategy = s,
    time_sec = round(results[[s]]$time, 4),
    mean_distance = round(results[[s]]$mean_dist, 4),
    total_distance = round(results[[s]]$total_dist, 2)
  )
}))

print(comparison)

## ----calipers, fig.alt="Overlapping histograms comparing match distances with and without caliper constraint, showing the caliper excludes distant matches"----
# Create data where some units are far apart
set.seed(202)
left_cal <- tibble(
  id = 1:50,
  x = c(rnorm(40, mean = 0, sd = 1), rnorm(10, mean = 5, sd = 0.5))  # Some outliers
)
right_cal <- tibble(
  id = 1:50,
  x = rnorm(50, mean = 0, sd = 1)
)

# Match without caliper - pairs everything
result_no_cal <- match_couples(
  left_cal, right_cal,
  vars = "x",
  auto_scale = FALSE
)

# Match with caliper - excludes poor matches
result_with_cal <- match_couples(
  left_cal, right_cal,
  vars = "x",
  max_distance = 1.5,  # Caliper: max distance = 1.5
  auto_scale = FALSE
)

cat("Without caliper:\n")
cat("  Matched:", result_no_cal$info$n_matched, "\n")
cat("  Mean distance:", round(mean(result_no_cal$pairs$distance), 3), "\n")
cat("  Max distance:", round(max(result_no_cal$pairs$distance), 3), "\n\n")

cat("With caliper (1.5):\n")
cat("  Matched:", result_with_cal$info$n_matched, "\n")
cat("  Mean distance:", round(mean(result_with_cal$pairs$distance), 3), "\n")
cat("  Max distance:", round(max(result_with_cal$pairs$distance), 3), "\n")

# Visualize caliper effect
ggplot(result_no_cal$pairs, aes(x = distance)) +
  geom_histogram(aes(fill = "No caliper"), bins = 30, alpha = 0.5) +
  geom_histogram(
    data = result_with_cal$pairs,
    aes(fill = "With caliper"),
    bins = 30,
    alpha = 0.5
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  labs(
    title = "Caliper Effect on Match Distances",
    x = "Distance",
    y = "Count",
    fill = "Condition"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank())

## ----caliper-sd, eval=FALSE---------------------------------------------------
# # Calculate pooled SD
# combined <- bind_rows(
#   left_data |> mutate(group = "left"),
#   right_data |> mutate(group = "right")
# )
# 
# pooled_sd <- sd(combined$age)  # For single variable
# caliper_width <- 0.2 * pooled_sd
# 
# result <- match_couples(
#   left_data, right_data,
#   vars = "age",
#   max_distance = caliper_width
# )

## ----caliper-empirical--------------------------------------------------------
# Fit all matches first
all_matches <- match_couples(left_cal, right_cal, vars = "x")

# Choose caliper at 90th percentile
caliper_90 <- quantile(all_matches$pairs$distance, 0.90)

# Refit with caliper
refined_matches <- match_couples(
  left_cal, right_cal,
  vars = "x",
  max_distance = caliper_90
)

cat("90th percentile caliper:", round(caliper_90, 3), "\n")
cat("Matches retained:",
    round(100 * refined_matches$info$n_matched / all_matches$info$n_matched, 1), "%\n")

## ----blocking-exact-----------------------------------------------------------
# Create multi-site data
set.seed(303)
multi_site <- bind_rows(
  tibble(
    id = 1:100,
    site = sample(c("A", "B", "C"), 100, replace = TRUE),
    age = rnorm(100, 50, 10),
    income = rnorm(100, 55000, 15000),
    group = "treatment"
  ),
  tibble(
    id = 101:250,
    site = sample(c("A", "B", "C"), 150, replace = TRUE),
    age = rnorm(150, 50, 10),
    income = rnorm(150, 55000, 15000),
    group = "control"
  )
)

left_site <- multi_site |> filter(group == "treatment")
right_site <- multi_site |> filter(group == "control")

# Create exact blocks by site
blocks <- matchmaker(
  left = left_site,
  right = right_site,
  block_type = "group",
  block_by = "site"
)

cat("Blocking structure:\n")
print(blocks$block_summary)

# Match within blocks
result_blocked <- match_couples(
  left = blocks$left,
  right = blocks$right,
  vars = c("age", "income"),
  block_id = "block_id",  # Use block IDs from matchmaker
  auto_scale = TRUE
)

# Verify exact site balance
result_blocked$pairs |>
  mutate(left_id = as.integer(left_id), right_id = as.integer(right_id)) |>
  left_join(left_site |> dplyr::select(id, site), by = c("left_id" = "id")) |>
  left_join(right_site |> dplyr::select(id, site), by = c("right_id" = "id"), suffix = c("_left", "_right")) |>
  count(site_left, site_right)

## ----blocking-cluster---------------------------------------------------------
# Create blocks based on age groups (data-driven)
cluster_blocks <- matchmaker(
  left = left_site,
  right = right_site,
  block_type = "cluster",
  block_vars = "age",
  n_blocks = 3
)

cat("Cluster-based blocks:\n")
print(cluster_blocks$block_summary)

# Match within clusters
result_clustered <- match_couples(
  left = cluster_blocks$left,
  right = cluster_blocks$right,
  vars = c("age", "income"),
  block_id = "block_id",
  auto_scale = TRUE
)

# Show age distribution by cluster
cluster_blocks$left |>
  group_by(block_id) |>
  summarise(
    n = n(),
    mean_age = mean(age),
    sd_age = sd(age)
  )

## ----balance-assessment-------------------------------------------------------
# Perform matching
match_result <- match_couples(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  auto_scale = TRUE
)

# Get matched samples
matched_left <- left_data |>
  filter(id %in% match_result$pairs$left_id)

matched_right <- right_data |>
  filter(id %in% match_result$pairs$right_id)

# Compute balance diagnostics
balance <- balance_diagnostics(
  result = match_result,
  left = left_data,
  right = right_data,
  vars = c("age", "income")
)

# Print balance summary
print(balance)

# Extract balance table for reporting
balance_table(balance)

## ----balance-plots, fig.alt="Bar chart comparing standardized differences before and after matching, with threshold lines at 0.1 and 0.25 showing improved balance after matching"----
# Before-after balance plot
# (Requires creating pre-match balance for comparison)

# For pre-match comparison, just compute summary statistics directly
pre_match_stats <- tibble(
  variable = c("age", "income"),
  std_diff = c(
    (mean(left_data$age) - mean(right_data$age)) / sqrt((sd(left_data$age)^2 + sd(right_data$age)^2) / 2),
    (mean(left_data$income) - mean(right_data$income)) / sqrt((sd(left_data$income)^2 + sd(right_data$income)^2) / 2)
  ),
  when = "Before"
)

# Combine for plotting
balance_comparison <- bind_rows(
  pre_match_stats,
  balance$var_stats |> dplyr::select(variable, std_diff) |> mutate(when = "After")
)

ggplot(balance_comparison, aes(x = variable, y = std_diff, fill = when)) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "#93c54b", linewidth = 0.8) +
  geom_hline(yintercept = c(-0.25, 0.25), linetype = "dashed", color = "#f47c3c", linewidth = 0.8) +
  geom_col(position = "dodge", width = 0.5) +
  geom_label(aes(x = 1.5, y = -0.1, label = "±0.1 excellent"),
             fill = "#93c54b", color = "white", inherit.aes = FALSE,
             size = 3, fontface = "bold", label.padding = unit(0.2, "lines")) +
  geom_label(aes(x = 1.5, y = 0.25, label = "±0.25 acceptable"),
             fill = "#f47c3c", color = "white", inherit.aes = FALSE,
             size = 3, fontface = "bold", label.padding = unit(0.2, "lines")) +
  labs(
    title = "Covariate Balance Before and After Matching",
    x = "Variable",
    y = "Standardized Difference",
    fill = "Timing"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank()) +
  coord_flip()

## ----real-world-data----------------------------------------------------------
set.seed(404)

# Simulate realistic scenario with selection bias
# Program attracts younger, more educated, currently employed individuals
create_participant <- function(n, is_treatment) {
  if (is_treatment) {
    tibble(
      id = 1:n,
      age = rnorm(n, mean = 35, sd = 8),
      education_years = rnorm(n, mean = 14, sd = 2),
      prior_earnings = rnorm(n, mean = 35000, sd = 10000),
      employed = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
      treatment = 1
    )
  } else {
    tibble(
      id = (n+1):(n+500),
      age = rnorm(500, mean = 42, sd = 12),
      education_years = rnorm(500, mean = 12, sd = 3),
      prior_earnings = rnorm(500, mean = 30000, sd = 12000),
      employed = sample(c(0, 1), 500, replace = TRUE, prob = c(0.5, 0.5)),
      treatment = 0
    )
  }
}

treatment_group <- create_participant(200, TRUE)
control_group <- create_participant(500, FALSE)

# Simulate outcome (earnings) with treatment effect
# True effect: +$5,000, with heterogeneity
treatment_group <- treatment_group |>
  mutate(
    earnings = prior_earnings +
      5000 +  # True treatment effect
      2000 * rnorm(n()) +  # Random variation
      100 * education_years  # Education effect
  )

control_group <- control_group |>
  mutate(
    earnings = prior_earnings +
      2000 * rnorm(n()) +
      100 * education_years
  )

# Examine baseline imbalance
cat("Pre-matching differences:\n")
cat("Age diff:",
    mean(treatment_group$age) - mean(control_group$age), "\n")
cat("Education diff:",
    mean(treatment_group$education_years) - mean(control_group$education_years), "\n")
cat("Prior earnings diff:",
    mean(treatment_group$prior_earnings) - mean(control_group$prior_earnings), "\n")

## ----real-world-matching------------------------------------------------------
# Match on baseline covariates
job_match <- match_couples(
  left = treatment_group,
  right = control_group,
  vars = c("age", "education_years", "prior_earnings", "employed"),
  auto_scale = TRUE,
  scale = "robust",
  return_diagnostics = TRUE
)

cat("Matching summary:\n")
cat("  Treated units:", nrow(treatment_group), "\n")
cat("  Matched treated:", job_match$info$n_matched, "\n")
cat("  Match rate:",
    round(100 * job_match$info$n_matched / nrow(treatment_group), 1), "%\n")

## ----real-world-balance-------------------------------------------------------
# Extract matched samples
matched_treated <- treatment_group |>
  filter(id %in% job_match$pairs$left_id)

matched_control <- control_group |>
  filter(id %in% job_match$pairs$right_id)

# Compute balance
job_balance <- balance_diagnostics(
  result = job_match,
  left = treatment_group,
  right = control_group,
  vars = c("age", "education_years", "prior_earnings", "employed")
)

print(job_balance)

# Check overall balance quality
cat("\nOverall balance:\n")
cat("  Mean |std diff|:", round(job_balance$overall$mean_abs_std_diff, 3), "\n")
cat("  Max |std diff|:", round(job_balance$overall$max_abs_std_diff, 3), "\n")
cat("  % with |std diff| > 0.1:",
    round(job_balance$overall$pct_large_imbalance, 1), "%\n")

## ----real-world-ate-----------------------------------------------------------
# Naive estimate (without matching) - BIASED
naive_effect <- mean(treatment_group$earnings) - mean(control_group$earnings)

# Matched estimate - accounts for baseline differences
matched_effect <- mean(matched_treated$earnings) - mean(matched_control$earnings)

# Paired t-test for significance
paired_comparison <- tibble(
  treated = matched_treated$earnings,
  control = matched_control$earnings[match(
    matched_treated$id,
    job_match$pairs$left_id
  )]
)

t_test <- t.test(paired_comparison$treated, paired_comparison$control, paired = TRUE)

# Report results
cat("Treatment Effect Estimates:\n\n")
cat("Naive (unmatched):\n")
cat("  Difference: $", round(naive_effect, 0), "\n")
cat("  (Upward biased due to selection)\n\n")

cat("Matched estimate:\n")
cat("  Difference: $", round(matched_effect, 0), "\n")
cat("  95% CI: ($", round(t_test$conf.int[1], 0), ", $",
    round(t_test$conf.int[2], 0), ")\n")
cat("  P-value:", format.pval(t_test$p.value, digits = 3), "\n")
cat("  (Closer to true effect of $5,000)\n")

## ----real-world-table, eval=FALSE---------------------------------------------
# # Table 1: Balance table
# balance_publication <- balance_table(job_balance)
# print(balance_publication)
# 
# # Table 2: Sample characteristics
# sample_table <- bind_rows(
#   matched_treated |>
#     summarise(
#       Group = "Treatment",
#       N = n(),
#       `Age (mean ± SD)` = sprintf("%.1f ± %.1f", mean(age), sd(age)),
#       `Education (years)` = sprintf("%.1f ± %.1f", mean(education_years), sd(education_years)),
#       `Prior Earnings` = sprintf("$%s ± %s",
#                                  format(round(mean(prior_earnings)), big.mark = ","),
#                                  format(round(sd(prior_earnings)), big.mark = ",")),
#       `Employed (%)` = sprintf("%.1f", 100 * mean(employed))
#     ),
#   matched_control |>
#     summarise(
#       Group = "Control",
#       N = n(),
#       `Age (mean ± SD)` = sprintf("%.1f ± %.1f", mean(age), sd(age)),
#       `Education (years)` = sprintf("%.1f ± %.1f", mean(education_years), sd(education_years)),
#       `Prior Earnings` = sprintf("$%s ± %s",
#                                  format(round(mean(prior_earnings)), big.mark = ","),
#                                  format(round(sd(prior_earnings)), big.mark = ",")),
#       `Employed (%)` = sprintf("%.1f", 100 * mean(employed))
#     )
# )
# 
# print(sample_table)
# 
# # Table 3: Treatment effect
# effect_table <- tibble(
#   Method = c("Unmatched", "Matched"),
#   `N (Treated)` = c(nrow(treatment_group), nrow(matched_treated)),
#   `N (Control)` = c(nrow(control_group), nrow(matched_control)),
#   `Effect Estimate` = sprintf("$%s", format(round(c(naive_effect, matched_effect)), big.mark = ",")),
#   `95% CI` = c("--", sprintf("($%s, $%s)",
#                             format(round(t_test$conf.int[1]), big.mark = ","),
#                             format(round(t_test$conf.int[2]), big.mark = ",")))
# )
# 
# print(effect_table)

## ----optimization-blocking, eval=FALSE----------------------------------------
# # Instead of matching 10,000 × 10,000:
# # Create 10 blocks of ~1,000 × 1,000 each
# blocks <- matchmaker(
#   left_large, right_large,
#   block_type = "cluster",
#   cluster_vars = "age",
#   n_clusters = 10
# )
# 
# # Much faster: 10 * O(1000^3) << O(10000^3)
# result <- match_couples(
#   blocks$left, blocks$right,
#   vars = covariates,
#   block_id = "block_id"
# )

## ----optimization-greedy, eval=FALSE------------------------------------------
# # Quick greedy match for exploration
# quick <- match_couples(
#   left_data, right_data,
#   vars = covariates,
#   strategy = "row_best"
# , method = "greedy")
# 
# # Assess balance
# balance_quick <- balance_diagnostics(quick, left_data, right_data, vars = covariates)
# 
# # If balance is acceptable, done!
# # If not, try optimal or add blocking

## ----optimization-caliper, eval=FALSE-----------------------------------------
# # Caliper removes distant pairs from cost matrix
# # Can dramatically reduce effective problem size
# result <- match_couples(
#   left_data, right_data,
#   vars = covariates,
#   max_distance = 0.25,  # Strict caliper
#   auto_scale = TRUE
# )

## ----poor-balance-fix, eval=FALSE---------------------------------------------
# # 1. Add more matching variables
# result <- match_couples(left, right,
#                         vars = c("age", "income", "education", "region"),  # Added!
#                         auto_scale = TRUE)
# 
# # 2. Tighten caliper (fewer but better matches)
# result <- match_couples(left, right, vars = vars,
#                         max_distance = 0.1)  # Was 0.5
# 
# # 3. Block on the problematic variable
# blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
# result <- match_couples(blocks$left, blocks$right, vars = other_vars,
#                         block_id = "block_id")

## ----few-matches-diagnosis, eval=FALSE----------------------------------------
# # Check covariate overlap
# library(ggplot2)
# combined <- bind_rows(
#   left |> mutate(group = "treatment"),
#   right |> mutate(group = "control")
# )
# ggplot(combined, aes(x = age, fill = group)) +
#   geom_density(alpha = 0.5) +
#   labs(title = "Check for Overlap")

## ----slow-matching-fix, eval=FALSE--------------------------------------------
# # For n > 3000: use greedy
# result <- match_couples(left, right, vars = vars, strategy = "sorted", method = "greedy")
# 
# # For n > 5000: add blocking
# blocks <- matchmaker(left, right, block_type = "cluster", n_blocks = 20)
# result <- match_couples(blocks$left, blocks$right, vars = vars,
#                         block_id = "block_id")

## ----workflow-diagram, fig.width=8, fig.height=6, echo=FALSE, fig.alt="Flowchart showing recommended matching workflow with iterative refinement loop"----
library(ggplot2)

# Define workflow nodes
nodes <- data.frame(
  label = c("1. Explore data\nIdentify confounders",
            "2. First match\nmatch_couples()",
            "3. Check balance\nbalance_diagnostics()",
            "Balance\nOK?",
            "4. Estimate\neffect",
            "Refine: caliper,\nblocking, more vars"),
  x = c(4, 4, 4, 4, 2, 6),
  y = c(6, 5, 4, 3, 2, 2),
  type = c("step", "step", "step", "decision", "step", "step")
)

# Define edges
edges <- data.frame(
  from_x = c(4, 4, 4, 4, 4, 6),
  from_y = c(6, 5, 4, 3, 3, 2),
  to_x = c(4, 4, 4, 2, 6, 4),
  to_y = c(5, 4, 3, 2, 2, 4),
  label = c("", "", "", "Yes", "No", ""),
  label_x = c(NA, NA, NA, 2.8, 5.2, 5.5),
  label_y = c(NA, NA, NA, 2.6, 2.6, 3)
)

ggplot() +
  # Draw straight edges (vertical arrows)
  geom_segment(data = edges[1:3, ],
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               color = "#3e3f3a", linewidth = 0.7) +

  # Draw decision branches
  geom_segment(data = edges[4:5, ],
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               color = "#3e3f3a", linewidth = 0.7) +
  # Draw return loop (curved)
  geom_curve(data = edges[6, ],
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
             arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
             color = "#3e3f3a", linewidth = 0.7, curvature = 0.3) +
  # Edge labels - no explicit color so CSS can control dark mode
  geom_text(data = edges[!is.na(edges$label_x), ],
            aes(x = label_x, y = label_y, label = label),
            size = 3, fontface = "italic") +
  # Step nodes (rectangles) - sandstone info color with white text
  geom_label(data = nodes[nodes$type == "step", ],
             aes(x = x, y = y, label = label), size = 3,
             fill = "#29abe0", color = "white",
             label.padding = unit(0.4, "lines"), label.r = unit(0.15, "lines"),
             lineheight = 0.9) +
  # Decision node (diamond) - sandstone warning color
  geom_point(data = nodes[nodes$type == "decision", ],
             aes(x = x, y = y), shape = 23, size = 20,
             fill = "#ffc107", color = "#f47c3c", stroke = 1.5) +
  # Decision text - use default color that adapts to dark mode
  geom_text(data = nodes[nodes$type == "decision", ],
            aes(x = x, y = y, label = label), size = 3, lineheight = 0.85) +
  # Title
  labs(title = "Matching Workflow") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)) +
  coord_cartesian(xlim = c(0, 8), ylim = c(1.5, 6.5))

## ----full-matching------------------------------------------------------------
# Full matching: every unit gets assigned to a group
result_full <- full_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  method = "optimal"   # min-cost max-flow (default)
)

result_full

## ----full-matching-comparison-------------------------------------------------
# Compare optimal vs greedy
result_full_greedy <- full_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  method = "greedy"
)

cat("Optimal: ", result_full$info$n_groups, "groups,",
    result_full$info$n_unmatched_left, "unmatched left,",
    result_full$info$n_unmatched_right, "unmatched right\n")
cat("Greedy:  ", result_full_greedy$info$n_groups, "groups,",
    result_full_greedy$info$n_unmatched_left, "unmatched left,",
    result_full_greedy$info$n_unmatched_right, "unmatched right\n")

## ----full-matching-constraints------------------------------------------------
# Constrain: each group gets 2-5 right units, caliper of 1.5
result_constrained <- full_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  min_controls = 2,
  max_controls = 5,
  caliper = 1.5
)

cat("Groups:", result_constrained$info$n_groups, "\n")
cat("Unmatched left:", result_constrained$info$n_unmatched_left, "\n")
cat("Unmatched right:", result_constrained$info$n_unmatched_right, "\n")

## ----full-matching-weights----------------------------------------------------
head(result_full$groups, 10)

## ----ratio-matching-----------------------------------------------------------
# Match 2 controls per treated unit
result_2to1 <- match_couples(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  auto_scale = TRUE,
  ratio = 2,
  method = "hungarian"
)

result_2to1$pairs

## ----replacement-matching-----------------------------------------------------
# Allow controls to be reused
result_wr <- match_couples(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  auto_scale = TRUE,
  replace = TRUE,
  method = "hungarian"
)

# Some controls may appear multiple times
n_distinct_controls <- length(unique(result_wr$pairs$right_id))
cat("Unique controls used:", n_distinct_controls, "out of", nrow(right_data), "\n")

## ----ps-matching--------------------------------------------------------------
# Combine data for propensity score estimation
combined <- rbind(
  transform(left_data[, c("id", "age", "income")], treated = 1L),
  transform(right_data[, c("id", "age", "income")], treated = 0L)
)

# Match on propensity scores (wider caliper for this example)
ps_result <- ps_match(
  formula = treated ~ age + income,
  data = combined,
  treatment = "treated",
  caliper_sd = 0.5,
  method = "hungarian"
)

ps_result

## ----cardinality-matching-----------------------------------------------------
# Maximize pairs subject to exact balance on the education distribution
card_result <- cardinality_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  fine = "education",
  auto_scale = TRUE
)

card_result$cardinality

## ----cardinality-moments------------------------------------------------------
card_bb <- cardinality_match(
  left = left_data[1:20, ],
  right = right_data[1:80, ],
  vars = c("age", "income"),
  max_std_diff = 0.1,
  node_limit = 10L,
  scale = "standardize"
)

card_bb$cardinality
card_bb$cardinality$constraints

## ----cardinality-heuristic----------------------------------------------------
card_heur <- cardinality_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  max_std_diff = 0.1,
  engine = "heuristic",
  scale = "standardize"
)

cat("Pairs matched:", card_heur$info$n_matched, "\n")
cat("Pruning iterations:", card_heur$info$pruning_iterations, "\n")
cat("Pairs removed:", card_heur$info$pairs_removed, "\n")

## ----cem-matching-------------------------------------------------------------
cem_result <- cem_match(
  left = left_data,
  right = right_data,
  vars = c("age", "income"),
  n_bins = "sturges"   # automatic bin width; also "fd", "scott", or an integer
)

cem_result

# Strata summary: how many units in each coarsened cell
head(cem_result$strata_summary)

## ----subclass-matching--------------------------------------------------------
# Combine data
combined <- rbind(
  transform(left_data[, c("id", "age", "income")], treated = 1L),
  transform(right_data[, c("id", "age", "income")], treated = 0L)
)

sub_result <- subclass_match(
  formula = treated ~ age + income,
  data = combined,
  treatment = "treated",
  n_subclasses = 5,
  estimand = "ATT"
)

sub_result
head(sub_result$subclass_summary)

## ----match-data-example-------------------------------------------------------
md <- couplr::match_data(result, left_data, right_data)
head(md)

## ----as-matchit-example, eval=FALSE-------------------------------------------
# mi <- as_matchit(result, left_data, right_data)
# 
# # cobalt balance table
# cobalt::bal.tab(mi)
# 
# # marginaleffects
# marginaleffects::avg_comparisons(model, vcov = "HC3", newdata = match.data(mi))

## ----sensitivity-analysis-----------------------------------------------------
# Simulate a modest treatment effect (small enough that hidden bias matters)
set.seed(99)
n_sa <- 50
left_sa <- data.frame(
  id = 1:n_sa,
  x = rnorm(n_sa, 5, 2),
  outcome = rnorm(n_sa, mean = 1.0, sd = 2)
)
right_sa <- data.frame(
  id = (n_sa + 1):(2 * n_sa),
  x = rnorm(n_sa, 5, 2),
  outcome = rnorm(n_sa, mean = 0.3, sd = 2)
)

# Match on covariate x
result_sa <- match_couples(
  left = left_sa,
  right = right_sa,
  vars = "x",
  method = "hungarian"
)

# How robust is the result to hidden bias?
sa <- sensitivity_analysis(
  result = result_sa,
  left = left_sa,
  right = right_sa,
  outcome_var = "outcome",
  gamma = seq(1, 2, by = 0.25)
)

sa

## ----autoplot-examples, fig.width=7, fig.height=4-----------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Distance distribution
  autoplot(result, type = "histogram")
}

## ----autoplot-balance, fig.width=7, fig.height=4------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Balance diagnostics love plot
  bal <- balance_diagnostics(result, left_data, right_data, vars = c("age", "income"))
  autoplot(bal, type = "love")
}

## ----autoplot-sensitivity, fig.width=7, fig.height=4--------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Sensitivity analysis curve
  autoplot(sa)
}

