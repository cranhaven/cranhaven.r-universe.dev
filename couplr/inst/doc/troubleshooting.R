## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  error = TRUE
)
library(couplr)
library(dplyr)

## ----error=TRUE---------------------------------------------------------------
try({
# This fails: all assignments have Inf cost
cost <- matrix(c(1, Inf, Inf, Inf, Inf, Inf, Inf, 2, 3), nrow = 3, byrow = TRUE)
result <- lap_solve(cost)
})

## -----------------------------------------------------------------------------
# Check for infeasible rows
check_feasibility <- function(cost_matrix) {
  finite_per_row <- rowSums(is.finite(cost_matrix))
  infeasible_rows <- which(finite_per_row == 0)

  if (length(infeasible_rows) > 0) {
    cat("Infeasible rows (no finite costs):", infeasible_rows, "\n")
    return(FALSE)
  }

  finite_per_col <- colSums(is.finite(cost_matrix))
  infeasible_cols <- which(finite_per_col == 0)

  if (length(infeasible_cols) > 0) {
    cat("Infeasible columns (no finite costs):", infeasible_cols, "\n")
    return(FALSE)
  }

  cat("Problem appears feasible\n")
  return(TRUE)
}

# Check the problematic matrix
cost <- matrix(c(1, Inf, Inf, Inf, Inf, Inf, Inf, 2, 3), nrow = 3, byrow = TRUE)
check_feasibility(cost)

## -----------------------------------------------------------------------------
# Remove rows with no valid assignments
cost <- matrix(c(1, Inf, Inf, Inf, Inf, Inf, Inf, 2, 3), nrow = 3, byrow = TRUE)
valid_rows <- rowSums(is.finite(cost)) > 0
cost_valid <- cost[valid_rows, , drop = FALSE]

if (nrow(cost_valid) > 0) {
  result <- lap_solve(cost_valid)
  cat("Matched", nrow(result), "of", nrow(cost), "rows\n")
}

## -----------------------------------------------------------------------------
# Replace Inf with high (but finite) penalty
cost_with_fallback <- cost
cost_with_fallback[!is.finite(cost_with_fallback)] <- 1e6  # Large penalty

result <- lap_solve(cost_with_fallback)
print(result)

## ----fig.width=7, fig.height=4, fig.alt="Density plot showing overlap between treatment and control groups on the age variable"----
# Simulate poor overlap scenario
set.seed(123)
left <- tibble(id = 1:50, age = rnorm(50, mean = 25, sd = 3))
right <- tibble(id = 1:50, age = rnorm(50, mean = 55, sd = 3))

# Visualize overlap
library(ggplot2)
combined <- bind_rows(
  left |> mutate(group = "Left"),
  right |> mutate(group = "Right")
)

ggplot(combined, aes(x = age, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Poor Covariate Overlap",
       subtitle = "No overlap means no good matches possible") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

## -----------------------------------------------------------------------------
# Example of poor balance
set.seed(456)
left <- tibble(
  id = 1:100,
  age = rnorm(100, 30, 5),
  income = rnorm(100, 80000, 20000)  # Very different from right
)
right <- tibble(
  id = 1:100,
  age = rnorm(100, 32, 5),
  income = rnorm(100, 40000, 10000)  # Very different income
)

result <- match_couples(left, right, vars = c("age", "income"), auto_scale = TRUE,
                        method = "hungarian")
balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))
print(balance)

## ----eval=FALSE---------------------------------------------------------------
# # Include additional relevant variables
# result <- match_couples(
#   left, right,
#   vars = c("age", "income", "education", "region"),  # More variables
#   auto_scale = TRUE
# )

## -----------------------------------------------------------------------------
result_strict <- match_couples(
  left, right,
  vars = c("age", "income"),
  max_distance = 0.3,  # Stricter caliper
  auto_scale = TRUE,
  method = "hungarian"
)

cat("Original matches:", result$info$n_matched, "\n")
cat("With caliper:", result_strict$info$n_matched, "\n")

balance_strict <- balance_diagnostics(result_strict, left, right, vars = c("age", "income"))
print(balance_strict)

## ----eval=FALSE---------------------------------------------------------------
# # Block on income tertiles to ensure exact balance
# left$income_cat <- cut(left$income, breaks = 3, labels = c("low", "mid", "high"))
# right$income_cat <- cut(right$income, breaks = 3, labels = c("low", "mid", "high"))
# 
# blocks <- matchmaker(left, right, block_type = "group", block_by = "income_cat")
# result_blocked <- match_couples(
#   blocks$left, blocks$right,
#   vars = c("age"),  # Match on age within income blocks
#   block_id = "block_id"
# )

## -----------------------------------------------------------------------------
# Compare scaling methods
for (scale_method in c("robust", "standardize", "range")) {
  res <- match_couples(left, right, vars = c("age", "income"),
                       auto_scale = TRUE, scale = scale_method,
                       method = "hungarian")
  bal <- balance_diagnostics(res, left, right, vars = c("age", "income"))
  cat(scale_method, "- max |std_diff|:",
      round(bal$overall$max_abs_std_diff, 3), "\n")
}

## -----------------------------------------------------------------------------
# Estimate runtime
estimate_runtime <- function(n, seconds_per_billion = 1) {
  ops <- n^3
  time_sec <- ops / 1e9 * seconds_per_billion

  if (time_sec < 60) {
    sprintf("%.1f seconds", time_sec)
  } else if (time_sec < 3600) {
    sprintf("%.1f minutes", time_sec / 60)
  } else {
    sprintf("%.1f hours", time_sec / 3600)
  }
}

cat("Estimated runtime for optimal matching:\n")
for (n in c(100, 500, 1000, 3000, 5000, 10000)) {
  cat(sprintf("  n = %5d: %s\n", n, estimate_runtime(n)))
}

## -----------------------------------------------------------------------------
set.seed(789)
n <- 500
large_left <- tibble(id = 1:n, x1 = rnorm(n), x2 = rnorm(n))
large_right <- tibble(id = 1:n, x1 = rnorm(n), x2 = rnorm(n))

# Greedy is much faster
time_greedy <- system.time({
  result_greedy <- match_couples(
    large_left, large_right,
    vars = c("x1", "x2"),
    strategy = "row_best"
  , method = "greedy")
})

cat("Greedy matching (n=500):", round(time_greedy["elapsed"], 2), "seconds\n")
cat("Quality (mean distance):", round(mean(result_greedy$pairs$distance), 4), "\n")

## ----eval=FALSE---------------------------------------------------------------
# # Create clusters to match within
# blocks <- matchmaker(
#   large_left, large_right,
#   block_type = "cluster",
#   block_vars = c("x1", "x2"),
#   n_blocks = 10  # 10 blocks of ~200 each
# )
# 
# # Match within blocks (10 x O(200^3) << O(2000^3))
# result_blocked <- match_couples(
#   blocks$left, blocks$right,
#   vars = c("x1", "x2"),
#   block_id = "block_id"
# )

## ----eval=FALSE---------------------------------------------------------------
# # For n > 1000, auction algorithm often faster
# result <- match_couples(
#   large_left, large_right,
#   vars = c("x1", "x2"),
#   method = "auction"
# )
# 
# # For sparse problems (many forbidden pairs)
# result <- match_couples(
#   left, right,
#   vars = vars,
#   max_distance = 0.5,  # Creates sparsity
#   method = "sap"       # Sparse algorithm
# )

## -----------------------------------------------------------------------------
# Compute once, reuse multiple times
dist_cache <- compute_distances(
  large_left, large_right,
  vars = c("x1", "x2"),
  scale = "robust"
)

# Fast: reuse cached distances
result1 <- match_couples(dist_cache, max_distance = 0.3, method = "hungarian")
result2 <- match_couples(dist_cache, max_distance = 0.5, method = "hungarian")
result3 <- match_couples(dist_cache, max_distance = 1.0, method = "hungarian")

## -----------------------------------------------------------------------------
# Memory requirements
memory_needed <- function(n) {
  bytes <- 8 * n^2
  if (bytes < 1e6) {
    sprintf("%.1f KB", bytes / 1e3)
  } else if (bytes < 1e9) {
    sprintf("%.1f MB", bytes / 1e6)
  } else {
    sprintf("%.1f GB", bytes / 1e9)
  }
}

cat("Memory for full distance matrix:\n")
for (n in c(1000, 5000, 10000, 20000, 50000)) {
  cat(sprintf("  n = %5d: %s\n", n, memory_needed(n)))
}

## ----eval=FALSE---------------------------------------------------------------
# result <- match_couples(left, right, vars = vars, method = "jv",
#                         memory_mode = "lazy")

## ----eval=FALSE---------------------------------------------------------------
# # Each block is much smaller
# blocks <- matchmaker(left, right, block_type = "cluster", n_blocks = 20)
# result <- match_couples(blocks$left, blocks$right, vars = vars, block_id = "block_id")

## ----eval=FALSE---------------------------------------------------------------
# # Increase to 16 GB (if available)
# memory.limit(size = 16000)

## -----------------------------------------------------------------------------
cost <- matrix(c(1, 2, 2, 2, 1, 2, 2, 2, 1), nrow = 3, byrow = TRUE)

result_jv <- lap_solve(cost, method = "jv")
result_hungarian <- lap_solve(cost, method = "hungarian")

cat("JV assignment:       ", result_jv$target, "\n")
cat("Hungarian assignment:", result_hungarian$target, "\n")
cat("JV total cost:       ", get_total_cost(result_jv), "\n")
cat("Hungarian total cost:", get_total_cost(result_hungarian), "\n")

## -----------------------------------------------------------------------------
# Check for ties
check_ties <- function(cost_matrix) {
  n <- nrow(cost_matrix)
  # Check if diagonal dominates (trivial ties)
  diag_costs <- diag(cost_matrix)
  if (length(unique(diag_costs)) < n) {
    cat("Tied costs on diagonal - multiple optima likely\n")
  }

  # Check cost uniqueness
  unique_costs <- length(unique(as.vector(cost_matrix)))
  total_entries <- length(cost_matrix)
  if (unique_costs < total_entries * 0.5) {
    cat("Many repeated costs - ties possible\n")
  }
}

check_ties(cost)

## -----------------------------------------------------------------------------
# Total cost should be identical
stopifnot(get_total_cost(result_jv) == get_total_cost(result_hungarian))
cat("Both methods found optimal solutions (same total cost)\n")

## -----------------------------------------------------------------------------
# Hungarian has consistent tie-breaking
result <- lap_solve(cost, method = "hungarian")

## -----------------------------------------------------------------------------
set.seed(42)
cost_perturbed <- cost + matrix(rnorm(9, 0, 1e-10), 3, 3)
result <- lap_solve(cost_perturbed)

## ----error=TRUE---------------------------------------------------------------
try({
left <- tibble(id = 1:5, age = c(25, 30, NA, 35, 40))
right <- tibble(id = 1:5, age = c(28, 32, 33, 36, 42))

# This may fail or give unexpected results
result <- match_couples(left, right, vars = "age")
})

## -----------------------------------------------------------------------------
left_clean <- left |> filter(!is.na(age))
right_clean <- right |> filter(!is.na(age))

result <- match_couples(left_clean, right_clean, vars = "age")
cat("Matched", result$info$n_matched, "pairs (excluded 1 left unit with NA)\n")

## -----------------------------------------------------------------------------
# Simple mean imputation
left_imputed <- left |>
  mutate(age = if_else(is.na(age), mean(age, na.rm = TRUE), age))

result <- match_couples(left_imputed, right, vars = "age")
cat("Matched", result$info$n_matched, "pairs (imputed 1 NA with mean)\n")

## -----------------------------------------------------------------------------
health <- preprocess_matching_vars(
  left, right,
  vars = "age"
)
print(health)

## ----eval=FALSE---------------------------------------------------------------
# # In R:
# remove.packages("couplr")
# install.packages("couplr")
# 
# # Or from GitHub:
# devtools::install_github("gcol33/couplr", force = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# devtools::clean_dll()
# devtools::load_all()

## -----------------------------------------------------------------------------
# Check if Rtools is properly configured
Sys.which("make")
Sys.which("g++")

## -----------------------------------------------------------------------------
cost <- matrix(c(1, NA, 3, 4), 2, 2)
cost[is.na(cost)] <- Inf  # Mark as forbidden
result <- lap_solve(cost)

## -----------------------------------------------------------------------------
# Always verify data before matching
stopifnot(nrow(left) > 0, nrow(right) > 0)

## ----eval=FALSE---------------------------------------------------------------
# # Minimal example template
# library(couplr)
# 
# # Minimal data that reproduces the issue
# set.seed(123)
# left <- tibble(id = 1:10, x = rnorm(10))
# right <- tibble(id = 1:10, x = rnorm(10))
# 
# # Code that causes the error
# result <- match_couples(left, right, vars = "x")
# 
# # Expected vs actual behavior
# # Expected: ...
# # Actual: [error message]
# 
# # Session info
# sessionInfo()

