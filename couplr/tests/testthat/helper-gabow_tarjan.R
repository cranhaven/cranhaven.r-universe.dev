# helper-gabow_tarjan.R
# Shared test helpers for Gabow-Tarjan solver tests
# Auto-sourced by testthat before test files run

# 1-feasible complementary slackness check (Gabow-Tarjan style)
#
# Conditions for cost matrix C:
#  - For all finite edges (i,j): u[i] + v[j] <= C[i,j] + 1
#  - For matched edges (i,j):   u[i] + v[j] >= C[i,j]
#
check_complementary_slackness <- function(cost, row_match, col_match, u, v, tol = 1e-6) {
  n <- nrow(cost)
  m <- ncol(cost)
  BIG_INT <- 1e15

  if (length(u) != n || length(v) != m) return(FALSE)
  if (length(row_match) != n || length(col_match) != m) return(FALSE)

  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cij <- cost[i, j]
      if (is.finite(cij) && cij < BIG_INT) {
        sum_duals <- u[i] + v[j]

        # Upper bound: u + v <= c + 1
        if (sum_duals - (cij + 1) > tol) {
          return(FALSE)
        }

        # Lower bound on matched edges: u + v >= c
        if (!is.na(row_match[i]) &&
            row_match[i] == j &&
            !is.na(col_match[j]) &&
            col_match[j] == i) {
          if (cij - sum_duals > tol) {
            return(FALSE)
          }
        }
      }
    }
  }

  TRUE
}

# Helper to compute assignment cost
assignment_cost <- function(cost, row_match) {
  n <- length(row_match)
  total <- 0
  for (i in seq_len(n)) {
    j <- row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(cost)) {
      total <- total + cost[i, j]
    }
  }
  total
}
