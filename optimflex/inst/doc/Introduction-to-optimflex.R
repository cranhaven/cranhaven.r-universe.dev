## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(optimflex)

## ----basic_usage--------------------------------------------------------------
# Define a simple objective function
quad_func <- function(x) {
  (x[1] - 5)^2 + (x[2] + 3)^2
}

# Run optimization
res <- bfgs(
  start = c(0, 0),
  objective = quad_func,
  control = list(
    use_grad = TRUE,
    tol_grad = 1e-6,
    use_rel_x = TRUE
  )
)

# Inspect results
res$par
res$converged

## ----comparison---------------------------------------------------------------
rosenbrock <- function(x) {
  100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
}

start_val <- c(-1.2, 1.0)

# Compare DFP and Double Dogleg
res_dfp <- dfp(start_val, rosenbrock)
res_dd  <- double_dogleg(start_val, rosenbrock, control = list(initial_delta = 2.0))

cat("DFP Iterations:", res_dfp$iter, "\n")
cat("Double Dogleg Iterations:", res_dd$iter, "\n")

