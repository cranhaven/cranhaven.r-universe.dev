## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "80%",
  dpi = 300
)

## ----setup--------------------------------------------------------------------
library(clockSim)
library(matrixStats)
library(dplyr)

## -----------------------------------------------------------------------------
model_gen <- getOdinGen()$continuous_LG
model <- model_gen$new()

sim_hours <- seq(from = 0, to = 2400, by = 1)
res <- model$run(sim_hours) |> as.data.frame()
res$time <- res$t
plot(plot_phase(res, M_T, C_N))
plot(plot_timeSeries(res, 0, 240, 1, 6, M_T, C_N))
print(compute_period(res$M_T |> tail(n = 240), method = "lomb"))

## -----------------------------------------------------------------------------
run_eta(model, sim_hours)

## -----------------------------------------------------------------------------
# Compute summary
summary <- res |>
  select(-t, -time) |>
  apply(2, summary)
# Only keep min/mean/max
summary <- summary[c(1,4,6),]
# Add on mean+N*spread, N=2,3,...,N_max
N_max <- 3 # For larger scan increase this. CRAN=3
get_multiples <- function(s, k) {
  # Extract components
  min <- s[1, ]
  mean <- s[2, ]
  delta <- s[3, ] - min
  
  # Create new rows using vectorized operations
  multiples <- outer(k, delta) |> sweep(2, mean, "+")
  attr(multiples, "original") <- s
  
  # Return
  multiples
}
summary <- get_multiples(summary, 2:N_max)
summary <- summary[,c("M_T", "M_P")] # Only RNA states
# Create grid
grid <- expand.grid(
  summary |> as.data.frame(), KEEP.OUT.ATTRS = FALSE)
#   User variables for initial state start with setUserInitial_
names(grid) <- paste0("setUserInitial_",names(grid))

## -----------------------------------------------------------------------------
default_attractor <- model_gen$new()$run(sim_hours)
default_attractor <- 
  default_attractor[(length(sim_hours)-240):length(sim_hours),]
stat.fn <- function(raw_run, reference = default_attractor){
  # Return code == 2 means successful integration (at least for lsoda)
  succ <- attr(raw_run, "istate")[1] == 2
  # Subset only the last 240 time points - should be stabilized
  raw_run <- raw_run[(nrow(raw_run)-240):nrow(raw_run),]
  # Compute normalized RMSE
  nrmse <- compute_rmse(raw_run, reference, normalize = "range")
  nrmse <- max(nrmse)
  # Compute cosine similarity
  cos <- compute_cosine(raw_run, reference)
  cos <- min(cos)
  # Return
  c(converged = succ, nrmse = nrmse, cos = cos)
}

## -----------------------------------------------------------------------------
print(bench::mark(stat.fn(model$run(sim_hours))))
print(bench::mark(model$run(sim_hours)))

## -----------------------------------------------------------------------------
scan <- 
  grid_scan(model_gen, grid, apply.fn = stat.fn, 
            n.core = 1, custom.export = "default_attractor",
            sim_hours)

process_scan <- function(){
  .scanDF <- scan |> unlist(use.names = FALSE) |> matrix(ncol = 3, byrow=TRUE)
  colnames(.scanDF) <- names(scan[[1]])
  result <- cbind(grid, .scanDF |> as.data.frame())
  summary(
    result |> select(converged, nrmse, cos)
    )
}

process_scan()

## ----fig.show="hold",out.width="30%"------------------------------------------
# Rerun scan
scan <- 
  grid_scan(model_gen, grid, apply.fn = identity,
            n.core = 1, custom.export = "default_attractor",
            sim_hours)
# Show first and last of grid
first <- grid[1,]
last <- grid[nrow(grid),]
print(first)
print(last)
plot(plot_phase(scan[[1]] |> as.data.frame(), M_T, C_N))
plot(plot_phase(scan[[nrow(grid)]] |> as.data.frame(), M_T, C_N))

## -----------------------------------------------------------------------------
new_grid <- grid
new_grid$k_sT <- 1.8 # 2X of original, check model$content()
new_model <- model_gen$new()
new_model$set_user(k_sT = 1.8)
default_attractor <- new_model$run(sim_hours)
default_attractor <- 
  default_attractor[(length(sim_hours)-240):length(sim_hours),]
# Then, repeat the same code above.
scan <- 
  grid_scan(model_gen, new_grid, apply.fn = stat.fn, 
            n.core = 1, custom.export = "default_attractor",
            sim_hours)

process_scan()

