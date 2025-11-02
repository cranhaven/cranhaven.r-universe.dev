## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "80%",
  dpi = 300
)

## ----setup--------------------------------------------------------------------
library(clockSim)
library(ggplot2)
library(dplyr)

mg <- getOdinGen()$noisy_LG
model <- mg$gen$new()

# Time steps
interval_hours <- 0.001
total_hours <- 2400
model$set_user(STEP_HOURS = interval_hours)
steps <- seq(from = 1, to = total_hours / interval_hours)

## -----------------------------------------------------------------------------
# A helper function for running simulation and convert unit
runSim <- function(model, steps, interval_hours){
  res <- model$run(steps)
  res <- res |> as.data.frame() |> mutate(time = step*interval_hours)
  res <- res |> filter(time %% 1 == 0) # Get results every hour
  plt1 <- plot_phase(
    res |> 
      select(step, time, C, C_N) |>
      # Convert units from count to nM
      mutate(across(-c(time, step), .fns = \(x) x*mg$count2nM)), 
    C, C_N)
  res.per <- res |> tail(n=240) # Only use last 240 hours for periodogram
  per <- compute_period(
    # Use M_T tim RNA data
    res.per |> pull(M_T),
    # Refer to the lomb::lsp().
    #   Only consider 18-30hour period, ofac=2 gives around ~1hour precision
    method = "lomb", from = 18, to = 30, type = "period", ofac = 2)
  
  return(list(
    plt.phase = plt1, lomb = per, res = res
  ))
}

## -----------------------------------------------------------------------------
# Turn noise off
model$set_user(NoiseVariance_M_T = 0)
run <- runSim(model, steps, interval_hours)
plot(run$plt.phase)
print(run$lomb)

# Turn noise on
model$set_user(NoiseVariance_M_T = -1)
run <- runSim(model, steps, interval_hours)
plot(run$plt.phase)
print(run$lomb)

## -----------------------------------------------------------------------------
model$set_user(k_sT = 1.8)

# Turn noise off
model$set_user(NoiseVariance_M_T = 0)
run <- runSim(model, steps, interval_hours)
plot(run$plt.phase)
print(run$lomb)

# Turn noise on
model$set_user(NoiseVariance_M_T = -1)
run <- runSim(model, steps, interval_hours)
plot(run$plt.phase)
print(run$lomb)

