## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "80%",
  dpi = 300
)

## -----------------------------------------------------------------------------
library(clockSim)
names(getOdinGen())

## -----------------------------------------------------------------------------
library(clockSim)
model <- getOdinGen()$continuous_LG$new()

# Running the model - specify the readout times
sim_total_hours <- 2400
times <- seq(from = 0, to = sim_total_hours, by = 1)
res_cont <- model$run(times) |> as.data.frame()

# Plot results
#   Phase portrait of the full simulation
#     X = TIM mRNA level (nM)
#     Y = Nuclear PER-TIM complex level (nM)
plot(plot_phase(res_cont, M_T, C_N))
#   Time series
#     subsample the result to 1hr precision for plotting (already TRUE)
#     X-axis time tick every 24hr
#     faceted plot of M_T and C_N states (see above)
#     plot 30-40day (first 10d DD constant condition)
res_cont$time <- times
plot(plot_timeSeries(res_cont, 24*30, 24*40, 1, 24, M_T, C_N))
#     plot 0-10day (first 10d LD condition)
plot(plot_timeSeries(res_cont, 0, 240, 1, 24, M_T, C_N))

# Compute clock period and power
#   Using 30-40day data (i.e., first 10-day in DD constant condition)
#     provide only M_T series; other series are locked in phase
print(compute_period(res_cont$M_T[(24*30):(24*40)])) # FFT method
print(compute_period(res_cont$M_T[(24*30):(24*40)], method = "lomb")) # LS periodogram

## -----------------------------------------------------------------------------
run_eta(model, times)

## -----------------------------------------------------------------------------
library(clockSim)
model <- getOdinGen()$discrete_LG$new()

# Specify time step and total simulation time
#   (0.002hr step, total 2400hr = 100day)
sim_step_hours <- 0.002
steps <- seq(from = 0, to = sim_total_hours/sim_step_hours)
model$set_user(STEP_HOURS = sim_step_hours)

# Run simulation once
res_dis <- model$run(steps) |> as.data.frame()
res_dis$time <- res_dis$step*sim_step_hours

# Plot results
#   Phase portrait of the full simulation
#     subset to 1-hour step to reduce unnecessary load in plotting
plot(plot_phase(res_dis |> dplyr::filter(time %% 1 == 0), M_T, C_N))
#   Time series
plot(plot_timeSeries(res_dis, 24*30, 24*40, 1, 24, M_T, C_N))
plot(plot_timeSeries(res_dis, 0, 240, 1, 24, M_T, C_N))

# Compute clock period and power
print(compute_period(res_dis$M_T[(24*30):(24*40)])) # FFT method
print(compute_period(res_dis$M_T[(24*30):(24*40)], method = "lomb")) # LS periodogram

## -----------------------------------------------------------------------------
run_eta(model, steps)

## ----fig.show="hold",out.width="20%"------------------------------------------
model <- getOdinGen()$discrete_LG$new()

# Test out a few step size settings
sim_step_hours <- c(0.01, 0.1, 0.15, 0.2)
for (step_hour in sim_step_hours){
  steps <- seq(from = 0, to = sim_total_hours/step_hour)
  model$set_user(STEP_HOURS = step_hour)
  res <- model$run(steps) |> as.data.frame()
  plot(
    plot_phase(res, M_T, C_N)+
      ggplot2::labs(title=paste0("Step=", step_hour)))
}

## -----------------------------------------------------------------------------
res_comp <- data.frame(
  time = res_cont[["time"]],
  cont = res_cont[["M_T"]]
)

# Test out a few step size settings
model <- getOdinGen()$discrete_LG$new()
sim_step_hours <- c(0.001 * 2^seq(from = 0, to = 3), 0.02)
for (step_hour in sim_step_hours){
  steps <- seq(from = 0, to = sim_total_hours/step_hour)
  model$set_user(STEP_HOURS = step_hour)
  res <- model$run(steps) |> as.data.frame()
  res$time <- res$step*step_hour
  res <- res |> dplyr::filter(time %in% res_comp$time)
  res_comp[[as.character(step_hour)]] <- res[["M_T"]]
}

# Compute correlation and plot it
cors <- sapply(sim_step_hours, \(step_hour){
  cor(res_comp[["cont"]], res_comp[[as.character(step_hour)]])
})
plot(sim_step_hours, cors, log = "x")

