## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bayestransmission)

## -----------------------------------------------------------------------------
pillar::glimpse(simulated.data)

## -----------------------------------------------------------------------------
table(CodeToEvent(simulated.data$type))

## ----paramexamples, results='hide'--------------------------------------------
# Fully specified parameter.
Param(init = 0, weight = 1, update = TRUE, prior = 0.5)
# Fixed parameter with explicit init
# Weight = 0 implies update=FALSE and prior is ignored.
Param(init = 0, weight = 0)
# Updated parameter that starts at zero.
Param(init = 0, weight = 1, update = TRUE)
# Short form for fixed parameter
Param(init = 0, weight = 0)

## -----------------------------------------------------------------------------
abx <- AbxParams(onoff = 0, delay = 0.0, life = 2.0)

## -----------------------------------------------------------------------------
abxrate <- AbxRateParams(
  # Fixed parameters when antibiotics are off
  uncolonized = Param(init = 1.0, weight = 0),
  colonized = Param(init = 1.0, weight = 0)
)

## -----------------------------------------------------------------------------
acquisition <- LinearAbxAcquisitionParams(
    base = Param(init = 0.001, weight = 1),     #< Base acquisition rate (Updated)
    time = Param(init = 1.0, weight = 0),       #< Time effect (Fixed)
    mass = Param(init = 1.0, weight = 1),       #< Mass Mixing (Updated)
    freq = Param(init = 1.0, weight = 1),       #< Frequency/Density effect (Updated)
    col_abx = Param(init = 1.0, weight = 0),    #< Colonized on antibiotics (Fixed)
    suss_abx = Param(init = 1.0, weight = 0),   #< Susceptible on antibiotics (Fixed)
    suss_ever = Param(init = 1.0, weight = 0)   #< Ever on antibiotics (Fixed)  
)

## -----------------------------------------------------------------------------
progression <- ProgressionParams(
    rate = Param(init = 0.0, weight = 0),      #< Base progression rate (Fixed for 2-state)
    abx  = Param(init = 1.0, weight = 0),      #< Currently on antibiotics (Fixed)
    ever_abx = Param(init = 1.0, weight = 0)   #< Ever on antibiotics (Fixed)
)

## -----------------------------------------------------------------------------
clearance <- ClearanceParams(
    rate = Param(init = 0.01, weight = 1),     #< Base clearance rate (Updated)
    abx  = Param(init = 1.0, weight = 0),      #< Currently on antibiotics (Fixed)
    ever_abx = Param(init = 1.0, weight = 0)   #< Ever on antibiotics (Fixed)
)

## -----------------------------------------------------------------------------
inunit <- ABXInUnitParams(
  acquisition = acquisition,
  progression = progression,
  clearance   = clearance
)

## -----------------------------------------------------------------------------
outcol <- OutOfUnitInfectionParams(
  acquisition = Param(init = 0.001, weight = 1),
  clearance = Param(init = 0.01, weight = 0),
  progression = Param(init = 0.0, weight = 0)
)

## -----------------------------------------------------------------------------
insitu <- InsituParams(
  # Starting 90/10 split uncolonized to colonized
  # For 2-state model, latent probability is 0
  probs = c(uncolonized = 0.90,
            latent = 0.0,
            colonized = 0.10),
  # Prior values for Bayesian updating  
  priors = c(1, 1, 1),
  # Which states to update (latent is fixed at 0 for 2-state model)
  doit = c(TRUE, FALSE, TRUE)
)

## -----------------------------------------------------------------------------
surv <- SurveillanceTestParams(
    # Probability of a positive test when uncolonized (false positive rate)
    # IMPORTANT: Must be > 0 to avoid -Inf likelihood. Use small value like 1e-10.
    # Setting to exactly 0.0 causes log(0) = -Inf if any uncolonized patient tests positive.
    uncolonized = Param(init = 1e-10, weight = 0),
    # Probability of a positive test when colonized (true positive rate/sensitivity)
    # Starting at 0.8, will be updated during MCMC
    colonized = Param(init = 0.8, weight = 1),
    # Latent state (for 2-state model, this is not used but must be specified)
    latent = Param(init = 0.0, weight = 0)
)


## -----------------------------------------------------------------------------
clin <- RandomTestParams(
    # Rate of testing when uncolonized
    uncolonized = ParamWRate(
      param = Param(init = 0.5, weight = 0), 
      rate = Param(init = 1.0, weight = 0)
    ),
    # Rate of testing when colonized  
    colonized = ParamWRate(
      param = Param(init = 0.5, weight = 0), 
      rate = Param(init = 1.0, weight = 0)
    ),
    # Latent state (for 2-state model, not used but must be specified)
    latent = ParamWRate(
      param = Param(init = 0.5, weight = 0), 
      rate = Param(init = 1.0, weight = 0)
    )
)

## -----------------------------------------------------------------------------
params <- LinearAbxModel(
  nstates = 2,
  Insitu = insitu,
  SurveillanceTest = surv,
  ClinicalTest = clin,
  OutOfUnitInfection = outcol,
  InUnit = inunit,
  Abx = abx,
  AbxRate = abxrate
)

## -----------------------------------------------------------------------------
system.time(
  results <- runMCMC(
    data = simulated.data_sorted,
    modelParameters = params,
    nsims = 100,
    nburn = 10,
    outputparam = TRUE,
    outputfinal = TRUE,
    verbose = FALSE
  )
)

## -----------------------------------------------------------------------------
# Convert parameters to data frame using package function
param_df <- mcmc_to_dataframe(results)

# Display first few rows
head(param_df)

## ----fig.width=10, fig.height=8-----------------------------------------------
library(ggplot2)
library(tidyr)

# Select key parameters for trace plots
trace_params <- param_df[, c("iteration", "insitu_colonized", "surv_test_col_pos", 
                              "outunit_acquisition", "inunit_base", 
                              "abxrate_colonized", "loglikelihood")]

# Convert to long format
trace_long <- pivot_longer(trace_params, 
                           cols = -iteration,
                           names_to = "parameter",
                           values_to = "value")

# Create trace plots
ggplot(trace_long, aes(x = iteration, y = value)) +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "MCMC Trace Plots",
       x = "Iteration",
       y = "Parameter Value")

## ----fig.width=10, fig.height=8-----------------------------------------------
# Remove burn-in if needed (here we already set nburn in the MCMC call)
# For demonstration, let's use all samples since nburn=0 was specified

# Create density plots for posterior distributions
ggplot(trace_long, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(value, na.rm = TRUE)), 
             color = "red", linetype = "dashed") +
  facet_wrap(~parameter, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Posterior Distributions",
       subtitle = "Red dashed line shows posterior mean",
       x = "Parameter Value",
       y = "Density")

## -----------------------------------------------------------------------------
# Calculate summary statistics for each parameter
library(dplyr)

summary_stats <- trace_long %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q025 = quantile(value, 0.025, na.rm = TRUE),
    q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

