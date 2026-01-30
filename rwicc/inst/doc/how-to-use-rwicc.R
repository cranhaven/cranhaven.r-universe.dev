## ---- include = FALSE---------------------------------------------------------

library(knitr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

set.seed(1)

library(rwicc)
theta_true = c(0.986, -3.88)
hazard_alpha = 1
hazard_beta = 0.5
sim_data = simulate_interval_censoring(
  "theta" = theta_true,
  "study_cohort_size" = 4500,
  "preconversion_interval_length" = 365,
  "hazard_alpha" = hazard_alpha,
  "hazard_beta" = hazard_beta)

# extract the participant-level and observation-level simulated data:
sim_participant_data = sim_data$pt_data
sim_obs_data = sim_data$obs_data
rm(sim_data)


## -----------------------------------------------------------------------------
library(pander)
pander(head(sim_participant_data))

## -----------------------------------------------------------------------------
pander(head(sim_obs_data))

## -----------------------------------------------------------------------------
EM_algorithm_outputs = fit_joint_model(
  obs_level_data = sim_obs_data,
  participant_level_data = sim_participant_data,
  bin_width = 7,
  verbose = FALSE)


## -----------------------------------------------------------------------------
names(EM_algorithm_outputs)

## -----------------------------------------------------------------------------

pander(EM_algorithm_outputs$Theta)

## -----------------------------------------------------------------------------
mu_est_EM = EM_algorithm_outputs$Mu
print(mu_est_EM)

## -----------------------------------------------------------------------------
EM_algorithm_outputs$converged

## -----------------------------------------------------------------------------
EM_algorithm_outputs$iterations

## -----------------------------------------------------------------------------
pander(EM_algorithm_outputs$convergence_metrics)

## -----------------------------------------------------------------------------

theta_est_midpoint = fit_midpoint_model(
  obs_level_data = sim_obs_data,
  participant_level_data = sim_participant_data
)

pander(theta_est_midpoint)

## -----------------------------------------------------------------------------
# uniform imputation:
theta_est_uniform = fit_uniform_model(
  obs_level_data = sim_obs_data,
  participant_level_data = sim_participant_data
)

pander(theta_est_uniform)

## ---- fig.width = 6, fig.asp = .75--------------------------------------------

plot1 = plot_CDF(
  true_hazard_alpha = hazard_alpha,
  true_hazard_beta = hazard_beta,
  omega.hat = EM_algorithm_outputs$Omega)

print(plot1)


## ---- fig.width = 6, fig.asp = .8---------------------------------------------

plot2 = plot_phi_curves(
  theta_true = theta_true,
  theta.hat_uniform = theta_est_uniform,
  theta.hat_midpoint = theta_est_midpoint,
  theta.hat_joint = EM_algorithm_outputs$Theta)

print(plot2)

