## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.width=7.2, fig.height=4)
set.seed(10)

## -----------------------------------------------------------------------------
# simulation functions
library(MGDrivE2)
# inheritance patterns
library(MGDrivE)
# plotting
library(ggplot2)

# basic inheritance pattern
cube <- MGDrivE::cubeMendelian()

## -----------------------------------------------------------------------------
# generate default set of entomological and epidemiological parameters
theta <- imperial_model_param_list_create()

# age distribution of the population 
age_vector <-
  c(0,
    11 / 12,
    1,
    4,
    5,
    14,
    15,
    59,
    60)

ft <- 0.4 # percent of symptomatic cases that are treated

## -----------------------------------------------------------------------------
# Places and transitions
SPN_P <- spn_P_epi_decoupled_node(params = theta, cube = cube)
SPN_T <- spn_T_epi_decoupled_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# Modify parameters with IRS and LLIN coverage
IRS_cov <- 0.2
LLIN_cov <- 0.3
theta <- add_interventions(theta, IRS_cov, LLIN_cov)

# calculate a target EIR from a given prevalence
prevalence <- 0.7
eir <- convert_prevalence_to_eir(prevalence, age_vector, ft, theta)

# calculate human and mosquito equilibrium
# this function updates theta and the cube and returns initial conditions
eqm <- equilibrium_Imperial_decoupled(age_vector, ft, eir, theta, cube, SPN_P)

# extract updated theta and full set of initial conditions
theta <- eqm$theta
cube <- eqm$cube
initialCons <- eqm$initialCons

## -----------------------------------------------------------------------------
# approximate hazards for continuous approximation
approx_hazards <- spn_hazards_decoupled(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = theta, type = "Imperial",
                              log_dd = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

## -----------------------------------------------------------------------------
dt <- 1
ode_out <- sim_trajectory_R_decoupled(
  x0 = initialCons$M0,
  h0 = initialCons$H,
  SPN_P = SPN_P,
  theta = theta,
  tmax = 100,
  inf_labels = SPN_T$inf_labels,
  dt = dt,
  S = S,
  hazards = approx_hazards,
  sampler = "ode-decoupled",
  events = NULL,
  verbose = FALSE,
  human_ode = "Imperial",
  cube = cube
)

# summarize females/humans by genotype
ode_female <- summarize_females_epi(out = ode_out$state, spn_P = SPN_P)
ode_humans <- summarize_humans_epiImperial(out = ode_out$state, index=1)


# plot
ggplot(data = ode_female) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Decoupled Approximation - Mosquito")

ggplot(data = ode_humans) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Decoupled Approximation - Human")


## -----------------------------------------------------------------------------
# delta t - one day
dt_stoch <- 0.1
dt <- 1
# run ode-decoupled simulation
tau_out <- sim_trajectory_R_decoupled(
  x0 = initialCons$M0,
  h0 = initialCons$H,
  SPN_P = SPN_P,
  theta = theta,
  tmax = 100,
  inf_labels = SPN_T$inf_labels,
  dt = dt,
  dt_stoch = dt_stoch,
  S = S,
  hazards = approx_hazards,
  sampler = "tau-decoupled",
  events = NULL,
  verbose = FALSE,
  human_ode = "Imperial",
  cube = cube, 
  maxhaz = 1e12
)

# summarize females/humans by genotype
tau_female <- summarize_females_epi(out = tau_out$state, spn_P = SPN_P)
tau_humans <- summarize_humans_epiImperial(out = tau_out$state, index=1)

# plot
ggplot(data = tau_female) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Decoupled Approximation - Mosquito")

ggplot(data = tau_humans) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Decoupled Approximation - Human")


## -----------------------------------------------------------------------------
r_times <- seq(from = 20, length.out = 5, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_S"),
                    "time" = r_times,
                    "value" = r_size,
                    "method" = "add",
                    stringsAsFactors = FALSE)


tau_out <- sim_trajectory_R_decoupled(
  x0 = initialCons$M0,
  h0 = initialCons$H,
  SPN_P = SPN_P,
  theta = theta,
  tmax = 100,
  inf_labels = SPN_T$inf_labels,
  dt = dt,
  dt_stoch = dt_stoch,
  S = S,
  hazards = approx_hazards,
  sampler = "tau-decoupled",
  events = events,
  verbose = FALSE,
  human_ode = "Imperial",
  cube = cube,
  maxhaz = 1e12
)

# summarize females/humans by genotype
tau_female <- summarize_females_epi(out = tau_out$state, spn_P = SPN_P)
tau_humans <- summarize_humans_epiImperial(out = tau_out$state, index=1)

# plot
ggplot(data = tau_female) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Decoupled Approximation - Mosquito")

ggplot(data = tau_humans) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Decoupled Approximation - Human")

