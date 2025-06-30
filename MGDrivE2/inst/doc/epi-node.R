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
# entomological and epidemiological parameters
theta <- list(
  # lifecycle parameters
  qE = 1/4,
  nE = 2,
  qL = 1/3,
  nL = 3,
  qP = 1/6,
  nP = 2,
  muE = 0.05,
  muL = 0.15,
  muP = 0.05,
  muF = 0.09,
  muM = 0.09,
  beta = 16,
  nu = 1/(4/24),
  # epidemiological parameters
  NH = 1000,
  X = 0.25,
  f = 1/3,
  Q = 0.9,
  b = 0.55,
  c = 0.15,
  r = 1/200,
  muH = 1/(62*365),
  qEIP = 1/11,
  nEIP = 6
)

# simulation parameters
tmax <- 250
dt <- 1

## -----------------------------------------------------------------------------
# augment the cube with RM transmission parameters
cube$c <- setNames(object = rep(x = theta$c, times = cube$genotypesN), nm = cube$genotypesID)
cube$b <- c("AA" = theta$b, "Aa" = 0.35, "aa" = 0)

## -----------------------------------------------------------------------------
# Places and transitions
SPN_P <- spn_P_epiSIS_node(params = theta, cube = cube)
SPN_T <- spn_T_epiSIS_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# SEI mosquitoes and SIS humans equilibrium
#  outputs required parameters in the named list "params"
#  outputs initial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_SEI_SIS(params = theta, phi = 0.5, log_dd = TRUE,
                                   spn_P = SPN_P, cube = cube)

## -----------------------------------------------------------------------------
# approximate hazards for continuous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "SIS",
                              log_dd = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 20, length.out = 5, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_S"),
                     "time" = r_times,
                     "value" = r_size,
                     "method" = "add",
                     stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
# run deterministic simulation
ODE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt, S = S,
                            hazards = approx_hazards, sampler = "ode", method = "lsoda",
                            events = events, verbose = FALSE)

# summarize females/males by genotype
ODE_female <- summarize_females_epi(out = ODE_out$state, spn_P = SPN_P)
ODE_male <- summarize_males(out = ODE_out$state)

# add sex for plotting
ODE_female$sex <- "Female"
ODE_male$sex <- "Male"
ODE_male$inf <- "S"

# plot
ggplot(data = rbind(ODE_female, ODE_male)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_grid(inf ~ sex, scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE solution - Mosquitoes")

## -----------------------------------------------------------------------------
# summarize females/humans by genotype
ODE_female <- summarize_females_epi(out = ODE_out$state, spn_P = SPN_P)
ODE_humans <- summarize_humans_epiSIS(out = ODE_out$state)

# add species for plotting
ODE_female$species <- "Mosquitoes"
ODE_humans$species <- "Humans"

# plot
ggplot(data = rbind(ODE_female,ODE_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ species, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.1

# run CLE simulation
CLE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = approx_hazards,
                            sampler = "cle", events = events, verbose = FALSE)

# summarize females/humans by genotype
CLE_female <- summarize_females_epi(out = CLE_out$state, spn_P = SPN_P)
CLE_humans <- summarize_humans_epiSIS(out = CLE_out$state)

# plot
ggplot(data = rbind(CLE_female,CLE_humans) ) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: CLE Approximation")

