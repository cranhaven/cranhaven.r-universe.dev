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
# number of adult female mosquitoes
NF <- 500

# entomological parameters
theta <- list(
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
  nu = 1/(4/24)
)

## -----------------------------------------------------------------------------
# "Places"
#  These are defined by Erlang-distributed life stages and genotypes
SPN_P <- spn_P_lifecycle_node(params = theta,cube = cube)

# Transitions
# This is a list of viable transitions from one "place" to another
SPN_T <- spn_T_lifecycle_node(spn_P = SPN_P,params = theta,cube = cube)

# Stoichiometry matrix
#  A sparse matrix representing the effect of each transition on each place
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# calculate equilibrium and setup initial conditions
#  outputs required parameters in the named list "params"
#  outputs intial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0", vectorized over:
#    all patches
#    any genotypes( includes XX/XY inheritance patterns)
#    allows initial ratios to vary
#    properly mates males/females
initialCons <- equilibrium_lifeycle(params = theta, NF = NF, phi = 0.5,
                                    log = TRUE, spn_P = SPN_P, cube = cube)

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "life",
                              log = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

# exact hazards for integer-valued state space
exact_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                             params = initialCons$params, type = "life",
                             log = TRUE, exact = TRUE, tol = NaN,
                             verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 20, length.out = 5, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType),
                     "time" = r_times,
                     "value" = r_size,
                     "method" = "add",
                     stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
# max simulation time
tmax <- 125
# time-step for output return, not the time-step of the sampling algorithm
dt <- 1

# run deterministic simulation
ODE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt, S = S,
                            hazards = approx_hazards, sampler = "ode", method = "lsoda",
                            events = events, verbose = FALSE)

## -----------------------------------------------------------------------------
# summarize females by genotype
ODE_out_f <- summarize_females(out = ODE_out$state, spn_P = SPN_P)

# summarize males by genotype
ODE_out_m <- summarize_males(out = ODE_out$state)

# add sex for plotting
ODE_out_f$sex <- "Female"
ODE_out_m$sex <- "Male"

# plot
ggplot(data = rbind(ODE_out_f, ODE_out_m)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_wrap(facets = vars(sex), scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE Solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.1

# tau sampling
PTS_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = exact_hazards,
                            sampler = "tau", events = events, verbose = FALSE)

# summarize females/males
PTS_out_f <- summarize_females(out = PTS_out$state, spn_P = SPN_P)
PTS_out_m <- summarize_males(out = PTS_out$state)

# add sex for plotting
PTS_out_f$sex <- "Female"
PTS_out_m$sex <- "Male"

# plot adults
ggplot(data = rbind(PTS_out_f, PTS_out_m)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_wrap(facets = vars(sex), scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Approximation")

## -----------------------------------------------------------------------------
# using the same parameters as above, along with the SPN_P object already created
# calculate equilibrium for lotka-volterra dynamics
initialCons <- equilibrium_lifeycle(params = theta, NF = NF, phi = 0.5,
                                    log = FALSE, spn_P = SPN_P,cube=cube)

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "life",
                              exact = FALSE, tol = 1e-8, verbose = FALSE,
                              log = FALSE)

# exact hazards for integer-valued state space
exact_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                             params = initialCons$params, type = "life",
                             exact = TRUE, tol = NaN, verbose = FALSE,
                             log = FALSE)

## -----------------------------------------------------------------------------
# deterministic simulation
ODE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            S = S, hazards = approx_hazards, sampler = "ode",
                            events = events, verbose = FALSE)

# summarize aquatic stages by genotype
ODE_out_e <- summarize_eggs_geno(out = ODE_out$state, spn_P = SPN_P)
ODE_out_l <- summarize_larvae_geno(out = ODE_out$state, spn_P = SPN_P)
ODE_out_p <- summarize_pupae_geno(out = ODE_out$state, spn_P = SPN_P)

# add stage name
ODE_out_e$stage <- "Egg"
ODE_out_l$stage <- "Larvae"
ODE_out_p$stage <- "Pupae"

# plot by genotype
ggplot(data = rbind(ODE_out_e, ODE_out_l,ODE_out_p)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_wrap(facets = vars(stage), scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Genotypes")

## -----------------------------------------------------------------------------
# summarize aquatic stages by Erlang stage
ODE_out_e <- summarize_eggs_stage(out = ODE_out$state, spn_P = SPN_P)
ODE_out_l <- summarize_larvae_stage(out = ODE_out$state, spn_P = SPN_P)
ODE_out_p <- summarize_pupae_stage(out = ODE_out$state, spn_P = SPN_P)

# add stage name
ODE_out_e$stage <- "Egg"
ODE_out_l$stage <- "Larvae"
ODE_out_p$stage <- "Pupae"

# plot by Erlang stage
ggplot(data = rbind(ODE_out_e, ODE_out_l,ODE_out_p)) +
  geom_line(aes(x = time, y = value, color = `Erlang-stage`)) +
  facet_wrap(facets = vars(stage), scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Erlang Dwell Stage")


## -----------------------------------------------------------------------------
# summarize females/males
ODE_out_f <- summarize_females(out = ODE_out$state, spn_P = SPN_P)
ODE_out_m <- summarize_males(out = ODE_out$state)

# add sex for plotting
ODE_out_f$sex <- "Female"
ODE_out_m$sex <- "Male"

# plot adults
ggplot(data = rbind(ODE_out_f, ODE_out_m)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_wrap(facets = vars(sex), scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Adult Stages")

## -----------------------------------------------------------------------------
# chemical langevin sampler
CLE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = approx_hazards,
                            sampler = "cle", events = events, verbose = FALSE)

# summarize females/males
CLE_out_f <- summarize_females(out = CLE_out$state, spn_P = SPN_P)
CLE_out_m <- summarize_males(out = CLE_out$state)

# add sex for plotting
CLE_out_f$sex <- "Female"
CLE_out_m$sex <- "Male"

# plot adults
ggplot(data = rbind(CLE_out_f, CLE_out_m)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_wrap(facets = vars(sex), scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: Chemical Langevin Approximation")

