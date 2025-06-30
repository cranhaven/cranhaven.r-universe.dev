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
# sparse migration
library(Matrix)

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
  NH = 250,
  X = c(1,0,0,0), # 0% disease incidence
  NFX = 1000,
  f = 1/3,
  Q = 0.9,
  b = 0.55,
  c = 0.15,
  delta = 1/5,
  r = 1/14,
  muH = 1/(62*365),
  qEIP = 1/11,
  nEIP = 3
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
SPN_P <- spn_P_epiSEIR_node(params = theta, cube = cube)
SPN_T <- spn_T_epiSEIR_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# SEI mosquitoes and SEIR humans equilibrium
#  outputs required parameters in the named list "params"
#  outputs initial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_SEI_SEIR(params = theta, log_dd = TRUE, spn_P = SPN_P,
                                    cube = cube)

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "SEIR",
                              log_dd = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

# exact hazards for integer-valued state space
exact_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                             params = initialCons$params, type = "SEIR",
                             log_dd = TRUE, exact = TRUE, verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 25, length.out = 5, by = 2)
r_size <- 50
m_events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_S"),
                       "time" = r_times,
                       "value" = r_size,
                       "method" = "add",
                       stringsAsFactors = FALSE)

h_events <- data.frame("var" = "H_E",
                       "time" = 5,
                       "value" = 100,
                       "method" = "add",
                       stringsAsFactors = FALSE)

events <- rbind(m_events, h_events)

## -----------------------------------------------------------------------------
# run deterministic simulation
ODE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt, S = S,
                            hazards = approx_hazards, sampler = "ode", method = "lsoda",
                            events = events, verbose = FALSE)

# summarize females/humans by genotype
ODE_female <- summarize_females_epi(out = ODE_out$state,spn_P = SPN_P)
ODE_humans <- summarize_humans_epiSEIR(out = ODE_out$state)

# add species for plotting
ODE_female$species <- "Mosquitoes"
ODE_humans$species <- "Humans"

# plot
ggplot(data = rbind(ODE_female, ODE_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ species, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.2

# run tau-leaping simulation
PTS_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = exact_hazards,
                            sampler = "tau", events = events, verbose = FALSE)

# summarize females/humans by genotype
PTS_female <- summarize_females_epi(out = PTS_out$state,spn_P = SPN_P)
PTS_humans <- summarize_humans_epiSEIR(out = PTS_out$state)

# add species for plotting
PTS_female$species <- "Mosquitoes"
PTS_humans$species <- "Humans"

# plot
ggplot(data = rbind(PTS_female, PTS_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ species, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Approximation")

## -----------------------------------------------------------------------------
# 3-population entomological and epidemiological parameters
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
  # epidemiological parameters
  NH = 250,
  X = c(1,0,0,0),
  NFX = 500, # needed if any X[ ,3] == 0
  f = 1/3,
  Q = 0.9,
  b = 0.55,
  c = 0.15,
  delta = 1/5,
  nu = 1/5,
  r = 1/14,
  muH = 1/(62*365),
  qEIP = 1/11,
  nEIP = 3
)

# simulation parameters
tmax <- 250
dt <- 2

## -----------------------------------------------------------------------------
# nodetypes
node_list <- c("h", "b", "m")
num_nodes <- length(node_list)

# human movement
h_move <- matrix(data = FALSE, nrow = num_nodes, ncol = num_nodes,
                 dimnames = list(node_list, node_list))
h_move[1,2] <- TRUE
h_move[2,1] <- TRUE

# mosquito movement
m_move <- matrix(data = FALSE, nrow = num_nodes, ncol = num_nodes,
                 dimnames = list(node_list, node_list))
m_move[2,3] <- TRUE
m_move[3,2] <- TRUE

# Places and transitions
SPN_P <- spn_P_epiSEIR_network(node_list = node_list, params = theta, cube = cube)
SPN_T <- spn_T_epiSEIR_network(node_list = node_list, spn_P = SPN_P, params = theta,
                               cube = cube, h_move = h_move, m_move = m_move)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# SEI mosquitoes and SEIR humans equilibrium
#  outputs required parameters in the named list "params"
#  outputs initial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_SEI_SEIR(params = theta,node_list = node_list,
                                    NF = 1000,
                                    phi=0.5,
                                    NH = 250,
                                    log_dd=TRUE,
                                    spn_P=SPN_P,
                                    pop_ratio_Aq=NULL, pop_ratio_F=NULL,
                                    pop_ratio_M=NULL, cube=cube)

## -----------------------------------------------------------------------------
# calculate movement rates and movement probabilities
gam <- calc_move_rate(mu = initialCons$params$muF, P = 0.05)

# set mosquito movement rates/probabilities
#  mosquitoes exist in nodes 2 and 3, not 1
mr_mosy <- c(NaN, gam, gam)
mp_mosy <- Matrix::sparseMatrix(i = c(2,3), j = c(3,2), x = 1, dims = dim(m_move))

# set human movement rates/probabilities
#  humans exist in nodes 1 and 2, not 3
mr_human <- c(1/7, 1/7, NaN)
mp_human <- Matrix::sparseMatrix(i = c(1,2), j = c(2,1), x = 1, dims = dim(h_move))

# put rates and probs into the parameter list
initialCons$params$mosquito_move_rates <- mr_mosy
initialCons$params$mosquito_move_probs <- mp_mosy
initialCons$params$human_move_rates <- mr_human
initialCons$params$human_move_probs <- mp_human

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params , type = "SEIR",
                              log_dd = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 25, length.out = 5, by = 2)
r_size <- 50
m_events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_S_3"),
                       "time" = r_times,
                       "value" = r_size,
                       "method" = "add",
                       stringsAsFactors = FALSE)

h_events <- data.frame("var" = "H_E_1",
                       "time" = 5,
                       "value" = 100,
                       "method" = "add",
                       "stringsAsFactors" = FALSE)

events <- rbind(m_events,  h_events)

## -----------------------------------------------------------------------------
# run deterministic simulation
ODE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt, S = S,
                            hazards = approx_hazards, sampler = "ode", method = "lsoda",
                            events = events, verbose = FALSE)

# summarize aquatic stages by genotype
ODE_e <- summarize_eggs_geno(out = ODE_out$state, spn_P = SPN_P)
ODE_l <- summarize_larvae_geno(out = ODE_out$state, spn_P = SPN_P)
ODE_p <- summarize_pupae_geno(out = ODE_out$state, spn_P = SPN_P)

# add stage name
ODE_e$stage <- "Egg"
ODE_l$stage <- "Larvae"
ODE_p$stage <- "Pupae"

# plot by genotype
ggplot(data = rbind(ODE_e, ODE_l,ODE_p)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_grid(stage ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Genotypes")

## -----------------------------------------------------------------------------
# summarize aquatic stages by Erlang stage
ODE_e <- summarize_eggs_stage(out = ODE_out$state, spn_P = SPN_P)
ODE_l <- summarize_larvae_stage(out = ODE_out$state, spn_P = SPN_P)
ODE_p <- summarize_pupae_stage(out = ODE_out$state, spn_P = SPN_P)

# add stage name
ODE_e$stage <- "Egg"
ODE_l$stage <- "Larvae"
ODE_p$stage <- "Pupae"

# plot by Erlang stage
ggplot(data = rbind(ODE_e, ODE_l,ODE_p)) +
  geom_line(aes(x = time, y = value, color = `Erlang-stage`)) +
  facet_grid(stage ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Erlang Dwell Stage")

## -----------------------------------------------------------------------------
# summarize females/males
ODE_f <- summarize_females_epi(out = ODE_out$state, spn_P = SPN_P)
ODE_m <- summarize_males(out = ODE_out$state)

# add sex for plotting
ODE_f$sex <- "Female"
ODE_m$sex <- "Male"
ODE_m$inf <- "S"

# plot adults
ggplot(data = rbind(ODE_f, ODE_m)) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_grid(sex ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Adult Stages")

## -----------------------------------------------------------------------------
# summarize females/humans by genotype
ODE_female <- summarize_females_epi(out = ODE_out$state, spn_P = SPN_P)
ODE_humans <- summarize_humans_epiSEIR(out = ODE_out$state)

# plot
ggplot(data = rbind(ODE_female,ODE_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.1

# run cle simulation
CLE_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = approx_hazards,
                            sampler = "cle", events = events, verbose = FALSE)

# summarize females/humans by genotype
CLE_female <- summarize_females_epi(out = CLE_out$state, spn_P = SPN_P)
CLE_humans <- summarize_humans_epiSEIR(out = CLE_out$state)

# plot
ggplot(data = rbind(CLE_female,CLE_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: CLE Approximation")


