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
  NH = 1000,
  X = 0.25,
  f = 1/3,
  Q = 0.9,
  b = 0.55,
  c = 0.15,
  r = 1/200,
  muH = 1/(62*365),
  qEIP = 1/11,
  nEIP = 2
)

# simulation parameters
tmax <- 250
dt <- 2

## -----------------------------------------------------------------------------
# augment the cube with RM transmission parameters
cube$c <- setNames(object = rep(x = theta$c, times = cube$genotypesN), nm = cube$genotypesID)
cube$b <- c("AA" = theta$b, "Aa" = 0.35, "aa" = 0)

## -----------------------------------------------------------------------------
# nodetypes
node_list <- c("m", "b", "h")
num_nodes <- length(node_list)

# human movement
h_move <- matrix(data = FALSE, nrow = num_nodes, ncol = num_nodes,
                 dimnames = list(node_list, node_list))
h_move[2,3] <- TRUE
h_move[3,2] <- TRUE

# mosquito movement
m_move <- matrix(data = FALSE, nrow = num_nodes, ncol = num_nodes,
                 dimnames = list(node_list, node_list))
m_move[1,2] <- TRUE
m_move[2,1] <- TRUE

# Places and transitions
SPN_P <- spn_P_epiSIS_network(node_list = node_list, params = theta, cube = cube)
SPN_T <- spn_T_epiSIS_network(node_list = node_list, spn_P = SPN_P, params = theta,
                              cube = cube, h_move = h_move, m_move = m_move)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# SEI mosquitoes and SIS humans equilibrium
#  outputs required parameters in the named list "params"
#  outputs intial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_SEI_SIS(params = theta, node_list = node_list,
                                   NF = 500, phi = 0.5, NH = 1000, pop_ratio_H = 0.15,
                                   log_dd = TRUE, spn_P = SPN_P, cube = cube)

## -----------------------------------------------------------------------------
# calculate movement rates and movement probabilities
gam <- calc_move_rate(mu = initialCons$params$muF, P = 0.05)

# set mosquito movement rates/probabilities
#  mosquitoes exist in nodes 1 and 2, not 3
mr_mosy <- c(gam, gam, NaN)
mp_mosy <- Matrix::sparseMatrix(i = c(1,2), j = c(2,1), x = 1, dims = dim(m_move))

# set human movement rates/probabilities
#  humans exist in nodes 2 and 3, not 1
mr_human <- c(NaN, 1/7, 1/7)
mp_human <- Matrix::sparseMatrix(i = c(2,3), j = c(3,2), x = 1, dims = dim(h_move))

# put rates and probs into the parameter list
initialCons$params$mosquito_move_rates <- mr_mosy
initialCons$params$mosquito_move_probs <- mp_mosy
initialCons$params$human_move_rates <- mr_human
initialCons$params$human_move_probs <- mp_human

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "SIS",
                              log_dd = TRUE, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

# exact hazards for integer-valued state space
exact_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                             params = initialCons$params, type = "SIS",
                             log_dd = TRUE, exact = TRUE, tol = NaN,
                             verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 20, length.out = 5, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_S_1"),
                     "time" = r_times,
                     "value" = r_size,
                     "method" = "add",
                     stringsAsFactors = FALSE)

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
  facet_grid(sex ~ node, scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE Solution - Adult Stages")

## -----------------------------------------------------------------------------
# summarize females/humans by genotype
ODE_female <- summarize_females_epi(out = ODE_out$state, spn_P = SPN_P)
ODE_humans <- summarize_humans_epiSIS(out = ODE_out$state)

# plot
ggplot(data = rbind(ODE_female,ODE_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: ODE Solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.1

# run tau-leaping simulation
PTS_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = exact_hazards,
                            sampler = "tau", events = events, verbose = FALSE)

# summarize females/humans by genotype
PTS_female <- summarize_females_epi(out = PTS_out$state, spn_P = SPN_P)
PTS_humans <- summarize_humans_epiSIS(out = PTS_out$state)

# plot
ggplot(data = rbind(PTS_female,PTS_humans) ) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = inf)) +
  facet_wrap(. ~ node, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Approximation")

