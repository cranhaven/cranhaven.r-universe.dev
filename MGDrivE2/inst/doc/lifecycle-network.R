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

# simulation parameters
tmax <- 125
dt <- 1

## -----------------------------------------------------------------------------
# adjacency matrix
#  specify where individuals can migrate
adj <- Matrix::sparseMatrix(i = c(1,2),j = c(2,1))
n <- nrow(adj)

# Places and transitions
SPN_P <- spn_P_lifecycle_network(num_nodes = n, params = theta,cube = cube)
SPN_T <- spn_T_lifecycle_network(spn_P = SPN_P, params = theta,cube = cube,m_move = adj)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# now that we have a network size, set adult females in each node
NF <- rep(x = 500, times = n)

# calculate equilibrium and setup initial conditions
#  outputs required parameters in the named list "params"
#  outputs intial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_lifeycle(params = theta, NF = NF, phi = 0.5,
                                    log_dd = TRUE, spn_P = SPN_P, cube = cube)


## -----------------------------------------------------------------------------
# calculate movement rates and movement probabilities
gam <- calc_move_rate(mu = theta$muF, P = 0.05)

move_rates <- rep(x = gam, times = n)
move_probs <- Matrix::sparseMatrix(i = {}, j = {},x = 0L,dims = dim(adj))

# uniform movement probabilities
rowprobs <- 1/rowSums(adj)
for(i in 1:nrow(move_probs)){
  cols <- Matrix::which(adj[i,])
  move_probs[i,cols] <- rep(rowprobs[i],length(cols))
}

# put rates and probs into the parameter list
initialCons$params$mosquito_move_rates <- move_rates
initialCons$params$mosquito_move_probs <- move_probs

## -----------------------------------------------------------------------------
# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = initialCons$params, type = "life",
                              log_dd = TRUE, exact = FALSE, tol = 1e-6,
                              verbose = FALSE)

# exact hazards for integer-valued state space
exact_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                             params = initialCons$params, type = "life",
                             log_dd = TRUE, exact = TRUE, tol = NaN,
                             verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 20, length.out = 5, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType, "_1"),
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
ODE_female <- summarize_females(out = ODE_out$state, spn_P = SPN_P)
ODE_male <- summarize_males(out = ODE_out$state)

# add sex for plotting
ODE_female$sex <- "Female"
ODE_male$sex <- "Male"

# plot
ggplot(data = rbind(ODE_female, ODE_male)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_grid(node ~ sex, scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE solution")

## -----------------------------------------------------------------------------
# delta t
dt_stoch <- 0.1

# tau leaping simulation
PTS_out <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                            dt_stoch = dt_stoch, S = S, hazards = exact_hazards,
                            sampler = "tau", events = events, verbose = FALSE)

# summarize females/males by genotype
PTS_female <- summarize_females(out = PTS_out$state, spn_P = SPN_P)
PTS_male <- summarize_males(out = PTS_out$state)

# add sex for plotting
PTS_female$sex <- "Female"
PTS_male$sex <- "Male"

# plot
ggplot(data = rbind(PTS_female, PTS_male)) +
  geom_line(aes(x = time, y = value, color = genotype)) +
  facet_grid(node ~ sex, scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: Tau-leaping Approximation")

