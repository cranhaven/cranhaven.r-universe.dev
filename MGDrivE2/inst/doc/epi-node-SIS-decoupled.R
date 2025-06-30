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
theta$a <- theta$f*theta$Q

# simulation parameters
tmax <- 250
dt <- 1

## -----------------------------------------------------------------------------
# augment the cube with RM transmission parameters
cube$c <- setNames(object = rep(x = theta$c, times = cube$genotypesN), nm = cube$genotypesID)
cube$b <- c("AA" = theta$b, "Aa" = 0.35, "aa" = 0)

## -----------------------------------------------------------------------------
# Places and transitions
# note decoupled sampling is only supported currently for one node.
SPN_P <- spn_P_epi_decoupled_node(params = theta, cube = cube)
SPN_T <- spn_T_epi_decoupled_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# SEI mosquitoes and SIS humans equilibrium
#  outputs required parameters in the named list "params"
#  outputs initial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_SEI_decoupled_mosy(params = theta, phi = 0.5, log_dd = TRUE,
                                   spn_P = SPN_P, cube = cube)

# augment with human equilibrium states
initialCons$H <- equilibrium_SEI_decoupled_human(params = theta)

## -----------------------------------------------------------------------------
# approximate hazards for continuous approximation
approx_hazards <- spn_hazards_decoupled(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
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
# delta t - one day
dt_stoch <- 0.1

# run tau-leaping simulation
tau_out <- sim_trajectory_R_decoupled(
  x0 = initialCons$M0,
  h0 = initialCons$H,
  SPN_P = SPN_P,
  theta = theta,
  tmax = tmax,
  inf_labels = SPN_T$inf_labels,
  dt = dt,
  dt_stoch = dt_stoch,
  S = S,
  hazards = approx_hazards,
  sampler = "tau-decoupled",
  events = events,
  verbose = FALSE,
  human_ode = "SIS",
  cube = cube
)

# summarize females/humans by genotype
tau_female <- summarize_females_epi(out = tau_out$state, spn_P = SPN_P)
tau_humans <- summarize_humans_epiSIS(out = tau_out$state)

# plot
ggplot(data = rbind(tau_female, tau_humans) ) +
  geom_line(aes(x = time, y = value, color = inf)) +
  facet_wrap(~ genotype, scales = "free_y") +
  theme_bw() +
  ggtitle("SPN: Tau Decoupled Solution")

