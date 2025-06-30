## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.width=7.2, fig.height=4)
set.seed(10)

## -----------------------------------------------------------------------------
step_Euler <- function(pn, dt = 0.01) {

  stopifnot(all(names(pn) %in% c("S","h")))

  return(
        function(x0, t0, deltat){
          x = x0
          tNow = t0
          termt = t0 + deltat
          repeat {
            h = pn$h(x, tNow)
            if(any(h > 1e6)){
              stop("rates too large, terminating simulation.\n\ttry reducing dt")
            }
            dx = pn$S %*% (h*dt)
            x = x + as.vector(dx)
            x[x<0] <- 0 # "absorption" at 0
            tNow = tNow+dt
            if(tNow > termt){
              return(list("x"=x,"o"=NULL))
            }
          }
        }

  )
}

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
# adule female mosquitoes
NF <- 500

# lifecycle parameters
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
tmax <- 75
dt <- 1

## -----------------------------------------------------------------------------
# Places and transitions
SPN_P <- spn_P_lifecycle_node(params = theta, cube = cube)
SPN_T <- spn_T_lifecycle_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# lifecycle equilibrium and initial conditions
init <- equilibrium_lifeycle(params = theta, NF = NF, spn_P=SPN_P, cube = cube)

# approximate hazards for continous approximation
approx_hazards <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                              params = init$params, exact = FALSE, tol = 1e-8,
                              verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 15, length.out = 3, by = 10)
r_size <- 50
events <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType),
                     "time" = r_times,
                     "value" = r_size,
                     "method" = "add",
                     stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
# function to evaluate
evaluate_haz <- function(M,t){vapply(X = approx_hazards$hazards,
                                     FUN = function(h){h(t=t, M=M)},
                                     FUN.VALUE = numeric(1), USE.NAMES = FALSE) }

# step function for hazard evaluation
Euler_stepper <- step_Euler(pn = list(S=S, h=evaluate_haz), dt = 0.1)

# checks for simulation time and events
sim_times <- MGDrivE2:::base_time(tt = tmax, dt = dt)
events <- MGDrivE2:::base_events(x0 = init$M0, events = events, dt = dt)

# fum simulation
euler_out <- MGDrivE2:::sim_trajectory_base_R(
  x0 = init$M0, times = sim_times,
  num_reps = 1,
  stepFun = Euler_stepper,
  events = events, verbose = FALSE
)

# summarize female/male
euler_female_out <- summarize_females(out = euler_out$state,spn_P = SPN_P)
euler_male_out <- summarize_males(out = euler_out$state)
euler_fm_out <- rbind(cbind(euler_female_out,"sex" = "F"),
                      cbind(euler_male_out, "sex" = "M"))

## -----------------------------------------------------------------------------
# run deterministic simulation
ODE_out <- sim_trajectory_R(
  x0 = init$M0, tmax = tmax, dt = dt, S = S,
  hazards = approx_hazards, sampler = "ode",
  events = events, verbose = FALSE
)

# summarize females/males
ODE_female_out <- summarize_females(out = ODE_out$state, spn_P = SPN_P)
ODE_male_out <- summarize_males(out = ODE_out$state)
ODE_fm_out <- rbind(cbind(ODE_female_out,"sex" = "F"),
                    cbind(ODE_male_out, "sex" = "M"))

## -----------------------------------------------------------------------------
# add method for plotting
euler_fm_out$method <- "euler"
ODE_fm_out$method <- "deSolve"

# plot adults
ggplot(data = rbind(euler_fm_out, ODE_fm_out)) +
  geom_line(aes(x = time, y = value, color = genotype, linetype = method),
            alpha=0.75) +
  facet_wrap(facets = vars(sex), scales = "fixed") +
  theme_bw() +
  ggtitle("SPN: ODE Solution")

