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
# parallel sims
library(parallel)

## -----------------------------------------------------------------------------
# load mortality data
data(mu_ts)

# vector of hourly timesteps
#  unit is in days, so 24 steps per day, 365 days
tt_ts <- 1:nrow(mu_ts)/24

# exaggerated amplitude of Grande Comore mortality rate, for visual effect
mu_ts_par <- mu_ts[ ,1] + ( (mu_ts[ ,1] - mean(mu_ts[ ,1]) ) * 50)

# approximating step function
step_approx <- stepfun(x = tt_ts, y = c(mu_ts_par[1], mu_ts_par),
                       f = 0, right = FALSE)

## -----------------------------------------------------------------------------
# long-form for ggplot2
df_ts <- data.frame("time" = tt_ts, "mu" = step_approx(v = tt_ts))

# plot
ggplot(data = df_ts) +
  geom_line(aes(x = time, y = mu), alpha = 0.75, color = "dodgerblue4") +
  xlab("Time (days)") +
  ylab("Adult Mortality") +
  theme_bw() +
  ggtitle("Grande Comore: Hourly Death Rates for One Year")

## ---- eval = FALSE------------------------------------------------------------
#  make_female_mort_haz <- function(trans, u, cube, params, exact = TRUE, tol = 1e-8){
#  
#    # rate constants
#    muF <- params$muF
#  
#    # which places have input arcs to this transition
#    s <- trans$s
#  
#    # weights of those arcs
#    w <- trans$s_w
#  
#    # omega is dependent on genotype
#    f_gen <- strsplit(x = u[s], split = "_", fixed = TRUE)[[1]][2]
#    omega <- cube$omega[f_gen]
#  
#    # return the hazard function
#    if(exact){
#  
#      # EXACT hazards (check enabling degree: for discrete simulation only)
#      return(
#        function(trans,M){
#          if(w <= M[s]){
#            return(muF * omega * M[s])
#          } else {
#            return(0)
#          }
#        }
#      )
#  
#    } else {
#  
#      # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
#      return(
#        function(trans,M){
#          haz <- muF * omega * M[s]
#          if(haz < tol){
#            return(0)
#          } else {
#            return(haz)
#          }
#        }
#      )
#  
#    }
#    # end of function
#  }

## -----------------------------------------------------------------------------
make_female_mort_haz_inhom <- function(trans, u, cube, params,
                                       exact = TRUE, tol = 1e-8){

  # mortality is a time-dependent hazard
  muF <- params$muF
  if(typeof(muF) != "closure"){
    stop("Inhomogeneous hazard 'make_female_mort_haz_inhom', ",
         "'muF' in 'params' list needs to be a function")
  }

  # which places have input arcs to this transition
  s <- trans$s

  # weights of those arcs
  w <- trans$s_w

  # omega is dependent on genotype
  f_gen <- strsplit(x = u[s], split = "_", fixed = TRUE)[[1]][2]
  omega <- cube$omega[f_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          return(muF(t) * omega * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        haz <- muF(t) * omega * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

## -----------------------------------------------------------------------------
make_male_mort_haz_inhom <- function(trans, u, cube, params,
                                     exact = TRUE, tol = 1e-8){

  # mortality is a time-dependent hazard
  muM <- params$muM
  if(typeof(muM) != "closure"){
    stop("Inhomogeneous hazard 'make_male_mort_haz_inhom', ",
         "value 'muM' in 'params' list needs to be a function")
  }

  # which places have input arcs to this transition
  s <- trans$s

  # weights of those arcs
  w <- trans$s_w

  # omega is dependent on genotype
  m_gen <- strsplit(x = u[s], split = "_", fixed = TRUE)[[1]][2]
  omega <- cube$omega[m_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          return(muM(t) * omega * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        haz <- muM(t) * omega * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

## -----------------------------------------------------------------------------
spn_hazards_lifecycle_node_inhom <- function(spn_P, spn_T, cube, params,
                                             log_dd = TRUE, exact = TRUE,
                                             tol = 1e-12, verbose = TRUE){

  if(tol > 1e-6 & !exact){
    warning("warning: hazard function tolerance ",tol," is large; ",
            "consider tolerance < 1e-6 for sufficient accuracy\n")
  }

  if(log_dd){
    if(!("K" %in% names(params))){
      stop("Specified logistic (carrying capacity) density-dependent larval ",
           "mortality, please specify parameter 'K' in params")
    }
  } else {
    if(!("gamma" %in% names(params))){
      stop("Specified Lotka-Volterra (carrying capacity) density-dependent ",
           "larval mortality, please specify parameter 'gamma' in params")
    }
  }

  # transitions and places
  v <- spn_T$v
  u <- spn_P$u
  n <- length(v)

  # get male and larvae indices
  l_ix <- as.vector(spn_P$ix[[1]]$larvae)
  m_ix <- spn_P$ix[[1]]$males

  # the hazard functions
  h <- setNames(object = vector("list",n), nm = v)


  if(verbose){
    pb <- txtProgressBar(min = 1,max = n,style = 3)
    pp <- 1

    cat(" --- generating hazard functions for SPN --- \n")
  }

  # make the hazards
  for(t in 1:n){

    type <- spn_T$T[[t]]$class

    # make the correct type of hazard
    if(type == "oviposit"){
      h[[t]] <- MGDrivE2:::make_oviposit_haz(trans = spn_T$T[[t]], u = u,
                                             cube = cube, params = params,
                                             exact = exact, tol = tol)
    } else if(type == "egg_adv"){
      h[[t]] <- MGDrivE2:::make_egg_adv_haz(trans = spn_T$T[[t]], u = u,
                                            cube = cube, params = params,
                                            exact = exact, tol = tol)
    } else if(type == "egg_mort"){
      h[[t]] <- MGDrivE2:::make_egg_mort_haz(trans = spn_T$T[[t]], u = u,
                                             cube = cube, params = params,
                                             exact = exact, tol = tol)
    } else if(type == "larvae_adv"){
      h[[t]] <- MGDrivE2:::make_larvae_adv_haz(trans = spn_T$T[[t]], u = u,
                                               cube = cube, params = params,
                                               exact = exact,tol = tol)
    } else if(type == "larvae_mort"){
      if(log_dd){
        h[[t]] <- MGDrivE2:::make_larvae_mort_haz_log(trans = spn_T$T[[t]], u = u,
                                                      l_ix = l_ix, node = 1,
                                                      cube = cube, params = params,
                                                      exact = exact, tol = tol)
      } else {
        h[[t]] <- MGDrivE2:::make_larvae_mort_haz_lk(trans = spn_T$T[[t]], u = u,
                                                     l_ix = l_ix, node = 1,
                                                     cube = cube, params = params,
                                                     exact = exact, tol = tol)
      }
    } else if(type == "pupae_adv"){
      h[[t]] <- MGDrivE2:::make_pupae_adv_haz(trans = spn_T$T[[t]], u = u,
                                              cube = cube, params = params,
                                              exact = exact, tol = tol)
    } else if(type == "pupae_mort"){
      h[[t]] <- MGDrivE2:::make_pupae_mort_haz(trans = spn_T$T[[t]], u = u,
                                               cube = cube, params = params,
                                               exact = exact, tol = tol)
    } else if(type == "pupae_2m"){
      h[[t]] <- MGDrivE2:::make_pupae_2male_haz(trans = spn_T$T[[t]], u = u,
                                                cube = cube, params = params,
                                                exact = exact, tol = tol)
    } else if(type == "pupae_2f"){
      h[[t]] <- MGDrivE2:::make_pupae_2female_haz(trans = spn_T$T[[t]], u = u,
                                                  m_ix = m_ix, cube = cube,
                                                  params = params, exact = exact,
                                                  tol = tol)
    } else if(type == "male_mort"){
      # inhomogeneous male mortality
      h[[t]] <- make_male_mort_haz_inhom(trans = spn_T$T[[t]], u = u ,cube = cube,
                                         params = params, exact = exact, tol = tol)
    } else if(type == "female_mort"){
      # inhomogeneous female mortality
      h[[t]] <- make_female_mort_haz_inhom(trans = spn_T$T[[t]], u = u, cube = cube,
                                           params = params, exact = exact, tol = tol)
    } else if(type == "pupae_2unmated"){
      h[[t]] <- MGDrivE2:::make_pupae_2unmated_haz(trans = spn_T$T[[t]], u = u,
                                                   m_ix = m_ix, cube = cube,
                                                   params = params, exact = exact,
                                                   tol = tol)
    } else if(type == "female_unmated_mate"){
      h[[t]] <- MGDrivE2:::make_unmated_2female_haz(trans = spn_T$T[[t]], u = u,
                                                    m_ix = m_ix, cube = cube,
                                                    params = params, exact = exact,
                                                    tol = tol)
    } else if(type == "female_unmated_mort"){
      h[[t]] <- make_female_mort_haz_inhom(trans = spn_T$T[[t]], u = u, cube = cube,
                                           params = params, exact = exact, tol = tol)
    } else {
      stop(paste0("error in making hazard function for unknown class type: ",type))
    }

    if(verbose){setTxtProgressBar(pb,t)}
  } # end loop

  if(verbose){
    close(pb)
    cat(" --- done generating hazard functions for SPN --- \n")
  }


  return(list("hazards"=h, "flag"=exact))
} # end function

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
  muF = mean(mu_ts[,1]),
  muM = mean(mu_ts[,1]),
  beta = 16,
  nu = 1/(4/24)
)

# simulation parameters
tmax <- 365
dt <- 1

# basic inheritance pattern
cube <- MGDrivE::cubeMendelian()

## -----------------------------------------------------------------------------
# Places and transitions for one node
SPN_P <- spn_P_lifecycle_node(params = theta, cube = cube)
SPN_T <- spn_T_lifecycle_node(spn_P = SPN_P, params = theta, cube = cube)

# Stoichiometry matrix
S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

## -----------------------------------------------------------------------------
# calculate equilibrium and setup initial conditions
#  outputs required parameters in the named list "params"
#  outputs intial equilibrium for adv users, "init
#  outputs properly filled initial markings, "M0"
initialCons <- equilibrium_lifeycle(params = theta, NF = NF, log_dd = TRUE,
                                    spn_P = SPN_P, cube = cube)

# store homogenous params
theta_hom <- initialCons$params

# update params for inhomogeneous hazards (these are function closures)
initialCons$params$muF <- step_approx
initialCons$params$muM <- step_approx

## -----------------------------------------------------------------------------
# ODE (inhomogeneous) hazards
approx_hazards <- spn_hazards_lifecycle_node_inhom(spn_P = SPN_P, spn_T = SPN_T,
                                                   cube = cube,
                                                   params = initialCons$params,
                                                   exact = FALSE, tol = 1e-8,
                                                   verbose = FALSE)

# ODE (homogeneous) hazards
approx_hazards_hom <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                                  params = theta_hom, type = "life", log_dd = TRUE,
                                  exact = FALSE, tol = 1e-8, verbose = FALSE)

# CTMC (inhomogeneous) hazards
exact_hazards <- spn_hazards_lifecycle_node_inhom(spn_P = SPN_P, spn_T = SPN_T,
                                                  cube = cube,
                                                  params = initialCons$params,
                                                  exact = TRUE, verbose = FALSE)

# CTMC (homogeneous) hazards
exact_hazards_hom <- spn_hazards(spn_P = SPN_P, spn_T = SPN_T, cube = cube,
                                  params = theta_hom, type = "life", log_dd = TRUE,
                                  exact = TRUE, verbose = FALSE)

## -----------------------------------------------------------------------------
# releases
r_times <- seq(from = 25, length.out = 5, by = 7)
r_size <- 50
events_f <- data.frame("var" = paste0("F_", cube$releaseType, "_", cube$wildType),
                       "time" = r_times,
                       "value" = r_size,
                       "method" = "add",
                       stringsAsFactors = FALSE)

events_m <- data.frame("var" = paste0("M_", cube$releaseType),
                       "time" = r_times,
                       "value" = r_size,
                       "method" = "add",
                       stringsAsFactors = FALSE)

events <- rbind(events_f,events_m)

## -----------------------------------------------------------------------------
# ODE (inhomogeneous) simulation
ODE_out_inhom <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                                  S = S, hazards = approx_hazards, sampler = "ode",
                                  method = "ode45", events = events, verbose = FALSE)

# summarize
ODE_female_inhom <- summarize_females(out = ODE_out_inhom$state, spn_P = SPN_P)
ODE_male_inhom <- summarize_males(out = ODE_out_inhom$state)

# combine for plotting later
ODE_out_inhom_melt <- rbind(cbind(ODE_female_inhom, "sex" = "F"),
                            cbind(ODE_male_inhom, "sex" = "M") )


# ODE (homogeneous) simulation
ODE_out_hom <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
                                S = S, hazards = approx_hazards_hom, sampler = "ode",
                                method = "ode45", events = events, verbose = FALSE)

# summarize
ODE_female_hom <- summarize_females(out = ODE_out_hom$state, spn_P = SPN_P)
ODE_male_hom <- summarize_males(out = ODE_out_hom$state)

# combine for plotting later
ODE_out_hom_melt <- rbind(
  cbind(ODE_female_hom, "sex" = "F"),
  cbind(ODE_male_hom, "sex" = "M")
)

ODE_out_inhom_melt$death <- "inhomogeneous"
ODE_out_hom_melt$death <- "homogeneous"

# plot everything together
ggplot(data = rbind(ODE_out_inhom_melt, ODE_out_hom_melt) ) +
  geom_path(aes(x = time, y = value,
                color = death)) +
  facet_grid(genotype ~ sex, scales = "free_y") +
  scale_color_manual(values = c("inhomogeneous" = "firebrick3",
                                "homogeneous" = "dodgerblue3")) +
  # final formatting
  theme_bw() +
  ggtitle("Constant vs Seasonally Time-Varying Mortality Rates") +
  guides(color = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  # parameters for stochastic simulations
#  num_rep <- 10
#  dt_stoch <- 1/24
#  
#  # run inhomogeneous simulations
#  PTS_out_inhom <- sim_trajectory_R(x0 = initialCons$M0, tmax = tmax, dt = dt,
#                                    dt_stoch = dt_stoch, S = S, num_reps = num_rep,
#                                    hazards = exact_hazards, sampler = "tau",
#                                    events = events, verbose = FALSE)
#  
#  # summarize females/males, add sex for plotting later
#  PTS_female <- cbind(summarize_females(out = PTS_out_inhom$state, spn_P = SPN_P),
#                      "sex" = "F")
#  PTS_male <- cbind(summarize_males(out = PTS_out_inhom$state), "sex" = "M")
#  
#  # reductions for plotting
#  PTS_inhom_par_unroll <- rbind(PTS_female, PTS_male)
#  
#  # summarize_* functions return long-form data for ggplot2
#  #  therefore, we take the first rep of data, the sim_time by num_genotypes
#  #  Then, we reorganize the data into an array, and take the mean over every
#  #  repetition
#  tot_time <- tmax + 1
#  num_geno <- cube$genotypesN
#  PTS_inhom_par_unroll_mean <- rbind(
#    cbind(PTS_female[1:(tot_time * num_geno), c("genotype", "sex", "time")],
#          "Mean" = as.vector(rowMeans(x = array(data = PTS_female$value,
#                                                dim = c(tot_time, num_geno, num_rep)),
#                                      dims = 2)) ),
#    cbind(PTS_male[1:(tot_time * num_geno), c("genotype", "sex", "time")],
#          "Mean" = as.vector(rowMeans(x = array(data = PTS_male$value,
#                                                dim = c(tot_time, num_geno, num_rep)),
#                                      dims = 2)) )
#    )

## ---- eval = FALSE------------------------------------------------------------
#  # parameters for stochastic simulations
#  num_core <- 1
#  M0 <- initialCons$M0
#  
#  # setup the cluster
#  cl <- parallel::makePSOCKcluster(names = num_core)
#  
#  # set parallel seed
#  parallel::clusterSetRNGStream(cl = cl, iseed = 18438L)
#  
#  # export required objects to each socket
#  parallel::clusterExport(cl=cl, varlist=c("M0", "tmax", "dt", "dt_stoch", "S",
#                                           "exact_hazards_hom", "events", "SPN_P"))
#  
#  # load MGDrivE2 on each socket
#  parallel::clusterEvalQ(cl=cl, expr={library(MGDrivE2)})

## ---- eval = FALSE------------------------------------------------------------
#  # run homogeneous simulations
#  PTS_hom_par <- parallel::clusterApplyLB(cl=cl, x=1:num_rep, fun=function(x){
#    # run trajectory
#    PTS_out_hom <- sim_trajectory_R(x0 = M0, tmax = tmax, dt = dt,
#                                    dt_stoch = dt_stoch, S = S, hazards = exact_hazards_hom,
#                                    sampler = "tau",events = events, verbose = FALSE)
#  
#    # summarize females/males for plotting  later
#    f_sum <- cbind(summarize_females(out = PTS_out_hom$state, spn_P = SPN_P),
#                   "rep"=x, "sex"="F")
#    m_sum <- cbind(summarize_males(out = PTS_out_hom$state), "rep"=x, "sex"="M")
#  
#    # return single dataframe
#    return(rbind(f_sum, m_sum))
#  })
#  
#  # stop cluster
#  parallel::stopCluster(cl)
#  
#  # summarize data
#  #  the Reduce() call generates a mean of the populations
#  PTS_hom_par_unroll <- do.call(what = rbind, args = PTS_hom_par)
#  PTS_hom_par_unroll_mean <- cbind(PTS_hom_par[[1]][ ,c("genotype", "sex", "time")],
#                                   "Mean" = Reduce(f = "+", x = lapply(X = PTS_hom_par,
#                                                                       FUN = function(x){x[,3]})
#                                                   )/num_rep)

## ---- eval = FALSE------------------------------------------------------------
#  # add mortality for plotting
#  PTS_inhom_par_unroll$death <- "inhomogeneous"
#  PTS_hom_par_unroll$death <- "homogeneous"
#  ODE_out_inhom_melt$death <- "inhomogeneous"
#  ODE_out_hom_melt$death <- "homogeneous"
#  
#  # plot everything together
#  ggplot(data = rbind(PTS_inhom_par_unroll, PTS_hom_par_unroll) ) +
#    geom_path(aes(x = time, y = value, group = interaction(rep, death),
#                  color = death), alpha = 0.15) +
#    facet_grid(genotype ~ sex, scales = "free_y") +
#    scale_color_manual(values = c("inhomogeneous" = "firebrick3",
#                                  "homogeneous" = "dodgerblue3")) +
#    # inhomogeneous mean
#    geom_line(data = PTS_inhom_par_unroll_mean, mapping = aes(x = time, y = Mean),
#              color = "firebrick4") +
#    # homogeneous mean
#    geom_line(data = PTS_hom_par_unroll_mean, mapping = aes(x = time, y = Mean),
#              color = "dodgerblue4") +
#    # ODE inhomogeneous
#    geom_line(data = ODE_out_inhom_melt, mapping = aes(x = time, y = value),
#              linetype = 2, color = "firebrick4") +
#    # ODE homogeneous
#    geom_line(data = ODE_out_hom_melt, mapping = aes(x = time, y = value),
#              linetype = 2, color = "dodgerblue4") +
#    # final formatting
#    theme_bw() +
#    ggtitle("Constant vs Seasonally Time-Varying Mortality Rates") +
#    guides(color = FALSE)

