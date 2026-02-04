#' Estimate the incubation or latency time of an infectious disease,
#' i.e. a doubly interval censored time-to-event
#'
#' @description
#' Estimate the distribution of doubly interval censored observations of
#' time-to-event allowing for (i) constant risk of initial event within the
#' window containing the time origin or a risk according to exponential growth
#' (as for infection risk in the beginning of an outbreak); (ii) different
#' shapes of the distribution (gamma, generalized gamma,Weibull); (iii) right
#' truncation; (iv) (partial) overlap of the two windows. Provides estimates of
#' the mean, median, 95th percentile and parameters, as well as diagnostics.
#'
#' @param dat data.frame with one row per individual and the variables L0, L1, R0,
#'  R1 representing the left and right window containing the time origin and
#'  endpoint, respectively. When right truncation needs to be addressed, an
#'  additional variable Trunc is required.
#' @param infection_risk_distribution either exponential growth ("exp_growth")
#' or a constant risk of infection ("constant") is assumed within the exposure
#' window.
#' @param exp_growth_rate when exponential growth is assumed, the estimated
#' growth factor r.
#' @param exp_growth_rate_SE the Standard Error of the estimated growth factor.
#' @param method assumed distribution for the time-to-event; can be "gamma",
#' "GenGamma" (generalized gamma) or "Weibull".
#' @param percentiles the percentiles of interest as a vector with probabilities.
#' @param right_truncation whether right truncation occurred in the data (T) or not
#'  (F); an additional variable 'Trunc' in the data represents the calendar
#'  truncation time.
#' @param iters the number of iterations for the MCMC chain.
#' @param burnin_period burnin_period, i.e. the number of initial iterationals to be
#' removed before analyzing the chains.
#' @param thin a thinning factor, meaning that every so many iterations is saved.
#' @param further_thin_plots additional thinning factor for plots (default is 10).
#' @param plot_rm_burnin omits the burnin period from the diagnostic plots,
#' as these iterations are removed from the actual analysis (default is T).
#'
#' @return A list: the estimates including Gelman diagnostic criterion;
#' the settings that were used to run the model; a diagnostic plot with the
#' running quantiles per parameter; a diagnostic plot with the running parameter
#' estimates.
#'
#' @details
#' The function estimates in the Bayesian framework, running JAGS via R and
#' employing three parallel Markov Chain Monte Carlo chains per model. We
#' extended the code by Charniga et al. (2022). The code for the diagnostic
#' plots is written by Ronald Geskus.
#'
#' @references Stacy, E. W., and G. A. Mihram, Parameter estimation for a
#' generalized gamma distribution, Technometrics, 7 (3), 349–358,
#' doi:10.1080/00401706.1965.10490268, 1965
#' @references Charniga, K., et al., Estimating the incubation period of
#' monkeypox virus during the 2022 multi-national outbreak, medRxiv,
#' doi:10.1101/2022.06.22.22276713, 2022
#' @references LeBauer et al., Translating Probability Density
#' Functions: From R to BUGS and Back Again, The R Journal, 2013
#' @references Plummer, M., JAGS user manual, 2017
#' \url{https://people.stat.sc.edu/hansont/stat740/jags_user_manual.pdf}
#' @references Rubio, J.F, The Generalised Gamma Distribution, 2020
#' \url{https://rpubs.com/FJRubio/GG}
#' @author Vera Arntzen, \email{v.h.arntzen@@math.leidenuniv.nl}
#' @keywords survival
#' @import rjags
#' @import tidyverse
#' @import coda
#' @import ggplot2
#' @import dplyr
#' @importFrom graphics par
#' @importFrom stats median
#' @importFrom stats qgamma
#' @importFrom stats quantile
#' @importFrom stats runif
#' @examples
#'
#'  # NB: the example takes a short while to run.
#'
#'  # Draw an exposure window width 1, 2, 3, 4, 5
#'  L1 <- sample(1:5, 100, replace = TRUE)
#'
#'  # Draw the infection moment from a uniform distribution on (L0, L1)
#'  L <- runif(100, 0, L1)
#'
#'  # Draw latency times (as estimated by Xin et al., 2022)
#'  times <- rgamma(100, shape = 4.05, rate = 0.74)
#'  R <- L + times
#'
#'  # Draw end of quarantine (last test moment)
#'  Q <- L1 + sample( c(5, 10, 15, 20, 25), 100, replace = TRUE)
#'
#'  # Define the data set
#'  mydat <- data.frame(R = R, L0 = 0, L1 = L1,
#'                      R0 = floor(R), R1 = floor(R + 1), Trunc = Q)
#'
#'  # Apply the truncation
#'  mydat <- mydat[which( (mydat$R > mydat$Trunc) == FALSE), ]
#'  mydat$R <- NULL
#'
#'  # If exposure ends after the last possible moment of the endpoint, end
#'  # exposure earlier
#'  mydat$L1 <- ifelse(mydat$L1 > mydat$R1, mydat$R1, mydat$L1)
#'
#'  # Run the model with truncation
#'   \donttest{Estimate_doublIn(dat = mydat,
#'  infection_risk_distribution = "constant",
#'  method = "gamma", percentiles = c(0.5, 0.9, 0.95, 0.99),
#'  right_truncation = TRUE, iters = 1000,
#'  burnin_period = 10, thin = 1,
#'  further_thin_plots = 1)}
#'
#'
#' @export
Estimate_doublIn <- function(dat,
  infection_risk_distribution = "constant", exp_growth_rate = NULL,
  exp_growth_rate_SE = NULL, method = "GenGamma",
  percentiles = c(0.5, 0.9, 0.95, 0.99),
  right_truncation = FALSE, iters = 5000, burnin_period = 250,
  thin = 1, further_thin_plots = 10, plot_rm_burnin = TRUE){

  # - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ -

  # Estimate doubly interval censored time-to-event

  # - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ -

# . . . . . General checks . . . . . . . . . . . . . . . . . . . . . . . . . . .

if(iters/thin < burnin_period){error("Burnin_period cannot be shorter than the
ratio of iterations/thinning. Please choose other values.")}

if( (right_truncation == T) & ("Trunc" %in% colnames(dat) == F) ){
error("To address right truncation, please add a variable named 'Trunc'.")}

if( is(dat, "data.frame") == FALSE ){error("'dat' must be a data.frame.")}

if( sum("L0" %in% colnames(dat), "L1" %in% colnames(dat),
        "R0" %in% colnames(dat), "R1" %in% colnames(dat)) != 4 ){
        error("One or multiple columns are missing: L0, L1, R0, R1.")}

if( sum(dat$L1 > dat$R1) > 0 ){
    error("One or multiple exposure windows end after the window containing the
          endpoint.")}

if( sum(is.na(dat %>% select(L0, L1, R0, R1))) > 0 ){
    error("There is at least one missing in columns L0, L1, R0, R1.")}

if( sum( dat$L1 < dat$L0 | dat$R1 < dat$R0 ) > 0 ){
    error("The end of a window cannot occur before its beginning.")}

# . . . . . Load helper functions . . . . . . . . . . . . . . . . . . . . . . .

  gg_running <- function(mcmc, probs=c(0.025, 0.5, 0.975), thin=1,
                         quantile = TRUE, burnin_period = 0, rm_burnin = T){
    ## plots chains (if quantile=FALSE) or running quantiles (if quantile=TRUE)
    ## in ggplot2
    ## mcmc: list of mcarray's (JAGS) or the sims.array component of OpenBUGS
    ## results
    ## chains: list, each chain one component, each component and array with
    ## n.iter rows and n.col parameters
    ## vars: the names of the parameters

    theme_set(theme_bw())
    cquantile <- function(z, probs) {
      cquant <- matrix(0, nrow = length(z), length(probs))
      for (i in seq(along = z))
        cquant[i, ] <- quantile(z[1:i], probs = probs, names = FALSE)
      cquant <- as.data.frame(cquant)
      names(cquant) <- c("lower", "median", "upper")
      return(cquant)
    }
    sels <- 1:dim(mcmc)[2]
    L <- dim(mcmc)[1]
    if(rm_burnin == F){ show <- seq(1,L,by=thin) } else {
      show <- seq(burnin_period + 1,L,by=thin)}
    n.chain <- dim(mcmc)[3]
    chains <- vector(mode="list",length=n.chain)
    for(i in 1:n.chain){
      chains[[i]] <- mcmc[show,sels,i]
    }
    var.names <- dimnames(mcmc)[[2]][sels]
    var.names <- factor(var.names, levels=var.names, labels=
                          c("par. 1", "par. 2", "par. 3")[1:length(var.names)])

    if(rm_burnin == F){
      Cumul <- data.frame(iter = seq(1,L,by=thin),
                          values = unlist(chains),
                          chain = as.factor(rep(1:length(chains),
                                                rep(prod(dim(chains[[1]])),n.chain))),
                          vars = rep(var.names, rep(length(chains[[1]][,1]),
                                                    length(var.names))))
    } else{
      Cumul <- data.frame(iter = seq(burnin_period + 1,L,by=thin),
                          values = unlist(chains),
                          chain = as.factor(rep(1:length(chains),
                                                rep(prod(dim(chains[[1]])),n.chain))),
                          vars = rep(var.names, rep(length(chains[[1]][,1]),
                                                    length(var.names))))
    }

    if(quantile){
      Cumul <- Cumul %>% group_by(vars,chain) %>% mutate(cquantile(values,
                                                                   c(0.025, 0.5, 0.975)))
      p_out <- ggplot(Cumul) +
        geom_line(aes(iter, median, group = chain, color = chain))+
        geom_line(aes(iter, lower, group=chain,color=chain))+
        geom_line(aes(iter, upper, group=chain,color=chain))

      if(rm_burnin == F){
        p_out <- p_out + geom_vline(xintercept = burnin_period, linetype = 2)
      }

      p_out <- p_out +
        labs(y = "2.5%, 50% and 97.5% percentile", x = "Iteration", color = "Chain") +
        scale_color_manual(values = c("mediumspringgreen", "maroon2", "darkblue")) +
        facet_wrap(~vars,scales="free_y")

    } else {
      p_out <- ggplot(Cumul) + geom_line(aes(iter, values, group = chain, color = chain),
                                         alpha=0.5)

      if(rm_burnin == F){
        p_out <- p_out + geom_vline(xintercept = burnin_period, linetype = 2)
      }

      p_out <- p_out +
        labs(y = "Parameter value", x = "Iteration", color = "Chain") +
        scale_color_manual(values = c("mediumspringgreen", "maroon2", "darkblue")) +
        facet_wrap(~vars,scales="free_y")+ theme(legend.position="null")

    }

  }

  qggamma <- function(p, theta, kappa, delta){

    ## Aim
    ## Compute the quantiles of the generalized gamma distribution

    ## Arguments
    ## p: quantile (probability, between 0 and 1)
    ## theta, kappa, delta: parameters (Stacy)

    ## Notes
    ## lambda = 1/theta; b = delta; r = kappa/delta

    out <- qgamma(p, shape = kappa/delta, scale = theta^delta)^(1/delta)
    return(out)
  }

# . . . . . Process the data . . . . . . . . . . . . . . . . . . . . . . . . . .

  # Start individual timelines from initiation of exposure
  dat$id <- 1:nrow(dat)
  dat$L1 <- dat$L1 - dat$L0;  dat$R0 <- dat$R0 - dat$L0;
  dat$R1 <- dat$R1 - dat$L0
  if(right_truncation == T){ dat$Trunc <- dat$Trunc - dat$L0 }
  dat$L0 <- 0

  # Guarantee doubly interval censored format:
  if( sum(dat$L0 == dat$L1) != 0 ){
    message("Some start window(s) are of width zero;
            please increase the width to use this function. Evaluation ended.")
    }
  if( sum(dat$R0 == dat$R1) != 0 ){
    message("Some end window(s) are of width zero;
            please increase the width to use this function. Evaluation ended.")
    }

  # Change names:
  dat$EL <- dat$L0; dat$ER <- dat$L1; dat$SL <- dat$R0; dat$SR <- dat$R1

  # Censoring type for endpoint:
  dat$type_S <- 1 # for interval censored: both min and max present.

  # Select the data for JAGS:
  if(right_truncation == T){
  jags_data <- list(
    ER = dat$ER, SL = dat$SL, SR = dat$SR, Trunc = dat$Trunc, type_S =
      dat$type_S
  )} else {
    jags_data <- list(
      ER = dat$ER, SL = dat$SL, SR = dat$SR, type_S = dat$type_S
    )
  }

# . . . . . Set initial values of E and Y . . . . . . . . . . . . . . . . . . .

  N <- nrow(dat)
  jags_data$Y <- rep(NA, N)
  E_init <- Y_init <- rep(NA, N)

  for(i in 1:N){

  if(dat$EL[i] == dat$ER[i] & dat$SL[i] == dat$SR[i]){

  # when windows completely overlap
  tmp_inits <- runif(2, min = dat$EL[i], max = dat$ER[i])
  E_init[i] <- min(tmp_inits)
  Y_init[i] <- max(tmp_inits) - E_init[i]

  # when the windows do not (completely) overlap
  }  else{
  E_init[i] <- runif(1, min = dat$EL[i], max = min(dat$ER[i], dat$SL[i]) )
  Y_init[i] <- runif(1, min = max(dat$ER[i], dat$SL[i]),
                        max = dat$SR[i]) - E_init[i]}

  }
  E_star_init <- jags_data$ER - E_init

# Set the parameters we want to track
          if(method == "GenGamma"){ parameters <- c("r", "lambda_gg", "b")
          } else if(method == "gamma"){ parameters <- c("r", "lambda_gg")
          } else if(method == "Weibull"){ parameters <- c("lambda_gg", "b")}

# . . . . . Models . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# without truncation

  model_GenGamma_constant <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(r, lambda_gg, b)
  }
  r ~ dnorm(0,1/1000)I(0, )
  b ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
}", sep = "")

  model_GenGamma_exponential <- paste("
model {
  for(i in 1:", N, "){
           type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(r, lambda_gg, b)
  }
  r ~ dnorm(0,1/1000)I(0, )
  b ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)
}", sep = "")

  model_gamma_constant <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(r, lambda_gg, 1)
  }
  r ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
}", sep = "")

  model_gamma_exponential <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(r, lambda_gg, 1)
  }
  r ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)
}", sep = "")

  model_Weibull_constant <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(1, lambda_gg, b)
  }
  b ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
}", sep = "")

  model_Weibull_exponential <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(1, lambda_gg, b)
  }
  b ~ dnorm(0,1/1000)I(0, )
  lambda_gg ~ dnorm(0,1/1000)I(0, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)
}", sep = "")

# with truncation

  model_GenGamma_constant_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(r, lambda_gg, b)T(0, Trunc[i] - E[i])
  }
  r ~ dnorm(0,1/1000)I(0.000000000001, )
  b ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
}", sep = "")

  model_GenGamma_exponential_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(r, lambda_gg, b)T(0, Trunc[i] - E[i])
  }
  r ~ dnorm(0,1/1000)I(0.000000000001, )
  b ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)
}", sep = "")

  model_gamma_constant_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(r, lambda_gg, 1)T(0, Trunc[i] - E[i])
}
  r ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
}", sep = "")

  model_gamma_exponential_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(r, lambda_gg, 1)T(0, Trunc[i] - E[i])
  }
  r ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)
}", sep = "")

  model_Weibull_constant_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E[i] ~ dunif(0, ER[i])
            Y[i] ~ dgen.gamma(1, lambda_gg, b)T(0, Trunc[i] - E[i])
  }
  b ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
}", sep = "")

  model_Weibull_exponential_trunc <- paste("
model {
  for(i in 1:", N, "){
            type_S[i] ~ dinterval( Y[i] , C[i, 1:2] )
            C[i,1] <- max( 0.000000001, (SL[i] - E[i]) )
            C[i,2] <- max( 0.000000001, (SR[i] - E[i]) )
            E_star[i] ~ dexp(exp_growth_rate) T(0, ER[i])
            E[i] <- ER[i] - E_star[i]
            Y[i] ~ dgen.gamma(1, lambda_gg, b)T(0, Trunc[i] - E[i])
  }
  b ~ dnorm(0,1/1000)I(0.000000000001, )
  lambda_gg ~ dnorm(0,1/1000)I(0.000000000001, )
  exp_growth_rate ~ dnorm(", exp_growth_rate, ", prec_r)
  prec_r <- 1/ (", exp_growth_rate_SE, "^ 2)

}", sep = "")

# . . . . . Select the model . . . . . . . . . . . . . . . . . . . . . . . . .

if(right_truncation == T){
  if( infection_risk_distribution == "constant" & method == "GenGamma"){
      model <- textConnection(model_GenGamma_constant_trunc)} else if(
      infection_risk_distribution == "exp_growth" & method == "GenGamma"){
      model <- textConnection(model_GenGamma_exponential_trunc)} else if(
      infection_risk_distribution == "constant" & method == "gamma"){
      model <- textConnection(model_gamma_constant_trunc)} else if(
      infection_risk_distribution == "exp_growth" & method == "gamma"){
      model <- textConnection(model_gamma_exponential_trunc)} else if(
      infection_risk_distribution == "constant" & method == "Weibull"){
      model <- textConnection(model_Weibull_constant_trunc)} else if(
      infection_risk_distribution == "exp_growth" & method == "Weibull"){
      model <- textConnection(model_Weibull_exponential_trunc)}
} else {
  if( infection_risk_distribution == "constant" & method == "GenGamma"){
      model <- textConnection(model_GenGamma_constant)} else if(
      infection_risk_distribution == "exp_growth" & method == "GenGamma"){
      model <- textConnection(model_GenGamma_exponential)} else if(
      infection_risk_distribution == "constant" & method == "gamma"){
      model <- textConnection(model_gamma_constant)} else if(
      infection_risk_distribution == "exp_growth" & method == "gamma"){
      model <- textConnection(model_gamma_exponential)} else if(
      infection_risk_distribution == "constant" & method == "Weibull"){
      model <- textConnection(model_Weibull_constant)} else if(
      infection_risk_distribution == "exp_growth" & method == "Weibull"){
      model <- textConnection(model_Weibull_exponential)}
}

# . . . . . Specify other initial values . . . . . . . . . . . . . . . . . . . .

 if(method == "GenGamma"){
    if(infection_risk_distribution == "constant"){
      jags_inits <-  function() {
        rc <- list(E = E_init, b = runif(1, 0, 1), r = runif(1, 0, 1),
                   lambda_gg = runif(1, 0, 1), Y = Y_init)
        return(rc)
      } } else if(infection_risk_distribution == "exp_growth"){
      jags_inits <-  function() {
        rc <- list(E_star = E_star_init, b = runif(1, 0, 1), r = runif(1, 0, 1),
                   lambda_gg = runif(1, 0, 1), Y = Y_init)
        return(rc)
      } }
  } else if(method == "gamma"){
    if(infection_risk_distribution == "constant"){
      jags_inits <-  function() {
        rc <- list(E = E_init, r = runif(1, 0, 1), lambda_gg = runif(1, 0, 1),
                   Y = Y_init)
        return(rc)
      } } else if(infection_risk_distribution == "exp_growth"){
      jags_inits <-  function() {
        rc <- list(E_star = E_star_init, r = runif(1, 0, 1),
                   lambda_gg = runif(1, 0, 1), Y = Y_init)
        return(rc)
      } }
  } else if(method == "Weibull"){
    if(infection_risk_distribution == "constant"){
      jags_inits <-  function() {
        rc <- list(E = E_init, b = runif(1, 0, 1), lambda_gg = runif(1, 0, 1),
          Y = Y_init)
        return(rc)
      } } else if(infection_risk_distribution == "exp_growth"){
      jags_inits <-  function() {
        rc <- list(E_star = E_star_init, b = runif(1, 0, 1),
                   lambda_gg = runif(1, 0, 1), Y = Y_init)
        return(rc)
      } }
  }

# . . . . . Run JAGS . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  model_jags <- jags.model(model, data = jags_data, inits = jags_inits,
                           n.chains = 3, quiet = F)
  model_samples <- coda.samples(model_jags, parameters, n.iter = iters,
                                  thin = thin, n.chains = 3)

  # Plots to check convergence
  # NB: If all is okay, for each of the three percentiles in the following figure
  # the chain-specific lines eventually overlap:
  model_samples_array <- as.array.mcmc.list(model_samples)
  p_running_quantiles <- gg_running(mcmc = model_samples_array,
                                    probs = c(0.025, 0.5, 0.975),
                                    thin = further_thin_plots, quantile = T,
                                    rm_burnin = plot_rm_burnin,
                                    burnin_period = burnin_period)
  # NB: If all is okay, the following figure looks like a blur:
  p_running_parests <- gg_running(mcmc = model_samples_array,
                                  probs = c(0.025, 0.5, 0.975), thin = further_thin_plots,
                                  burnin_period = burnin_period,
                                  rm_burnin = plot_rm_burnin,
                                  quantile = F)

  # Make all of the chains a single matrix with a burnin period removed

  samples_chain1 <- as.matrix(model_samples[[1]][,])[(burnin_period + 1):(iters/thin),]
  samples_chain2 <- as.matrix(model_samples[[2]][,])[(burnin_period + 1):(iters/thin),]
  samples_chain3 <- as.matrix(model_samples[[3]][,])[(burnin_period + 1):(iters/thin),]

  # Recreate MCMC object
  model_fit <- list(
    as.mcmc(samples_chain1),
    as.mcmc(samples_chain2),
    as.mcmc(samples_chain3)
  )
  chains_fit <- rbind(samples_chain1, samples_chain2, samples_chain3)
  colnames(chains_fit) <- varnames(model_fit[[1]])
  chains_fit <- as.data.frame(chains_fit)

  # Gelman diagnostic criterion
  # Rule of thumb: smaller than or equal to 1.1 means adequate convergence.
  temp <- gelman.diag(model_fit)
  dataframe <- as.data.frame(temp$psrf)
  gelman_tab <- dataframe[rownames(dataframe) %in% c("b", "r","lambda_gg"), ]
  colnames(gelman_tab) <- c("est", "upper_CI")
  gelman_tab$lower_CI <- NA
  rownames(gelman_tab) <- paste( "Gelman diag.: ", rownames(gelman_tab),
                                 sep = "")

# . . . . . Process the results . . . . . . . . . . . . . . . . . . . . . . .

if (method == "GenGamma"){
    inc_fit <- rbind(
      quantile(1/chains_fit$lambda_gg, # theta
        prob = c(0.5, 0.025, 0.975)),
      quantile(chains_fit$r * chains_fit$b, # kappa
        prob = c(0.5, 0.025, 0.975)),
      quantile(chains_fit$b,       # delta
        prob = c(0.5, 0.025, 0.975)),
      quantile( (gamma(chains_fit$r + (1/chains_fit$b))/ # mean
            (chains_fit$lambda_gg * gamma(chains_fit$r) )),
        prob = c(0.5, 0.025, 0.975)),
      quantile(((gamma(chains_fit$r + (2/chains_fit$b)) * gamma(chains_fit$r) -
                   gamma(chains_fit$r + 1/chains_fit$b)^2)/
            ( (chains_fit$lambda_gg^2) * gamma(chains_fit$r)^2 )), # variance
        prob = c(0.5, 0.025, 0.975))
    )
    for (q in percentiles){
      tmp <- qggamma(p = q, theta = 1/chains_fit$lambda_gg,
                     kappa = chains_fit$r * chains_fit$b,
                     delta = chains_fit$b)
      inc_fit <- rbind(inc_fit, c( median(tmp),
                                   quantile(tmp, prob = c(0.025, 0.975)) ) )
    }
} else if (method == "gamma"){
    inc_fit <- rbind(
      quantile(1/chains_fit$lambda_gg,  # theta
        prob = c(0.5, 0.025, 0.975)),
      quantile(chains_fit$r * 1, # kappa
        prob = c(0.5, 0.025, 0.975)),
      c(1, 1, 1), # delta
      quantile(
        (gamma(chains_fit$r + 1)/
            (chains_fit$lambda_gg * gamma(chains_fit$r) ) # mean
        ), prob = c(0.5, 0.025, 0.975)),
      quantile((
          (gamma(chains_fit$r + 2) * gamma(chains_fit$r) -
             gamma(chains_fit$r + 1)^2)/
            ( (chains_fit$lambda_gg^2) * gamma(chains_fit$r)^2 ) # variance
        ), prob = c(0.5, 0.025, 0.975))
    )
    for (q in percentiles){
      tmp <- qggamma(p = q, theta = 1/chains_fit$lambda_gg,
                     kappa = chains_fit$r, delta = 1)
      inc_fit <- rbind(inc_fit, c( median(tmp), quantile(tmp,
                                                         prob = c(0.025, 0.975))
                                   )
      )
    }
    } else if (method == "Weibull"){
    inc_fit <- rbind(
      quantile(1/chains_fit$lambda_gg, prob = c(0.5, 0.025, 0.975)),  # theta
      quantile(1 * chains_fit$b, prob = c(0.5, 0.025, 0.975)),  # kappa
      quantile(chains_fit$b, prob = c(0.5, 0.025, 0.975)),  # delta
      quantile( (gamma(1 + (1/chains_fit$b))/
            (chains_fit$lambda_gg * gamma(1) ) # mean
        ), prob = c(0.5, 0.025, 0.975)),
      quantile(         ((gamma(1 + (2/chains_fit$b)) * gamma(1) -
                            gamma(1 + 1/chains_fit$b)^2)/
            ( (chains_fit$lambda_gg^2) * gamma(1)^2 ) # variance
        ), prob = c(0.5, 0.025, 0.975)))
    for (q in percentiles){
      tmp <- qggamma(p = q, theta = 1/chains_fit$lambda_gg,
                     kappa = chains_fit$b, delta = chains_fit$b)
      inc_fit <- rbind(inc_fit, c( median(tmp), quantile(tmp,
                                                  prob = c(0.025, 0.975)) )
      )
    }
}

  colnames(inc_fit) <- c("est", "lower_CI", "upper_CI")
  rownames(inc_fit) <- c("theta", "kappa", "delta", "mean", "variance",
                           percentiles)
  inc_fit <- rbind(inc_fit, gelman_tab)
  inc_fit <- as.data.frame(inc_fit)
  inc_fit$par <- rownames(inc_fit)
  inc_fit$iters <- iters
  inc_fit$thinning <- thin
  inc_fit$burnin_period <- burnin_period
  inc_fit$f <- method
  inc_fit$g <- infection_risk_distribution
  inc_fit$right_truncation <- right_truncation

  estimates <- inc_fit %>% select(par, est, lower_CI, upper_CI)
  settings <- inc_fit %>% select(f, g, right_truncation, iters, thinning,
                                 burnin_period)

  out <- list(estimates = estimates, settings = settings,
              plot_running_quantiles = p_running_quantiles,
              plot_parameters = p_running_parests)

  close(model)

  return(out)

}
