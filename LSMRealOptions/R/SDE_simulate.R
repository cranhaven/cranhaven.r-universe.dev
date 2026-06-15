## GBM_simulate
#' Simulate the geometric Brownian motion (GBM) stochastic process through Monte Carlo simulation
#'
#' @description
#'
#' GBM is a commonly used stochastic process to simulate the price paths of stock prices and other assets, in which the log of the asset follows a random walk process with drift.
#' The \code{GBM_simulate} function utilizes antithetic variates as a simple variance reduction technique.
#'
#' @param n The total number of price paths to simulate
#' @param t The forecasting period, in years
#' @param mu The drift term of the GBM process
#' @param sigma The volatility term of the GBM process
#' @param S0 The initial value of the underlying asset
#' @param dt The discrete time step of observations, in years
#'
#'@details
#'
#'A stochastic process S(t) is a geometric brownian motion that follows the following continuous-time stochastic differential equation:
#'\deqn{ \frac{dS(t)}{S(t)} = \mu dt + \sigma dW(t)}{dS(t)/S(t) = mu dt + sigma dW(t)}
#'
#'Where \eqn{\mu}{'mu'} is the drift term, \eqn{\sigma}{'sigma'} the volatility term and \eqn{W_{t}}{W(t)} is defined as a Weiner process.
#'
#'The GBM is log-normally distributed.
#'
#'@return A matrix of simulated price paths of the GBM process. Each column corresponds to a simulated price path, and each
#'row corresponds to a simulated observed price of the simulated price paths at each discrete time period.
#'
#'@examples
#'## 100 simulations of 1 year of monthly price paths:
#'Simulated <- GBM_simulate(n = 100,
#'                          t = 1,
#'                          mu = 0.05,
#'                          sigma = 0.2,
#'                          S0 = 100,
#'                          dt = 1/12)
#'@export
GBM_simulate <- function(n, t, mu, sigma, S0, dt){

  if(any(!is.numeric(c(n,t,mu,sigma,S0,dt)))) stop("arguments must be numeric!")
  if(length(c(n,t,mu,sigma,S0,dt))>6) stop("arguments must be of length one!")

  #Calculations:
  n <- round(n)
  N_sim <- ifelse(n %% 2 == 0, n, n + 1)
  nloops <- N_sim/2
  time_periods <- seq(0, t, dt)
  nsteps <- length(time_periods) - 1

  ##Drift:
  drift <- (mu - 0.5 * sigma^2) * dt
  ##Shock:
  shock <- matrix(stats::rnorm((nsteps) * nloops, sd = sigma), ncol = nloops) * sqrt(dt)

  output <- matrix(NA, nrow = nsteps+1, ncol = N_sim)
  output[1,] <- log(S0)

  #Values:
  output[2:(nsteps+1), seq(1, N_sim, 2)] <- log(S0) + apply(drift + shock, MARGIN = 2, cumsum)
  #Antithetic Values:
  output[2:(nsteps+1), seq(2, N_sim, 2)] <- log(S0) + apply(drift - shock, MARGIN = 2, cumsum)

  return(exp(output[,1:n]))
}



## IGBM_simulate
#' Simulate the inhomogeneous geometric Brownian motion (IGBM) stochastic process through Monte Carlo simulation
#'
#' @description
#'
#' The inhomogeneous geometric Brownian motion, also known as the integrated GBM process, is a member of the general affine class of stochastic process that has been reported to be well suited for modelling energy prices.
#' The \code{IGBM_simulate} function utilizes antithetic variates as a simple variance reduction technique.
#'
#' @param n The total number of price paths to simulate
#' @param t The forecasting period, in years
#' @param reversion_rate The reversion rate term of the IGBM process
#' @param sigma The volatility term of the IGBM process
#' @param equilibrium The equilibrium term of the IGBM process
#' @param S0 The initial value of the underlying asset
#' @param dt The discrete time step of observations, in years
#'
#'A stochastic process S(t) is an IGBM that follows the following continuous-time stochastic differential equation:
#'\deqn{ dS(t) = reversion_rate(equilibrium - S(t)) dt + \sigma dW(t)}{dS(t) = reversion_rate(equilibrium - S(t)) dt + sigma dW(t)}
#'
#'Where 'reversion_rate' is the rate of reversion term, 'equilibrium' is the equilibrium value the process reverts towards, \eqn{\sigma}{'sigma'} the volatility term and \eqn{W_{t}}{W(t)} is defined as a Weiner process.
#'
#'
#'@return A matrix of simulated price paths of the IGBM process. Each column corresponds to a simulated price path, and each
#'row corresponds to a simulated observed price of the simulated price paths at each discrete time period.
#'
#'@examples
#'## 100 simulations of 1 year of monthly price paths:
#'Simulated <- IGBM_simulate(n = 100,
#'                          t = 1,
#'                          reversion_rate = 1,
#'                          sigma = 0.2,
#'                          equilibrium = 100,
#'                          S0 = 100,
#'                          dt = 1/12)
#'@export
IGBM_simulate <- function(n, t, reversion_rate, sigma, equilibrium,  S0, dt){

  if(any(!is.numeric(c(n,t,reversion_rate, equilibrium,sigma,S0,dt)))) stop("arguments must be numeric!")
  if(length(c(n,t,reversion_rate,equilibrium, sigma,S0,dt))>7) stop("arguments must be of length one!")

  #Calculations:
  n <- round(n)
  N_sim <- ifelse(n %% 2 == 0, n, n + 1)
  nloops <- N_sim/2
  time_periods <- seq(0, t, dt)
  nsteps <- length(time_periods) - 1

  ##Shock:
  shock <- matrix(stats::rnorm((nsteps) * nloops, sd = sigma), ncol = nloops) * sqrt(dt)

  output <- matrix(NA, nrow = nsteps+1, ncol = N_sim)
  output[1,] <- log(S0)


  for(t in 1:nrow(shock)){

    #Log-distribution:
    ##MR Drift:
    drift <- ((reversion_rate * (equilibrium - exp(output[t,])) / exp(output[t,])) - 0.5 * sigma^2 ) * dt

    ##Values:
    output[t, seq(1, N_sim, 2)] <-  output[t, seq(1, N_sim, 2)] +  drift[seq(1, N_sim, 2)] + shock[t,]
    ##Antithetic Values:
    output[t+1, seq(2, N_sim, 2)] =  output[t, seq(2, N_sim, 2)] +  drift[seq(2, N_sim, 2)] - shock[t,]
  }

  return(exp(output[,1:n]))
}



## GOU_simulate
#' Simulate the geometric Ornstein-Uhlenbeck (GOU) stochastic process through Monte Carlo simulation
#'
#'
#' @description
#'
#' The geometric Ornstein-Uhlenbeck process is a member of the general affine class of stochastic process. The Ornstein-Uhlenbeck process is a Gaussian process, a Markov process, is temporally homogeneous and exhibits mean-reverting behaviour.
#' The \code{IGBM_simulate} function utilizes antithetic variates as a simple variance reduction technique.
#'
#' @param n The total number of price paths to simulate
#' @param t The forecasting period, in years
#' @param reversion_rate The reversion rate term of the GOU process
#' @param sigma The volatility term of the GOU process
#' @param equilibrium The equilibrium term of the GOU process
#' @param risk_premium The risk premium of the GOU process
#' @param S0 The initial value of the underlying asset
#' @param dt The discrete time step of observations, in years
#'
#'A stochastic process S(t) is an IGBM that follows the following continuous-time stochastic differential equation:
#'\deqn{ dS(t)/ = reversion_rate(equilibrium - S(t)) dt + \sigma dW(t)}{dS(t) = reversion_rate(equilibrium - S(t)) dt + sigma dW(t)}
#'
#'\deqn{\frac{dS(t)}{S(t)} = equilibrium - (reversion_rate * S(t) + risk_premium) dt + \sigma dW(t)}{dS(t)/S(t) = dS(t)/S(t) = equilibrium + (- reversion_rate * S(t) - risk_premium) dt + sigma dW(t)}
#'
#'Where 'reversion_rate' is the rate of reversion term, 'equilibrium' is the equilibrium value the process reverts towards, 'risk_premium' is the risk premium of the process, \eqn{\sigma}{'sigma'} the volatility term and \eqn{W_{t}}{W(t)} is defined as a Weiner process.
#'
#'
#'@return A matrix of simulated price paths of the GOU process. Each column corresponds to a simulated price path, and each
#'row corresponds to a simulated observed price of the simulated price paths at each discrete time period.
#'
#'@examples
#'## 100 simulations of 1 year of monthly price paths:
#'Simulated <- GOU_simulate(n = 100,
#'                          t = 1,
#'                          reversion_rate = 1,
#'                          sigma = 0.2,
#'                          equilibrium = 100,
#'                          risk_premium = 0.05,
#'                          S0 = 100,
#'                          dt = 1/12)
#'@export
GOU_simulate <- function(n, t, reversion_rate, sigma, equilibrium, risk_premium,  S0, dt){

  if(any(!is.numeric(c(n,t,reversion_rate,sigma, equilibrium, risk_premium, S0,dt)))) stop("arguments must be numeric!")
  if(length(c(n,t,reversion_rate,sigma, equilibrium, risk_premium, S0,dt))>8) stop("arguments must be of length one!")

  #Calculations:
  n <- round(n)
  N_sim <- ifelse(n %% 2 == 0, n, n + 1)
  nloops <- N_sim/2
  time_periods <- seq(0, t, dt)
  nsteps <- length(time_periods) - 1

  output <- matrix(NA, nrow = nsteps+1, ncol = N_sim)
  output[1,] <- log(S0) - log(equilibrium)

  ###Mean-Reverting/Ornstein-Uhlenbeck:
  mu <- - risk_premium / reversion_rate
  shock <- matrix(stats::rnorm((nsteps) * nloops, sd = sigma), ncol = nloops) * sqrt(dt)

  for(loop in 2:(nsteps+1)){
    #Values:
    output[loop, seq(1, N_sim, 2)] <- output[loop-1, seq(1, N_sim, 2)] +
      reversion_rate * (mu - output[loop-1, seq(1, N_sim, 2)]) * dt +
      shock[loop-1,]

    #Antithetic Values:
    output[loop, seq(2, N_sim, 2)] <- output[loop-1, seq(2, N_sim, 2)] +
      reversion_rate * (mu - output[loop-1, seq(2, N_sim, 2)]) * dt -
      shock[loop-1,]
  }

  return(exp(log(equilibrium) + output[,1:n]))

}
