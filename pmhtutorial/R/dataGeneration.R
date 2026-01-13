##############################################################################
# Generating data from a LGSS model
#
# Johan Dahlin <uni (at) johandahlin.com.nospam>
# Documentation at https://github.com/compops/pmh-tutorial
# Published under GNU General Public License
##############################################################################

#' Generates data from a linear Gaussian state space model
#' @description 
#' Generates data from a specific linear Gaussian state space model of the form 
#' \eqn{ x_{t} = \phi x_{t-1} + \sigma_v v_t } and \eqn{ y_t = x_t + 
#' \sigma_e e_t }, where \eqn{v_t} and \eqn{e_t} denote independent standard 
#' Gaussian random variables, i.e. \eqn{N(0,1)}.
#' @param theta The parameters \eqn{\theta=\{\phi,\sigma_v,\sigma_e\}} of the 
#' LGSS model. The parameter \eqn{\phi} that scales the current state in 
#' the state dynamics is restricted to [-1,1] to obtain a stable model. 
#' The standard deviations of the state process noise \eqn{\sigma_v} 
#' and the observation process noise \eqn{\sigma_e} must be positive.
#' @param noObservations The number of time points to simulate.
#' @param initialState The initial state.
#'
#' @return
#' The function returns a list with the elements: 
#' \itemize{
#' \item{x: The latent state for \eqn{t=0,...,T}.}
#' \item{y: The observation for \eqn{t=0,...,T}.}
#' }
#' @references 
#' Dahlin, J. & Schon, T. B. "Getting Started with Particle 
#' Metropolis-Hastings for Inference in Nonlinear Dynamical Models." 
#' Journal of Statistical Software, Code Snippets,
#' 88(2): 1--41, 2019.
#' @author 
#' Johan Dahlin \email{uni@@johandahlin.com}
#' @keywords 
#' datagen
#' @export
#' @importFrom stats rnorm

generateData <- function(theta, noObservations, initialState) 
{
  phi <- theta[1] 
  sigmav <- theta[2]
  sigmae <- theta[3]
  
  state <- matrix(0, nrow = noObservations + 1, ncol = 1)
  observation <- matrix(0, nrow = noObservations + 1, ncol = 1)
  
  state[1] <- initialState
  observation[1] <- NA
  
  for (t in 2:(noObservations + 1)) {
    state[t] <- phi * state[t - 1] + sigmav * rnorm(1)
    observation[t] <- state[t] + sigmae * rnorm(1)
  }
  
  list(x = state, y = observation)
}