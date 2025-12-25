# ------------------------------------------------------------------------------
# Copyright (C) 2019 University of Liège
# <penPHcure is an R package for for estimation, variable selection and 
#  simulation of the semiparametric proportional-hazards (PH) cure model with 
#  time-varying covariates.>
# Authors: Alessandro Beretta & Cédric Heuchenne
# Contact: a.beretta@uliege.be
# 
# Licence as published by the Free Software Foundation, either version 3 of the 
# Licence, or any later version. This program is distributed in the hope that it 
# will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General 
# Public Licence for more details. You should have received a copy of the GNU 
# General Public Licence along with this program.
# If not, see <http://www.gnu.org/licenses/>.
# ------------------------------------------------------------------------------
#
#' @title Simulation of a PH cure model with time-varying covariates
#' @description This function allows to simulate data from a PH cure model with time-varying covariates:
#'  \itemize{
#'   \item the event-times are generated on a continuous scale from a piecewise exponential distribution conditional on time-varying covariates and regression coefficients \code{beta0}, using a method similar to the one described in \emph{Hendry (2014)}. The time varying covariates are constant in the intervals \eqn{(s_{j-1},s_j]}, for \eqn{j=1,,,,J}.
#'   \item the censoring times are generated from an exponential distribution truncated above \eqn{s_J};
#'   \item the susceptibility indicators are generated from a logistic regression model conditional on time-invariant covariates and regression coefficients \code{b0}.
#'  }
#'  
#' @param N the sample size (number of individuals). By default, \code{N = 500}.
#' @param S a numeric vector containing the end of the time intervals, in ascending order, over which the time-varying covariates are constant (the first interval start at 0). By default, \code{S = seq(0.1, 5, by=0.1)}.
#' @param beta0 a numeric vector with the true regression coefficients in the latency (survival)  component, used to generate the event times. By default, \code{beta0 = c(1,0,-1,0)}.
#' @param b0 a numeric vector with the true coefficients in the incidence (cure) component, used to generate the susceptibility indicators. By default, \code{b0 = c(1.2,-1,0,1,0)}.
#' @param lambdaC a positive numeric value, parameter of the truncated exponential distribution used to generate the censoring times. By default, \code{lambdaC = 1}.
#' @param gamma a positive numeric value, parameter controlling the shape of the baseline hazard function: \eqn{\lambda_0(t) = \gamma t^{\gamma-1}}. By default, \code{gamma = 1}.
#' @param mean_CURE a numeric vector of means for the variables used to generate the susceptibility indicators. By default, all zeros.
#' @param mean_SURV a numeric vector of means for the variables used to generate the event-times. By default, all zeros.
#' @param sd_CURE a numeric vector of standard deviations for the variables used to generate the susceptibility indicators. By default, all ones.
#' @param sd_SURV a numeric vector of standard deviations for the variables used to generate the event-times. By default, all ones.
#' @param cor_CURE the correlation matrix of the variables used to generate the susceptibility indicators. By default, an identity matrix.
#' @param cor_SURV the correlation matrix of the variables used to generate the event-times. By default, an identity matrix.
#' @param X [optional] a matrix of time-invariant covariates used to generate the susceptibility indicators, with dimension \code{N} by \code{length(b0)-1L}. By default, \code{X = NULL}.
#' @param Z [optional] an array of time-varying covariates used to generate the censoring times, with dimension \code{length(S)} by \code{length(beta)} by \code{N}. By default, \code{Z = NULL}.
#' @param C [optional] a vector of censoring times with \code{N} elements.  By default, \code{C = NULL}.
#' 
#' @return A \code{data.frame} with columns:
#'  \item{\code{id}}{unique ID number associated to each individual.}
#'  \item{\code{tstart}}{start of the time interval.}
#'  \item{\code{tstop}}{end of the time interval.}
#'  \item{\code{status}}{event indicator, 1 if the event occurs or 0, otherwise.}
#'  \item{\code{z.?}}{one or more columns of covariates used to generate the survival times.}
#'  \item{\code{x.?}}{one or more columns of covariates used to generate the susceptibility indicator (constant over time).}
#'  In addition, it contains the following attributes:
#'  \item{\code{perc_cure}}{Percentage of individuals not susceptible to the event of interest.}
#'  \item{\code{perc_cens}}{Percentage of censoring.}
#'  
#' @details 
#'  By default, the time-varying covariates in the latency (survival) component are generated from a multivariate normal distribution with means \code{mean_SURV}, standard deviations \code{sd_SURV} and correlation matrix \code{cor_SURV}. Otherwise, they can be provided by the user using the argument \code{Z}. In this case, the arguments \code{mean_SURV}, \code{sd_SURV} and \code{cor_SURV} will be ignored. 
#'  
#'  By default, the time-invariant covariates in the incidence (cure) component are generated from a multivariate normal distribution with means \code{mean_CURE}, standard deviations \code{sd_CURE} and correlation matrix \code{cor_CURE}. Otherwise, they can be provided by the user using the argument \code{X}. In this case, the arguments \code{mean_CURE}, \code{sd_CURE} and \code{cor_CURE} will be ignored. 
#' 
#' @references
#'  \insertRef{Hendry_2014}{penPHcure}
#' 
#' @details 
#'  
#' @examples 
#' ### Example 1:
#' ###  - event-times generated from a Cox's PH model with unit baseline hazard
#' ###    and time-varying covariates generated from independent standard normal 
#' ###    distributions over the intervals (0,s_1], (s_1,s_2], ..., (s_1,s_J]. 
#' ###  - censoring times generated from an exponential distribution truncated 
#' ###    above s_J.
#' ###  - covariates in the incidence (cure) component generated from independent 
#' ###    standard normal distributions.
#' 
#' # Define the sample size
#' N <- 250
#' # Define the time intervals for the time-varying covariates
#' S <- seq(0.1, 5, by=0.1)
#' # Define the true regression coefficients (incidence and latency)  
#' b0 <- c(1,-1,0,1,0)
#' beta0 <- c(1,0,-1,0)
#' # Define the parameter of the truncated exponential distribution (censoring) 
#' lambdaC <- 1.5
#' # Simulate the data
#' data1 <- penPHcure.simulate(N = N,S = S,
#'                             b0 = b0,
#'                             beta0 = beta0,
#'                             lambdaC = lambdaC)
#'
#'                            
#' ### Example 2:
#' ###  Similar to the previous example, but with a baseline hazard function 
#' ###   defined as lambda_0(t) = 3t^2.
#' 
#' # Define the sample size
#' N <- 250
#' # Define the time intervals for the time-varying covariates
#' S <- seq(0.1, 5, by=0.1)
#' # Define the true regression coefficients (incidence and latency)  
#' b0 <- c(1,-1,0,1,0)
#' beta0 <- c(1,0,-1,0)
#' # Define the parameter controlling the shape of the baseline hazard function
#' gamma <- 3
#' # Simulate the data
#' data2 <- penPHcure.simulate(N = N,S = S,
#'                             b0 = b0,
#'                             beta0 = beta0,
#'                             gamma = gamma)
#' 
#' 
#' ### Example 3:
#' ###  Simulation with covariates in the cure and survival components generated
#' ###   from multivariate normal (MVN) distributions with specific means, 
#' ###   standard deviations and correlation matrices.
#' 
#' # Define the sample size
#' N <- 250
#' # Define the time intervals for the time-varying covariates
#' S <- seq(0.1, 5, by=0.1)
#' # Define the true regression coefficients (incidence and latency)  
#' b0 <- c(-1,-1,0,1,0)
#' beta0 <- c(1,0,-1,0)
#' # Define the means of the MVN distribution (incidence and latency)  
#' mean_CURE <- c(-1,0,1,2)
#' mean_SURV <- c(2,1,0,-1)
#' # Define the std. deviations of the MVN distribution (incidence and latency)  
#' sd_CURE <- c(0.5,1.5,1,0.5)
#' sd_SURV <- c(0.5,1,1.5,0.5)
#' # Define the correlation matrix of the MVN distribution (incidence and latency)  
#' cor_CURE <- matrix(NA,4,4)
#' for (p in 1:4)
#'   for (q in 1:4)
#'     cor_CURE[p,q] <- 0.8^abs(p - q)
#' cor_SURV <- matrix(NA,4,4)
#' for (p in 1:4)
#'   for (q in 1:4)
#'     cor_SURV[p,q] <- 0.8^abs(p - q)
#' # Simulate the data
#' data3 <- penPHcure.simulate(N = N,S = S,
#'                             b0 = b0,
#'                             beta0 = beta0,
#'                             mean_CURE = mean_CURE,
#'                             mean_SURV = mean_SURV,
#'                             sd_CURE = sd_CURE,
#'                             sd_SURV = sd_SURV,
#'                             cor_CURE = cor_CURE,
#'                             cor_SURV = cor_SURV)
#' 
#' 
#' ### Example 4:
#' ###  Simulation with covariates in the cure and survival components from a 
#' ###   data generating process specified by the user. 
#' 
#' # Define the sample size
#' N <- 250
#' # Define the time intervals for the time-varying covariates
#' S <- seq(0.1, 5, by=0.1)
#' # Define the true regression coefficients (incidence and latency)  
#' b0 <- c(1,-1,0,1,0)
#' beta0 <- c(1,0,-1,0)
#' # As an example, we simulate data with covariates following independent
#' #  standard uniform distributions. But the user could provide random draws 
#' #  from any other distribution. Be careful!!! X should be a matrix of size 
#' #  N x length(b0) and Z an array of size length(S) x length(beta0) x N.
#' X <- matrix(runif(N*(length(b0)-1)),N,length(b0)-1)
#' Z <- array(runif(N*length(S)*length(beta0)),c(length(S),length(beta0),N))
#' data4 <- penPHcure.simulate(N = N,S = S,
#'                             b0 = b0,
#'                             beta0 = beta0,
#'                             X = X,
#'                             Z = Z)
#' 
#' 
#' ### Example 5:
#' ###  Simulation with censoring times from a data generating process 
#' ###   specified by the user
#' 
#' # Define the sample size
#' N <- 250
#' # Define the time intervals for the time-varying covariates
#' S <- seq(0.1, 5, by=0.1)
#' # Define the true regression coefficients (incidence and latency)  
#' b0 <- c(1,-1,0,1,0)
#' beta0 <- c(1,0,-1,0)
#' # As an example, we simulate data with censoring times following
#' #  a standard uniform distribution between 0 and S_J.
#' #  Be careful!!! C should be a numeric vector of length N.
#' C <- runif(N)*max(S)
#' data5 <- penPHcure.simulate(N = N,S = S,
#'                             b0 = b0,
#'                             beta0 = beta0,
#'                             C = C)
#'                            
#' @importFrom stats runif
#' @importFrom MASS mvrnorm
#' 
#' @export
penPHcure.simulate <- function(N=500,
                               S=seq(0.1, 5, by=0.1),
                               b0=c(1.2,-1,0,1,0),
                               beta0=c(1,0,-1,0),
                               gamma = 1,
                               lambdaC = 1,
                               mean_CURE = rep(0,length(b0)-1L),
                               mean_SURV = rep(0,length(beta0)),
                               sd_CURE = rep(1,length(b0)-1L),
                               sd_SURV = rep(1,length(beta0)),
                               cor_CURE = diag(length(b0)-1L),
                               cor_SURV = diag(length(beta0)),
                               X=NULL,Z=NULL,C=NULL){
  
  if (gamma<=0 || !is.finite(gamma))
    stop("penPHcure.simulate :: argument 'gamma' not correctly specified.
         It should be a numeric value greater than 0.")
  
  if (is.null(X)){
    cov_CURE <- diag(sd_CURE) %*% cor_CURE %*% diag(sd_CURE)
    X <- mvrnorm(N,mu = mean_CURE,Sigma = cov_CURE)
  } else if (!is.matrix(X) || ncol(X)!=(length(b0)-1L) || nrow(X)!=N){
    stop('penPHcure.simulate :: argument X missing or not correctly specified.
         It should be a matrix of dimensions ', N,'x',length(b0)-1,'.')
  }
  
  if (is.null(Z)){
    cov_SURV <- diag(sd_SURV) %*% cor_SURV %*% diag(sd_SURV)
    Z <- array(NA,c(length(S),length(beta0),N))
    for (i in 1:N){
      Z[,,i] <- mvrnorm(length(S),mu = mean_SURV,Sigma = cov_SURV)
    }
  } else {
    dimZ <- dim(Z)
    if (!is.array(Z) || dimZ[1]!=length(S) || 
        dimZ[2]!=length(beta0) || dimZ[3]!=N)
      stop('penPHcure.simulate :: argument Z missing or not correctly specified.
           It should be an array of dimensions ', 
           length(S),'x',length(beta0),'x',N,'.')
  }
  
  if (is.null(C)) 
    C <- -log(1-runif(N)*(1-exp(-lambdaC*max(S))))/lambdaC
  else if (!is.vector(C) || length(C)!=N)
    stop('penPHcure.simulate :: argument C should be a vector of lenght ',N,'.')
  
  # datagen <- datagen_cure_cpp(beta0,b0,lambda0,S,N,Z,X,C)
  datagen <- datagen_cure_cpp(beta0,b0,lambdaC,S,N,Z,X,C,gamma)
  
  data <- data.frame(datagen$data)
  
  colnames(data) <- c("id","tstart","tstop","status",
                      paste("z.",1:length(beta0),sep=''),
                      paste("x.",1:(length(b0) - 1L),sep=''))
  
  attr(data,"perc_cure") <- datagen$perc_cure
  attr(data,"perc_cens") <- datagen$perc_cens
  
  return(data)
}