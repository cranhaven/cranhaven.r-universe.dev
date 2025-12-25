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
#'@title Predict method for PHcure.object
#'@description Compute probabilities to be susceptible and survival probabilities (conditional on being susceptible) for a model fitted by \code{\link{penPHcure}} with the argument \code{pen.type = "none"}.
#'@param object an object of class \code{\link{PHcure.object}}.
#'@param newdata a data.frame in counting process format.
#'@param X [optional] a matrix of time-invariant covariates. It is not required, unless argument \code{X} was supplied in the call to the \code{\link{penPHcure}} function.
#'@param ... ellipsis to pass extra arguments.
#'@usage
#' \method{predict}{PHcure}(object, newdata, X = NULL,...)
#'@return An object of class \code{predict.PHcure}, a list including the following elements:
#' \item{\code{CURE}}{a numeric vector containing the probabilities to be susceptible to the event of interest: \deqn{P(Y_i=1|x_i) = \frac{ e^{\mathbf{x}_i'\hat{\mathbf{b}}}   }{1+e^{\mathbf{x}_i'\hat{\mathbf{b}}}},} where \eqn{\mathbf{x}_i} is a vector of time-invariant covariates and \eqn{\hat{\mathbf{b}}} is a vector of estimated coefficients.}
#' \item{\code{SURV}}{a numeric vector containing the survival probabilities (conditional on being susceptible to the event of interest): \deqn{S(t_i|Y_i=1,\bar{\mathbf{z}}_i(t))=\exp\left(-\sum_{j=1}^K (t_{(j-1)}-t_{(j)}) \hat{\lambda}_{0j} I(t_{(j)}\leq t_i) e^{\mathbf{z}_i(t_{(j)})\hat{\boldsymbol{\beta}}}\right),} where \eqn{t_{(1)}<t_{(2)}<...<t_{(K)}} denotes the \eqn{K} ordered event-times, \eqn{\mathbf{z}_i(t)} is a vector of time-varying covariates, \eqn{\hat{\boldsymbol{\beta}}} is a vector of estimated coefficients and \eqn{\hat{\lambda}_{0j}} is the estimated baseline hazard function (constant in the interval \eqn{(t_{(j-1)},t_{(j)}]}). }
#'
#'@details
#'If argument \code{X} was not supplied in the call to the \code{\link{penPHcure}} function, the probabilities to be susceptible are computed using the covariates retrieved using the same \code{which.X} method as in the \code{\link{penPHcure}} function call.
#'
#'@examples
#' # Generate some data (for more details type ?penPHcure.simulate in your console)
#' set.seed(12) # For reproducibility
#' data <- penPHcure.simulate(N=250)
#'  
#' # Fit standard cure model (without inference)
#' fit <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                       event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                  cureform = ~ x.1 + x.2 + x.3 + x.4,data = data)
#' 
#' # Use the predict method to obtain the probabilities for the fitted model
#' pred.fit <- predict(fit,data)
#' 
#' # Use the predict method to make prediction for new observations.
#' #  For example, two individuals censored at time 0.5 and 1.2, respectively,
#' #  and all cavariates equal to 1.
#' newdata <- data.frame(tstart=c(0,0),tstop=c(0.5,1.2),status=c(0,0),
#'                       z.1=c(1,1),z.2=c(1,1),z.3=c(1,1),z.4=c(1,1),
#'                       x.1=c(1,1),x.2=c(1,1),x.3=c(1,1),x.4=c(1,1))
#' pred.fit.newdata <- predict(fit,newdata)
#' # The probabilities to be susceptible are:
#' pred.fit.newdata$CURE
#' # [1] 0.6761677 0.6761677
#' # The survival probabilities (conditional on being susceptible) are:
#' pred.fit.newdata$SURV
#' # [1] 0.5591570 0.1379086
#' 
#' 
#'@export
predict.PHcure <- function(object, newdata, X = NULL,...){
  nobs <- nrow(newdata)
  Z <- model.matrix(object$survform,data=newdata)[,-1,drop=FALSE]
  SurvObj <- model.frame(object$survform,data=newdata)[,1]
  tstart <- SurvObj[,1]
  tstop <- SurvObj[,2]
  status <- SurvObj[,3]
  if(!is.null(object$cureform)){
    if (!is.null(X))
      warning('predict.PHcure :: argument X not required.')
    X <- model.matrix(object$cureform,data=newdata)[,drop=FALSE]
    init_cpp <- initialize_PHcure_cpp(tstart,tstop,status,X,object$which.X)
    X_FIX <- init_cpp$X_FIX
  } else{
    init_cpp <- initialize_PHcure_noX_cpp(tstart,tstop,status)
    if (is.null(X) || !is.matrix(X) || 
        nrow(X)!=init_cpp$N || ncol(X)!=(length(object$b)-1L))
      stop('predict.PHcure :: argument X not, or not correctly, specified.
           It should be a matrix with ',init_cpp$N,' rows and ',
           (length(object$b)-1L),' columns.')
    else X_FIX <- cbind(1,X)
  }
  
  extra <- list(...)
  if (!is.null(extra$b0) && !is.null(extra$beta0) && !is.null(extra$cumhaz0)){
    b <- extra$b0
    if(!is.vector(b) || any(!is.finite(b)) || length(b)!=ncol(X_FIX))
      stop('predict.PHcure :: extra argument "b" not correctly specified.
           It should be a numeric vector of length, ',ncol(X_FIX))
    beta <- extra$beta0
    if(!is.vector(beta) || any(!is.finite(beta)) || length(beta)!=ncol(Z))
      stop('predict.PHcure :: extra argument "beta" not correctly specified.
           It should be a numeric vector of length, ',ncol(Z))
    cumhaz <- extra$cumhaz0
    if(!is.vector(cumhaz) || any(!is.finite(cumhaz)) || 
       length(cumhaz)!=object$K)
      stop('predict.PHcure :: extra argument "cumhaz" not correctly specified.
           It should be a numeric vector of length, ',object$K)
  } else {
    b <- object$b
    beta <- object$beta
    cumhaz <- object$cumhaz
  }
  haz <- diff(c(0,cumhaz))
  event_times <- attr(object$cumhaz,"time")
  
  CURE <-  1/(1+exp(-X_FIX%*%b))
  SURV <- compute_survival_cpp(beta,haz,tstart,tstop,init_cpp$N,nobs,
                               event_times,Z,FALSE)

  output <- list(CURE = as.vector(CURE),
                 SURV = as.vector(SURV))
  class(output) <- 'predict.PHcure'
  return(output)
}