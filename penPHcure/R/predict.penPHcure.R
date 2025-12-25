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
#' @title Predict method for penPHcure.object
#' @description Compute probabilities to be susceptible and survival probabilities (conditional on being susceptible) for a model fitted by \code{\link{penPHcure}} with the argument \code{pen.type = "SCAD" | "LASSO"}.
#' @param object an object of class \code{\link{PHcure.object}}.
#' @param newdata a data.frame in counting process format.
#' @param crit.type a character string indicating the criterion used to select the tuning parameters, either \code{"AIC"} or \code{"BIC"}. By default \code{crit.type = "BIC"}.  
#' @param X [optional] a matrix of time-invariant covariates.
#' @param ... ellipsis to pass extra arguments.
#' @usage 
#'  \method{predict}{penPHcure}(object, newdata, crit.type=c("BIC","AIC"), X = NULL,...)
#' @return An object of class \code{predict.penPHcure}, a list including the following elements:
#' \item{\code{CURE}}{a numeric vector containing the probabilities to be susceptible to the event of interest: \deqn{P(Y_i=1|x_i) = \frac{ e^{\mathbf{x}_i'\hat{\mathbf{b}}}   }{1+e^{\mathbf{x}_i'\hat{\mathbf{b}}}},} where \eqn{\mathbf{x}_i} is a vector of time-invariant covariates and \eqn{\hat{\mathbf{b}}} is a vector of estimated coefficients.}
#' \item{\code{SURV}}{a numeric vector containing the survival probabilities (conditional on being susceptible to the event of interest): \deqn{S(t_i|Y_i=1,\bar{\mathbf{z}}_i(t))=\exp\left(-\sum_{j=1}^K (t_{(j-1)}-t_{(j)}) \hat{\lambda}_{0j} I(t_{(j)}\leq t_i) e^{\mathbf{z}_i(t_{(j)})\hat{\boldsymbol{\beta}}}\right),} where \eqn{t_{(1)}<t_{(2)}<...<t_{(K)}} denotes the \eqn{K} ordered event-times, \eqn{\mathbf{z}_i(t)} is a vector of time-varying covariates, \eqn{\hat{\boldsymbol{\beta}}} is a vector of estimated coefficients and \eqn{\hat{\lambda}_{0j}} is the estimated baseline hazard function (constant in the interval \eqn{(t_{(j-1)},t_{(j)}]}). }
#'@details
#'If the model selected by means of the BIC criterion differs from the one selected by the AIC criterion, with the argument \code{crit.type} it is possible to specify which model to use for the calculation of the probabilities. 
#'
#'If argument \code{X} was not supplied in the call to the \code{\link{penPHcure}} function, the probabilities to be susceptible are computed using the covariates retrieved using the same \code{which.X} method as in the \code{\link{penPHcure}} function call.
#'
#'@examples 
#' 
#' # Generate some data (for more details type ?penPHcure.simulate in your console)
#' set.seed(12) # For reproducibility
#' data <- penPHcure.simulate(N=250)
#' 
#' ### Tune penalized cure model with SCAD penalties
#' # First define the grid of possible values for the tuning parameters.
#' pen.tuneGrid <- list(CURE = list(lambda = c(0.01,0.03,0.05,0.07,0.09),
#'                                  a = 3.7),
#'                      SURV = list(lambda = c(0.01,0.03,0.05,0.07,0.09),
#'                                  a = 3.7))
#' # Tune the penalty parameters.
#' tuneSCAD <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                            event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                       cureform = ~ x.1 + x.2 + x.3 + x.4,
#'                       data = data,pen.type = "SCAD",
#'                       pen.tuneGrid = pen.tuneGrid,
#'                       print.details = FALSE)
#'                       
#' # Use the predict method to obtain the probabilities for the selected model.
#' #  By default, the model is the one selected on the basis of the BIC criterion.
#' pred.tuneSCAD.BIC <- predict(tuneSCAD,data)
#' # Otherwise, to return the probabilities for the model selected on the basis 
#' #  of the AIC criterion, the user can set argument crit.type = "AIC": 
#' pred.tuneSCAD.AIC <- predict(tuneSCAD,data,crit.type="AIC")
#' 
#' # Use the predict method to make prediction for new observations.
#' #  For example, two individuals censored at time 0.5 and 1.2, respectively,
#' #  and all cavariates equal to 1.
#' newdata <- data.frame(tstart=c(0,0),tstop=c(0.5,1.2),status=c(0,0),
#'                       z.1=c(1,1),z.2=c(1,1),z.3=c(1,1),z.4=c(1,1),
#'                       x.1=c(1,1),x.2=c(1,1),x.3=c(1,1),x.4=c(1,1))
#' pred.tuneSCAD.newdata.BIC <- predict(tuneSCAD,newdata)
#' pred.tuneSCAD.newdata.AIC <- predict(tuneSCAD,newdata,crit.type="AIC")
#' # The probabilities to be susceptible for the BIC selected model are:
#' pred.tuneSCAD.newdata.BIC$CURE
#' # [1] 0.6456631 0.6456631
#' # The probabilities to be susceptible for the AIC selected model are:
#' pred.tuneSCAD.newdata.BIC$CURE
#' # [1] 0.6456631 0.6456631
#' # The survival probabilities (conditional on being susceptible) for the BIC 
#' #  selected model are:
#' pred.tuneSCAD.newdata.BIC$SURV
#' # [1] 0.5624514 0.1335912
#' # The survival probabilities (conditional on being susceptible) for the AIC 
#' #  selected model are:
#' pred.tuneSCAD.newdata.AIC$SURV
#' # [1] 0.5624514 0.1335912
#' @export
predict.penPHcure <- function(object, newdata, crit.type=c("BIC","AIC"), X = NULL,...){
  crit.type <- match.arg(crit.type)
  if (crit.type=="BIC"){
    b <- object$BIC$b
    beta <- object$BIC$beta
    cumhaz <- object$BIC$cumhaz
    event_times <- attr(object$BIC$cumhaz,"time")
  } else {
    b <- object$AIC$b
    beta <- object$AIC$beta
    cumhaz <- object$AIC$cumhaz
    event_times <- attr(object$BIC$cumhaz,"time")
  }
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
        nrow(X)!=init_cpp$N || ncol(X)!=(length(b)-1L))
      stop('predict.PHcure :: argument X not, or not correctly, specified.
           It should be a matrix with ',init_cpp$N,' rows and ',
           length(b)-1L,' columns.')
    else X_FIX <- cbind(1,X)
  }
  
  
  haz <- diff(c(0,cumhaz))
  
  CURE <-  1/(1+exp(-X_FIX%*%b))
  SURV <- compute_survival_cpp(beta,haz,tstart,tstop,init_cpp$N,nobs,
                               event_times,Z,FALSE)
  
  output <- list(CURE = as.vector(CURE),
                 SURV = as.vector(SURV))
  class(output) <- 'predict.penPHcure'
  return(output)
}