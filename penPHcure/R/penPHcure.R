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
#' @title Variable selection in PH cure model with time-varying covariates
#' @description This function allows to fit a PH cure model with time varying covariates, to compute confidence intervals for the estimated regression coefficients or to make variable selection through a LASSO/SCAD-penalized model.
#' @param formula a formula object, with the response on the left of a \code{~} operator and the variables to be included in the latency (survival) component on the right. The response must be a survival object returned by the \code{Surv(time,time2,status)} function.  
#' @param cureform a one-sided formula object of the form \code{~ x1 + x2 + ...} with the covariates to be included in the incidence (cure) component.
#' @param data a data.frame (in a counting process format) in which to interpret the variables named in the \code{formula} and \code{cureform} arguments.
#' @param tol a positive numeric value used to determine convergence of the NR and EM algorithms. By default, \code{tol = 1e-6}.
#' @param inference a logical value. If \code{TRUE} and \code{pen.type == "none"}, confidence intervals for the regression coefficient estimates are computed using the basic/percentile bootstrap method. By default \code{inference = FALSE}.
#' @param pen.type a character string used to specify the type of penalty used to make variable selection: either \code{"none"}, \code{"SCAD"} or \code{"LASSO"}. By default, \code{pen.type="none"}, only a standard model is fitted without performing variable selection.
#' @param maxIterNR a positive integer: the maximum number of iterations to attempt for convergence of the Newton-Raphson (NR) algorithm (Cox's and logistic regression model). By default \code{maxIterNR = 500}.
#' @param maxIterEM a positive integer: the maximum number of iterations to attempt for convergence of the Expectation-Maximization (EM) algorithm. By default \code{maxIterEM = 500}.
#' @param standardize a logical value. If \code{TRUE}, the values of the covariates are standardized (centered and scaled), such that their mean and variance will be equal to 0 and 1, respectively. By default, \code{standardize = TRUE}.
#' @param ties a character string used to specify the method for handling ties: either \code{"efron"} or \code{"breslow"}. By default, \code{ties = "efron"}.
#' @param print.details a logical value. If \code{TRUE}, tracing information on the progress of the routines is produced. By default \code{print.details = TRUE}.
#' @param warnings a logical value. If \code{TRUE}, possible warnings from the NR and EM algorithms are produced. By default \code{warnings = FALSE}.
#' @param SV a list with elements \code{b} and \code{beta}, numeric vectors of starting values for the regression coefficients in the incidence (cure) and latency (survival) component, respectively. By default \code{SV = NULL}.
#' @param pen.weights a list with elements named \code{CURE} and \code{SURV}, positive numeric vectors of penalty weights for the covariates in the incidence (cure) and latency (survival) component, respectively. By default, all weights are set equal to 1, except for the intercept in the incidence (cure) component (always equal to 0). 
#' @param pen.tuneGrid a list with elements named \code{CURE} and \code{SURV}, named lists of tuning parameter vectors. If \code{pen.type == "SCAD"} they should contain two numeric vectors of possible tuning parameters: \code{lambda} and \code{a}. Whereas, if \code{pen.type == "LASSO"}, only one vector \code{lambda}. By default \code{lambda = exp(seq(-7,0,length.out = 10))} and \code{a = 3.7}. 
#' @param pen.thres.zero a positive numeric value used as a threshold. After fitting the penalized PH cure model, the estimated regression coefficients with an absolute value lower than this threshold are set equal to zero. By default, \code{pen.thres.zero = 1e-06}.
#' @param constraint a logical value. If \code{TRUE}, the model makes use of the zero-tail constraint, classifying the individuals with censoring times grater than the largest event time as non-susceptible. For more details, see \insertCite{Sy_Taylor_2000;textual}{penPHcure}. By default \code{constraint = TRUE}.
#' @param nboot a positive integer: the number of bootstrap resamples for the construction of the confidence intervals (used only when \code{inference = TRUE}). By default, \code{nboot = 100}.
#' @param epsilon a positive numeric value used as a perturbation of the penalty function. By default, \code{epsilon = 1e-08}.
#' @param X a matrix of time-invariant covariates to be included in the incidence (cure) component. If the user provide such matrix, the arguments \code{cureform} and \code{which.X} will be ignored. By default, \code{X = NULL}.
#' @param which.X character string used to specify the method used to transform the covariates included in the incidence (cure) component from time-varying to time-invariant. There are two options: either take the last observation (\code{"last"}) or the mean over the full history of the covariates (\code{"mean"}). By default, \code{which.X = "last"}.  
#' 
#' @return If the argument \code{pen.type = "none"}, this function returns a \code{\link{PHcure.object}}. Otherwise, if \code{pen.type == "SCAD" | "LASSO"}, it returns a \code{\link{penPHcure.object}}.
#' 
#' @seealso \code{\link{penPHcure-package}}, \code{\link{PHcure.object}}, \code{\link{penPHcure.object}}
#' 
#' @details 
#' When the starting values (\code{SV}) are not specified and \code{pen.type == "none"}:
#' \itemize{
#'  \item \code{SV$b} is set equal to the estimates of a logistic regression model with the event indicator (0=censored, 1=event) as dependent variable; and
#'  \item \code{SV$beta} is set equal to the estimates of a standard Cox's model.
#' }
#' Whereas, if \code{pen.type == "SCAD" | "LASSO"}, both vectors are filled with zeros.
#' 
#' When performing variable selection (\code{pen.type == "SCAD" | "LASSO"}), a penalized PH cure model is fitted for each possible combination of the tuning parameters in \code{pen.tuneGrid}. Two models are selected on the basis of the Akaike and Bayesian Information Criteria: \deqn{AIC=-ln(\hat{L})+2df,} \deqn{BIC=-ln(\hat{L})+ln(n)df,} where \eqn{ln(\hat{L})} is the value of the log-likelihood at the penalized MLEs, \eqn{df} is the value of the degrees of freedom (number of non-zero coefficients) and \eqn{n} is the sample size.
#' 
#' Regarding the possible tuning parameters in \code{pen.tuneGrid}, the numeric vectors \code{lambda} and \code{a} should contain values >= 0 and > 2, respectively. 
#' 
#' @references
#'  \insertRef{Beretta_Heuchenne_2019}{penPHcure}
#'  
#'  \insertRef{Sy_Taylor_2000}{penPHcure}
#'  
#'  
#' 
#' @examples 
#' \donttest{
#' # Generate some data (for more details type ?penPHcure.simulate in your console)
#' data <- penPHcure.simulate()
#' 
#' ### Standard PH cure model
#' 
#' # Fit standard cure model (without inference)
#' fit <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                       event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                  cureform = ~ x.1 + x.2 + x.3 + x.4,data = data)
#' # The returned PHcure.object has methods summary and predict, 
#' #  for more details type ?summary.PHcure or ?predict.PHcure in your console.
#' 
#' # Fit standard cure model (with inference)
#' fit2 <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                        event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                   cureform = ~ x.1 + x.2 + x.3 + x.4,data = data,
#'                   inference = TRUE)
#' # The returned PHcure.object has methods summary and predict, 
#' #  for more details type ?summary.PHcure or ?predict.PHcure in your console.
#' 
#' 
#' ### Tune penalized cure model with SCAD penalties
#' 
#' # First define the grid of possible values for the tuning parameters.
#' pen.tuneGrid <- list(CURE = list(lambda = exp(seq(-7,-2,length.out = 10)),
#'                                  a = 3.7),
#'                      SURV = list(lambda = exp(seq(-7,-2,length.out = 10)),
#'                                  a = 3.7))
#' # Tune the penalty parameters.
#' tuneSCAD <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                            event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                       cureform = ~ x.1 + x.2 + x.3 + x.4,
#'                       data = data,pen.type = "SCAD",
#'                       pen.tuneGrid = pen.tuneGrid)
#' # The returned penPHcure.object has methods summary and predict, for more
#' #  details type ?summary.penPHcure or ?predict.penPHcure in your console.
#' 
#' ### Tune penalized cure model with LASSO penalties
#' 
#' # First define the grid of possible values for the tuning parameters.
#' pen.tuneGrid <- list(CURE = list(lambda = exp(seq(-7,-2,length.out = 10))),
#'                      SURV = list(lambda = exp(seq(-7,-2,length.out = 10))))
#' # Tune the penalty parameters.
#' tuneLASSO <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                             event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                        cureform = ~ x.1 + x.2 + x.3 + x.4,
#'                        data = data,pen.type = "LASSO",
#'                        pen.tuneGrid = pen.tuneGrid)
#' # The returned penPHcure.object has methods summary and predict, for more
#' #  details type ?summary.penPHcure or ?predict.penPHcure in your console.
#' }
#' @export

penPHcure <- function(formula,cureform,data,X=NULL,maxIterNR = 500,
                      maxIterEM = 500,tol = 1e-6,standardize = TRUE,
                      ties=c("efron","breslow"),SV = NULL,
                      which.X=c("last","mean"),inference = FALSE,nboot = 100,
                      constraint = TRUE,pen.type = c("none","SCAD","LASSO"),
                      pen.weights = NULL,pen.tuneGrid = NULL,epsilon = 1e-8,
                      pen.thres.zero=1e-6,print.details = TRUE,warnings = FALSE){
  
  if (missing(formula)) {
    stop('penPHcure :: Missing argument "formula" with no default.')
  } else if (missing(data)) {
    stop('penPHcure :: Missing argument "data" with no default.')
  } else if (missing(cureform) && is.null(X)) {
    stop('penPHcure :: Missing argument, neither "cureform" nor "X" is specified.') 
  } 
  
  fcall <- match.call()
  pen.type <- match.arg(pen.type)
  ties <- match.arg(ties)
  ties <- ifelse(ties=="breslow",0,1)
  which.X <- match.arg(which.X)
  
  if (print.details) 
    message("\nInitializing PH cure model with time-varying covariates...")
  
  init <- penPHcure.initialize(formula,cureform,data,X,ties,
                               maxIterNR,maxIterEM,tol,constraint,
                               standardize,which.X,print.details,warnings)
  
  if (print.details) {
    message("\nNumber of individuals: ",init$N)
    message("Censoring proportion: ",round(init$censoring,3))
    message("Tied failure times: ",init$isTies)
    message("Number of unique failure times: ",init$K)
    message("Number of covariates in the survival component: ",init$ncovSURV)
    message("Number of covariates in the cure component: ",init$ncovCURE)
    message("\nChecking starting values...")
  }
  
  init$SV <- penPHcure.check.start_values(SV,init,pen.type)
  
  if (pen.type=="none"){
    if (print.details)
      message("\nFitting standard PH cure model with time-varying covariates...", 
              " Please wait...")
    stdfit <- penPHcure.stdfit(init)
    if (inference){
      estim <- list(b=stdfit$b,beta=stdfit$beta)
      stdfit$inference <- penPHcure.inference(formula,cureform,data,
                                              X,estim,init,nboot)
    }
    output <- stdfit
  } else {
    pen.weights <- penPHcure.check.pen.weights(pen.weights,init)
    pen.tuneGrid <- penPHcure.check.pen.tuneGrid(pen.tuneGrid,pen.type)
    if (print.details)
      message("\nTuning ",pen.type,"-penalized PH cure model with time-varying",
              " covariates... Please wait...")
    penfit <- penPHcure.tune(init,pen.tuneGrid,pen.type,
                             pen.weights,pen.thres.zero,epsilon)
    output <- penfit
  }
  
  if (print.details) message("\nDONE!")

  output$call <- fcall
  
  return(output)
}