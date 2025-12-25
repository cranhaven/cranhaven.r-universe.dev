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
#' @title Summary method for penPHcure.object
#' @description Produces a summary of a fitted penalized PH cure model, after selection of the tuning parameters, based on AIC or BIC criteria.
#' @param object an object of class \code{\link{penPHcure.object}}.
#' @param crit.type a character string indicating the criterion used to select the tuning parameters, either \code{"AIC"} or \code{"BIC"}. By default \code{crit.type = "BIC"}.  
#' @param ... ellipsis to pass extra arguments.
#' @usage
#'  \method{summary}{penPHcure}(object,crit.type=c("BIC","AIC"),...)
#' @return An object of class \code{summary.penPHcure}, a list including the following elements:
#'  \item{\code{N}}{the sample size (number of individuals).}
#'  \item{\code{censoring}}{the proportion of censored individuals.}
#'  \item{\code{K}}{the number of unique failure times.}
#'  \item{\code{isTies}}{logical value: \code{TRUE} in case of tied event times.}
#'  \item{\code{pen.type}}{a character string indicating the type of penalty used, either \code{"SCAD"} or \code{"LASSO"}.}
#'  \item{\code{crit.type}}{a character string indicating the criterion used to select tuning parameters, either \code{"AIC"} or \code{"BIC"}. By default \code{crit.type = "BIC"}.}
#'  \item{\code{tune_params}}{a list with elements named \code{CURE} and \code{SURV} containing the selected tuning parameters, which minimize the AIC/BIC criterion.}
#'  \item{\code{crit}}{value of the minimized AIC/BIC criterion.}
#'  \item{\code{CURE}}{a matrix where in the first column the estimated regression coefficients in the cure (incidence) component are provided. If the argument \code{inference} (in the \code{\link{penPHcure}} function) was set equal to \code{TRUE}, two additional columns for the confidence intervals are provided.}
#'  \item{\code{SURV}}{a matrix where in the first column the estimated regression coefficients in the survival (latency) component are provided. If the argument \code{inference} (in the \code{\link{penPHcure}} function) was set equal to \code{TRUE}, two additional columns for the confidence intervals are provided.}
#' @examples 
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
#' # Use the summary method to see the results
#' summary(tuneSCAD)
#' # 
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # +++             [ Variable selection ]             +++
#' # ------------------------------------------------------
#' # Sample size:  250
#' # Censoring proportion:  0.5
#' # Number of unique event times: 125
#' # Tied failure times:  FALSE
#' # Penalty type:  SCAD
#' # Selection criterion:  BIC
#' # 
#' # ------------------------------------------------------
#' # +++               Tuning parameters                +++
#' # ------------------------------------------------------
#' #  Cure (incidence) --- lambda:  0.07 
#' #                            a:  3.7 
#' # 
#' #  Survival (latency) - lambda:  0.07 
#' #                            a:  3.7 
#' # 
#' #  BIC =  -118.9359 
#' # 
#' # ------------------------------------------------------
#' # +++                Cure (incidence)                +++
#' # +++     [ Coefficients of selected covariates ]    +++
#' # ------------------------------------------------------
#' #              Estimate
#' # (Intercept)  0.872374
#' # x.1         -0.958260
#' # x.3          0.685916
#' # 
#' # ------------------------------------------------------
#' # +++              Survival (latency)                +++
#' # +++     [ Coefficients of selected covariates ]    +++
#' # ------------------------------------------------------
#' #      Estimate
#' # z.1  0.991754
#' # z.3 -1.008180
#' 
#' # By default, the summary method for the penPHcure.object returns the selected 
#' #  variables based on the BIC criterion. For AIC, the user can set the 
#' #  argument crit.type equal to "AIC". 
#' summary(tuneSCAD,crit.type = "AIC")
#' # 
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # +++             [ Variable selection ]             +++
#' # ------------------------------------------------------
#' # Sample size:  250
#' # Censoring proportion:  0.5
#' # Number of unique event times: 125
#' # Tied failure times:  FALSE
#' # Penalty type:  SCAD
#' # Selection criterion:  AIC
#' # 
#' # ------------------------------------------------------
#' # +++               Tuning parameters                +++
#' # ------------------------------------------------------
#' #  Cure (incidence) --- lambda:  0.07 
#' #                            a:  3.7 
#' # 
#' #  Survival (latency) - lambda:  0.07 
#' #                            a:  3.7 
#' # 
#' #  AIC =  -136.5432 
#' # 
#' # ------------------------------------------------------
#' # +++                Cure (incidence)                +++
#' # +++     [ Coefficients of selected covariates ]    +++
#' # ------------------------------------------------------
#' #              Estimate
#' # (Intercept)  0.872374
#' # x.1         -0.958260
#' # x.3          0.685916
#' # 
#' # ------------------------------------------------------
#' # +++              Survival (latency)                +++
#' # +++     [ Coefficients of selected covariates ]    +++
#' # ------------------------------------------------------
#' #      Estimate
#' # z.1  0.991754
#' # z.3 -1.008180
#' 
#' @export
summary.penPHcure <- function(object,crit.type=c("BIC","AIC"),...){
  crit.type <- match.arg(crit.type)
  digits <- 6
  if (crit.type=="BIC"){
    table_CURE <- round(matrix(object$BIC$b),digits)
    table_SURV <- round(matrix(object$BIC$beta),digits)
    colnames(table_SURV) <- colnames(table_CURE) <- c("Estimate")
    rownames(table_CURE) <- names(object$BIC$b)
    rownames(table_SURV) <- names(object$BIC$beta)
    sel_cov_CURE <- (object$BIC$b!=0) #Keep only the selcted covariates
    sel_cov_SURV <- (object$BIC$beta!=0)
    table_CURE <- table_CURE[sel_cov_CURE,,drop = F]
    table_SURV <- table_SURV[sel_cov_SURV,,drop = F]
    tune_params <- object$BIC$tune_params
    crit <- object$BIC$crit
  } else {
    table_CURE <- round(matrix(object$AIC$b),digits)
    table_SURV <- round(matrix(object$AIC$beta),digits)
    colnames(table_SURV) <- colnames(table_CURE) <- c("Estimate")
    rownames(table_CURE) <- names(object$AIC$b)
    rownames(table_SURV) <- names(object$AIC$beta)
    sel_cov_CURE <- (object$AIC$b!=0) #Keep only the selcted covariates
    sel_cov_SURV <- (object$AIC$beta!=0)
    table_CURE <- table_CURE[sel_cov_CURE,,drop = F]
    table_SURV <- table_SURV[sel_cov_SURV,,drop = F]
    tune_params <- object$AIC$tune_params
    crit <- object$AIC$crit
  }
  
  output <- list(N = object$N,
                 censoring = object$censoring,
                 K = object$K,
                 isTies = object$isTies,
                 pen.type = object$pen.type,
                 crit.type = crit.type,
                 tune_params = tune_params,
                 crit = crit,
                 CURE = table_CURE,
                 SURV = table_SURV)
  class(output) <- 'summary.penPHcure'
  return(output)
}