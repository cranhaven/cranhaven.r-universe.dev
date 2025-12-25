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
#' @title Summary method for PHcure.object
#' @description Produces a summary of a fitted PH cure model
#' @param object an object of class \code{\link{PHcure.object}}.
#' @param conf.int a character string indicating the method to compute bootstrapped confidence intervals: \code{"percentile"} or \code{"basic"}. By default \code{conf.int = "basic"}.  
#' @param conf.int.level confidence level. By default \code{conf.int.level = 0.95}.
#' @param ... ellipsis to pass extra arguments.
#' @usage
#'  \method{summary}{PHcure}(object, conf.int = c("basic","percentile"), conf.int.level = 0.95,...) 
#' @return An object of class \code{summary.PHcure}, a list including the following elements:
#'  \item{\code{N}}{the sample size (number of individuals).}
#'  \item{\code{censoring}}{the proportion of censored individuals.}
#'  \item{\code{K}}{the number of unique failure times.}
#'  \item{\code{isTies}}{a logical value, equal to \code{TRUE} in case of tied event times.}
#'  \item{\code{conf.int}}{a character string indicating the method used to compute the bootstrapped confidence intervals: \code{"percentile"}, \code{"basic"} or \code{"no"}. The latter is returned when the \code{\link{penPHcure}} function was called with the argument \code{inference = FALSE}.}
#'  \item{\code{conf.int.level}}{confidence level used to compute the bootstrapped confidence intervals.}
#'  \item{\code{nboot}}{the number of bootstrap resamples for the construction of the confidence intervals.}
#'  \item{\code{logL}}{the value of the log-likelihood for the estimated model.}
#'  \item{\code{CURE}}{a matrix with one column containing the estimated regression coefficients in the incidence (cure) component. In case the function \code{\link{penPHcure}} was called with the argument \code{inference = TRUE}, two additional columns for the confidence intervals are provided.}
#'  \item{\code{SURV}}{a matrix where in the first column the estimated regression coefficients in the latency (survival) component. In case the function \code{\link{penPHcure}} was called with the argument \code{inference = TRUE}, two additional columns for the confidence intervals are provided.}
#'
#' @examples
#' # For reproducibility
#' set.seed(12) 
#' # If you use R v3.6 or greater, uncomment the following line
#' # RNGkind(sample.kind="Rounding") 
#' 
#' # Generate some data (for more details type ?penPHcure.simulate in your console)
#' data <- penPHcure.simulate(N=250)
#'  
#' # Fit standard cure model (without inference)
#' fit <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                       event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                  cureform = ~ x.1 + x.2 + x.3 + x.4,data = data)
#'
#' # Use the summary method to see the results
#' summary(fit)
#' #
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # ------------------------------------------------------
#' # Sample size: 250
#' # Censoring proportion: 0.5
#' # Number of unique event times: 125
#' # Tied failure times: FALSE
#' # 
#' # log-likelihood:  74.11 
#' # 
#' # ------------------------------------------------------
#' # +++         Cure (incidence) coefficients          +++
#' # ------------------------------------------------------
#' #              Estimate
#' # (Intercept)  0.889668
#' # x.1         -0.972653
#' # x.2         -0.051580
#' # x.3          0.714611
#' # x.4          0.156169
#' # 
#' # ------------------------------------------------------
#' # +++         Survival (latency) coefficients         +++
#' # ------------------------------------------------------
#' #      Estimate
#' # z.1  0.996444
#' # z.2 -0.048792
#' # z.3 -1.013562
#' # z.4  0.079422
#' 
#' # Fit standard cure model (with inference). nboot = 30 bootstrap resamples
#' #  are used to compute the confidence intervals.  
#' fit2 <- penPHcure(Surv(time = tstart,time2 = tstop,
#'                        event = status) ~ z.1 + z.2 + z.3 + z.4,
#'                   cureform = ~ x.1 + x.2 + x.3 + x.4,data = data,
#'                   inference = TRUE,print.details = FALSE,nboot = 30)
#' # Use the summary method to see the results
#' summary(fit2)
#' #
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # ------------------------------------------------------
#' # Sample size: 250
#' # Censoring proportion: 0.5
#' # Number of unique event times: 125
#' # Tied failure times: FALSE
#' # 
#' # log-likelihood:  74.11 
#' # 
#' # ------------------------------------------------------
#' # +++     Cure (incidence) coefficient estimates     +++
#' # +++          and 95% confidence intervals *        +++
#' # ------------------------------------------------------
#' #              Estimate      2.5%     97.5%
#' # (Intercept)  0.889668  0.455975  1.092495
#' # x.1         -0.972653 -1.414194 -0.503824
#' # x.2         -0.051580 -0.557843  0.304632
#' # x.3          0.714611  0.206211  1.081819
#' # x.4          0.156169 -0.011555  0.464841
#' # 
#' # ------------------------------------------------------
#' # +++    Survival (latency) coefficient estimates    +++
#' # +++          and 95% confidence intervals *        +++
#' # ------------------------------------------------------
#' #      Estimate      2.5%     97.5%
#' # z.1  0.996444  0.750321  1.130650
#' # z.2 -0.048792 -0.204435  0.073196
#' # z.3 -1.013562 -1.127882 -0.780339
#' # z.4  0.079422 -0.100677  0.193522
#' # 
#' # ------------------------------------------------------
#' # * Confidence intervals computed by the basic 
#' #   bootstrap method, with 30 replications.
#' # ------------------------------------------------------
#' 
#' # By default, confidence intervals are computed by the basic bootstrap method.
#' #  Otherwise, the user can specify the percentile bootstrap method.
#' summary(fit2,conf.int = "percentile")
#' #
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # ------------------------------------------------------
#' # Sample size: 250
#' # Censoring proportion: 0.5
#' # Number of unique event times: 125
#' # Tied failure times: FALSE
#' # 
#' # log-likelihood:  74.11 
#' # 
#' # ------------------------------------------------------
#' # +++     Cure (incidence) coefficient estimates     +++
#' # +++          and 95% confidence intervals *        +++
#' # ------------------------------------------------------
#' #              Estimate      2.5%     97.5%
#' # (Intercept)  0.889668  0.686842  1.323362
#' # x.1         -0.972653 -1.441483 -0.531112
#' # x.2         -0.051580 -0.407791  0.454684
#' # x.3          0.714611  0.347404  1.223011
#' # x.4          0.156169 -0.152503  0.323893
#' # 
#' # ------------------------------------------------------
#' # +++    Survival (latency) coefficient estimates    +++
#' # +++          and 95% confidence intervals *        +++
#' # ------------------------------------------------------
#' #      Estimate      2.5%     97.5%
#' # z.1  0.996444  0.862238  1.242567
#' # z.2 -0.048792 -0.170779  0.106852
#' # z.3 -1.013562 -1.246785 -0.899242
#' # z.4  0.079422 -0.034678  0.259521
#' # 
#' # ------------------------------------------------------
#' # * Confidence intervals computed by the percentile 
#' #   bootstrap method, with 30 replications.
#' # ------------------------------------------------------
#' 
#' # By default, a 95% confidence level is used. Otherwise, the user can specify 
#' #  another confidence level: e.g. 90%.
#' summary(fit2,conf.int.level = 0.90)
#' # 
#' # ------------------------------------------------------
#' # +++   PH cure model with time-varying covariates   +++
#' # ------------------------------------------------------
#' # Sample size: 250
#' # Censoring proportion: 0.5
#' # Number of unique event times: 125
#' # Tied failure times: FALSE
#' # 
#' # log-likelihood:  74.11 
#' # 
#' # ------------------------------------------------------
#' # +++     Cure (incidence) coefficient estimates     +++
#' # +++          and 90% confidence intervals *        +++
#' # ------------------------------------------------------
#' #              Estimate        5%       95%
#' # (Intercept)  0.889668  0.467864  1.074423
#' # x.1         -0.972653 -1.397088 -0.618265
#' # x.2         -0.051580 -0.527389  0.249460
#' # x.3          0.714611  0.314140  1.028425
#' # x.4          0.156169  0.033802  0.436361
#' # 
#' # ------------------------------------------------------
#' # +++    Survival (latency) coefficient estimates    +++
#' # +++          and 90% confidence intervals *        +++
#' # ------------------------------------------------------
#' #      Estimate        5%       95%
#' # z.1  0.996444  0.767937  1.125745
#' # z.2 -0.048792 -0.158821  0.050965
#' # z.3 -1.013562 -1.120989 -0.800606
#' # z.4  0.079422 -0.086063  0.180392
#' # 
#' # ------------------------------------------------------
#' # * Confidence intervals computed by the basic 
#' #   bootstrap method, with 30 replications.
#' # ------------------------------------------------------
#' 
#' @export
summary.PHcure <- function(object,conf.int = c("basic","percentile"),
                           conf.int.level = 0.95,...){
  conf.int <- match.arg(conf.int)
  digits <- 6
  if (is.null(object$inference)){
    table_CURE <- round(as.matrix(object$b),digits)
    table_SURV <- round(as.matrix(object$beta),digits)
    colnames(table_SURV) <- colnames(table_CURE) <- c("Estimate")
    conf.int <- "no"
    nboot <- 0
  } else {
    alpha <- 1 - conf.int.level
    nboot <- object$inference$nboot
    perc_boot_CURE <- t(apply(object$inference$bs,2,
                              quantile,c(alpha/2,1-alpha/2)))
    perc_boot_SURV <- t(apply(object$inference$betas,2,
                              quantile,c(alpha/2,1-alpha/2)))
    if (conf.int=="percentile"){
      table_CURE <- round(cbind(object$b,perc_boot_CURE),digits)
      table_SURV <- round(cbind(object$beta,perc_boot_SURV),digits)
    } else if (conf.int=="basic"){
      table_CURE <- round(cbind(object$b,
                          2*object$b-perc_boot_CURE[,2],
                          2*object$b-perc_boot_CURE[,1]),digits)
      table_SURV <- round(cbind(object$beta,
                          2*object$beta-perc_boot_SURV[,2],
                          2*object$beta-perc_boot_SURV[,1]),digits)
    }
    colnames(table_SURV) <- 
      colnames(table_CURE) <- c("Estimate",
                                paste(alpha/2*100,"%",sep=''),
                                paste((1-alpha/2)*100,"%",sep=''))
  }
  rownames(table_CURE) <- names(object$b)
  rownames(table_SURV) <- names(object$beta)
  
  output <- list(N = object$N,
                 censoring = object$censoring,
                 K = object$K,
                 isTies = object$isTies,
                 conf.int = conf.int,
                 conf.int.level = conf.int.level,
                 nboot = nboot,
                 logL = object$logL,
                 CURE = table_CURE,
                 SURV = table_SURV)

  class(output) <- 'summary.PHcure'
  
  return(output)
}