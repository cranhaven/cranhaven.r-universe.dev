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
# ------------------------------------------------------------------------------+
#
#' @name penPHcure.object
#' @title Penalized PH cure model object
#' @description This class of objects is returned by the function \code{\link{penPHcure}} when is called with the argument \code{pen.type = "SCAD" | "LASSO"}. Objects of this class have methods for the functions \code{summary} and \code{predict}.
#' @param AIC a list with elements containing the results of the selected model based on the Akaike information criterion (AIC). See Details.
#' @param BIC a list with elements containing the results of the selected model based on the Bayesian Information Criterion (BIC). See Details.
#' @param pen.type a character string indicating the type of penalty used, either \code{"SCAD"} or \code{"LASSO"}.
#' @param tuneGrid a data.frame containing the values of the AIC and BIC criteria for each combination of the tuning parameters.
#' @param pen.weights a list with elements named \code{CURE} and \code{SURV}, containing the penalty weights. For more details, see \code{\link{penPHcure}}.
#' @param N the sample size (number of individuals).
#' @param K the number of unique failure times.
#' @param isTies logical value: \code{TRUE} in case of tied event times.
#' @param censoring the proportion of censored individuals.
#' @param which.X character string indicating the method used to transform the covariates included in the incidence (cure) component from time-varying to time-invariant. See \code{\link{penPHcure}} for more details.
#' @param survform a formula object with all variables involved in the latency (survival) component of the model.
#' @param cureform a formula object with all variables involved in the incidence (survival) component of the model.
#' @param call object of class \code{call}, with all the specified arguments.
#' 
#' @seealso \code{\link{penPHcure}}
#' 
#' @details
#' The lists \code{AIC} and \code{BIC} contain the results of the selected model based on the Akaike information criterion (AIC) and Bayesian Information Criterion (BIC), respectively. They are composed by the following elements:
#' \itemize{
#'   \item \code{crit}: value of the minimized AIC/BIC criterion.
#'   \item \code{b}: a numeric vector with the estimated regression coefficients in the cure (incidence) component.
#'   \item \code{beta}: a numeric vector with the true estimated coefficients in the survival (latency) component.
#'   \item \code{cumhaz}: a numeric vector with the estimated cumulative baseline hazard function at the unique event times (reported in the \code{"names"} attribute).
#'   \item \code{tune_params}: a list with elements named \code{CURE} and \code{SURV} containing the selected tuning parameters, which minimize the AIC/BIC criterion.
#' }
NULL