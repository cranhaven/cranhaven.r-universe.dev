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
#' @name penPHcure-package
#' @useDynLib penPHcure
#' @import survival
#' @importFrom Rcpp evalCpp
#' @importFrom stats quantile model.frame model.matrix sd as.formula
#' @importFrom utils setTxtProgressBar txtProgressBar 
#' @importFrom Rdpack reprompt
#' 
#' @title Variable Selection in Proportional-Hazards Cure Model with Time-Varying Covariates
#' 
#' @docType package
#' 
#' @description 
#' 
#' Contrary to standard survival analysis models, which rely on the assumption that the entire population will eventually experience the event of interest, mixture cure models allow to split the population in susceptible and non-susceptible (cured) individuals.
#' 
#' In this R package, we implement the semi-parametric proportional-hazards (PH) cure model of \insertCite{Sy_Taylor_2000;textual}{penPHcure} extended to time-varying covariates.
#' If we define \eqn{T} as the time-to-event, the survival function for the entire population is given by
#' \deqn{S(t)=(1-p)+pS(t|Y=1)}
#' where \eqn{p} is the incidence (i.e. probability of being susceptible) and \eqn{S(t|Y=1)} is the latency (i.e. survival function conditional on being susceptible). 
#' 
#' The incidence is modeled by a logistic regression model: \deqn{p=P(Y=1|\mathbf{x}_i)=\exp(\mathbf{x}_i'\textbf{b})/(1+\exp(\mathbf{x}_i'\textbf{b)}),} where \eqn{\textbf{x}_i} is a vector of time-invariant covariates (including the intercept) and \eqn{\mathbf{b}} a vector of unknown coefficients.
#' Whereas, the latency is modeled by a Cox’s PH model: \deqn{\lambda(t|Y=1,\textbf{z}_i(t))=\lambda_{0}(t|Y=1)e^{\textbf{z}_i'(t)\boldsymbol{\beta}},} where \eqn{\textbf{z}_i(t)} is a vector of time-varying covariates, \eqn{\lambda_{0}(t|Y=1)} is an arbitrary conditional baseline hazard function and \eqn{\boldsymbol{\beta}} is a vector of unknown coefficients.
#' 
#' The function \code{\link{penPHcure}} allows to:
#' \itemize{
#'  \item estimate the regression coefficients (\eqn{\textbf{b}}, \eqn{\boldsymbol{\beta}}) and the baseline hazard function \eqn{\lambda_{0}(t|Y=1)}; 
#'  \item compute confidence intervals for the estimated regression coefficients using the basic/percentile bootstrap method;
#'  \item perform variable selection with the SCAD-penalized likelihood technique proposed by \insertCite{Beretta_Heuchenne_2019;textual}{penPHcure}.
#' }
#' 
#' Moreover, the function \code{\link{penPHcure.simulate}} allows to simulate data from a PH cure model, where the event-times are generated on a continuous scale from a piecewise exponential distribution conditional on time-varying covariates, using a method similar to the one described in \insertCite{Hendry_2014;textual}{penPHcure}.
#' 
#' @references \insertAllCited{}
#'     
#' @seealso \code{\link{penPHcure}}, \code{\link{penPHcure.simulate}}
#' 
#' 
NULL