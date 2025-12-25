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
#' @name PHcure.object
#' @title Standard PH cure model object
#' @description This class of objects is returned by the function \code{\link{penPHcure}} when is called with the argument \code{pen.type = "none"}. Objects of this class have methods for the functions \code{summary} and \code{predict}. 
#' @param b a numeric vector with the estimated regression coefficients in the cure (incidence) component.
#' @param beta a numeric vector with the true estimated regression coefficients in the survival (latency) component.
#' @param cumhaz a numeric vector with the estimated cumulative baseline hazard function at the unique event times (reported in the \code{"names"} attribute).
#' @param logL the value of the log-likelihood for the estimated model.
#' @param converged an integer to indicate if the Expectation-Maximization (EM) algorithm converged. The possible values are: \code{1} if it converged, \code{-1} if it exceeded the maximum number of iterations or \code{-2} if it stopped due to non-finite elements in the regression coefficients.
#' @param iter the maximum number of iteration before the convergence of the Expectation-Maximization (EM) algorithm.
#' @param N the sample size (number of individuals).
#' @param K the number of unique failure times.
#' @param isTies logical value: \code{TRUE} in case of tied event times.
#' @param censoring the proportion of censored individuals.
#' @param which.X character string indicating the method used to transform the covariates included in the incidence (cure) component from time-varying to time-invariant. See \code{\link{penPHcure}} for more details.
#' @param survform a formula object with all variables involved in the latency (survival) component of the model.
#' @param cureform a formula object with all variables involved in the incidence (survival) component of the model.
#' @param inference [optional] a list with elements named \code{bs}, \code{betas} and \code{nboot}. The elements \code{bs} and \code{betas} are matrices containing on each row the estimated regression coefficients in the incidence and latency components, respectively, for each of the \code{nboot} bootstrap resamples. This object is returned only if the function \code{\link{penPHcure}} was called with the argument inference = TRUE.
#' @param call object of class \code{call}, with all the specified arguments.
#' 
#' @seealso \code{\link{penPHcure}}
NULL
