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
#' @title Criminal Recidivism Data
#' 
#' @description 
#' A sample of 432 inmates released from Maryland state prisons followed for one year after release \insertCite{Rossi_1980}{penPHcure}.
#' The aim of this study was to investigate the relationship between the time to first arrest after release and some covariates observed during the follow-up period.
#' Most of the variables are constant over time, except one binary variable denoting whether the individual was working full time during the follow-up period.
#' 
#' @source 
#' The \code{Rossi} dataset in the \code{RcmdrPlugin.survival} package \insertCite{RcmdrPlugin.survival}{penPHcure} is the source of these data, which have been converted into counting process format.
#' 
#' 
#' @format 
#' A \code{data.frame} in counting process format with 1405 observations for 432 individuals on the following 13 variables.
#' \describe{
#'   \item{\code{id}}{integer. Identification code for each individual.}
#'   \item{(\code{tstart}, \code{tstop}]}{integers. Time interval of the observation (in weeks). Observation for each individual start after the first release.}
#'   \item{\code{arrest}}{factor with 2 levels ("no", "yes"). Denote whether the individual has been arrested during the 1 year follow-up period or not.}
#'   \item{\code{fin}}{factor with 2 levels ("no", "yes"). Denote whether the inmate received financial aid after release.}
#'   \item{\code{age}}{integer. Age in years at the time of release.}
#'   \item{\code{race}}{factor with 2 levels ("black", "other"). Denote whether the race of the individual is black or not.}
#'   \item{\code{wexp}}{factor with 2 levels ("no", "yes"). Denote whether the individual had full-time work experience before incarceration or not.}
#'   \item{\code{mar}}{factor with 2 levels ("yes", "no"). Denote whether the inmate was married at the time of release or not.}
#'   \item{\code{paro}}{factor with 2 levels ("no", "yes"). Denote whether the inmate was released on parole or not.}
#'   \item{\code{prio}}{integer. The number of convictions an inmate had prior to incarceration.}
#'   \item{\code{educ}}{factor with 3 levels ("3", "4", "5"). Level of education: 
#'                      \itemize{
#'                        \item "3": <=9th degree; 
#'                        \item "4": 10th or 11th degree; and 
#'                        \item "5": >=12 degree.
#'                      }}
#'   \item{\code{emp}}{factor with 2 levels ("no", "yes"). Denote whether the individual was working full time during the observed time interval.}
#' }
#' @docType data
#' @usage 
#' cpRossi
#' 
#' data(cpRossi,package="penPHcure")
#' 
#' 
#' @references \insertAllCited{}
#' 
"cpRossi"