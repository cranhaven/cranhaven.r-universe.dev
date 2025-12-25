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
#'@export
print.summary.penPHcure <- function(x,...){
  cat('\n------------------------------------------------------\n')
  cat('+++   PH cure model with time-varying covariates   +++\n')
  cat('+++             [ Variable selection ]             +++\n')
  cat('------------------------------------------------------\n')
  cat('Sample size: ',x$N)
  cat('\nCensoring proportion: ',x$censoring)
  cat('\nNumber of unique event times:',x$K)
  cat('\nTied failure times: ',x$isTies)
  cat('\nPenalty type: ',x$pen.type)
  cat('\nSelection criterion: ',x$crit.type)
  cat('\n\n')
  cat('------------------------------------------------------\n')
  cat('+++               Tuning parameters                +++\n')
  cat('------------------------------------------------------\n')
  if (x$pen.type == "SCAD") {
    cat(' Cure (incidence) --- lambda: ',x$tune_params$CURE[1],'\n')
    cat('                           a: ',x$tune_params$CURE[2],'\n\n')
    cat(' Survival (latency) - lambda: ',x$tune_params$SURV[1],'\n')
    cat('                           a: ',x$tune_params$SURV[2],'\n\n')
  } else if (x$pen.type == "LASSO") {
    cat(' Cure (incidence) --- lambda: ',x$tune_params$CURE[1],'\n')
    cat(' Survival (latency) - lambda: ',x$tune_params$SURV[1],'\n\n')
  }
  if (x$crit.type == 'AIC') cat(' AIC = ',x$crit,'\n')
  else if (x$crit.type == 'BIC') cat(' BIC = ',x$crit,'\n')
  cat('\n')
  cat('------------------------------------------------------\n')
  cat('+++                Cure (incidence)                +++\n')
  cat('+++     [ Coefficients of selected covariates ]    +++\n')
  cat('------------------------------------------------------\n')
  print(x$CURE)
  cat('\n')
  cat('------------------------------------------------------\n')
  cat('+++              Survival (latency)                +++\n')
  cat('+++     [ Coefficients of selected covariates ]    +++\n')
  cat('------------------------------------------------------\n')
  print(x$SURV)
}