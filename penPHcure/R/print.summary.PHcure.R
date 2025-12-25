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
print.summary.PHcure <- function(x,...)
{
  cat('\n------------------------------------------------------\n')
  cat('+++   PH cure model with time-varying covariates   +++\n')
  cat('------------------------------------------------------\n')
  cat('Sample size:',x$N)
  cat('\nCensoring proportion:',x$censoring)
  cat('\nNumber of unique event times:',x$K)
  cat('\nTied failure times:',x$isTies)
  cat('\n\n')
  
  cat('log-likelihood: ',round(x$logL,2),'\n\n')
  
  cat('------------------------------------------------------\n')
  if (x$conf.int=="no")
    cat('+++         Cure (incidence) coefficients          +++\n')
  else {
    cat('+++     Cure (incidence) coefficient estimates     +++\n')
    cat('+++          and ',x$conf.int.level*100,
        '% confidence intervals *        +++\n',sep='')
  }
  cat('------------------------------------------------------\n')
  print(x$CURE)
  
  cat('\n------------------------------------------------------\n')
  if (x$conf.int=="no")
    cat('+++         Survival (latency) coefficients         +++\n')
  else {
    cat('+++    Survival (latency) coefficient estimates    +++\n')
    cat('+++          and ',x$conf.int.level*100,
        '% confidence intervals *        +++\n',sep='')
  }
  cat('------------------------------------------------------\n')
  print(x$SURV)
  
  if (x$conf.int!="no"){
    cat('\n------------------------------------------------------\n')
    cat('* Confidence intervals computed by the',x$conf.int,
        '\n  bootstrap method, with',x$nboot,'replications.')
    cat('\n------------------------------------------------------\n')
  }
}
