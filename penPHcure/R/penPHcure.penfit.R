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

penPHcure.penfit <- function(init,pen.type,pen.tune_params,
                             pen.weights,epsilon,pen.thres.zero){
  
  SV <- init$SV
  
  fit <- fit_pen_cure_cpp(SV$b,SV$beta,
                          SV$base_hazard,init$tstart,
                          init$tstop,init$sort1,
                          init$sort2,init$status,
                          init$fail_times,init$nfails,
                          init$nobs_i,init$status_FIX,
                          init$X_FIX,init$Z,
                          init$ties,init$warnings,
                          init$maxIterNR,init$maxIterEM,
                          init$constraint,init$tol,
                          pen.type,pen.tune_params$CURE,
                          pen.tune_params$SURV,pen.weights$CURE,
                          pen.weights$SURV,epsilon,
                          pen.thres.zero)

  return(fit)
}