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

penPHcure.stdfit <- function(init){
  SV <- init$SV
  
  # Fit standard cure model
  fit <- fit_cure_cpp(SV$b,SV$beta,SV$base_hazard,
                      init$tstart,init$tstop,
                      init$sort1,init$sort2,
                      init$status,init$fail_times,
                      init$nfails,init$nobs_i,
                      init$status_FIX,init$X_FIX,
                      init$Z,init$ties,
                      init$warnings,init$maxIterNR,
                      init$maxIterEM,init$tol,
                      init$constraint)
  
  # If covariates were standardized, rescale the estimated coefficients
  if (init$standardize){
    Z_mean <- attr(init$Z,"scaled:center")
    Z_sd <- attr(init$Z,"scaled:scale")
    X_mean <- attr(init$X_FIX,"scaled:center")
    X_sd <- attr(init$X_FIX,"scaled:scale")
    fit$beta <- fit$beta/Z_sd
    temp <- fit$b[1] - sum(fit$b*X_mean/X_sd)
    fit$b <- fit$b/X_sd
    fit$b[1] <- temp
    fit$cumhaz <- fit$cumhaz/exp(sum(Z_mean*fit$beta))
  }
  
  fit$b <- as.vector(fit$b)
  names(fit$b) <- colnames(init$X_FIX)
  fit$beta <- as.vector(fit$beta)
  names(fit$beta) <- colnames(init$Z)
  fit$cumhaz <- as.vector(fit$cumhaz)
  attr(fit$cumhaz,"time") <- init$fail_times
  fit$which.X <- init$which.X
  fit$survform <- init$survform
  fit$cureform <- init$cureform
  
  class(fit) <- 'PHcure'
  
  return(fit)
  
}