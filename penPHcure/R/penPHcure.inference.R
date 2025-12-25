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

penPHcure.inference <- function(formula,cureform,data,X,estim,init,nboot){

  bs <- matrix(NA,nboot,ncol(init$X_FIX))
  betas <- matrix(NA,nboot,ncol(init$Z))
  SV <- list(b = estim$b,
             beta = estim$beta)
  
  # If covariates were standardized, rescale the estimated coefficients
  if (init$standardize){
    Z_sd <- attr(init$Z,"scaled:scale")
    X_mean <- attr(init$X_FIX,"scaled:center")
    X_sd <- attr(init$X_FIX,"scaled:scale")
    SV$beta <- SV$beta*Z_sd
    SV$b <- SV$b*X_sd
    SV$b[1] <- SV$b[1] + sum(SV$b*X_mean/X_sd)
  }
  
  if (init$print.details) {
    message("\nPerforming inference via bootstrapping... Please wait ...")
    pb <- txtProgressBar(1, nboot, style=3)
  }
  
  boot <- 1

  while (boot<=nboot){
    idResample <- sample(1:init$N,replace = TRUE)
    idx <- NULL
    for (i in 2:init$N){
      idx <- c(idx,init$posSTART[idResample[i]]:init$posEND[idResample[i]])
    }
    newdata <- data[idx,]
    if (!is.null(X)) newX <- X[idResample,]
    else newX <- X
    
    newinit <- penPHcure.initialize(formula,cureform,newdata,
                                    newX,init$ties,init$maxIterNR,
                                    init$maxIterEM,init$tol,init$constraint,
                                    init$standardize,init$which.X,
                                    init$print.details,init$warnings)

    newinit$SV <- penPHcure.check.start_values(SV,newinit)

    fit <- penPHcure.stdfit(newinit)
    
    if (fit$converged==1){
      bs[boot,] <- fit$b
      betas[boot,] <- fit$beta
      if (init$print.details)
        setTxtProgressBar(pb, boot)
      boot <- boot + 1
    } 
  }

  inference <- list(bs=bs,betas=betas,nboot=nboot)

  return(inference)
}