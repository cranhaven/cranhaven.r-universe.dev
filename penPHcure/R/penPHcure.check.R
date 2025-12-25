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

penPHcure.check.start_values <- function(SV,init,pen.type){
  ties <- init$ties
  warnings <- init$warnings
  maxIterNR <- init$maxIterNR
  tol <- init$tol
  
  check1 <- is.null(SV) || !is.list(SV)
  
  if (check1){
    SV <- list()
    if (pen.type=="none"){
      fitLOGIT <- fit_logit_cpp(rep(0,init$ncovCURE+1L),init$X_FIX,
                                init$status_FIX,warnings,maxIterNR,tol)
      fitCOX <- fit_cox_cpp(rep(0,init$ncovSURV),init$nobs,init$tstart,
                            init$tstop,init$sort1,init$sort2,init$status,
                            rep(0,init$nobs),init$Z,ties,warnings,
                            maxIterNR,tol)
      SV$b <- as.vector(fitLOGIT$coef)
      SV$beta <- as.vector(fitCOX$coef)
    } else {
      SV$beta <- rep(0,init$ncovSURV)
      SV$b <- rep(0,init$ncovCURE+1L)
    }
  }

  checkCURE <- is.null(SV$b) || !is.vector(SV$b) || 
    any(!is.finite(SV$b)) || length(SV$b)!=(init$ncovCURE+1L)
  
  if (checkCURE){
    warning('penPHcure.check.start_values :: starting values for the 
             coefficients in the cure (incidence) component 
             not correctly specified.')
    fitLOGIT <- fit_logit_cpp(rep(0,init$ncovCURE+1L),init$X_FIX,
                              init$status_FIX,warnings,maxIterNR,tol)
    SV$b <- as.vector(fitLOGIT$coef)
  } else if (init$standardize) {
    # If covariates were standardized, rescale the starting values
    X_mean <- attr(init$X_FIX,"scaled:center")
    X_sd <- attr(init$X_FIX,"scaled:scale")
    SV$b <- SV$b*X_sd
    SV$b[1] <- SV$b[1] + sum(SV$b*X_mean/X_sd)
  }
  
  checkSURV <- is.null(SV$beta) || !is.vector(SV$beta) || 
    any(!is.finite(SV$beta)) || length(SV$beta)!=init$ncovSURV
  
  if (checkSURV){
    warning('penPHcure.check.start_values :: starting values for the 
             coefficients in the survival (latency) component 
             not correctly specified.')
    fitCOX <- fit_cox_cpp(rep(0,init$ncovSURV),init$nobs,init$tstart,
                          init$tstop,init$sort1,init$sort2,init$status,
                          rep(0,init$nobs),init$Z,ties,warnings,
                          maxIterNR,tol)
    SV$beta <- as.vector(fitCOX$coef)
  } else if (init$standardize){
    # If covariates were standardized, rescale the starting values
    Z_sd <- attr(init$Z,"scaled:scale")
    SV$beta <- SV$beta*Z_sd
  }
  
  if (any(!is.finite(SV$b))) 
    SV$b <- rep(0,init$ncovCURE+1)
  
  if (any(!is.finite(SV$beta))) 
    SV$beta <- rep(0,init$ncovSURV)
  
  SV$base_hazard <- cox_base_hazard_cpp(SV$beta,init$nobs,init$K,
                                        init$tstart,init$tstop,init$status,
                                        init$sort1,init$sort2,init$Z,
                                        rep(0,init$nobs),ties)
  
  if (any(!is.finite(SV$base_hazard)))  
    SV$base_hazard <- c(0,diff(init$fail_times))
  
  return(SV)
}



penPHcure.check.pen.tuneGrid <- function(pen.tuneGrid,pen.type){
  
  if ( is.null(pen.tuneGrid) || !is.list(pen.tuneGrid)){
    if (pen.type == "SCAD") {
      pen.tuneGrid <- list(CURE = list(lambda = exp(seq(-7,0,length.out = 10)),
                                       a=3.7),
                           SURV = list(lambda = exp(seq(-7,0,length.out = 10)),
                                       a=3.7))
    } else if (pen.type == "LASSO"){
      pen.tuneGrid <- list(CURE = list(lambda = exp(seq(-7,0,length.out = 10))),
                           SURV = list(lambda = exp(seq(-7,0,length.out = 10))))
    }
  } else {
    if (!is.vector(pen.tuneGrid$CURE$lambda) || 
        any(pen.tuneGrid$CURE$lambda<0)){
      warning('penPHcure.check.pen.tuneGrid :: pen.tuneGrid$CURE$lambda should 
               be a numeric vector with values >= 0, default values were used.')
      pen.tuneGrid$CURE$lambda <- exp(seq(-7,0,length.out = 10))
    }
    if (!is.vector(pen.tuneGrid$SURV$lambda) || 
        any(pen.tuneGrid$SURV$lambda<0)){
      warning('penPHcure.check.pen.tuneGrid :: pen.tuneGrid$SURV$lambda should 
               be a numeric vector with values >= 0, default values were used.')
      pen.tuneGrid$SURV$lambda <- exp(seq(-7,0,length.out = 10))
    }
    if (pen.type == "SCAD") {
      if (!is.vector(pen.tuneGrid$CURE$a) || any(pen.tuneGrid$CURE$a<=2)){
        warning('penPHcure.check.pen.tuneGrid :: pen.tuneGrid$CURE$a should be a 
                 numeric vector with values > 2, default values were used.')
        pen.tuneGrid$CURE$a <- 3.7
      }
      if (!is.vector(pen.tuneGrid$SURV$a) || any(pen.tuneGrid$SURV$a<=2)){
        warning('penPHcure.check.pen.tuneGrid :: pen.tuneGrid$SURV$a should be a 
                 numeric vector with values > 2, default values were used.')
        pen.tuneGrid$SURV$a <- 3.7
      }
    }
  }
  
  return(pen.tuneGrid)
}



penPHcure.check.pen.weights <- function(pen.weights,init){
  ncovCURE <- init$ncovCURE
  ncovSURV <- init$ncovSURV
  if (is.null(pen.weights) || !is.list(pen.weights)){
    pen.weights <- list(CURE = rep(1,ncovCURE), 
                        SURV = rep(1,ncovSURV))
    # We never penalize the intercept in the cure component.
    pen.weights$CURE <- c(0,pen.weights$CURE) 
  } else{
    if (is.null(pen.weights$CURE) || !is.vector(pen.weights$CURE) || 
        any(pen.weights$CURE<0) || any(!is.finite(pen.weights$CURE)) || 
        length(pen.weights$CURE)!=init$ncovCURE ){
      warning('penPHcure.check.pen.weights :: penalty weights for the cure 
               (incidence) component not correctly specified.')
      pen.weights$CURE <- rep(1,ncovCURE)
    }
    # We never penalize the intercept in the cure component.
    pen.weights$CURE <- c(0,pen.weights$CURE)
    if(is.null(pen.weights$SURV) || !is.vector(pen.weights$SURV) || 
       any(pen.weights$SURV<0) || any(!is.finite(pen.weights$SURV)) || 
       length(pen.weights$SURV)!=init$ncovSURV ){
      warning('penPHcure.check.pen.weights :: penalty weights for the survival 
               (latency) component not correctly specified.')
      pen.weights$SURV <- rep(1,ncovSURV)
    }
  } 
  return(pen.weights)
}

