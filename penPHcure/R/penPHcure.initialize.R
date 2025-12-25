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

penPHcure.initialize <- function(formula,cureform,data,X,ties,maxIterNR,
                                 maxIterEM,tol,constraint,standardize,which.X,
                                 print.details,warnings){
  init <- list()
  init$nobs <- nrow(data)
  mdl <- model.frame(formula,data=data)
  init$survform <- formula
  SurvObj <- mdl[,1]
  init$tstart <- SurvObj[,1]
  init$tstop <- SurvObj[,2]
  init$status <- SurvObj[,3]
  init$sort1 <- order(-init$tstart) - 1L # C++ indexing 
  init$sort2 <- order(-init$tstop) - 1L  # C++ indexing 
  fail_times <- sort(init$tstop[init$status==1])
  init$nfails <- as.numeric(table(fail_times))
  init$fail_times <- unique(fail_times)
  init$K <- length(init$fail_times)
  init$Z <- model.matrix(formula,data=data)[,-1,drop=FALSE]
  if(is.null(X)){
    X <- model.matrix(cureform,data=data)[,drop=FALSE]
    init$cureform <- cureform
    if (!any(colnames(X)=="(Intercept)"))
      stop('penPHcure.initialize :: intercept must be 
           included in the incidence component.')
    init_cpp <- initialize_PHcure_cpp(init$tstart,init$tstop,
                                      init$status,X,which.X)
    init$X_FIX <- init_cpp$X_FIX
    colnames(init$X_FIX) <- colnames(X)
    rownames(init$X_FIX) <- 1:init_cpp$N
    init$which.X <- which.X
  } else{
    init_cpp <- initialize_PHcure_noX_cpp(init$tstart,init$tstop,init$status)
    if (!is.matrix(X) || nrow(X)!=init_cpp$N)
      stop('penPHcure.initialize :: argument X in function penPHcure() 
           not, or not correctly, specified. It should be a matrix with ',
           init_cpp$N,' rows.')
    else init$X_FIX <- cbind(1,X)
  }
  
  init$N <- init_cpp$N
  init$status_FIX <- init_cpp$status_FIX
  init$nobs_i <- init_cpp$nobs_i
  init$posEND <- cumsum(init_cpp$nobs_i)
  init$posSTART <- init$posEND - as.vector(init_cpp$nobs_i) + 1L
  
  if (is.null(colnames(init$X_FIX)))
    colnames(init$X_FIX) <- c("(Intercept)",
                              paste("x.",1:ncol(X),sep=''))
  if (is.null(rownames(init$X_FIX)))
    rownames(init$X_FIX) <- 1:init$N
  
  if (standardize){
    Z_mean <- apply(init$Z,2,mean)
    X_mean <- apply(init$X_FIX,2,mean)
    X_mean["(Intercept)"] <- 0
    Z_sd <- apply(init$Z,2,sd)
    X_sd <- apply(init$X_FIX,2,sd)
    X_sd["(Intercept)"] <- 1
    init$Z <- scale(init$Z,center=Z_mean,scale=Z_sd)
    init$X_FIX <- scale(init$X_FIX,center=X_mean,scale=X_sd)
  }
  
  if (any(!is.finite(init$X_FIX)))
    stop('penPHcure.initialize :: non-finite elements in the covariates 
         provided in the cure (incidence) component.')
  
  if (any(!is.finite(init$Z)))
    stop('penPHcure.initialize :: non-finite elements in the covariates 
         provided in the survival (latency) component.')

  init$standardize <- standardize
  init$ncovCURE <- ncol(init$X_FIX) - 1L
  init$ncovSURV <- ncol(init$Z)
  init$censoring <- 1-sum(init$status_FIX)/init$N
  init$isTies <- any(init$nfails>1)
  init$ties <- ties
  init$maxIterNR <- maxIterNR
  init$maxIterEM <- maxIterEM
  init$tol <- tol 
  init$constraint <- constraint
  init$warnings <- warnings
  init$print.details <- print.details
  
  class(init) <- 'PHcure.init'
  
  return(init)
}