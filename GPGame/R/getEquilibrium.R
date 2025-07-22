#----------------------------------------------------------------
#' Computes the equilibrium of three types of games, given a matrix of objectives (or a set of matrices) and the structure of the strategy space.
#' @title Equilibrium computation of a discrete game for a given matrix with objectives values
#' @param Z is a matrix of size [\code{npts x nsim*nobj}] of objective values, see details,
#' @param equilibrium considered type, one of \code{"NE"}, \code{"NKSE"}, \code{"KSE"}, \code{"CKSE"}
#' @param nobj nb of objectives (or players)
#' @param n.s scalar of vector. If scalar, total number of strategies (to be divided equally among players),
#'  otherwise number of strategies per player.
#' @param expanded.indices is a matrix containing the indices of the \code{integ.pts} on the grid, see \code{\link[GPGame]{generate_integ_pts}}
#' @param return.design Boolean; if \code{TRUE}, the index of the optimal strategy is returned (otherwise only the pay-off is returned)
#' @param sorted Boolean; if \code{TRUE}, the last column of expanded.indices is assumed to be sorted in increasing order. This provides a substantial efficiency gain.
#' @param cross Should the simulation be crossed? (For "NE" only - may be dropped in future versions)
#' @param kweights kriging weights for \code{CKS} (TESTING)
## ' @param NSobs Nadir and Shadow points of the observations (useful for \code{KSE} in the non-noisy case), number of columns must match with \code{Z}
#' @param Nadir,Shadow optional vectors of size \code{nobj}. Replaces the nadir and/or shadow point for \code{KSE}. Some coordinates can be set to \code{Inf} (resp. -\code{Inf}).
#' @param calibcontrol an optional list for calibration problems, containing \code{target} a vector of target values for the objectives, 
#' \code{log} a Boolean stating if a log transformation should be used or not and 
#' \code{offset} a (small) scalar so that each objective is log(offset + (y-T^2)).
#' @return A list with elements:
#' \itemize{
#' \item \code{NEPoff} vector of pay-offs at the equilibrium or matrix of pay-offs at the equilibria;
#' \item \code{NE} the corresponding design(s), if \code{return.design} is \code{TRUE}.
#' }
#' @details If \code{nsim=1}, each line of \code{Z} contains the pay-offs of the different players for a given strategy s: [obj1(s), obj2(s), ...].
#' The position of the strategy \code{s} in the grid is given by the corresponding line of \code{expanded.indices}. If \code{nsim>1}, (vectorized call) \code{Z} contains
#' different trajectories for each pay-off: each line is [obj1_1(x), obj1_2(x), ... obj2_1(x), obj2_2(x), ...].
#' @export
#' @importFrom Rcpp evalCpp
#' @examples
#' \donttest{
#' ## Setup
#' fun <- function (x)
#' {
#'   if (is.null(dim(x)))    x <- matrix(x, nrow = 1)
#'   b1 <- 15 * x[, 1] - 5
#'   b2 <- 15 * x[, 2]
#'   return(cbind((b2 - 5.1*(b1/(2*pi))^2 + 5/pi*b1 - 6)^2 + 10*((1 - 1/(8*pi)) * cos(b1) + 1),
#'                -sqrt((10.5 - b1)*(b1 + 5.5)*(b2 + 0.5)) - 1/30*(b2 - 5.1*(b1/(2*pi))^2 - 6)^2-
#'                 1/3 * ((1 - 1/(8 * pi)) * cos(b1) + 1)))
#' }
#'
#' d <- nobj <- 2
#'
#' # Generate grid of strategies for Nash and Nash-Kalai-Smorodinsky
#' n.s <- c(11,11) # number of strategies per player
#' x.to.obj <- 1:2 # allocate objectives to players
#' integcontrol <- generate_integ_pts(n.s=n.s,d=d,nobj=nobj,x.to.obj=x.to.obj,gridtype="cartesian")
#' integ.pts <- integcontrol$integ.pts
#' expanded.indices <- integcontrol$expanded.indices
#'
#' # Compute the pay-off on the grid
#' response.grid <- t(apply(integ.pts, 1, fun))
#'
#' # Compute the Nash equilibrium (NE)
#' trueEq <- getEquilibrium(Z = response.grid, equilibrium = "NE", nobj = nobj, n.s = n.s,
#'                          return.design = TRUE, expanded.indices = expanded.indices,
#'                          sorted = !is.unsorted(expanded.indices[,2]))
#'
#' # Pay-off at equilibrium
#' print(trueEq$NEPoff)
#'
#' # Optimal strategy
#' print(integ.pts[trueEq$NE,])
#'
#' # Index of the optimal strategy in the grid
#' print(expanded.indices[trueEq$NE,])
#'
#' # Plots
#' oldpar <- par(mfrow = c(1,2))
#' plotGameGrid(fun = fun, n.grid = n.s, x.to.obj = x.to.obj, integcontrol=integcontrol,
#'              equilibrium = "NE")
#'
#' # Compute KS equilibrium (KSE)
#' trueKSEq <- getEquilibrium(Z = response.grid, equilibrium = "KSE", nobj = nobj,
#'                          return.design = TRUE, sorted = !is.unsorted(expanded.indices[,2]))
#'
#' # Pay-off at equilibrium
#' print(trueKSEq$NEPoff)
#'
#' # Optimal strategy
#' print(integ.pts[trueKSEq$NE,])
#'
#' plotGameGrid(fun = fun, n.grid = n.s, integcontrol=integcontrol,
#'              equilibrium = "KSE", fun.grid = response.grid)
#'
#' # Compute the Nash equilibrium (NE)
#' trueNKSEq <- getEquilibrium(Z = response.grid, equilibrium = "NKSE", nobj = nobj, n.s = n.s,
#'                          return.design = TRUE, expanded.indices = expanded.indices,
#'                          sorted = !is.unsorted(expanded.indices[,2]))
#'
#' # Pay-off at equilibrium
#' print(trueNKSEq$NEPoff)
#'
#' # Optimal strategy
#' print(integ.pts[trueNKSEq$NE,])
#'
#' # Index of the optimal strategy in the grid
#' print(expanded.indices[trueNKSEq$NE,])
#'
#' # Plots
#' plotGameGrid(fun = fun, n.grid = n.s, x.to.obj = x.to.obj, integcontrol=integcontrol,
#'              equilibrium = "NKSE")
#' par(oldpar)
#' }
getEquilibrium <- function(Z, equilibrium = c("NE", "NKSE", "KSE", "CKSE"), nobj=2, n.s, expanded.indices=NULL, return.design=FALSE,
                           sorted=FALSE, cross=FALSE, kweights = NULL, Nadir=NULL, Shadow=NULL, calibcontrol=NULL){
  #### Choose appropriate function ###################
  if (equilibrium=="NE"){
    return(getNashEquilibrium(Z = Z, nobj = nobj, n.s = n.s, expanded.indices = expanded.indices, return.design = return.design, sorted = sorted, cross = cross))
  } else if (equilibrium=="KSE") {
    return(getKSequilibrium(Z = Z, nobj = nobj, n.s = n.s, return.design = return.design, sorted = sorted, Nadir=Nadir, Shadow=Shadow))
  } else if (equilibrium=="CKSE"){
    return(getCKSequilibrium(Z = Z, nobj = nobj, n.s = n.s, return.design = return.design, sorted = sorted, cross = cross,
                             kweights = kweights, calibcontrol=calibcontrol))
  } else if (equilibrium=="NKSE") {
    return(getNKSequilibrium(Z = Z, nobj = nobj, n.s = n.s, expanded.indices = expanded.indices, return.design = return.design, sorted = sorted, cross = cross))
  } else {
    stop("wrong crit \n")
  }
}

#----------------------------------------------------------------
#' Computes the equilibrium of finite Kalai-Smorodinski games given a matrix of objectives (or a set of matrices) and the structure of the strategy space.
#' @title Nash equilibrium computation
#' @param Z is a matrix of size [npts x nsim*nobj] of objective values, see details,
#' @param nobj nb of objectives (or players)
#' @param n.s scalar of vector. If scalar, total nb of strategies (to be divided equally among players), otherwise nb of strategies per player.
#' @param expanded.indices is a matrix containing the indices of the integ.pts on the grid
#' @param return.design Boolean; if TRUE, the index of the optimal strategy is returned (otherwise only the pay-off is returned)
#' @param sorted Boolean; if TRUE, the last column of expanded.indices is assumed to be sorted in increasing order. This provides a substantial efficiency gain.
#' @param cross if TRUE, all the combinations of trajectories are used
#' @param ... not used, for compatibility
#' @details If \code{nsim}=1, each line of Z contains the pay-offs of the different players for a given strategy s: [obj1(s), obj2(s), ...].
#' The position of the strategy s in the grid is given by the corresponding line of \code{expanded.indices}. If \code{nsim}>1, (vectorized call) Z contains
#' different trajectories for each pay-off: each line is [obj1_1(x), obj1_2(x), ... obj2_1(x), obj2_2(x), ...].
#' @noRd
getNashEquilibrium <- function(Z, nobj=2, n.s, expanded.indices=NULL, return.design=FALSE, sorted=FALSE, cross=FALSE,...){
  
  if(cross){
    nsim <- ncol(Z)/nobj
    
    if(!sorted)
      warning("Non sorted case not implemented with cross")
    
    combisim <- NULL
    for(i in 1:nobj)
      combisim <- c(combisim, list(0:(nsim - 1))) ## starts at 0 for Rcpp indices compatibility
    combisim <- as.matrix(expand.grid(combisim))
    NE <- PSNE_sparseMat_cross(n.s, Z, expanded.indices - as.integer(1), combisim = combisim, ncross = nsim^(nobj-1))
    
    if (return.design == FALSE) return(getPoffsCross(isNash = NE, Poffs = Z, combisim = combisim, nsim = nsim))
    else                     return(list(NEPoff = getPoffsCross(isNash = NE, Poffs = Z, combisim = combisim, nsim = nsim), NE = unlist(apply(NE, 2, which))))
    
  }
  
  
  if (sorted) {
    NE <- PSNE_sparseMat_sorted(n.s, Z, expanded.indices - as.integer(1)) ## -1 for compatibility with Rcpp
  } else {
    NE <- PSNE_sparseMat(n.s, Z, expanded.indices - as.integer(1)) ## -1 for compatibility with Rcpp
  }
  
  NEPoff <- matrix(NA, 1, nobj)
  
  if (!is.null(NE) && length(which(NE)) > 0) {
    if (!return.design) return(getPoffs(NE, Z, nsim = ncol(Z)/nobj, nobj))
    else return(list(NEPoff = getPoffs(NE, Z, nsim = ncol(Z)/nobj, nobj), NE = unlist(apply(NE, 2, which))))
  }else{
    if(return.design)
      return(list(NEPoff = NEPoff, NE = NA))
    return(NEPoff)
  }
}
#----------------------------------------------------------------
#' Computes the equilibrium of finite Nash/Kalai-Smorodinski games given a matrix of objectives (or a set of matrices) and the structure of the strategy space.
#' @title Nash/Kalai-Smorodinski equilibrium
#' @param Z is a matrix of size [npts x nsim*nobj] of objective values, see details,
#' @param nobj nb of objectives (or players)
#' @param n.s scalar of vector. If scalar, total nb of strategies (to be divided equally among players), otherwise nb of strategies per player.
#' @param expanded.indices is a matrix containing the indices of the integ.pts on the grid
#' @param return.design Boolean; if TRUE, the index of the optimal strategy is returned (otherwise only the pay-off is returned)
#' @param cross if TRUE, all the combinations of trajectories are used
#' @param ... not used, for compatibility
#' @details If \code{nsim}=1, each line of Z contains the pay-offs of the different players for a given strategy s: [obj1(s), obj2(s), ...].
#' The position of the strategy s in the grid is given by the corresponding line of \code{expanded.indices}. If \code{nsim}>1, (vectorized call) Z contains
#' different trajectories for each pay-off: each line is [obj1_1(x), obj1_2(x), ... obj2_1(x), obj2_2(x), ...].
#'
#' @noRd
getNKSequilibrium <- function(Z, nobj=2, n.s, return.design=FALSE, expanded.indices=NULL, cross=FALSE, ...){
  
  # allShadow <- getNashEquilibrium(Z=Z, nobj=nobj, n.s=n.s, expanded.indices=expanded.indices, cross=cross)
  # if (any(is.na(allShadow))) {
  #   NEPoff <- rep(NA, nobj)
  #   NE <- NA
  # } else {
  
  nsim <- ncol(Z) / nobj
  
  if (cross) {
    indices <- list()
    for (u in 1:nobj) indices[[u]] <- (1:nsim)+(u-1)*nsim
    Jmat <- as.matrix(expand.grid(indices))
  } else {
    Jmat <- matrix(NA, nsim, nobj)
    for (u in 1:nsim) {
      Jmat[u,] <- seq(u, ncol(Z), nsim)
    }
  }
  
  # NEPoff <- matrix(NA, nrow(Jmat), nobj)
  # NE     <- rep(NA, nrow(Jmat))
  NE <- NEPoff <- NULL
  
  for (u in 1:nrow(Jmat)) {
    
    # Only look at first NE if there are several
    Nadir <- getNashEquilibrium(Z=Z, nobj=nobj, n.s=n.s, expanded.indices=expanded.indices, cross=cross)[1,]
    
    J <- Jmat[u,]
    # I <- which(!is_dominated(t(Z[,J])))
    I <- nonDom(Z[,J, drop = FALSE], return.idx = TRUE) 
    Zred <- Z[I, J, drop=FALSE]
    Shadow <- colMins(Zred) # apply(Zred, 2, min)
    
    # Shadow <- allShadow[u,]
    
    if (nrow(Zred)==1) {
      i <- 1
    } else {
      alldist2 <- rowSums((Zred - matrix(rep(Nadir, nrow(Zred)), ncol=nobj, byrow=T))^2) -
        as.numeric(((Zred - matrix(rep(Nadir, nrow(Zred)), ncol=nobj, byrow=T))%*%(Nadir - Shadow))^2) /
        drop(crossprod(Nadir - Shadow, Nadir - Shadow))
      i <- which.min(alldist2)
    }
    NEPoff <- rbind(NEPoff, Zred[i,])
    NE     <- c(NE, I[i])
  }
  
  if(is.null(NEPoff)){
    NEPoff <- matrix(NA, nrow(Jmat), nobj)
    NE     <- rep(NA, nrow(Jmat))
  }
  
  # }
  if (return.design==FALSE) return(NEPoff)
  else                     return(list(NEPoff=NEPoff, NE=I[i]))
}
#----------------------------------------------------------------
#' Computes the equilibrium of finite Kalai-Smorodinski games given a matrix of objectives (or a set of matrices) and the structure of the strategy space.
#' @title Kalai-Smorodinski equilibrium computation
#' @param Z is a matrix of size [npts x nsim*nobj] of objective values, see details,
#' @param nobj nb of objectives (or players)
#' @param return.design Boolean; if TRUE, the index of the optimal strategy is returned (otherwise only the pay-off is returned)
#' @param Nadir,Shadow optional vectors of size \code{nobj} to fix the Nadir and/or Shadow to a preset value. If only a subset of values needs to be defined, 
#' the other coordinates can be set to \code{Inf} (resp. -\code{Inf}).
#' @param ... not used, for compatibility
#' @details If \code{nsim}=1, each line of Z contains the pay-offs of the different players for a given strategy s: [obj1(s), obj2(s), ...].
#' The position of the strategy s in the grid is given by the corresponding line of \code{expanded.indices}. If \code{nsim}>1, (vectorized call) Z contains
#' different trajectories for each pay-off: each line is [obj1_1(x), obj1_2(x), ... obj2_1(x), obj2_2(x), ...].
#' @noRd
#' @importFrom stats var
#' @importFrom matrixStats colMins colMaxs
getKSequilibrium <- function(Z, nobj=2, return.design=FALSE, Nadir = NULL, Shadow=NULL, ...){
  
  nsim <- ncol(Z) / nobj
  
  # Jmat <- matrix(NA, nsim, nobj)
  # for (u in 1:nsim) {
  #   Jmat[u,] <- seq(u, ncol(Z), nsim)
  # }
  Jmat <- matrix(1:(nsim*nobj), nsim, nobj)
  
  NEPoff <- matrix(NA, nrow(Jmat), nobj)
  NE     <- rep(NA, nrow(Jmat))
  
  for (u in 1:nrow(Jmat)) {
    J <- Jmat[u,]
    
    # non-dominated points are useful mostly for Shadow computation
    if(is.null(Nadir)){
      I <- nonDom(Z[,J, drop = FALSE], return.idx = TRUE)
    }else{
      I <- 1:nrow(Z)
    }
    
    Zred <- Z[I, J, drop=FALSE]
    
    if (nrow(Zred)==1){
      i <- 1
    } else {
      Shadow_emp <- colMins(Zred)
      Nadir_emp  <- colMaxs(Zred)
      
      if (!is.null(Nadir)) Nadir_emp <- pmin(Nadir, Nadir_emp)
      if (!is.null(Shadow)) Shadow_emp <- pmax(Shadow, Shadow_emp)
      
      i <- getKS_cpp(Zred, Nadir = Nadir_emp, Shadow = Shadow_emp)
    }
    
    NEPoff[u,] <- Zred[i,,drop = FALSE]
    NE[u]     <- I[i]
  }
  
  if (return.design==FALSE) return(NEPoff)
  else                     return(list(NEPoff=NEPoff, NE=NE))
}

#----------------------------------------------------------------
#' Computes the equilibrium of finite Kalai-Smorodinski games given in the copula space, given a matrix of objectives (or a set of matrices) and the structure of the strategy space.
#' @title Copula Kalai-Smorodinski equilibrium computation
#' @param Z is a matrix of size [npts x nsim*nobj] of objective values, see details,
#' @param nobj nb of objectives (or players)
#' @param return.design Boolean; if TRUE, the index of the optimal strategy is returned (otherwise only the pay-off is returned)
#' @param kweights kriging weights for \code{CKS} (TESTING)
#' @param Nadir,Shadow optional vectors of size \code{nobj} to fix the Nadir and/or Shadow to a preset value. By default, in the copula space the nadir point is 1 and the shadow point is 0.
#' @param calibcontrol an optional list for calibration problems, containing \code{target} a vector of target values for the objectives, 
#' \code{log} a Boolean stating if a log transformation should be used or not and 
#' \code{offset} a (small) scalar so that each objective is log(offset + (y-T^2)).
#' @param ... not used, for compatibility
#' @details If \code{nsim}=1, each line of Z contains the pay-offs of the different players for a given strategy s: [obj1(s), obj2(s), ...].
#' The position of the strategy s in the grid is given by the corresponding line of \code{expanded.indices}. If \code{nsim}>1, (vectorized call) Z contains
#' different trajectories for each pay-off: each line is [obj1_1(x), obj1_2(x), ... obj2_1(x), obj2_2(x), ...].
#' @noRd
#' @importFrom stats var
getCKSequilibrium <- function(Z, nobj=2, return.design=FALSE, kweights = NULL, Nadir = NULL, Shadow=NULL, calibcontrol=NULL, ...){
  
  if(is.null(Nadir)) Nadir <- rep(1, nobj)
  if(is.null(Shadow)) Shadow <- rep(0, nobj)
  
  nsim <- ncol(Z) / nobj
  
  # Jmat <- matrix(NA, nsim, nobj)
  # for (u in 1:nsim) {
  #   Jmat[u,] <- seq(u, ncol(Z), nsim)
  # }
  Jmat <- matrix(1:(nsim*nobj), nsim, nobj)
  
  NEPoff <- matrix(NA, nrow(Jmat), nobj)
  NE     <- rep(NA, nrow(Jmat))
  
  if(!is.null(kweights)){
    Zrand <- matrix(NA, nrow = nrow(kweights[[1]]), ncol = ncol(Z))
    for(jj in 1:ncol(Jmat)) {
      if (!is.null(calibcontrol$target)) {
        # Calibration mode
        Zrand[, Jmat[,jj]] <- kweights[[jj]] %*% calibcontrol$Y[,Jmat[,jj]]
        Zrand[, Jmat[,jj]] <- (Zrand[, Jmat[,jj]] - calibcontrol$target[jj])^2
        if (calibcontrol$log) {
          Zrand[,jj] <- log(Zrand[,jj] + calibcontrol$offset)
        }
      } else {
        # Regular mode
        Zrand[,Jmat[,jj]] <- kweights[[jj]] %*% Z[,Jmat[,jj]]
      }
    }
  }
  
  for (u in 1:nrow(Jmat)) {
    J <- Jmat[u,]
    
    # # filtering dominated points could be useful only for limiting the cost of relative rank computation
    # if(is.null(kweights)){
    #   I <- 1:nrow(Z)
    # }else{
    #   I <- nonDom(Z[,J, drop = FALSE], return.idx = TRUE)
    # }
    # Zred <- Z[I,J, drop=FALSE]
    
    Zred <- Z[,J, drop=FALSE]
    
    if (nrow(Zred)==1) {
      i <- 1
    } else {
      if (!is.null(kweights)){
        i <- getCKS(Zrand[,J, drop = FALSE], Nadir = Nadir, Shadow = Shadow, Zred = Zred)$id
          
        # Ztarget <- getCKS(Zrand[,J, drop = FALSE], Nadir = Nadir, Shadow = Shadow)$CKS

        # Find closest point from Ztarget in Zred
        # i <- which.min(rowSums(sweep(Zred, 2, Ztarget, "-")^2))
        # i <- which.min(rowSums((Zred - matrix(Ztarget, nrow = length(I), ncol = nobj, byrow = T))^2)) # NOTE : can in fact be more efficient compared to sweep
      } else {
        i <- getCKS(Zred, Nadir = Nadir, Shadow = Shadow)$id
      }
    }
    NEPoff[u,] <- Zred[i,,drop = FALSE]
    NE[u]     <- i
  }
  if (return.design==FALSE) return(NEPoff)
  else                     return(list(NEPoff=NEPoff, NE=NE))
}


#' Compute KS equilibrium once Shadow and Nadir are given (minimization)
#' @param Z matrix ([npts x nobj] of objective values)
#' @param Nadir,Shadow vectors defining the line intersecting the Pareto front
#' @return list with elements \code{KS} for the element of \code{Z} realizing the KS and \code{id} for its row number.
#' @details slower, used for testing now 
#' @noRd
#' @importFrom matrixStats rowMins
getKS <- function(Z, Nadir, Shadow){
  ratios <- sweep(sweep(Z, 2, Nadir, "-"), 2, Shadow - Nadir, "/")
  i <- which.max(rowMins(ratios))
  return(list(KS = Z[i,, drop = FALSE], id = i))
}


#' Compute CKS equilibrium with given Shadow and Nadir (minimization)
#' @param Z matrix ([npts x nobj] of objective values), supposedly iid
#' @param Nadir,Shadow vectors defining the line intersecting the Pareto front
#' @param Zred matrix ([npts2 x nobj] of objective values), that may not be iid and in which the CKS is ultimately sought
#' @return list with elements \code{CKS} for the element of \code{Z} realizing the CKS and \code{id} for its row number.  
#' @noRd
#' @importFrom matrixStats colRanks
getCKS <- function(Z, Nadir, Shadow, Zred = NULL){
  if(!is.null(Zred)){
    # U2 <- rel_ranks_cpp(Z, Zred)
    
    # Ub <- apply(rbind(Z, Zred), 2, faster_rank)[-(1: nrow(Z)),,drop = F]
    Ub <- colRanks(rbind(Z, Zred), preserveShape = TRUE)[-(1:nrow(Z)),,drop = F]
    # Ur <- apply(Zred, 2, faster_rank)
    Ur <- colRanks(Zred, preserveShape = TRUE)
    U2 <- Ub - Ur
    
    CKS <- getKS_cpp(U2, Nadir = Nadir, Shadow = Shadow)
    return(list(CKS = Zred[CKS,, drop = FALSE], id = CKS))
  }else{
    # U <- apply(Z, 2, faster_rank)
    U <- colRanks(Z, preserveShape = TRUE)
    CKS <- getKS_cpp(U, Nadir = Nadir, Shadow = Shadow)
    return(list(CKS = Z[CKS,, drop = FALSE], id = CKS))
  }

}


## Seems faster
faster_rank <- function(x){
  return(order(order(x)))
}

#' #' @param Zrand iid matrix
#' #' @param Zred matrix from which the CKSE is searched, not iid
#' #' @noRd
#' rel_ranks <- function(Zrand, Zred){
#'   # Ured <- t(apply(Zred, 1, function(x) return(colSums(Zrand < x))))
#'   Ured <- matrix(NA, nrow = nrow(Zred), ncol = ncol(Zred))
#'   for(i in 1:nrow(Zred)){
#'     for(j in 1:ncol(Zrand)){
#'       Ured[i,j] <- sum(Zrand[,j] < Zred[i,j])
#'     }
#'   }
#'   return(Ured)
#' }

