#' Estimate distances between oscillators
#' 
#' Estimates the distances between the oscillators of a \code{\link{ODEnetwork}}
#' from an equilibrium state.
#' 
#' @param odenet [\code{ODEnetwork}]\cr
#' List of class \code{\link{ODEnetwork}}.
#' @param equilibrium [\code{numeric(n)}]\cr
#' The desired equilibrium positions of the oscillators.
#' @param distGround [\code{character(1)}] or [\code{character(n)}]\cr
#' \code{"combined"} estimates one value for all distances of the oscillators to the ground.
#' Optimisation starts from \code{median(equilibrium)}.\cr
#' \code{"individual"} estimates individual distance values for every oscillator.
#' Optimisation starts from \code{equilibrium}.\cr
#' \code{"fixed"} no estimation of the distances to the ground. 
#' Set to diagonal of distances matrix in \code{\link{ODEnetwork}}.\cr
#' \code{character(n)} specifies groups of oscillators which distances to the ground are 
#' estimated by the same value. Optimisation starts from \code{median(equilibrium)} of the 
#' specified groups.\cr
#' Default is \code{"combined"}
#' @param optim.control [\code{list()}]\cr
#' A list of control parameters for optim.
#' See \code{\link{optim}}.
#' @return an extended list of class \code{\link{ODEnetwork}}.\cr
#' Matrix of distances is added or overwritten.
#' @export
#' @examples
#' masses <- c(1, 1)
#' dampers <- diag(c(1, 1))
#' springs <- diag(c(1, 1))
#' springs[1, 2] <- 1
#' equilibrium <- c(1/3, 5/3)
#' odenet <- ODEnetwork(masses, dampers, springs)
#' estimateDistances(odenet, equilibrium)$distances
#' estimateDistances(odenet, equilibrium, distGround="individual")$distances

estimateDistances <- function(odenet, equilibrium
                , distGround=c("combined", "individual", "fixed", c("A", "B", "123", "A"))
                , optim.control=list()) {
  UseMethod("estimateDistances")
}

#' @method estimateDistances ODEnetwork
#' @export
estimateDistances.ODEnetwork <- function(odenet, equilibrium, distGround="combined"
                                         , optim.control=list()) {
  # number of oscillators
  cN <- length(odenet$masses)
  # Equilibrium
  assertNumeric(equilibrium, any.missing=FALSE, len=cN)
  assertVector(equilibrium, strict=TRUE)
  # distances to the ground
  assert(
    checkCharacter(distGround, any.missing=FALSE, len=1L)
    , checkCharacter(distGround, any.missing=FALSE, len=cN)
    )
  # check arguments of distGround
  if (cN > 1 && length(distGround) == 1) {
    assertChoice(distGround, c("combined", "individual", "fixed"))
  }
  
  # delete names
  names(equilibrium) <- NULL
  
  # exception for one mass
  if (cN == 1 && distGround != "fixed") {
    odenet <- updateOscillators(odenet, ParamVec=c(r.1=equilibrium))
    return(odenet)
  }
  
  # create parameter vector
  cParams <- numeric()
  # check distance estimation to the ground
  if (length(distGround) == 1) {
    if (distGround == "combined") {
      # one parameter for all distances to the ground
      cParams <- c(r.glob = stats::median(equilibrium))
    } else if (distGround == "individual") {
      # one parameter for each distance
      cParams <- c(equilibrium)
      names(cParams) <- paste("r.glob", 1:cN, sep=".")
    }
  } else {
    # character vector indicates the groups for the parameter estimation
    for (grp in unique(distGround)) {
      cParams <- c(cParams, stats::median(equilibrium[distGround == grp]))
      names(cParams)[length(cParams)] <- paste("r.glob", paste(which(distGround == grp), collapse = "."), sep = ".")
    }
  }
  
  # add distances between oscillators with respect to springs and dampers to parameter vector
  locat.spring <- which(odenet$springs != 0, arr.ind=TRUE)
  
  ## Ohne Diagnoale, Eintraege doppelt nicht noetig
  locat.ok <- apply(locat.spring, 1, function(x) x[1] < x[2])
  
  if (sum(locat.ok) == 0) { # exit, if no free parameters available
    message("All parameters are fixed.")
    return(odenet)
  }

  ## matrix() neotig, falls nur eine Verbindung
  locat.spring <- matrix(locat.spring[locat.ok, ], ncol=2)
  
  if (is.null(nrow(locat.spring))) locat.spring <- t(locat.spring)
  
  for (i in 1:nrow(locat.spring)) {
    cParams <- c(cParams, odenet$distances[locat.spring[i,1], locat.spring[i,2]])
    names(cParams)[length(cParams)] <- paste(c("r", locat.spring[i ,]), collapse = ".")
  }
  
  # calculate target vector
  mK <- odenet$springs
  diag(mK) <- -rowSums(mK)
  mK <- -mK
  bTarget <- -mK %*% equilibrium
  
  # Cacluate regularisation target for distances
#   dista <- diag(equilibrium)
  dista <- odenet$distances
  if (nrow(dista) == 30) {
    dista <- rep(345, 30)
    dista[c(12, 20)] <- 220
    dista <- diag(dista)
  }
  
  for (i in 1:nrow(locat.spring)) {
    row <- locat.spring[i,1]
    col <- locat.spring[i,2]
    dista[row, col] <- diff(c(dista[row,row], dista[col, col]))
  }

  pTarget <- dista[locat.spring]
  names(pTarget) <- paste("r.", apply(locat.spring, 1, paste, collapse="."), sep="")
  pTarget <- c(cParams[grep("glob", names(cParams))], pTarget)
  if (nrow(dista) == 30) {
    pTarget[c(1, 2)] <- c(345, 220)
  }
#   print(pTarget)
  
  # define cost function
  distCost <- function(cParameters, pTarget) {
#     cParameters <- splitGlobalParams(cParameters)
    odenet <- updateOscillators(odenet, ParamVec=splitGlobalParams(cParameters))
    # get distances and convert to correct form
    mR <- odenet$distances
    diag(mR) <- -diag(mR)
    mR[lower.tri(mR)] <- -mR[lower.tri(mR)]
    # calculate vector b with b_i = sum(k_ij*r_ij, j=1..n)
    b <- diag(odenet$springs %*% t(mR))
    
    ## Gewichte definieren
    # target.zero <- pTarget == 0
    # gewi <- rep(1, length(pTarget))
    # if (sum(target.zero) > 0) gewi[target.zero] <- 1
    # if (sum(!target.zero) > 0) gewi[!target.zero] <- 1/pTarget[!target.zero]
    
#     gewi <- rep(1, length(cParameters))
    
    # return(sum((b-bTarget)^2) +  sum(gewi %*% (cParameters - pTarget)^2))
#     print("cParams:\n")
#     print(cParams)
#     print("cParameters:\n")
#     print(cParameters)
#     print("pTarget:\n")
#     print(pTarget)
    delta.b <- sum((b-bTarget)^2)
#     print(sprintf("Resid b: %.2f", delta.b))
#     print(sprintf("Resid Param: %.2f", sum((cParameters-pTarget)^2)))
    return(delta.b
           + sum((cParameters-pTarget)^2) * exp(-10*delta.b)
           )
        
    # return SSE
    #     return(sum((b-bTarget)^2))
    # return residuals
    # return(bTarget-b)
  }
  
  # split the parameter vector with respect to estimate (grouped) global distances
  splitGlobalParams <- function(cParameters) {
    if (sum(grepl("r\\.glob", names(cParameters))) > 0) {
      # estimate different groups of global distances
      # extract the values
      globVal <- cParameters[grep("r\\.glob", names(cParameters))]
      cParameters <- cParameters[-grep("r\\.glob", names(cParameters))]
      # one global distance, or different ones
      if (length(globVal) == 1) {
        lstMassGrps <- list(1:length(odenet$masses))
      } else {
        lstMassGrps <- gsub("r\\.glob\\.", "", names(globVal))
        lstMassGrps <- strsplit(lstMassGrps, ".", fixed = TRUE)
      }
      # multiply values to the correct r.i's
      for (i in length(lstMassGrps):1) {
        cParameters <- c(rep(globVal[i], length(lstMassGrps[[i]])), cParameters)
        names(cParameters)[1:length(lstMassGrps[[i]])] <- paste("r", lstMassGrps[[i]], sep = ".")
      }
    }
    return(cParameters)
  }
  
  # optimise parameters
#   print(paste("Params:", length(cParams)))
#   print(paste("Resid:", length(distCost(cParams, pTarget))))
  
  firstFit <- stats::optim(cParams, distCost, pTarget=pTarget, method="BFGS", control=optim.control)
  ## Check, ob neuer Lauf nennenswert besseres Ergebnis bringt
  checkFit <- stats::optim(firstFit$par, distCost, pTarget=pTarget, method="BFGS", control=optim.control)
  if (checkFit$value/firstFit$value < 0.999)
    warning("Optimization by estimateDistances() seems to be unsuccessful!")
  
  #   # Throw warnings
  #   if (firstFit$convergence != 0) {
  #     warningf(paste("No successful completition. Code:", firstFit$convergence))
  #   }
  #   if (firstFit$value > 1e-7 * distCost(cParams)) {
  #   if (firstFit$value > 10 * sqrt(.Machine$double.eps) * distCost(cParams)) {
  #     warningf(paste("The SSE of the distances is large:", firstFit$value))
  #   }
  
  # update the optimal values to the odenet
  odenet <- updateOscillators(odenet, ParamVec=splitGlobalParams(checkFit$par))
  
  return(odenet)
}
