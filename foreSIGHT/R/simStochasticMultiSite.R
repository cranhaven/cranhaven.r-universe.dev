# functions for multisite stochastic simulations

#----------------------------------------------------------------------------

simulateTargetMarg <- function(optimArgs = NULL,
                               simVar = NULL,
                               modelTag = NULL,
                               modelInfo = NULL,
                               attSel = NULL,
                               attPrim = NULL,
                               attInfo = NULL,
                               attInd = NULL,
                               datInd = NULL,
                               initCalibPars = NULL,
                               targetLoc = NULL,
                               parLoc = NULL,
                               parSim = NULL,
                               setSeed = NULL,
                               iRepTarg = NULL,
                               file = NULL,
                               obs = NULL,
                               spatialArgs = NULL) {
  nMod <- length(modelTag)
  
  if (nMod > 1) {
    stop("cannot have (nsite>1)&(nMod>1)")
  }
  
  timeStep <- aggNameShort[[modelInfoList[[modelTag]]$timeStep]]
  nTimes <- datInd[[modelTag]][[timeStep]]$nTimes
  
  ### DM NOTE: CODE CURRENTLY NOT PROPERLY SETUP TO WORK WITH MULTIPLE MODELS - PROB NEED TO ADD MODEL TO SIM LIST
  sim <- list(sites = NULL)
  
  mod <- modelTag[1]
  nsite <- dim(obs[[simVar[mod]]])[2]
  sites <- colnames(obs[[simVar[mod]]])
  if (is.null(sites)) {
    sites <- paste0("site", seq(1, nsite))
  }
  
  set.seed(setSeed)
  if (nsite == 1) {
    spatCorMatIn <- NULL
    MVTsampleMat <- matrix(stats::rnorm(n = nTimes, mean = 0., sd = 1.), ncol = 1)
  } else {
    if (!is.null(spatialArgs$spatCorMatIn)) {
      spatCorMatIn <- spatialArgs$spatCorMatIn
    } else {
      spatCorMatIn <- diag(nsite)
    }
    spatCorMatIn_PD <- as.matrix(Matrix::nearPD(spatCorMatIn, keepDiag = T)$mat)
    
    MVTsampleMat <- mvtnorm::rmvnorm(n = nTimes, sigma = spatCorMatIn_PD)
    colnames(MVTsampleMat) <- sites
  }
  
  simMultiSite <- list(sites = NULL, P = NULL) ### NEED TO FIX THIS UP FOR VARIABLES OTHER THAN P
  for (s in 1:nsite) {
    site <- sites[s]
    cat(site, "\n")
    # GET ATTRIBUTES OF OBSERVED DATA (testing attribute calc function)
    banner("OBSERVED BASELINE ATTRIBUTE CALCULATION", file)
    progress("Calculating attributes...", file)
    
    attObs <- attribute.calculator(
      attSel = attSel[unlist(attInd[[simVar[mod]]])],
      data = obs[[simVar[mod]]][, s],
      datInd = datInd[["obs"]][[timeStep]]
    )
    
    attObs <- unlist(attObs)
    attObs <- attObs[attSel] # unlist attObs and make sure order is correct
    
    progress(paste("Attributes of observed series - ", paste(attSel, ": ", signif(attObs, digits = 5), collapse = ", ", sep = ""), sep = ""), file)
    progress("Attributes calculated OK", file) # NEED SOME ACTUAL CHECKING HERE BEFORE PRONOUNCING OK
    
    obsTmp <- obs
    obsTmp[[simVar[mod]]] <- obs[[simVar[mod]]][, s]
    
    simMultiSite$sites[[site]] <- simulateTarget(
      optimArgs = optimArgs,
      simVar = simVar,
      modelTag = modelTag,
      modelInfo = modelInfo,
      attSel = attSel,
      attPrim = attPrim,
      attInfo = attInfo,
      attInd = attInd,
      datInd = datInd,
      initCalibPars = initCalibPars,
      targetLoc = targetLoc,
      attObs = attObs,
      parLoc = parLoc,
      parSim = parSim,
      setSeed = setSeed,
      iRepTarg = iRepTarg,
      obs = obsTmp,
      file = file,
      randomUnitNormalVector = MVTsampleMat[, s]
    )
    
    if (nsite > 1) {
      simMultiSite$P$sim <- cbind(simMultiSite$P$sim, simMultiSite$sites[[site]]$P$sim)
    }
  }
  
  if (nsite > 1) {
    sim <- simMultiSite
    sim$cor_par <- spatCorMatIn
  } else {
    sim <- simMultiSite$sites[[1]]
  }
  
  return(sim)
}

#----------------------------------------------------------------------------

simulateTargetCor <- function(optimArgs = NULL,
                              simIn = NULL,
                              simVar = NULL,
                              modelTag = NULL,
                              modelInfo = NULL,
                              attSel = NULL,
                              attPrim = NULL,
                              attInfo = NULL,
                              attInd = NULL,
                              datInd = NULL,
                              targetLoc = NULL,
                              attObs = NULL,
                              parLoc = NULL,
                              setSeed = NULL,
                              file = NULL,
                              randomUnitNormalVector = NULL, ### remove this
                              obs = NULL,
                              spatialArgs = NULL) {
  nMod <- length(modelTag)
  if (nMod > 1) {
    stop("cannot have nMod>1")
  } else {
    mod <- modelTag[[1]]
  }
  sites <- names(simIn$sites)
  nsite <- length(sites)
  timeStep <- aggNameShort[[modelInfoList[[modelTag]]$timeStep]]
  nTimes <- datInd[[modelTag]][[timeStep]]$nTimes
  
  modelInfoSites <- list()
  for (s in 1:nsite) {
    site <- sites[s]
    modelInfoSites[[site]] <- modelInfo
    modelInfoSites[[site]][[modelTag[mod]]]$minBound <- modelInfoSites[[site]][[modelTag[mod]]]$maxBound <- simIn$sites[[site]][[simVar]]$parS
  }
  
  cor_par <- matrix(nrow = nsite, ncol = nsite)
  for (s1 in 1:(nsite - 1)) {
    site1 <- sites[s1]
    cor_par[s1, s1] <- 1
    
    for (s2 in (s1 + 1):nsite) {
      site2 <- sites[s2]
      cor_obs <- stats::cor(obs$P[, c(s1, s2)])[1, 2]
      if (!is.null(spatialArgs$spatCorFac)) {
        cor_obs <- spatialArgs$spatCorFac * cor_obs
      }
      
      rho_1_2_list <- seq(0.01, 1, 0.01)
      cor_sim_list <- c()
      
      corMat <- matrix(nrow = 2, ncol = 2)
      corMat[1, 1] <- corMat[2, 2] <- 1
      
      for (i in 1:length(rho_1_2_list)) {
        corMat[1, 2] <- corMat[2, 1] <- rho_1_2_list[i]
        
        set.seed(setSeed)
        corMat_PD <- as.matrix(Matrix::nearPD(corMat, keepDiag = T)$mat)
        
        MVTsampleMat <- mvtnorm::rmvnorm(n = nTimes, sigma = corMat_PD)
        
        obsTmp1 <- obs
        obsTmp1[[simVar[mod]]] <- obs[[simVar[mod]]][, s1]
        
        sim1 <- simClim(
          parS = simIn$sites[[site1]][[simVar[1]]]$par,
          modelTag = modelTag,
          modelInfo = modelInfo[[mod]],
          datInd = datInd[[mod]][[timeStep]],
          randomTerm = list(randomUnitNormalVector = MVTsampleMat[, 1]),
          auxInfo = list(obs = obsTmp1)
        )
        
        obsTmp2 <- obs
        obsTmp2[[simVar[mod]]] <- obs[[simVar[mod]]][, s2]
        
        sim2 <- simClim(
          parS = simIn$sites[[site2]][[simVar[1]]]$par,
          modelTag = modelTag,
          modelInfo = modelInfo[[mod]],
          datInd = datInd[[mod]][[timeStep]],
          randomTerm = list(randomUnitNormalVector = MVTsampleMat[, 2]),
          auxInfo = list(obs = obsTmp2)
        )
        
        cor_sim_list[i] <- stats::cor(sim1, sim2)
      }
      
      diff <- abs(cor_sim_list - cor_obs)
      cor_par[s1, s2] <- cor_par[s2, s1] <- rho_1_2_list[which(diff == min(diff))]
    }
    cor_par[nsite, nsite] <- 1
  }
  
  sim <- list(sites = NULL, P = NULL)
  set.seed(setSeed)
  cor_par_PD <- as.matrix(Matrix::nearPD(cor_par, keepDiag = T)$mat)
  MVTsampleMat <- mvtnorm::rmvnorm(n = nTimes, sigma = cor_par_PD)
  
  for (s in 1:nsite) {
    site <- sites[s]
    
    obsTmp <- obs
    obsTmp[[simVar[mod]]] <- obs[[simVar[mod]]][, s]
    
    # note currently not setup to include auxInfo (i.e. obs, wdStatus)
    sim$sites[[site]] <- simClim(
      parS = simIn$sites[[site]][[simVar[1]]]$par,
      modelTag = modelTag,
      modelInfo = modelInfo[[mod]],
      datInd = datInd[[mod]][[timeStep]],
      randomTerm = list(randomUnitNormalVector = MVTsampleMat[, s]),
      auxInfo = list(obs = obsTmp)
    )
    
    sim$P$sim <- cbind(sim$P$sim, sim$sites[[site]])
  }
  
  sim$cor_par <- cor_par
  
  return(sim)
}
