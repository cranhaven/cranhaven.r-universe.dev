
# Utilities for Single-season 2-species occupancy function, based on Richmond et al 2010.

# These functions are used by occ2sps and occ2sps0


# function to get the occupancy matrix
# logitpsiX is a matrix with LOGITS of psiA, psiBa, psiBA in columns
#   and a row for each site
# output is a matrix with columns for probabilities of 
#   both sps present, A only, B only, neither, and a row for each site
getlogPHI <- function(logitpsiX) {
  logpsiX <- plogis(logitpsiX, log.p=TRUE)
  log1mpsiX <- plogis( -logitpsiX, log.p=TRUE)
  cbind(logpsiX[, 1] + logpsiX[, 3],   # both
    logpsiX[, 1] + log1mpsiX[, 3],     # A only
    log1mpsiX[, 1] + logpsiX[, 2],     # B only
    log1mpsiX[, 1] + log1mpsiX[, 2])   # neither
}

# Do the detection matrix for all sites
# logitpX is a MATRIX with COLUMNS for LOGITS of pA, pB, rA, rBa, rBA
# output is a matrix with columns for likelihoods given 
#   both sps present, A only, B only, neither, and a row for each site
getlogP <- function(DHA, DHB, logitpX)  {
  logpX <- plogis(logitpX, log.p = TRUE)
  log1mpX <- plogis( -logitpX, log.p = TRUE)
  if(nrow(logitpX) == 1) {
    logpX <- matrix(logpX, nrow(DHA), 5, byrow=TRUE)
    log1mpX <- matrix(log1mpX, nrow(DHA), 5, byrow=TRUE)
  }
  logP <- matrix(NA, nrow(DHA), 4)
  for(i in 1:nrow(logP)) {
    dhA <- DHA[i, ]
    dhB <- DHB[i, ]
    # prob of detecting B if both present conditional on detection of A
    logprobCapB <- dhA * logpX[i, 5] + (1 - dhA) * logpX[i, 4]
    log1mprobCapB <- dhA * log1mpX[i, 5] + (1 - dhA) * log1mpX[i, 4]
    logP[i, ] <- c(
      # Both sps present, use the r's
      sum(dhA * logpX[i, 3] + (1 - dhA) * log1mpX[i, 3],      # A
        dhB * logprobCapB + (1 - dhB) * log1mprobCapB, na.rm=TRUE),  # B
      # Sps A present, B absent, use pA
      if(sum(dhB, na.rm=TRUE) > 0) { -Inf } else {
        sum(dhA * logpX[i, 1] + (1 - dhA) * log1mpX[i, 1], na.rm=TRUE) # A
      },
      # Sps A absent, B present, use pB
      if(sum(dhA, na.rm=TRUE) > 0) { -Inf } else {
        sum(dhB * logpX[i, 2] + (1 - dhB) * log1mpX[i, 2], na.rm=TRUE) # B
      },
      # Neither present
      if(sum(dhA, dhB, na.rm=TRUE) > 0) { -Inf } else { 0 } )
  }
  return(logP)
}


