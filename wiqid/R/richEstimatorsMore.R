# Calculation of richness estimators which are not included in EstimateS.
# (or at least not in version 8.2)

# JackA1, JackA2 : First and second order jackknife ABUNDANCE-BASED
# richness estimators
# ================================================================
# See Gotelli & Colwell 2011 p41

richJackA1 <- function(cntVec)  {
  # cntVec = vector of counts, one element per species

  # Convert a matrix into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)

  # Get number of species observed, and uniques:
  Sobs <- sum(cntVec > 0)
  f1 <- sum(cntVec == 1)
  return(Sobs + f1)
}

# JackA2 : Second order jackknife ABUNDANCE-BASED richness estimator
# ==================================================================

richJackA2 <- function(cntVec)  {
  # cntVec = vector of counts, one element per species

  # Convert a matrix into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)

  # Get number of sites, species observed, and uniques:
  Sobs <- sum(cntVec > 0)
  f1 <- sum(cntVec == 1)
  f2 <- sum(cntVec == 2)
  return(Sobs + 2 * f1 - f2)
}

# Rennolls & Laumonier (2006) 'shadow species' ABUNDANCE-BASED estimator of richness
# ==================================================================================

richRenLau <- function(cntVec)  {
  # cntVec = vector of counts, one element per species

  # Convert a matrix into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)

  fk <- table(cntVec[cntVec > 0])
  k <- as.numeric(names(fk))
  n <- sum(cntVec)
  C <- if(is.na(fk["1"])) 1 - fk["1"] / n else 1 # Good's (1953) coverage estimator
  pik <- 1 - (1 - (C * k / n))^n  # (corrected) inclusion probability
  nuk <- 1 / pik - 1              # shadow sps / observed sps
  shadows <- round(fk * nuk)      # est. number of shadow species
  return(sum(fk, shadows))
}
