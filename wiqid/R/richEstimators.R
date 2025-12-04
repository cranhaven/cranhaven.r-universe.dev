# Calculation of richness estimators used in EstimateS.

# See the EstimateS Users Guide:
# http://viceroy.eeb.uconn.edu/estimates/EstimateSPages/EstSUsersGuide/EstimateSUsersGuide.htm
# downloaded 2013-03-14.

# The output corresponds to the last row of the EstimateS output.
# --------------------------------------------------------------------

# ACE : Anne Chao's "ACE" ABUNDANCE-BASED richness estimator
# ==========================================================
# This corresponds to the last row of EstimateS output column 16, "ACE"

richACE <- function(cntVec, threshold = 10) {
  # 'cntVec' should be a vector of species counts (abundances)
  # threshold is the max. abundance for rare species

  # Convert a matrix/dataframe into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)

  cntVec <- cntVec[cntVec > 0]
  Srare <- sum(cntVec <= threshold)     # Number of rare species
  Nrare <- sum(cntVec[cntVec <= threshold])  # Number if indivs in rare species
  F1 <- sum(cntVec == 1)        # Number of singletons
  Sobs <- length(cntVec)
  if(F1 == Nrare)
    return(Sobs + F1 * (F1 - 1) / 2)  # Chao1 (F2=0 in this case)
  Cace <- 1 - F1 / Nrare
  Fi <- table(cntVec[cntVec <= threshold])
  Fn <- as.numeric(names(Fi))
  gamma2 <- max(Srare * sum(Fn * (Fn - 1) * Fi) / (Cace * Nrare * (Nrare - 1)) - 1, 0)

  return(Sobs - Srare + Srare / Cace + F1 * gamma2 / Cace)
}

# ICE : Anne Chao's "ICE" INCIDENCE-BASED richness estimator
# ==========================================================
# This corresponds to the last row of EstimateS output column 18, "ICE"

richICE <- function(incMat, threshold = 10) {
  # 'incMat' should be a matrix of species incidences
  # threshold is the max. incidences for infrequent species

  # Convert abundances to incidences: 
  incMat <- round(incMat) > 0
  incVec <- rowSums(incMat)
  m <- ncol(incMat)
  infr <- incVec <= threshold & incVec > 0     # Which species are infrequent
  Sinfr <- sum(infr)     # Number of infrequent species
  Ninfr <- sum(incVec[infr])  # Number of incidences of infrequent species
  minfr <- sum(colSums(incMat[infr, , drop=FALSE]) > 0) # Number of samples with >=1 infrequent sps
  Q1 <- sum(incVec == 1)        # Number of uniques
  Sobs <- sum(incVec > 0)
  if(Q1 == Ninfr)
    return(Sobs + ((m - 1) / m) * (Q1 * (Q1 - 1) / 2))  # Chao2 Eqn 4 (Q2=0 in this case)
  Cice <- 1 - Q1 / Ninfr
  Qj <- table(incVec[infr])
  Qn <- as.numeric(names(Qj))
  gamma2 <- max(Sinfr * minfr * sum(Qn * (Qn - 1) * Qj) / 
              (Cice * (minfr - 1) * Ninfr^2) - 1, 0)

  return(Sobs - Sinfr + Sinfr / Cice + Q1 * gamma2 / Cice)
}

# Chao1 : Anne Chao's "Chao1" ABUNDANCE-BASED richness estimator
# ==============================================================
# This corresponds to the last row of EstimateS output columns 20-23
#   Chao 1 Mean
# 	Chao 1 95% CI Lower Bound
#  	Chao 1 95% CI Upper Bound
# 	Chao 1 SD (analytical)

# Equation numbers refer to Appendix B of EstimateS 8.2 Users Guide at

richChao1 <- function(cntVec, correct=FALSE, ci = 0.95)  {
  # cntVec = vector of counts, one element per species
  # correct : if TRUE, bias-corrected Chao1 is calculated
  # ci : the desired confidence interval.
  if(ci > 1 | ci < 0.5)
    stop("ci must be between 0.5 and 1")
  crit <- qnorm(1 - (1 - ci[1]) / 2)

  # Convert a matrix into a vector and round: 
  cntVec <- round(cntVec)
  if(is.matrix(cntVec) || is.data.frame(cntVec))
    cntVec <- rowSums(cntVec)

  # Get number of individuals and of species observed:
  N <- sum(cntVec)
  Sobs <- sum(cntVec > 0)
  # Get number of singletons and doubletons:
  F1 <- sum(cntVec == 1)
  F2 <- sum(cntVec == 2)

  # Calculate Chao1
  # T = number of undetected species = Chao1 - Sobs
  if(correct | F2 == 0) {
    T <- F1 * (F1 - 1) / (2 * (F2 + 1))  # Eqn 2
  } else {
    T <- F1^2 / (2 * F2)                 # Eqn 1 
  }
  # If F1 == 0, these evaluate correctly to T = 0.
  Chao1 <- Sobs + T

  # Calculate variance
  if(F1 == 0) {
    # Eqn 8
    var <- Sobs * exp(-N / Sobs) * (1 - exp(-N / Sobs))
  } else if(F2 == 0) {
    # Eqn 7
    var <- (F1*(F1-1) / 2) + (F1*(2*F1 - 1)^2 / 4) - (F1^4 / (4 * Chao1))
  } else if(correct) {
    # Eqn 6
    var <- (F1*(F1-1) / (2 * (F2+1))) + (F1*(2*F1 - 1)^2 / (4*(F2+1)^2)) +
              (F1^2 * F2 * (F1-1)^2 / (4 * (F2+1)^4))
  } else {
    # Eqn 5
    var <- F2 * ((F1 / F2)^2 / 2 + (F1 / F2)^3 + (F1 / F2)^4 / 4)
  }

  # Calculate  CI
  if(F1 == 0) {
    # Eqn 14
    P <- exp(-N / Sobs)
    low <- max(Sobs, Sobs / (1-P) - crit * sqrt(Sobs * P / (1-P)))
    upp <- Sobs / (1-P) + crit * sqrt(Sobs * P / (1-P))
  } else {
    # Eqn 13
    if(T == 0) {  # in this case K is undefined
      low <- upp <- Sobs
    } else {
      K <- exp(crit * sqrt(log(1 + var / T^2)))
      low <- Sobs + T / K
      upp <- Sobs + T * K
    }
  }

  return(c(Chao1 = Chao1, Chao1Low = low, Chao1Upp = upp, Chao1SD = sqrt(var)))
}

# Chao2 : Anne Chao's "Chao2" INCIDENCE-BASED richness estimator
# ==============================================================
# This corresponds to the last row of EstimateS output columns 24-27
#   Chao 2 Mean
# 	Chao 2 95% CI Lower Bound
#  	Chao 2 95% CI Upper Bound
# 	Chao 2 SD (analytical)

richChao2 <- function(incMat, correct=FALSE, ci = 0.95)  {
  # incMat = a 0/1 matrix of incidence data, species x sites
  # correct : if TRUE, bias-corrected Chao2 is calculated
  # ci : the desired confidence interval.
  if(ci > 1 | ci < 0.5)
    stop("ci must be between 0.5 and 1")
  crit <- qnorm(1 - (1 - ci[1]) / 2)

  # Convert a count matrix into an incidence matrix: 
  incMat <- round(incMat) > 0  

  # Get number of sites, incidences, and species observed:
  m <- ncol(incMat)
  M <- sum(incMat)
  incVec <- rowSums(incMat)
  Sobs <- sum(incVec > 0)
  # Get number of uniques and duplicates:
  Q1 <- sum(incVec == 1)
  Q2 <- sum(incVec == 2)

  # Calculate Chao2
  # T = number of undetected species = Chao1 - Sobs
  if(correct | Q2 == 0) {
    T <- ((m - 1) / m) * (Q1 * (Q1 - 1) / (2 * (Q2 + 1)))  # Eqn 4
  } else {
    T <- Q1^2 / (2 * Q2)                 # Eqn 3 
  }
  # If Q1 == 0, both these evaluate correctly to T = 0.
  Chao2 <- Sobs + T

  # Calculate variance
  if(Q1 == 0) {
    # Eqn 12
    var <- Sobs * exp(-M / Sobs) * (1 - exp(-M / Sobs))
  } else if(Q2 == 0) {
    # Eqn 11
    var <- ((m - 1) / m) * (Q1*(Q1-1) / 2) + 
           ((m - 1) / m)^2 * (Q1*(2*Q1 - 1)^2 / 4) - 
           ((m - 1) / m)^2 * (Q1^4 / (4 * Chao2))
  } else if(correct) {
    # Eqn 10
    var <- ((m - 1) / m) * (Q1*(Q1-1) / (2 * (Q2+1))) + 
           ((m - 1) / m)^2 * (Q1*(2*Q1 - 1)^2 / (4*(Q2+1)^2)) +
           ((m - 1) / m)^2 * (Q1^2 * Q2 * (Q1-1)^2 / (4 * (Q2+1)^4))
  } else {
    # Eqn 9
    var <- Q2 * ((Q1 / Q2)^2 / 2 + (Q1 / Q2)^3 + (Q1 / Q2)^4 / 4)
  }

  # Calculate CI
  if(Q1 == 0) {
    # Eqn 14
    P <- exp(-M / Sobs)
    low <- max(Sobs, Sobs / (1-P) - crit * sqrt(Sobs * P / (1-P)))
    upp <- Sobs / (1-P) + crit * sqrt(Sobs * P / (1-P))
  } else {
    # Eqn 13
    if(T == 0) {  # in this case K is undefined
      low <- upp <- Sobs
    } else {
      K <- exp(crit * sqrt(log(1 + var / T^2)))
      low <- Sobs + T / K
      upp <- Sobs + T * K
    }
  }

  return(c(Chao2 = Chao2, Chao2Low = low, Chao2Upp = upp, Chao2SD = sqrt(var)))
}

# Jack1 : First order jackknife INCIDENCE-BASED richness estimator
# ================================================================

richJack1 <- function(incMat)  {
  # incMat = a 0/1 matrix of incidence data, species x sites

  # Convert a count matrix into an incidence matrix: 
  incMat <- round(incMat) > 0  

  # Get number of sites, species observed, and uniques:
  m <- ncol(incMat)
  incVec <- rowSums(incMat)
  Sobs <- sum(incVec > 0)
  Q1 <- sum(incVec == 1)

  return(Sobs + Q1 * (m - 1) / m)
}

# Jack2 : Second order jackknife INCIDENCE-BASED richness estimator
# =================================================================

richJack2 <- function(incMat)  {
  # incMat = a 0/1 matrix of incidence data, species x sites

  # Convert a count matrix into an incidence matrix: 
  incMat <- round(incMat) > 0  

  # Get number of sites, species observed, uniques and duplicates:
  m <- ncol(incMat)
  incVec <- rowSums(incMat)
  Sobs <- sum(incVec > 0)
  Q1 <- sum(incVec == 1)
  Q2 <- sum(incVec == 2)

  return(Sobs + Q1 * (2*m - 3) / m - Q2 * (m - 2)^2 / m / (m-1))
}

# Bootstrap : Bootstrap INCIDENCE-BASED richness estimator
# ========================================================

richBoot <- function(incMat)  {
  # incMat = a 0/1 matrix of incidence data, species x sites

  # Convert a count matrix into an incidence matrix: 
  incMat <- round(incMat) > 0  

  # Get number of sites, species observed, and proportion of samples that contain species k:
  m <- ncol(incMat)
  incVec <- rowSums(incMat)
  Sobs <- sum(incVec > 0)
  pk <- incVec / m

  return(Sobs + sum((1-pk) ^ m))
}

# MMMeans : Michaelis-Menton INCIDENCE-BASED richness estimator

# This uses Mao's tau, producing what EstS calls MMMeans. MMRuns is not calculated as it
#   is inferior to MMMeans as described in the Users Guide

# This function uses Raaijmakers' MLEs as given in 
#   Colwell, R K; J A Coddington. 1994. Estimating terrestrial biodiversity through
#   extrapolation. Philosophical Transactions of the Royal Society of London B 345:101-118.

richMM <- function(incMat)  {
  # incMat = a 0/1 matrix of incidence data, species x sites
  if(ncol(incMat) == 1)  # Can't fit curve to 1 data point!
    return(0)
  # get estimates of species for subsets:
  tau <- richRarefy(incMat)[, 1]
  Xi <- tau / seq(tau)
  Xbar <- mean(Xi)
  Yi <- tau
  Ybar <- mean(Yi)
  Syy <- sum((Yi - Ybar)^2)
  Sxx <- sum((Xi - Xbar)^2)
  Sxy <- sum((Xi - Xbar) * (Yi - Ybar))
  Bhat <- (Xbar * Syy - Ybar * Sxy) / (Ybar* Sxx - Xbar * Sxy)
  Shat <- Ybar + Bhat * Xbar
  if(Shat < 0 || !is.finite(Shat))
    return(NA_real_)
  return(Shat)
  }

