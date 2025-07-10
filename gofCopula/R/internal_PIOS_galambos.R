# Internal functions for the derivation of the galambos copula for the PIOSRn and
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d density
.galambos.12.density <- function(theta, u) { 
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  lu1 <- -log(u1)
  lu2 <- -log(u2)

  lu1t <- (lu1^theta)
  lu2t <- (lu2^theta)

  (exp((((lu1^(-theta)) + (lu2^(-theta)))^(-1 / theta))) * (-(lu1 * 
    (((lu1^(-theta)) + (lu2^(-theta)))^(1 / theta)) * (lu1t + lu2t) * 
    (lu1t - lu2 * (((lu1^(-theta)) + (lu2^(-theta)))^(1 / theta)) * 
    (lu1t + lu2t))) + lu2t * (-((lu2^(1 + theta)) * (((lu1^(-theta)) + 
    (lu2^(-theta)))^(1 / theta))) + lu1t * (1 + (((lu1^(-theta)) + 
    (lu2^(-theta)))^(1 / theta)) * (1 - lu2 + theta))))) /
    (lu1 * lu2 * (((lu1^(-theta)) + (lu2^(-theta)))^(2 / theta)) * 
    ((lu1t + lu2t)^2))
}

# 2d first derivative wrt theta of the log-density
.galambos.12.V <- function(theta, u1, u2) { 
  lu1 <- -log(u1)
  lu2 <- -log(u2)
  plu1mt <- lu1^(-theta)
  plu2mt <- lu2^(-theta)
  plu12invt <- (plu1mt + plu2mt)^(1 / theta)
  logplu <- log(plu1mt + plu2mt)
  llu1 <- log(lu1)
  llu2 <- log(lu2)

  ((lu2^(1 + 3 * theta)) * plu12invt * (-1 + (-1 + lu1) * plu12invt) * 
    (theta * llu1 + logplu) + (lu1^(1 + 3 * theta)) * plu12invt * (-1 + 
    (-1 + lu2) * plu12invt) * (theta * llu2 + logplu) + (lu1^(2 * theta)) * 
    (lu2^theta) * (theta * llu2 + logplu - plu12invt * (theta * (lu1 + theta) * 
    llu1 + (-3 + lu1 + lu2 - 2 * theta) * theta * llu2 + (-3 + 2 * lu1 + lu2 - 
    theta) * logplu) + (plu12invt^2) * (theta * (theta + (-1 + lu2 - theta) * 
    (lu1 + theta) * llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * 
    lu2 + theta)) * llu2) + (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * 
    logplu)) + (lu1^theta) * (lu2^(2 * theta)) * (theta * llu1 + logplu + 
    plu12invt * (theta * (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * 
    (lu2 + theta) * llu2 + (3 - lu1 - 2 * lu2 + theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + 
    lu2 * (-1 + theta) + ((1 + theta)^2)) * llu1 + (-1 + lu1 - theta) * 
    (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * 
    logplu))) / (plu12invt * ((lu1^theta) + (lu2^theta)) * ((lu2^(1 + 2 * 
    theta)) * plu12invt * (-1 + lu1 * plu12invt) + (lu1^(1 + 2 * theta)) * 
    plu12invt * (-1 + lu2 * plu12invt) + (lu1^theta) * (lu2^theta) * 
    (1 + 2 * lu1 * lu2 * (plu12invt^2) - plu12invt * 
    (-1 + lu1 + lu2 - theta))) * (theta^2))
}

# 2d second derivative wrt theta of the log-density
.galambos.12.S <- function(theta, u1, u2) { 

  lu1 <- -log(u1)
  lu2 <- -log(u2)
  plu1mt <- lu1^(-theta)
  plu2mt <- lu2^(-theta)
  lu1t <- lu1^theta
  lu2t <- lu2^theta
  plu12invt <- (plu1mt + plu2mt)^(1 / theta)
  logplu <- log(plu1mt + plu2mt)
  llu1 <- log(lu1)
  llu2 <- log(lu2)
  t2 <- theta^2
  tp12 <- (1 + theta)^2
  lu12t <- lu1^(1 + 2 * theta)
  lu13t <- lu1^(1 + 3 * theta)
  lu22t <- lu2^(1 + 2 * theta)
  lu23t <- lu2^(1 + 3 * theta)
  aaa <- lu2t * theta * llu1 + lu1t * theta * llu2 + (lu1t + lu2t) * logplu

  (-2 * (lu1t + lu2t) * (lu22t * plu12invt * (-1 + lu1 * plu12invt) + 
    lu12t * plu12invt * (-1 + lu2 * plu12invt) + lu1t * lu2t * (1 + 2 * 
    lu1 * lu2 * (plu12invt^2) - plu12invt * (-1 + lu1 + lu2 - theta))) * 
    (lu23t * plu12invt * (-1 + (-1 + lu1) * plu12invt) * (theta * llu1 + 
    logplu) + lu13t * plu12invt * (-1 + (-1 + lu2) * plu12invt) * 
    (theta * llu2 + logplu) + (lu1^(2 * theta)) * lu2t * (theta * llu2 + 
    logplu - plu12invt * (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 
    2 * theta) * theta * llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (-1 + lu2 - theta) * (lu1 + theta) * 
    llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * lu2 + theta)) * 
    llu2) + (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * logplu)) + lu1t * 
    (lu2^(2 * theta)) * (theta * llu1 + logplu + plu12invt * (theta * 
    (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * (lu2 + theta) * llu2 + 
    (3 - lu1 - 2 * lu2 + theta) * logplu) + (plu12invt^2) * (theta * 
    (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * (-1 + theta) + tp12) * 
    llu1 + (-1 + lu1 - theta) * (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * 
    (-1 + 3 * lu2) + theta) * logplu))) - (lu22t * plu12invt * 
    (-1 + lu1 * plu12invt) + lu12t * plu12invt * (-1 + lu2 * plu12invt) + 
    lu1t * lu2t * (1 + 2 * lu1 * lu2 * (plu12invt^2) - plu12invt * 
    (-1 + lu1 + lu2 - theta))) * theta * (lu1t * llu1 + lu2t * llu2) *
    (lu23t * plu12invt * (-1 + (-1 + lu1) * plu12invt) * (theta * llu1 + 
    logplu) + lu13t * plu12invt * (-1 + (-1 + lu2) * plu12invt) *
    (theta * llu2 + logplu) + (lu1^(2 * theta)) * lu2t * (theta * llu2 + 
    logplu - plu12invt * (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 2 * 
    theta) * theta * llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (-1 + lu2 - theta) * 
    (lu1 + theta) * llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * 
    (-1 + 2 * lu2 + theta)) * llu2) + (1 - lu2 + lu1 * (-2 + 3 * lu2) + 
    theta) * logplu)) + lu1t * (lu2^(2 * theta)) * (theta * llu1 + logplu + 
    plu12invt * (theta * (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * 
    (lu2 + theta) * llu2 + (3 - lu1 - 2 * lu2 + theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * 
    (-1 + theta) + ((1 + theta)^2)) * llu1 + (-1 + lu1 - theta) * 
    (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * 
    logplu))) + ((lu22t * plu12invt * (-1 + lu1 * plu12invt) + lu12t * 
    plu12invt * (-1 + lu2 * plu12invt) + lu1t * lu2t * (1 + 2 * lu1 * lu2 * 
    (plu12invt^2) - plu12invt * (-1 + lu1 + lu2 - theta))) * aaa * (lu23t * 
    plu12invt * (-1 + (-1 + lu1) * plu12invt) * (theta * llu1 + logplu) + 
    lu13t * plu12invt * (-1 + (-1 + lu2) * plu12invt) * (theta * llu2 + 
    logplu) + (lu1^(2 * theta)) * lu2t * (theta * llu2 + logplu - plu12invt * 
    (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 2 * theta) * theta * 
    llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu) + (plu12invt^2) * 
    (theta * (theta + (-1 + lu2 - theta) * (lu1 + theta) * llu1 + 
    ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * lu2 + theta)) * llu2) + 
    (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * logplu)) + lu1t * 
    (lu2^(2 * theta)) * (theta * llu1 + logplu + plu12invt * (theta * 
    (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * (lu2 + theta) * llu2 + 
    (3 - lu1 - 2 * lu2 + theta) * logplu) + (plu12invt^2) * (theta * 
    (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * (-1 + theta) + tp12) *
    llu1 + (-1 + lu1 - theta) * (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * 
    (-1 + 3 * lu2) + theta) * logplu)))) / theta - ((2 * lu12t * plu12invt *
    (lu1t + lu2t) * (-1 + lu2 * plu12invt) * t2 * llu1 + lu1t * lu2t * 
    (lu1t + lu2t) * (1 + 2 * lu1 * lu2 * (plu12invt^2) - plu12invt * 
    (-1 + lu1 + lu2 - theta)) * t2 * llu1 + 2 * lu22t * plu12invt * 
    (lu1t + lu2t) * (-1 + lu1 * plu12invt) * t2 * llu2 + lu1t * lu2t * 
    (lu1t + lu2t) * (1 + 2 * lu1 * lu2 * (plu12invt^2) - plu12invt * 
    (-1 + lu1 + lu2 - theta)) * t2 * llu2 - lu12t * lu2 * (plu12invt^2) * 
    (lu2t * theta * llu1 + lu1t * theta * llu2 + (lu1t + lu2t) * logplu) - 
    lu1 * lu22t * (plu12invt^2) * aaa + lu22t * plu12invt * 
    (1 - lu1 * plu12invt) * aaa + lu12t * plu12invt * (1 - lu2 * plu12invt) * 
    aaa + lu1t * lu2t * plu12invt * ((lu1t + lu2t) * t2 - 4 * lu1 * lu2 * 
    plu12invt * aaa - (1 - lu1 - lu2 + theta) * (lu2t * theta * llu1 + lu1t * 
    theta * llu2 + (lu1t + lu2t) * logplu))) * (lu23t * plu12invt * 
    (-1 + (-1 + lu1) * plu12invt) * (theta * llu1 + logplu) + lu13t * 
    plu12invt * (-1 + (-1 + lu2) * plu12invt) * (theta * llu2 + logplu) + 
    (lu1^(2 * theta)) * lu2t * (theta * llu2 + logplu - plu12invt *
    (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 2 * theta) * theta * 
    llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu) + (plu12invt^2) *
    (theta * (theta + (-1 + lu2 - theta) * (lu1 + theta) * llu1 + 
    ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * lu2 + theta)) * llu2) +
    (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * logplu)) + lu1t * 
    (lu2^(2 * theta)) * (theta * llu1 + logplu + plu12invt *
    (theta * (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * (lu2 + theta) * 
    llu2 + (3 - lu1 - 2 * lu2 + theta) * logplu) + (plu12invt^2) *
    (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * (-1 + theta) + 
    tp12) * llu1 + (-1 + lu1 - theta) * (lu2 + theta) * llu2) +
    (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * logplu)))) / 
    theta + (lu1t + lu2t) * (lu22t * plu12invt * (-1 + lu1 * plu12invt) +
    lu12t * plu12invt * (-1 + lu2 * plu12invt) + lu1t * lu2t * (1 + 2 * 
    lu1 * lu2 * (plu12invt^2) - plu12invt * (-1 + lu1 + lu2 - theta))) * 
    theta * ((lu1t * lu23t * plu12invt * (-1 - plu12invt + lu1 * plu12invt) * 
    (llu1 - llu2)) / (lu1t + lu2t) + (lu13t * lu2t *
    plu12invt * (-1 - plu12invt + lu2 * plu12invt) * (-llu1 + llu2)) / 
    (lu1t + lu2t) + 3 * lu23t * plu12invt * (-1 + (-1 + lu1) * plu12invt) * 
    llu2 * (theta * llu1 + logplu) + 3 * lu13t * plu12invt * (-1 + (-1 + lu2) * 
    plu12invt) * llu1 * (theta * llu2 + logplu) - ((-1 + lu1) * lu23t * 
    (plu12invt^2) * (theta * llu1 + logplu) * aaa) /
    ((lu1t + lu2t) * t2) - (lu23t * plu12invt * (-1 + (-1 + lu1) * plu12invt) * 
    (theta * llu1 + logplu) * (lu2t * theta * llu1 + lu1t * theta * llu2 +
    (lu1t + lu2t) * logplu)) / ((lu1t + lu2t) * t2) - (lu13t * (-1 + lu2) * 
    (plu12invt^2) * (theta * llu2 + logplu) * (lu2t * theta * llu1 + lu1t * 
    theta * llu2 + (lu1t + lu2t) * logplu)) / ((lu1t + lu2t) * t2) - (lu13t * 
    plu12invt * (-1 + (-1 + lu2) * plu12invt) * (theta * llu2 + logplu) *
    aaa) / ((lu1t + lu2t) * t2) + 2 * (lu1^(2 * theta)) * lu2t * llu1 * 
    (theta * llu2 + logplu - plu12invt * (theta * (lu1 + theta) * llu1 + 
    (-3 + lu1 + lu2 - 2 * theta) * theta * llu2 + (-3 + 2 * lu1 + lu2 - 
    theta) * logplu) + (plu12invt^2) * (theta * (theta + (-1 + lu2 - theta) * 
    (lu1 + theta) * llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * 
    (-1 + 2 * lu2 + theta)) * llu2) + (1 - lu2 + lu1 * (-2 + 3 * lu2) + 
    theta) * logplu)) + (lu1^(2 * theta)) * lu2t * llu2 * (theta * llu2 + 
    logplu - plu12invt * (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 2 * 
    theta) * theta * llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (-1 + lu2 - theta) * (lu1 + theta) * 
    llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * lu2 + theta)) * 
    llu2) + (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * logplu)) + 
    (lu1^(2 * theta)) * lu2t * (llu2 - (lu2t * llu1 + lu1t * llu2) / 
    (lu1t + lu2t) + plu12invt * (-(theta * llu1) - (lu1 + theta) * llu1 - 
    (-3 + lu1 + lu2 - 2 * theta) * llu2 + 2 * theta * llu2 + ((-3 + 2 * lu1 + 
    lu2 - theta) * (lu2t * llu1 + lu1t * llu2)) /
    (lu1t + lu2t) + logplu) + (plu12invt^2) * (theta + (-1 + lu2 - theta) * 
    (lu1 + theta) * llu1 + ((1 + theta) * (1 - lu2 + theta) + lu1 * (-1 + 2 * 
    lu2 + theta)) * llu2 - ((1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * 
    (lu2t * llu1 + lu1t * llu2)) /
    (lu1t + lu2t) + theta * (1 - (1 + lu1 - lu2 + 2 * theta) * llu1 + 
    (2 + lu1 - lu2 + 2 * theta) * llu2) + logplu) + (plu12invt *
    aaa * (theta * (lu1 + theta) * llu1 + (-3 + lu1 + lu2 - 2 * theta) * 
    theta * llu2 + (-3 + 2 * lu1 + lu2 - theta) * logplu)) / 
    ((lu1t + lu2t) * t2) - (2 * (plu12invt^2) * aaa * (theta * (theta + 
    (-1 + lu2 - theta) * (lu1 + theta) * llu1 + ((1 + theta) * 
    (1 - lu2 + theta) + lu1 * (-1 + 2 * lu2 + theta)) * llu2) +
    (1 - lu2 + lu1 * (-2 + 3 * lu2) + theta) * logplu)) / 
    ((lu1t + lu2t) * t2)) + lu1t * (lu2^(2 * theta)) * llu1 * 
    (theta * llu1 + logplu + plu12invt * (theta * (3 - lu1 - lu2 + 2 * theta) * 
    llu1 - theta * (lu2 + theta) * llu2 + (3 - lu1 - 2 * lu2 + theta) * 
    logplu) + (plu12invt^2) * (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + 
    lu2 * (-1 + theta) + tp12) * llu1 + (-1 + lu1 - theta) * 
    (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * 
    logplu)) + 2 * lu1t * (lu2^(2 * theta)) * llu2 * (theta * llu1 + 
    logplu + plu12invt * (theta * (3 - lu1 - lu2 + 2 * theta) * llu1 - theta * 
    (lu2 + theta) * llu2 + (3 - lu1 - 2 * lu2 + theta) * logplu) + 
    (plu12invt^2) * (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * 
    (-1 + theta) + tp12) * llu1 + (-1 + lu1 - theta) * (lu2 + theta) * llu2) +
    (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * logplu)) + lu1t * 
    (lu2^(2 * theta)) * (llu1 - (lu2t * llu1 + lu1t * llu2) / (lu1t + lu2t) +
    (plu12invt^2) * (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * 
    (-1 + theta) + tp12) * llu1 + (-1 + lu1 - theta) * (lu2 + theta) * llu2 -
    ((1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * 
    (lu2t * llu1 + lu1t * llu2)) / (lu1t + lu2t) +
    theta * (1 + (2 - lu1 + lu2 + 2 * theta) * llu1 + (-1 + lu1 - lu2 - 
    2 * theta) * llu2) + logplu) + (plu12invt * ((-(lu1^(1 + theta)) + lu2t * 
    (lu2 + 3 * theta) + lu1t * (3 - lu2 + 4 * theta)) * llu1 + 
    ((lu1^(1 + theta)) + lu1t * (-3 + lu2 - 3 * theta) - lu2t * (lu2 + 2 * 
    theta)) * llu2 + (lu1t + lu2t) * logplu)) / 
    (lu1t + lu2t) - (plu12invt * aaa * (theta * (3 - lu1 - lu2 + 2 * theta) * 
    llu1 - theta * (lu2 + theta) * llu2 + (3 - lu1 - 2 * lu2 + theta) * 
    logplu)) / ((lu1t + lu2t) * t2) - (2 * (plu12invt^2) *
    aaa * (theta * (theta + (lu1 * (-1 + 2 * lu2 - theta) + lu2 * 
    (-1 + theta) + ((1 + theta)^2)) * llu1 + (-1 + lu1 - theta) * 
    (lu2 + theta) * llu2) + (1 - 2 * lu2 + lu1 * (-1 + 3 * lu2) + theta) * 
    logplu)) / ((lu1t + lu2t) * t2)))) / (plu12invt *
    ((lu1t + lu2t)^2) * ((lu22t * plu12invt * (-1 + lu1 * plu12invt) +
    lu12t * plu12invt * (-1 + lu2 * plu12invt) + lu1t * lu2t * 
    (1 + 2 * lu1 * lu2 * (plu12invt^2) - plu12invt * 
    (-1 + lu1 + lu2 - theta)))^2) * (theta^3))
}

