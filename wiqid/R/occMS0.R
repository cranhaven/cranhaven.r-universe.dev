
# Multiseason occupancy

# See MacKenzie et al (2006) "Occupancy..." p194ff

occMS0 <- function(DH, occsPerSeason, ci=0.95, verify=TRUE, ...) {

  # ** DH is detection data in a 1/0/NA matrix or data frame, sites in rows,
  #    detection occasions in columns..
  # ** occsPerSeason is a scalar or vector with the number of occasions per season
  # ci is the required confidence interval.

  if(verify)
    DH <- verifyDH(DH, allowNA=TRUE)

  crit <- fixCI(ci)

  # Check for all-NA rows (eg, Grand Skinks data set!)
  allNA <- rowSums(!is.na(DH)) == 0
  if(any(allNA))
    DH <- DH[!allNA, ]

    # Deal with occsPerSeason
  nOcc <- ncol(DH)
  if(length(occsPerSeason) == 1)
    occsPerSeason <- rep(occsPerSeason, nOcc/occsPerSeason)
  if(sum(occsPerSeason) != nOcc)
    stop("Detection data do not match occasions per season.")
  nseas <- length(occsPerSeason)
  seasonID <- rep(1:nseas, occsPerSeason)
  getLast <- function(dh, grp) max(which(rowsum(dh, grp) > 0))
  last <- as.vector(apply((!is.na(DH))*1, 1, getLast, grp=factor(seasonID)))
  DHplus <- as.matrix(cbind(last, DH))  # This speeds things up by a factor of 60

  beta.mat <- matrix(NA_real_, 4, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("psi1", "gamma", "epsilon", "p")
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL

  nll <- function(param) {
    psi1 <- plogis(param[1])
    gam <- plogis(param[2])
    eps <- plogis(param[3])
    p <- plogis(param[4])
    PHI0 <- c(psi1, 1-psi1)
    PHIt <- matrix(c(1-eps, gam, eps, 1-gam), 2)
    Prh <- apply(DHplus, 1, Prh1, p=p, PHI0=PHI0, PHIt=PHIt, seasonID)
    return(min(-sum(log(Prh)), .Machine$double.xmax))
  }

  # res <- nlm(nll, start, hessian=TRUE)
  nlmArgs <- list(...)
  nlmArgs$f <- nll
  nlmArgs$p <- rep(0, 4)
  nlmArgs$hessian <- TRUE
  res <- do.call(nlm, nlmArgs)
  if(res$code > 2)   # exit code 1 or 2 is ok.
    warning(paste("Convergence may not have been reached (nlm code", res$code, ")"))
  beta.mat[,1] <- res$estimate
  logLik <- -res$minimum

  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  # if (!inherits(varcov0, "try-error") && all(diag(varcov0) > 0)) {
  if (!inherits(varcov0, "try-error") ) {
    npar <- 4
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE  # tidy later
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
  }

  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plogis(beta.mat[, -2]),
              logLik = c(logLik=logLik, df=npar, nobs=nrow(DH)),
              ci = ci,
              link="logit",
              index = list(psi1 = 1, gamma = 2, epsilon = 3, p = 4) )
  class(out) <- c("wiqid", "list")
  return(out)
}

# ....................................................

# A function to get Pr(dh) for a single detection history,
#   ie, one row of DH.
# Not exported

Prh1 <- function(dhp, p, PHI0, PHIt, seasonID) {
  last <- dhp[1]
  dh <- dhp[-1]
  if(all(is.na(dh)))
    return(1)
  pvec <- p * dh + (1-p)*(1-dh)
  res <- PHI0
  if(last > 1)
    for(j in 1:(last-1)) {
      if(!all(is.na(pvec[seasonID==j])))  {
        D <- diag(c(prod(pvec[seasonID==j], na.rm=TRUE),
                    1-max(dh[seasonID==j], na.rm=TRUE)))
        res <- res %*% D
      }
      res <- res %*% PHIt
    }
  PT <- c(prod(pvec[seasonID==last], na.rm=TRUE), 1-max(dh[seasonID==last], na.rm=TRUE))
  res <- res %*% PT
}
