
# Single-season 2-species occupancy function, based on Richmond et al 2010.

# Richmond et al (2010) interpret "species A detected" to mean
#  "species A detected at the site on the occasion in question."
# See the example in Eqn2 p2038.

# DHA is detection history of the dominant species
# DHB is detection history of the subordinate species
# model is now a list of two-sided formulae (new 2015-09-19)
#  The default is list(psiA~1, psiBa~1, pA~1, pB~1). If not included in the model
#  list, the remaining parameters are assigned the following values:
#  psiBA <- psiBa, rA <- pA, rBa <- pB, rBA <- rBa.

occ2sps <- function(DHA, DHB, model=NULL, data=NULL, ci=0.95, verify=TRUE)  {

  crit <- fixCI(ci)

  DHA <- as.matrix(DHA)
  DHB <- as.matrix(DHB)
  if(verify) {
    if(!all(dim(DHA) == dim(DHB)))
      stop("DHA and DHB do not have the same number of rows and columns.")
    DHA <- verifyDH(DHA, allowNA = TRUE)
    DHB <- verifyDH(DHB, allowNA = TRUE)
    # Check that the NAs match up
    if(!all(is.na(DHA) == is.na(DHB)))
      stop("Missing values in DHA and DHB do not match up.")
  }

  # Standardise the model:
  model <- stdModel(model, list(psiA=~1, psiBa=~1, pA=~1, pB=~1))
  # Check for invalid submodels in 'model':
  parNames <- c("psiA", "psiBa", "psiBA", "pA", "pB", "rA", "rBa", "rBA")
  ok <- names(model) %in% parNames
  if(any(!ok))
    stop("Invalid submodels for: ", paste(names(model)[!ok], collapse=", "))
  # modPars is a vector of length 8 which maps the submodels needed to the
  #   elements of 'model':
  modPars <- pmatch(parNames, names(model))
  names(modPars) <- parNames
  if(is.null(model$psiBA))
    modPars[3] <- modPars[2]  # psiBA <- psiBa
  if(is.null(model$rA))
    modPars[6] <- modPars[4]  # rA <- pA
  if(is.null(model$rBa))
    modPars[7] <- modPars[5]  # rBa <- pB
  if(is.null(model$rBA))
    modPars[8] <- modPars[7]  # rBA <- rBa

  if(is.null(data))  {
    out <- occ2sps0(DHA, DHB, modPars, ci=ci)
    out$formulae <- model
    out$index <- list("psiA"=1, "psiBa"=2, "psiBA"=3, "pA"=4, "pB"=5, "rA"=6, "rBa"=7, "rBA"=8)
    return(out)
  }

  M <- length(model)  # Number of elements in the model
  nSites <- nrow(DHA)
  site.names <- rownames(data)
  if(is.null(site.names))
    site.names <- 1:nSites

  data <- as.data.frame(stddata(data, nocc=NULL, scaleBy=NULL))
  # Get factor levels and scaling values (needed for prediction)
  xlev <- lapply(data[sapply(data, is.factor)], levels)
  scaling <- lapply(data[sapply(data, is.numeric)],
    getScaling, scaleBy = 1)
  data <- as.data.frame(lapply(data, doScaling, scaleBy = 1))

  # Build model matrices
  modMatList <- vector('list', M)
  for(i in 1:M)
    modMatList[[i]] <- modelMatrix(model[[i]], data)
  parK <- sapply(modMatList, ncol)    # Number of parameters for each model matrix
  K <- sum(parK)  # total number of parameters
  idK <- rep(1:M, parK)  # specifies which of the K parameters belongs to each model matrix
  index <- vector('list', length(model)) # needed for 'predict'
  names(index) <- names(model)
  for(i in seq_along(model))
    index[[i]] <- (1:K)[idK == i]
  # Get coefficient names
  coefNames <- paste(rep(names(model), parK),
      unlist(lapply(modMatList, colnames)), sep=":")

  # Functions getlogPHI and getlogP moved to file occ2sps_utils.R 2017-10-30

  # objects to hold output
  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- coefNames
  logLik <- NA_real_
  npar <- NA_real_
  varcov <- NULL
  lp.mat <- matrix(NA_real_, nSites * 8, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- as.vector(t(outer(parNames, site.names, paste, sep=":")))

  # Do the neg log lik function:
  logitreal0 <- matrix(NA, nSites, M)
  nll <- function(params) {
    for(i in 1:M) {
      betas <- params[idK == i]
      logitreal0[, i] <- modMatList[[i]] %*% betas
    }
    logitreal <- logitreal0[, modPars]
    logPHI <- getlogPHI(logitreal[, 1:3]) ### pass logit to getlogPHI
    logP <- getlogP(DHA, DHB, logitreal[, 4:8]) ### pass logit to getlogP
    loglik <- apply(logPHI + logP, 1, logSumExp)
    return(min(-sum(loglik), .Machine$double.xmax))
  }

  # Run mle estimation with optim:
  params <- rep(0, K)
  res <- optim(params, nll, method="L-BFGS-B", lower=-10, upper=10, hessian=TRUE)
  if(res$convergence > 0) {
    warning(paste("Convergence may not have been reached.", res$message))
  } else {
    logLik <- -res$value
  }

  beta.mat[,1] <- res$par
  lp.mat0 <- matrix(NA, nSites, M)
  for(i in 1:M) {
    betas <- res$par[idK == i]
    lp.mat0[, i] <- modMatList[[i]] %*% betas
  }
  lp.mat[, 1] <- as.vector(lp.mat0[, modPars])

  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  if (!inherits(varcov0, "try-error")) {
    npar <- K
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, beta.mat[, 1], "+")
    SElp0 <- matrix(NA, nSites, M)
    for(i in 1:M) {
      SElp0[, i] <- sqrt(getFittedVar(modMatList[[i]], varcov[idK == i, idK == i]))
    }
    SElp <- as.vector(SElp0[, modPars])
    lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
  }

  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plogis(lp.mat),
              logLik = c(logLik=logLik, df=npar, nobs=nSites),
              ci = ci,
              formulae = model,
              index = index,
              xlev = xlev,
              scaling = scaling)
  class(out) <- c("wiqid", "list")
  return(out)
}
