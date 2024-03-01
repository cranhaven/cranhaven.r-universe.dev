
###########################################################
#### Function to fit event-dependent model starts here ####
###########################################################
eventdepenexp <- function (indiv, astart, aend, aevent, adrug, aedrug, expogrp = 0, 
                           sameexpopar = T, agegrp = NULL, dataformat = "stack", verbose = F, 
                           tolerance = 1e-08, itermax = 100, data) 
{
  yon <- deparse(substitute(adrug))
  yon1 <- as.formula(paste("z", "~", yon))
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1]
  adrug <- eval(substitute(adrug), data, parent.frame())
  if ((dataformat == "multi" & !is.null(ncol(adrug)))) {
    adrug <- data.frame(adrug)
    adrug <- list(adrug)
  }
  else if (dataformat == "stack" & !is.null(ncol(adrug))) {
    adrug <- data.frame(adrug)
    adrug1 <- list()
    for (i in 1:ncol(adrug)) {
      adrug1[[i]] <- adrug[, i]
    }
    adrug <- adrug1
  }
  else if (length(adrugcolnames) == 1 & length(adrug) != 1) {
    adrug <- list(adrug)
  }
  else {
    adrug <- adrug
  }
  for (i in 1:length(adrug)) {
    adrug[[i]] <- data.frame(adrug[[i]])
  }
  ncoladrug <- NULL
  for (i in 1:length(adrug)) {
    ncoladrug[i] <- ncol(adrug[[i]])
  }
  for (i in 1:length(adrug)) {
    colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug) + 
                                              1)[-(length(ncoladrug) + 1)][i]:cumsum(ncoladrug)[i]]
  }
  colname <- adrugcolnames[1]
  indiv <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  if ((dataformat == "multi" & !is.null(ncol(aedrug)))) {
    aedrug <- data.frame(aedrug)
    aedrug <- list(aedrug)
  }
  else if (dataformat == "stack" & !is.null(ncol(aedrug))) {
    aedrug <- data.frame(aedrug)
    aedrug1 <- list()
    for (i in 1:ncol(aedrug)) {
      aedrug1[[i]] <- aedrug[, i]
    }
    aedrug <- aedrug1
  }
  else if (length(adrugcolnames) == 1 & length(aedrug) != 1) {
    aedrug <- list(aedrug)
  }
  else {
    aedrug <- aedrug
  }
  if (dataformat == "stack") {
    adrug_all <- list()
    for (i in 1:length(adrug)) {
      adrug_all[[i]] <- adrug_matrix(indiv, aevent, adrug[[i]])
    }
    aedrug_all <- list()
    for (i in 1:length(aedrug)) {
      aedrug_all[[i]] <- adrug_matrix(indiv, aevent, aedrug[[i]])
    }
  }
  else if (dataformat == "multi") {
    adrug_all <- adrug
    aedrug_all <- aedrug
  # The above two lines 77 and 78 changed as below to sort the data by indiv and adrug
    
    adrug_all <- list()
    for (i in 1:length(adrug)) {
      adrug_all[[i]] <- cbind(indiv, aevent, adrug[[i]])
      adrug_all[[i]] <- data.frame(adrug_all[[i]][order(adrug_all[[i]][,1],adrug_all[[i]][,3]), -c(1,2)])
    }
    
    aedrug_all <- list()
    for (i in 1:length(aedrug)) {
      aedrug_all[[i]] <- cbind(indiv, aevent, aedrug[[i]])
      aedrug_all[[i]] <- data.frame(aedrug_all[[i]][order(aedrug_all[[i]][,1],aedrug_all[[i]][,3]), -c(1,2)])
      
    }
    
    
  }
  else {
    stop("dataformat should be multi or stack")
  }
  adrug <- as.matrix(adrug_all[[1]])
  aedrug <- as.matrix(aedrug_all[[1]])
  
  data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend)))
  data1 <- data1[order(data1$indiv), ]
  
  if (length(data1$indiv) != length(unique(data1$indiv))) {
    warning("Multiple events per case detected: analysis restricted to first events")
    ord <- order(data1$indiv)
    minev <- unlist(tapply(data1$aevent[ord], data1$indiv[ord], 
                           function(x) rep(min(x), length(x))), use.names = F)
    data1$aevent <- minev[order(ord)]
    first.event <- 1 - duplicated(data1$indiv)
  }
  else {
    first.event <- rep(1, length(data1$indiv))
  }
  if (expogrp[[1]][1] < 0) {
    stop("Pre-exposure risk periods not allowed")
  }
  aevent <- data1$aevent
  nrem <- 0
  if (is.matrix(adrug)) {
    for (i in 1:ncol(adrug)) {
      nrem <- nrem + sum(ifelse(first.event == 1, aevent < 
                                  adrug[, i], 0), na.rm = T)
      adrug[, i] <- ifelse(aevent < adrug[, i], NA, adrug[, 
                                                          i])
      aedrug[, i] <- ifelse(aevent < adrug[, i], NA, aedrug[, 
                                                            i])
    }
  }
  else {
    nrem <- nrem + sum(ifelse(first.event == 1, aevent < 
                                adrug, 0), na.rm = T)
    adrug <- ifelse(aevent < adrug, NA, adrug)
    aedrug <- ifelse(aevent < adrug, NA, aedrug)
  }
  if (verbose == T) {
    cat(paste0("No. exposures after first event (treated as missing): ", 
               nrem))
    cat("\n")
  }
  combinedoses <- ifelse(sameexpopar == T, 1, 0)
  riskstart <- ifelse(is.null(expogrp), 0, expogrp[[1]][1])
  if (riskstart > 0) {
    expogrP <- if (is.list(expogrp)) {
      c(0, as.vector(expogrp[[1]]))
    }
    else {
      c(0, expogrp)
    }
  }
  else {
    expogrP <- if (is.list(expogrp)) {
      as.vector(expogrp[[1]])
    }
    else {
      expogrp
    }
  }
  all.data <- data.frame(indiv = data1$indiv, astart = data1$astart, 
                         aend = data1$aend, aevent = aevent, adrug, aedrug, first.event = first.event)
  all.data_fe <- all.data[all.data$first.event == 1, ]
  adrug_fe <- adrug[first.event == 1, ]
  aedrug_fe <- aedrug[first.event == 1, ]
  base.dat <- subset(formatdata(indiv = indiv, astart = astart, 
                                aend = aend, aevent = aevent, adrug = list(adrug_fe), 
                                aedrug = list(aedrug_fe), expogrp = expogrP, sameexpopar = F, 
                                agegrp = agegrp, dataformat = "multi", data = all.data_fe), 
                     select = c(1, 2, 7, 8, 6))
  numrisk <- length(expogrP)
  base.dat$risk <- as.numeric(as.character(base.dat[, 4]))
  base.dat$dose <- ceiling(base.dat$risk/numrisk)
  if (riskstart > 0) {
    base.dat$risk <- ifelse(base.dat$risk%%numrisk == 1, 
                            0, base.dat$risk - base.dat$dose)
  }
  maxdose <- max(base.dat$dose)
  increment <- ceiling(log10(maxdose + 1))
  base.dat$indivL <- as.numeric(as.character(base.dat$indivL))
  base.dat$expo <- rep(0, length(base.dat$indivL))
  base.dat$weight <- ifelse(base.dat$event == 1, base.dat$risk, 
                            0)
  base.dat$maxw <- unlist(tapply(base.dat$weight, base.dat$indivL, 
                                 function(x) rep(max(x), length(x))), use.names = F)
  base.dat$previ <- rep(0, length(base.dat$indivL))
  temp <- unlist(tapply(base.dat[, 4], base.dat$indivL, function(x) rep(x[1], 
                                                                        length(x))), use.names = F)
  base.dat$previ <- ifelse(temp == 0, 0, -1)
  rm(temp)
  stack.dat <- subset(base.dat, base.dat$previ == 0)
  nlevel0 <- sum(unique(stack.dat$indivL) > 0)
  if (verbose == T) {
    cat(paste0("No. events included at stack level ", 0, 
               ": ", nlevel0))
    cat("\n")
  }
  for (i in 1:maxdose) {
    temp1 <- ifelse(base.dat$dose == i, i, 0)
    temp2 <- unlist(tapply(temp1, base.dat$indivL, cumsum), 
                    use.names = F)
    temp3 <- ifelse((temp1 == temp2) & temp1 > 0, i, 0)
    base.dat$previ <- unlist(tapply(temp3, base.dat$indivL, 
                                    cumsum), use.names = F)
    rm(temp1, temp2, temp3)
    stack.dat.i <- subset(base.dat, base.dat$previ == i)
    stack.dat.i$indivL <- stack.dat.i$indivL + i/(10^increment)
    stack.dat.i$expo <- ifelse(stack.dat.i$dose == i, stack.dat.i$risk, 
                               0)
    stack.dat.i$weight <- ifelse(stack.dat.i$event == 1 & 
                                   stack.dat.i$dose > i, stack.dat.i$risk, 0)
    stack.dat.i$maxw <- unlist(tapply(stack.dat.i$weight, 
                                      stack.dat.i$indivL, function(x) rep(max(x), length(x))), 
                               use.names = F)
    stack.dat <- rbind(stack.dat, stack.dat.i)
    nleveli <- sum(unique(stack.dat.i$indivL) > 0)
    if (verbose == T) {
      cat(paste0("No. events included at stack level ", 
                 i, ": ", nleveli))
      cat("\n")
    }
  }
  fitrisk <- ifelse(riskstart > 0, numrisk - 1, numrisk)
  if (combinedoses == 1) {
    stack.dat$expo <- as.factor(ifelse(stack.dat$expo > 0, 
                                       stack.dat$risk - fitrisk * (stack.dat$dose - 1), 
                                       0))
    stack.dat$weight <- ifelse(stack.dat$weight > 0, stack.dat$weight - 
                                 fitrisk * (stack.dat$dose - 1), 0)
    stack.dat$maxw <- ifelse(stack.dat$maxw > 0, stack.dat$maxw - 
                               (ceiling(stack.dat$maxw/fitrisk) - 1) * fitrisk, 
                             0)
  }
  else {
    stack.dat$expo <- as.factor(stack.dat$expo)
  }
  stack.dat$indivL <- as.factor(stack.dat$indivL)
  stack.dat$expo1 <- stack.dat$expo
  colnames(stack.dat)[which(names(stack.dat) == "expo1")] <- colname
  lenbeta <- ifelse(combinedoses == 1, fitrisk, maxdose * fitrisk)
  lenalpha <- length(agegrp)
  if (lenalpha > 0) {
    fmla1 = as.formula(paste("wevent~", colname[1], "+", 
                             "age"))
  }
  else {
    fmla1 <- as.formula(paste("wevent~", colname[1]))
  }
  diff <- 1
  numiter <- 0
  beta <- rep(0, lenbeta)
  while (diff > tolerance * lenbeta & numiter < itermax) {
    stack.dat$wevent <- stack.dat$event
    for (i in 1:lenbeta) {
      stack.dat$wevent <- ifelse(stack.dat$weight == i, 
                                 exp(-beta[i]), stack.dat$wevent)
    }
    betaold <- beta
    indivL <- stack.dat$indivL
    interval <- stack.dat$interval
    mod <- suppressWarnings(gnm(fmla1, eliminate = indivL, 
                                offset = log(interval), family = poisson, data = stack.dat))
    beta <- as.vector(mod$coefficients[1:lenbeta])
    diff <- as.numeric(sqrt(t(beta - betaold) %*% (beta - 
                                                     betaold)))
    numiter <- numiter + 1
    if (verbose == T) {
      cat(paste("iteration:", numiter))
      cat("\n")
      cat(paste("beta:", 1:lenbeta, beta))
      cat("\n")
    }
    if (numiter == itermax) 
      warning("Maximum number of iterations reached")
  }
  beta <- as.vector(mod$coefficients[1:lenbeta])
  alpha <- NULL
  if (lenalpha > 0) {
    a1 <- lenbeta + 1
    a2 <- lenbeta + lenalpha
    alpha <- as.vector(mod$coefficients[a1:a2])
    rm(a1, a2)
  }
  ncases <- sum(unique(floor(as.numeric(as.character(stack.dat$indivL)))) > 
                  0)
  residual <- (stack.dat$wevent - mod$fitted.values)
  Hmat <- (-1) * solve(vcov(mod))
  Rmat <- matrix(rep(residual, ncases), nrow = ncases, byrow = T)
  for (i in 1:ncases) {
    Rmat[i, ] <- Rmat[i, ] * (floor(as.numeric(as.character(stack.dat$indivL))) == 
                                i)
  }
  Sbeta <- matrix(rep(1, length(stack.dat$indivL) * lenbeta), 
                  ncol = lenbeta)
  if (combinedoses == 0) {
    for (j in 1:lenbeta) {
      Sbeta[, j] <- Sbeta[, j] * (as.numeric(as.character(stack.dat$expo)) == 
                                    j) * (stack.dat$previ == ceiling(j/fitrisk))
    }
  }
  else {
    for (j in 1:lenbeta) {
      Sbeta[, j] <- Sbeta[, j] * (as.numeric(as.character(stack.dat$expo)) == 
                                    j) * (stack.dat$previ >= 1)
    }
  }
  eeebeta <- Rmat %*% Sbeta
  if (lenalpha > 0) {
    Salpha <- matrix(rep(1, length(stack.dat$indivL) * lenalpha), 
                     ncol = lenalpha)
    for (j in 1:lenalpha) {
      Salpha[, j] <- Salpha[, j] * (as.numeric(as.character(stack.dat$age)) == 
                                      j + 1)
    }
    eeealpha <- Rmat %*% Salpha
  }
  if (lenalpha > 0) {
    Mmat <- cbind(eeebeta, eeealpha)
  }
  else {
    Mmat <- eeebeta
  }
  meat <- t(Mmat) %*% Mmat
  if (combinedoses == 0) {
    deebeta <- matrix(apply(expand.grid(1:lenbeta, 1:lenbeta), 
                            1, function(x) {
                              (-1) * sum(residual[(stack.dat$previ == ceiling(x[1]/fitrisk)) & 
                                                    (as.numeric(as.character(stack.dat$expo)) == 
                                                       x[1]) & (stack.dat$maxw == x[2])])
                            }), ncol = lenbeta, byrow = F)
  }
  else {
    deebeta <- matrix(apply(expand.grid(1:lenbeta, 1:lenbeta), 
                            1, function(x) {
                              (-1) * sum(residual[(stack.dat$previ >= 1) & 
                                                    (as.numeric(as.character(stack.dat$expo)) == 
                                                       x[1]) & (stack.dat$maxw == x[2])])
                            }), ncol = lenbeta, byrow = F)
  }
  if (lenalpha > 0) {
    deealpha <- matrix(apply(expand.grid(1:lenalpha, 1:lenbeta), 
                             1, function(x) {
                               (-1) * sum(residual[(as.numeric(as.character(stack.dat$age)) == 
                                                      x[1] + 1) & (stack.dat$maxw == x[2])])
                             }), ncol = lenbeta, byrow = F)
  }
  if (lenalpha > 0) {
    Amat <- cbind(rbind(deebeta, deealpha), matrix(rep(0, 
                                                       lenalpha * (lenbeta + lenalpha)), ncol = lenalpha, 
                                                   nrow = lenbeta + lenalpha))
  }
  else {
    Amat <- deebeta
  }
  bread <- Amat + Hmat
  sandwich <- solve(bread) %*% meat %*% t(solve(bread))
  ses <- sqrt(diag(sandwich))
  # result <- list("VarCov"=sandwich, "coefficients"=summary.sccs1(mod = mod, sandwich=sandwich, ses = ses, ncases = length(unique(data1$indiv)), 
  #                        nevents = nrow(data1)))
  result <- summary.sccs(mod = mod, sandwich=sandwich, ses = ses, ncases = length(unique(data1$indiv)), 
                         nevents = nrow(data1))
  return(result)
}

