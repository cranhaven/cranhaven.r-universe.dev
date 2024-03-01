formatdata <- function (indiv, astart, aend, aevent, adrug, aedrug, expogrp = list(), 
                         washout = list(), sameexpopar = list(), agegrp = NULL, seasongrp = NULL, 
                         dob = NULL, cov = cbind(), dataformat = "stack", data) 
{
  if (is.null(data)) {
    indiv <- indiv
    astart <- astart
    aend <- aend
    aevent <- aevent
    adrug <- adrug
    aedrug <- aedrug
    expogrp <- expogrp
    washout <- washout
    sameexpopar <- sameexpopar
    agegrp <- agegrp
    seasongrp <- seasongrp
    dob <- dob
    cov <- cov
    ncoladrug <- NULL
    for (i in 1:length(adrug)) {
      ncoladrug[i] <- ncol(adrug[[i]])
    }
    adrugcolnames <- NULL
    for (i in 1:length(adrug)) {
      adrugcolnames[c(1, cumsum(ncoladrug) + 1)[-(length(ncoladrug) + 
                                                    1)][i]:cumsum(ncoladrug)[i]] <- colnames(adrug[[i]])
    }
  }
  else {
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
    else if (length(adrugcolnames) == 1 & length(adrug) != 
             1) {
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
    
    
    indiv <- eval(substitute(indiv), data, parent.frame())
    astart <- eval(substitute(astart), data, parent.frame())
    aend <- eval(substitute(aend), data, parent.frame())
    aevent <- eval(substitute(aevent), data, parent.frame())
    aedrug <- eval(substitute(aedrug), data, parent.frame())
    cov <- eval(substitute(cov), data, parent.frame())
    dob <- eval(substitute(dob), data, parent.frame())
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
    else if (length(adrugcolnames) == 1 & length(aedrug) != 
             1) {
      aedrug <- list(aedrug)
    }
    else {
      aedrug <- aedrug
    }
  }
  
  
  
  for (i in 1:length(adrug)) {
    adrug[[i]] <- adrug[[i]] - 1
  }
  for (i in 1:length(aedrug)) {
    aedrug[[i]] <- data.frame(aedrug[[i]])
  }
  if (length(expogrp) == 0) {
    for (i in 1:length(adrug)) {
      expogrp[[i]] <- 0
    }
  }
  else if (length(adrug) == 1 & length(expogrp) > 1) {
    expogrp <- list(c(expogrp))
  }
  else {
    expogrp <- expogrp
  }
  if (length(washout) == 0) {
    for (i in 1:length(adrug)) {
      washout[[i]] <- 0
    }
  }
  else if (length(adrug) == 1 & length(washout) > 1) {
    washout <- list((c(washout) - 1))
    washout[[1]][length(washout[[1]])] <- washout[[1]][length(washout[[1]])] + 
      1
  }
  else {
    for (i in 1:length(adrug)) washout[[i]][1:(length(washout[[i]]) - 
                                                 1)] <- washout[[i]][1:(length(washout[[i]]) - 1)] - 
        1
  }
  if (length(sameexpopar) == 0) {
    for (i in 1:length(adrug)) {
      sameexpopar[[i]] <- TRUE
    }
  }
  else {
    sameexpopar <- sameexpopar
  }
  if (length(agegrp) == 0) {
    agegrp <- agegrp
  }
  else {
    agegrp <- agegrp - 1
  }
  
  
  
  # data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend, 
  #                                 cov)))
  
  
  # data1$astart <- data1$astart - 1
  
  if (is.null(dob)) {
    # data1 <- data1
    data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend, 
                                     cov)))
  }
  else {
    #data1$dob <- data.frame(unique(cbind(indiv, as.Date(formatC(dob, width = 8, format = "d", 
    #                             flag = "0"), "%d%m%Y"))))[,2]
    #data1$dob <- data.frame(unique(cbind(indiv, dob)))[,2]
    data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend, 
                                     cov, dob)))
    
    data1$dob <- as.Date(formatC(data1$dob, width = 8, format = "d", 
                                 flag = "0"), "%d%m%Y")
    
  }
  
  data1$astart <- data1$astart - 1
  
  
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
    # adrug_all <- adrug
    # aedrug_all <- aedrug
    
    # 06-11-2021 changed the above two lines (184 and 185) as below
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
  expo1 <- list()
  expo2 <- list()
  expo <- list()
  for (i in 1:length(adrug)) {
    expo1[[i]] <- matrix(, nrow(adrug_all[[i]]), ncol(adrug_all[[i]]) * 
                           length(expogrp[[i]]))
  }
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      for (j in 1:length(expogrp[[i]])) {
        expo1[[i]][, ((1 * j) + length(expogrp[[i]]) * 
                        (k - 1))] <- adrug_all[[i]][, k] + expogrp[[i]][j]
      }
    }
  }
  for (i in 1:length(adrug)) {
    expo2[[i]] <- matrix(, nrow(aedrug_all[[i]]), ncol(aedrug_all[[i]]) * 
                           length(washout[[i]]))
  }
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(aedrug_all[[i]])) {
      for (j in 1:length(washout[[i]])) {
        expo2[[i]][, ((1 * j) + length(washout[[i]]) * 
                        (k - 1))] <- aedrug_all[[i]][, k] + washout[[i]][j]
      }
    }
  }
  for (i in 1:length(adrug)) {
    expo[[i]] <- matrix(, nrow(adrug_all[[i]]), ncol(adrug_all[[i]]) * 
                          length(expogrp[[i]]) + ncol(aedrug_all[[i]]) * length(washout[[i]]))
  }
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      expo[[i]][, (1 + (length(expogrp[[i]]) + (length(washout[[i]]))) * 
                     (k - 1)):((length(expogrp[[i]]) + (length(washout[[i]]))) * 
                                 k)] <- cbind(expo1[[i]][, (1 + length(expogrp[[i]]) * 
                                                              (k - 1)):(length(expogrp[[i]]) * k)], expo2[[i]][, 
                                                                                                               (1 + length(washout[[i]]) * (k - 1)):(length(washout[[i]]) * 
                                                                                                                                                       k)])
    }
  }
  data1 <- data1[order(data1$indiv), ] # New 18-10-2021
  # data1 <- data1[order(data1$indiv), ]
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]])) {
      expo[[i]][, j] <- ifelse(is.na(expo[[i]][, j]), data1$aend, 
                               expo[[i]][, j])
    }
  }
  for (i in 1:length(adrug)) {
    for (k in 2:ncol(expo[[i]])) {
      expo[[i]][, k - 1] <- ifelse(expo[[i]][, k] < expo[[i]][, 
                                                              k - 1], expo[[i]][, k], expo[[i]][, k - 1])
    }
  }
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]])) expo[[i]][, j] <- pmin(data1$aend, 
                                                        expo[[i]][, j])
    expo[[i]][, j] <- pmax(data1$astart, expo[[i]][, j])
  }
  if (dataformat == "stack") {
    expolev <- list()
    for (i in 1:length(adrug)) {
      expolev[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + 
                                     length(washout[[i]]) - 1)), 0), times = ncol(adrug_all[[i]]))
    }
  }
  else {
    expolevdiff <- list()
    expolevsame <- list()
    for (i in 1:length(adrug)) {
      expolevdiff[[i]] <- seq(1:((length(expogrp[[i]]) + 
                                    length(washout[[i]]) - 1) * ncol(adrug_all[[i]])))
    }
    for (i in 1:length(adrug)) {
      for (k in 1:ncol(adrug_all[[i]])) {
        expolevdiff[[i]] <- append(expolevdiff[[i]], 
                                   0, after = (length(expogrp[[i]]) + length(washout[[i]]) - 
                                                 1) * (k) + (k - 1))
      }
    }
    for (i in 1:length(adrug)) {
      expolevsame[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + 
                                         length(washout[[i]]) - 1)), 0), times = ncol(adrug_all[[i]]))
    }
    expolev <- list()
    for (i in 1:length(adrug)) {
      expolev[[i]] <- if (sameexpopar[i] == TRUE) {
        expolev[[i]] <- expolevsame[[i]]
      }
      else {
        expolev[[i]] <- expolevdiff[[i]]
      }
    }
  }
  if (is.null(seasongrp)) {
    seasongrpY2 <- seasongrp
  }
  else {
    yearsspan <- (as.numeric(format(min(data1$dob + data1$astart), 
                                    "%Y")):as.numeric(format(max(data1$dob + data1$aend), 
                                                             "%Y")))
    seasongrpY <- rep(seasongrp, times = length(yearsspan))
    seasongrpY1 <- as.numeric(paste0(seasongrpY, rep(yearsspan, 
                                                     each = length(seasongrp))))
    seasongrpY1 <- sort(as.Date(formatC(seasongrpY1, width = 8, 
                                        format = "d", flag = "0"), "%d%m%Y"))
    seasongrpY2 <- data.frame(matrix(, nrow(data1), length(seasongrpY1)))
    for (i in 1:length(seasongrpY1)) {
      seasongrpY2[, i] <- as.numeric(seasongrpY1[i] - data1$dob) - 
        1
    }
    for (i in 1:ncol(seasongrpY2)) {
      seasongrpY2[, i] <- pmin(data1$aend, seasongrpY2[, 
                                                       i])
      seasongrpY2[, i] <- pmax(data1$astart, seasongrpY2[, 
                                                         i])
    }
    seasongrpY2 <- as.matrix(seasongrpY2)
  }
  ncolexpo <- NULL
  for (i in 1:length(adrug)) {
    ncolexpo[i] <- ncol(expo[[i]])
  }
  ncuts <- sum(ncolexpo) + length(agegrp) + 2 + length(seasongrpY2[1, 
  ])
  nevents <- nrow(data1)
  ind <- rep(1:nevents, times = ncuts)
  eventday <- rep(data1$aevent, times = ncuts)
  allexpo <- matrix(NA, nrow = nrow(data1), ncol = sum(ncolexpo))
  for (i in 1:length(adrug)) {
    allexpo[, (c(1, (cumsum(ncolexpo)[-length(ncolexpo)] + 
                       1)))[i]:(cumsum(ncolexpo))[i]] <- expo[[i]]
  }
  cutp <- c(as.matrix(data1$astart), as.matrix(data1$aend), 
            allexpo, rep(agegrp, each = nevents), seasongrpY2)
  o <- order(ind, cutp)
  ind = as.factor(ind[o])
  cutp = cutp[o]
  eventday = eventday[o]
  interval <- c(0, cutp[2:length(ind)] - cutp[1:length(ind) - 
                                                1])
  interval <- ifelse(cutp <= data1$astart[ind], 0, interval)
  interval <- ifelse(cutp > data1$aend[ind], 0, interval)
  event <- ifelse(data1$aevent[ind] > cutp - interval, 1, 0)
  event <- ifelse(data1$aevent[ind] <= cutp, event, 0)
  if (is.null(seasongrp)) {
    season <- seasongrp
  }
  else {
    cutpseason <- (format((cutp + data1$dob[ind]), "%m/%d"))
    seasonlevel <- c(1:(length(seasongrp)))
    seasoncutpts <- unique(format(seasongrpY1, "%m/%d"))
    season <- rep(seasonlevel[length(seasonlevel)], times = nevents * 
                    ncuts)
    for (i in 1:length(seasongrp)) {
      season <- ifelse(cutpseason >= seasoncutpts[i], seasonlevel[i], 
                       season)
    }
    season <- as.factor(season)
  }
  agegr <- cut(cutp, breaks = c(min(data1$astart), agegrp, 
                                max(data1$aend)), labels = FALSE)
  agegr <- as.factor(agegr)
  exgr <- matrix(0, nrow = (nevents * ncuts), ncol = length(adrug))
  exgr <- data.frame(exgr)
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(expo[[i]])) {
      exgr[, i] <- ifelse(cutp > expo[[i]][, k][ind], expolev[[i]][k], 
                          exgr[, i])
    }
  }
  for (i in 1:ncol(exgr)) {
    exgr[, i] <- as.factor(exgr[, i])
  }
  colnames(exgr) <- adrugcolnames[(c(0, cumsum(ncoladrug)) + 
                                     1)[-(ncol(exgr) + 1)]]
  exgr1 <- cbind(exgr, trial = rep(1, time = nrow(exgr)))
  if (is.null(season)) {
    chopdat <- data.frame(cbind(indivL = ind[interval != 
                                               0], event = event[interval != 0], eventday = eventday[interval != 
                                                                                                       0], lower = (cutp[interval != 0] - interval[interval != 
                                                                                                                                                     0]) + 1, upper = cutp[interval != 0], interval = interval[interval != 
                                                                                                                                                                                                                 0], age = agegr[interval != 0], exgr1[interval != 
                                                                                                                                                                                                                                                         0, ]))
    chopdat <- chopdat[, -ncol(chopdat)]
    chopdat <- data.frame(cbind(chopdat, apply(data1, 2, 
                                               function(x) rep(x, times = data.frame((table(chopdat$indivL)))$Freq))))
  }
  else {
    chopdat <- data.frame(cbind(indivL = ind[interval != 
                                               0], event = event[interval != 0], eventday = eventday[interval != 
                                                                                                       0], lower = (cutp[interval != 0] - interval[interval != 
                                                                                                                                                     0]) + 1, upper = cutp[interval != 0], interval = interval[interval != 
                                                                                                                                                                                                                 0], season = season[interval != 0], age = agegr[interval != 
                                                                                                                                                                                                                                                                   0], exgr1[interval != 0, ]))
    chopdat <- chopdat[, -ncol(chopdat)]
    chopdat <- data.frame(cbind(chopdat, apply(data1[, -ncol(data1)], 
                                               2, function(x) rep(x, times = data.frame((table(chopdat$indivL)))$Freq))))
  }
  chopdat$astart <- chopdat$astart + 1
  return(chopdat)
}
