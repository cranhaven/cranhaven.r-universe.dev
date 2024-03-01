standardsccs <- function(formula, indiv, astart, aend, aevent, adrug, aedrug, expogrp = list(), washout = list(), 
                              sameexpopar = list(), agegrp = NULL, seasongrp=NULL, dob=NULL, dataformat="stack", data) {
  
  
  if (dataformat!="multi" & dataformat!="stack"){
    
    stop("Please input dataformat as multi or stack")
  }
  
  #if (!is.null(seasongrp) & is.null(dob)){
  
  #  stop("Please input date of birth (dob)")
  #}
  
  yon <- deparse(substitute(adrug)) 
  yon1 <- as.formula(paste("z", "~", yon)) 
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1] 
  # colname  <- deparse(substitute(adrug))
  adrug  <- eval(substitute(adrug), data, parent.frame())
  
  # Changing adrug to a list if given as cbind(adrug1, adrug2,...) or adrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(adrug)))) {
    adrug <- data.frame(adrug)
    adrug <- list(adrug) 
  } else if (dataformat=="stack" & !is.null(ncol(adrug))){
    adrug <- data.frame(adrug)
    adrug1 <- list()
    for (i in 1:ncol(adrug)){
      adrug1[[i]] <- adrug[,i]
    }
    adrug <- adrug1
  } else if (length(adrugcolnames)==1 & length(adrug)!=1) {
    adrug <- list(adrug)
    
  } else {
    adrug <- adrug
  }
  
  
  for (i in 1:length(adrug)){
    adrug[[i]] <- data.frame(adrug[[i]])
  }
  
  ncoladrug <- NULL
  for (i in 1:length(adrug)){
    ncoladrug[i] <- ncol(adrug[[i]]) 
  }
  
  
  for (i in 1:length(adrug)) {
    colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug)+1)[-(length(ncoladrug)+1)][i]:cumsum(ncoladrug)[i]]
  }
  
  colname  <- adrugcolnames
  
  # colname  <- deparse(substitute(adrug))
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  # adrug <- data.frame(adrug)
  
  # if (ncol(adrug)==1) {
  #  colnames(adrug) <- colname
  # } else { 
  #  colnames(adrug) <- colnames(adrug)
  #}
  
  
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  dob <- eval(substitute(dob), data, parent.frame())
  
  
  # Changing aedrug to a list if given as cbind(aedrug1, aedrug2,...) or aedrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(aedrug)))) {
    aedrug <- data.frame(aedrug)
    aedrug <- list(aedrug) 
  } else if (dataformat=="stack" & !is.null(ncol(aedrug))){
    aedrug <- data.frame(aedrug)
    aedrug1 <- list()
    for (i in 1:ncol(aedrug)){
      aedrug1[[i]] <- aedrug[,i]
    }
    aedrug <- aedrug1
  } else if (length(adrugcolnames)==1 & length(aedrug)!=1) {
    aedrug <- list(aedrug)
    
  } else {
    aedrug <- aedrug
  }
  
  
  # ------------------Getting the fixed covariates from the formula ------------#
  
  qq <- all.vars(as.formula(formula))[-c(which(all.vars(as.formula(formula))=="age"), which(all.vars(as.formula(formula))=="season"), which(all.vars(as.formula(formula))=="event"))]
  
  if (length(qq)==0) {
    cov <- cbind()
  }   else {
    cova <- qq[is.na(match(qq, colname))]
    cov <- data.frame(data[, cova])
    colnames(cov) <- cova
  }
  
  chopdat <- formatdata(indiv=indiv, astart=astart, aend=aend, aevent=aevent, adrug=adrug, aedrug=aedrug, expogrp = expogrp, washout = washout , 
                             sameexpopar = sameexpopar, agegrp = agegrp, seasongrp=seasongrp, dob=dob, cov=cov, dataformat=dataformat, data=NULL)
  
  fmla <- paste(formula, "+", "strata(indivL)", "+", "offset(log(interval))")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- clogit(formula = fmla1, data = chopdat)
  summary <- summary(mod)
  return(summary)
}
