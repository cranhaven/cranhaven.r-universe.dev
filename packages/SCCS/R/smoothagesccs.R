#--------------------------------------------#
# Afunction that fits a semi-parametric SCCS #
# where age related relative incidence       #
#function is represented by spline function  #
#--------------------------------------------#

smoothagesccs <- function (indiv, astart, aend, aevent, adrug, aedrug, expogrp = 0, washout = NULL, 
                            
                            kn=12, sp = NULL, data) {
  
  sameexpopar = list()
  dataformat="stack"
  
  # extract names of the exposures
  yon <- deparse(substitute(adrug)) 
  yon1 <- as.formula(paste("z", "~", yon)) 
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1] 
  # colname  <- deparse(substitute(adrug))
  
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
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
  #---------------------------------------------------------------------#  
  
  
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
  
  #-----------------------------------------------# 
  
  # 08-12-21#
  # Change the lists adrug and aedrug to a dataframes to help sorting the data set by indiv and adrug #
  
  adrugsort <- matrix(NA, nrow = length(indiv), ncol = length(adrug))
  for (i in 1:length(adrug)){
    adrugsort[,i] <- adrug[[i]]
  }
  
  aedrugsort <- matrix(NA, nrow = length(indiv), ncol = length(aedrug))
  for (i in 1:length(aedrug)){
    aedrugsort[,i] <- aedrug[[i]]
  }
  #-------------------------------------------------------------#
  # Combine all the variable 
  data_sort <- data.frame(cbind(indiv, astart, aend, aevent, adrugsort, aedrugsort))
  data_sort <- data_sort[order(data_sort$indiv, data_sort[,5]), ]
  
  indiv <- data_sort$indiv; astart <- data_sort$astart; aend <- data_sort$aend; aevent <- data_sort$aevent
  #--------------------------------------------------------------# 
  
  adrug_sort <- list(data_sort[,5:(4+length(adrug))]) 
  aedrug_sort <- list(data_sort[,(5+length(adrug)):ncol(data_sort)]) 
  
  
  for (i in 1:length(adrug)){
    adrug[[i]] <- data.frame(adrug_sort[[i]])
  }
  
  # adrug <- (adrug)-1
  for (i in 1:length(adrug)) {
    adrug[[i]] <- adrug[[i]]-1
    
  }
  
  
  # aedrug 
  
  for (i in 1:length(aedrug)) {
    aedrug[[i]] <- data.frame(aedrug_sort[[i]])
  }
  
  
  # column names of the adrugs #
  ncoladrug <- NULL
  for (i in 1:length(adrug)){
    ncoladrug[i] <- ncol(adrug[[i]]) 
  }
  
  
  for (i in 1:length(adrug)) {
    colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug)+1)[-(length(ncoladrug)+1)][i]:cumsum(ncoladrug)[i]]
  }
  
  # if (ncol(adrug)==1) {
  #  colnames(adrug) <- colname
  #} else { 
  #  colnames(adrug) <- colnames(adrug)
  #}
  
  
  
  # Exposure periods cut points ###
  if (length(expogrp)==0) {
    for (i in 1:length(adrug)){
      expogrp[[i]] <- 0
    }
    
  } else if(length(adrug)==1 & length(expogrp)>1) {
    expogrp <- list(c(expogrp))
  } else {
    expogrp <- expogrp
  }
  
  # Washout period cut points ###
  
  if (length(washout)==0) {
    for (i in 1:length(adrug)){
      washout[[i]] <- 0
    }
    
  } else if(length(adrug)==1 & length(washout)>1) {
    washout <- list((c(washout)-1))
    washout[[1]][length( washout[[1]])] <- washout[[1]][length( washout[[1]])]+1
    
  } else {
    for (i in 1:length(adrug))  
      washout[[i]][1:(length(washout[[i]])-1)] <- washout[[i]][1:(length(washout[[i]])-1)]-1
  }
  
  
  # sameexpopar ###
  
  if (length(sameexpopar)==0) {
    for (i in 1:length(adrug)){
      sameexpopar[[i]] <- TRUE
    }
  } else {
    
    sameexpopar <- sameexpopar 
  }
  
  
  # combine all the variables but exposures
  
  data1 <- data.frame(unique(cbind(indiv, aevent, astart, aend)))
  # data1 <- data1[order(data1$indiv), ]
  
  #---------------------------------------# 
  
  
  if (dataformat=="stack") {
    # List of all adrug matrices
    adrug_all <- list()
    
    for (i in 1:length(adrug)) {
      adrug_all[[i]] <- adrug_matrix(indiv, aevent, adrug[[i]]) 
    }
    
    # List of all aedrug matrices
    
    aedrug_all <- list()
    
    for (i in 1:length(aedrug)) {
      aedrug_all[[i]] <- adrug_matrix(indiv, aevent, aedrug[[i]]) 
    }
    
  } else if (dataformat=="multi") {
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
    
  } else {
    stop("dataformat should be multi or stack")
  } 
  # adrug and aedrug + expogrps and washout periods
  expo1 <- list()   # for exposure groups
  expo2 <- list()   # for washout periods 
  expo  <- list()    # expo1 and expo2 both together
  
  # filling values of expo1
  
  for (i in 1:length(adrug)) {
    
    expo1[[i]] <- matrix(NA, nrow(adrug_all[[i]]), ncol(adrug_all[[i]])*length(expogrp[[i]]))
    
  } 
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      for (j in 1:length(expogrp[[i]])) {
        expo1[[i]][, ((1 * j) + length(expogrp[[i]]) * (k - 1))] <- adrug_all[[i]][, 
                                                                                   k] + expogrp[[i]][j]
      }
    }
  }
  
  
  #------------------- new 29 - Oct - 2017----#
  for (i in 1:length(expo1)) {
    for (j in 1:ncol(expo1[[i]]))
      expo1[[i]][, j] <- pmin(aedrug_all[[i]], expo1[[i]][, j])
    #expo1[[i]][, j] <- pmax(data1$astart, expo1[[i]][, j])
  }
  
  #-----------------------------------------------------#
  
  # filling values of expo2 
  
  for (i in 1:length(adrug)) {
    
    expo2[[i]] <- matrix(NA, nrow(aedrug_all[[i]]), ncol(aedrug_all[[i]])*length(washout[[i]]))
    
  } 
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(aedrug_all[[i]])) {
      for (j in 1:length(washout[[i]])) {
        expo2[[i]][, ((1 * j) + length(washout[[i]]) * (k - 1))] <- aedrug_all[[i]][, 
                                                                                    k] + washout[[i]][j]
      }
    }
  }
  
  
  # Combine expo1 and expo2 inside expo
  
  for (i in 1:length(adrug)) {
    expo[[i]] <- matrix(NA, nrow(adrug_all[[i]]), ncol(adrug_all[[i]]) * length(expogrp[[i]]) + 
                          ncol(aedrug_all[[i]]) * length(washout[[i]]))
  }
  
  
  for (i in 1:length(adrug)) {
    for (k in 1:ncol(adrug_all[[i]])) {
      expo[[i]][, (1 + (length(expogrp[[i]]) + (length(washout[[i]]))) * (k - 
                                                                            1)):((length(expogrp[[i]]) + (length(washout[[i]]))) * k)] <- cbind(expo1[[i]][, 
                                                                                                                                                           (1 + length(expogrp[[i]]) * (k - 1)):(length(expogrp[[i]]) * 
                                                                                                                                                                                                   k)], expo2[[i]][, (1 + length(washout[[i]]) * (k - 1)):(length(washout[[i]]) * 
                                                                                                                                                                                                                                                             k)])
    }
  }
  
  
  # Changing NA's to aend 
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]])){
      expo[[i]][,j] <- ifelse(is.na(expo[[i]][,j]), data1$aend, expo[[i]][,j])
    }
  }
  ##########################################################################################
  
  
  # exposure risk periods new periods take precedence
  
  for (i in 1:length(adrug)) {
    for (k in 2:ncol(expo[[i]])) {
      expo[[i]][, k - 1] <- ifelse(expo[[i]][, k] < expo[[i]][, k - 1], expo[[i]][, 
                                                                                  k], expo[[i]][, k - 1])
    }
  }
  
  
  # Replace values greater than aend and less than astart by aend and astart respectively
  
  
  for (i in 1:length(expo)) {
    for (j in 1:ncol(expo[[i]]))
      expo[[i]][, j] <- pmin(data1$aend, expo[[i]][, j])
    expo[[i]][, j] <- pmax(data1$astart, expo[[i]][, j])
  }
  
  # Setting exposure levels
  
  if (dataformat=="stack"){
    expolev <- list()
    
    for (i in 1:length(adrug)) {
      expolev[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + length(washout[[i]]) - 
                                     1)), 0), times = ncol(adrug_all[[i]]))
    }
  } else {
    
    expolevdiff <- list()
    expolevsame <- list()
    
    # differenet exposure parameters
    
    for (i in 1:length(adrug)){
      expolevdiff[[i]] <- seq(1:((length(expogrp[[i]]) + length(washout[[i]]) - 
                                    1) * ncol(adrug_all[[i]]))) 
      
    }
    
    for (i in 1:length(adrug)) {
      for (k in 1:ncol(adrug_all[[i]])){
        expolevdiff[[i]] <- append(expolevdiff[[i]], 0, after = (length(expogrp[[i]]) + length(washout[[i]]) - 1) * (k) + (k - 1))
        
      }
    }
    
    # Same exposure parameters 
    for (i in 1:length(adrug)) {
      expolevsame[[i]] <- rep(c(seq(1:(length(expogrp[[i]]) + length(washout[[i]]) - 
                                         1)), 0), times = ncol(adrug_all[[i]]))
    }
    
    
    expolev <- list()
    
    for (i in 1:length(adrug)) {
      
      expolev[[i]] <- if (sameexpopar[i] == TRUE) {
        expolev[[i]] <- expolevsame[[i]]
        #return
        #expolev[[i]]
      } else {
        expolev[[i]] <- expolevdiff[[i]]
        #return
        #expolev[[i]]
      }
    }       
  }
  
  
  
  ncolexpo <- NULL
  for (i in 1:length(adrug)){
    ncolexpo[i] <- ncol(expo[[i]]) 
  }
  
  
  
  ## combine all matrices in expo
  allexpo <- matrix(NA, nrow=nrow(data1), ncol=sum(ncolexpo ))
  
  
  for (i in 1:length(adrug)){
    
    allexpo[,(c(1, (cumsum(ncolexpo)[-length(ncolexpo)] + 1)))[i]:(cumsum(ncolexpo))[i]] <- expo[[i]]
    
  }
  
  # Start splines here
  # knots
  knots1 <- seq(min(astart), max(aend)+0.0005, length=kn)
  msplinedesign4 <- dmsplinedesign(aevent, knots1=knots1, 4, 0)
  
  
  startobs <- ispline(astart , knots1, 4)  # Start of observation
  
  endobs <- ispline(aend , knots1, 4)    # end of observation
  
  
  basisobj_age <- create.bspline.basis(knots1,kn+2)
  penaltymatrix <- bsplinepen(basisobj_age)
  
  
  
  #--------------------------- New ----------------------------------------#
  
  neg.LLnex <- function(p, lambda) {
    
    -(sum(rowsum(log(msplinedesign4[, 1:(kn+2)]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2), data1$indiv))
      
      -(sum(rowsum(log((endobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2) - (startobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
                       
      ), data1$indiv)))
      
      -(lambda)*(t(c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)%*%penaltymatrix%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
    )
  }
  
  
  llex <- function(p) {
    
    -(sum(rowsum(log(msplinedesign4[, 1:(kn+2)]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2), indiv))
      
      -(sum(rowsum(log((endobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2) - (startobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
                       
      ), indiv)))
      
    )
  }
  
  #--- Smoothing Parameter selection --------# 
  
  cv <- function(lambda) {
    
    p0 <- c(rep(1/(kn+1), kn+1))
    
    outopt1 <- optim(p0, neg.LLnex, lambda=lambda, gr = NULL, method = c("BFGS"),  hessian = TRUE)
    par <- outopt1$par
    hes <- outopt1$hessian
    #p0 <- outopt1$par
    
    cvs <- llex(par) + sum(diag(pseudoinverse(hes[1:(kn+1), 1:(kn+1)])%*%(hes[1:(kn+1), 1:(kn+1)] - ((lambda*(8*penaltymatrix*((c(par[1:(ceiling((kn+2+1)/2))], 1, par[((ceiling((kn+2+1)/2))+1):(kn+1)]))%*%t(c(par[1:(ceiling((kn+2+1)/2))], 1, par[((ceiling((kn+2+1)/2))+1):(kn+1)]))) - 4*(diag(as.vector(penaltymatrix%*%(c(par[1:(ceiling((kn+2+1)/2))], 1, par[((ceiling((kn+2+1)/2))+1):(kn+1)]))^2)))))[-((ceiling((kn+2+1)/2))+1),-((ceiling((kn+2+1)/2))+1)]))))
    
    return(cvs)
    
  }
  
  if (is.null(sp)) {
    smpar <- optim(0.00001, cv, method=c("Brent"), lower = 0.00001, upper = 150000000, control = list(reltol=1e-2))
    
  } else {
    
    smpar<-list("par"= sp, "value"=cv(sp))
  }
  
  lambda1 <- smpar$par
  smparcv <- smpar$value
  
  #----------------------------------------------------------------------------#
  
  expo <- allexpo # this might change when multi type exposures are included    
  expolev2 <- c(seq(1:(ncol(expo)-1)), 0) # COuld be changed when the function is updated to include multi type exposures
  
  exgr <- rep(0, nrow(data))
  
  for(i in 1:ncol(expo)){
    exgr <- ifelse(aevent > expo[,i], expolev2[i],exgr)
  }
  
  expolev1 <- c(0, expolev2[-length(expolev2)])
  expodummy <- matrix(0, nrow=nrow(data), ncol=length(expolev1))
  
  for (j in 1:length(expolev1)) {
    
    expodummy[,j] <- ifelse(exgr==rep(expolev1[j], times=nrow(expodummy)), 1, 0)
    
  }
  
  
  # I-splines  of the cut points
  
  #startobs <- ispline(data1$startob , knots1, 4)  # Start of observation
  #endobs <- ispline(data1$endob , knots1, 4)
  
  ispexp <- list() # I-splines of exposure cut points 
  
  for (i in 1:ncol(expo)){
    
    ispexp[[i]] <- ispline(expo[,i], knots1, 4)
    
  }
  
  
  #expoisp <- matrix(0, nrow(expo), (length(ispexp)-1)) # 
  
  
  neg.LL <- function(p, lambda) {
    
    
    expoisp <- matrix(0, nrow(expo), (length(ispexp)-1)) # 
    
    
    for (i in 1: ncol(expoisp)) {
      
      expoisp[,i] <- (((exp(p[kn+1+i])*(ispexp[[i+1]]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)) -
                         (exp(p[kn+1+i])*(ispexp[[i]]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)))
                      
                      - 
                        ((ispexp[[i+1]]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)  -
                           (ispexp[[i]]%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)))
      
    }
    
    
    
    -(sum(rowsum(log(msplinedesign4%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2), data1$indiv)) + sum(rowsum(as.matrix(expodummy[,2:ncol(expodummy)])%*%(p[(kn+2):length(p)]), data1$indiv))
      
      
      -(sum(rowsum(log(endobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2 - (startobs%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
                       
                       + rowSums(expoisp)
                       
                       
                       
      ), data1$indiv)))
      
      - (lambda)*(t(c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)%*%penaltymatrix%*%c(p[1:(ceiling((kn+2+1)/2))], 1, p[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
    )
    
  }
  
  p0f <- c(rep(1/(kn+1), kn+1), rep(2, (ncol(expo)-1))) # Initial values for the parameters
  
  out <- optim(p0f, neg.LL, lambda=lambda1, method = c("BFGS"), hessian = TRUE)
  
  #ris <- exp(out$par[(kn+2):length(out$par)]) # relative incidence estimate from splines
  ris <- out$par[(kn+2):length(out$par)] # relative incidence estimate from splines
  
  exponame <- NULL
  for (i in 1:length(expogrp[[1]])) {
    exponame[i] <- paste(adrugcolnames,i, sep="")
  }
  
  
  washoutname <- NULL
  
  if (length(washout[[1]])==1){
    washoutname <- washoutname
    
  } else {
    
    for (i in 1:(length(washout[[1]]) -1)){
      
      washoutname[i] <- paste("washout",i, sep="")
    }
  }
  
  names(ris) <- c(exponame, washoutname) 
  
  se <- sqrt(diag(as.matrix(pseudoinverse(out$hessian)[(kn+2):length(out$par),(kn+2):length(out$par)])))
  
  # estimates <- cbind(RI=ris, se=se)
  
  ageri <- dmsplinedesign(seq(min(astart), max(aend)), knots1=knots1, 4, 0)
  
  ageaxis <- seq(min(astart), max(aend)) 
  
  # agepara <- (c(out$par[1:(ceiling((kn+2+1)/2))], 1, out$par[((ceiling((kn+2+1)/2))+1):(kn+1)]))/sqrt(t((c(out$par[1:(ceiling((kn+2+1)/2))], 1, out$par[((ceiling((kn+2+1)/2))+1):(kn+1)]))%*%(c(out$par[1:(ceiling((kn+2+1)/2))], 1, out$par[((ceiling((kn+2+1)/2))+1):(kn+1)]))))
  
  ageriesti <- ageri%*%(c(out$par[1:(ceiling((kn+2+1)/2))], 1, out$par[((ceiling((kn+2+1)/2))+1):(kn+1)])^2)
  #ageriesti <- ageri%*%agepara^2
  
  
  # results <- list("coef" = ris, "se"=se, "age" =ageriesti/max(cumsum(ageriesti)), "ageaxis"= ageaxis, "smoothingpara"=format(lambda1,  scientific = T,digits=2), "cv"=smparcv)
  results <- list("coef" = ris, "se"=se, "age" =ageriesti/ageriesti[1], "ageaxis"= ageaxis, "smoothingpara"=lambda1, "cv"=smparcv)
  
  class(results) <- "smoothagesccs"
  return(results)
}
