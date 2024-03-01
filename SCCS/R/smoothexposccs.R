#------------------------------------------------#
#  The function fits SCCS with smooth exposure   #
#  function and piecewise constant age effect    #
#------------------------------------------------#

smoothexposccs <- function(indiv, astart, aend, aevent, adrug, aedrug, agegrp, kn=12, sp = NULL, data) {
  
  agegrp <- agegrp-1
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  
  #adrug <- adrug - 1
  adrug <- pmax(astart, adrug)
  adrug <- pmin(aend, adrug)
  
  aedrug <- pmax(astart, aedrug)
  aedrug <- pmin(aend, aedrug)
  
  adrug <- ifelse(is.na(adrug), aend, adrug)
  aedrug <- ifelse(is.na(aedrug), aend, aedrug)
  
  ex1 <- pmin(aend, adrug)
  ex2 <- pmin(aend, aedrug)
  
  expo <- cbind(ex1, ex2)
  
  
  # d <- c(min(dat$startob), agegrp, max(dat$endob))
  d <- matrix( rep( t( agegrp ) , nrow(data) ), nrow=nrow(data), ncol=length(agegrp), byrow=T)
  # expo <- cbind(dat$st_risk, dat$end_risk)
  #expo <- cbind(data$st_risk, data$end_risk)
  
  # dd <- cbind(d, expo, data$astart, data$aend)
  dd <- cbind(d, expo, astart, aend)
  #dd <- cbind(d, expo, dat$startob, dat$endob)
  ddd <- t(apply(dd, 1, sort))
  
  for (i in 1:ncol(ddd)){
    ddd[,i] <- pmax(ddd[,i], astart)
    ddd[,i] <- pmin(ddd[,i], aend)
    
  }
  
  # Determine age group of the intervals
  
  #catage <- function(x){
  #  agecatagories <- (cut(x, breaks=c(min(data$startob)-1, agegrp, max(data$endob)), labels = FALSE))[-1]
  #  return(agecatagories)
  #}
  #agecat <- apply(ddd, 1, catage)
  
  agecat <- matrix(0, nrow=nrow(ddd), ncol=(ncol(ddd)-1))
  
  for (i in 1:nrow(agecat)){  # Note change this using apply fun
    
    agecat[i, ] <- (cut(ddd[i,], breaks=c(min(astart)-1, agegrp, max(aend)), labels = FALSE))[-1]
  }
  
  # Represent the age groups as dummy variables
  
  #-----------------------------------------------------#
  # A function that creates dummies
  
  agedummyfun <- function(x, agegrp){
    
    agelevel <- seq(1:(length(agegrp)+1))
    agedumi <- matrix(NA, nrow=nrow(agecat), ncol=length(agelevel))
    
    for(i in 1:length(agelevel))
      
      agedumi[,i] <- ifelse(x==agelevel[i], 1, 0)
    
    return(agedumi)
  }
  #-----------------------------------------------------#
  
  agedummy <- list()
  
  for (i in 1:(ncol(ddd)-1)){
    
    agedummy[[i]] <- agedummyfun(agecat[,i], agegrp)
    
  }
  
  # Define exposure status of each interval. 
  exgr <- matrix(0, nrow=nrow(ddd), ncol=(ncol(ddd)-1))
  
  # expolevel <- c(1, 0) 
  
  for (i in 1:(ncol(ddd)-1)){
    
    #for(j in 1:2)
    
    exgr[,i] <- ifelse(ddd[,i]>=expo[,1] & ddd[,(i+1)]<=expo[,2], 1, 0)
  }
  #--------------------------------------------------------------------------------------#
  #======================#
  #  Start splines here  #
  #======================#
  
  # Take only events whose event occured within the exposure effect to determine knots
  #data4 <- data.frame(data)
  timesincevac <- aevent - adrug
  risklen <- aedrug - adrug
  expostatus <- ifelse(timesincevac < 0 | timesincevac > risklen, 0, 1)
  timesincevacwithinexpo <- timesincevac[expostatus==1] 
  timesincevacwithinexpo<- c(timesincevacwithinexpo, 0, max(risklen)+0.00001)
  
  
  knots1 <- seq(min(timesincevacwithinexpo), max(timesincevacwithinexpo), length=kn)
  
  #-------------------------------------------#
  # Define design matrix for time since start #
  #    of exposure based on order4 M-splines  #
  #-------------------------------------------#
  
  timesincevacdesign <- dmsplinedesign(timesincevac, knots1, 4, 0)
  
  basisobj_exposure <- create.bspline.basis(knots1,kn+2)
  penaltymatrix <- bsplinepen(basisobj_exposure)
  
  
  #------------------------------------------#
  #     Smoothing parameter selection        #
  #------------------------------------------#
  
  timesinceex <- timesincevac
  timesinceex <- ifelse(timesinceex < 0, 0, timesinceex)
  timesinceex <- ifelse(timesinceex > risklen, max(risklen), timesinceex)
  
  #fdata1$expostatus <- ifelse(fdata1$eventday < fdata1$st_risk | fdata1$eventday > fdata1$end_risk, 0, 1)
  Mexeventsinceex = dmsplinedesign(timesinceex, knots1, 4, 0)
  Iexendrisk      = ispline(aedrug-adrug, knots1, 4)
  Iexstrisk       = ispline(adrug-adrug, knots1, 4)
  
  #---------------------------------------------#
  # Penalized negative log likelihood function  #
  #    when there is no age effect              #
  #---------------------------------------------#
  
  
  neg.LLna <- function(p, lambda) {
    
    -(sum(log(rep(1, nrow(data)))) # rep(1, nrow(fdata1)) is used to indicate the age effect is 1
      + sum(expostatus*(log(Mexeventsinceex%*%p[1:length(p)]^2)))
      
      -(sum(log((adrug - astart)
                
                + ((Iexendrisk%*%p[1:length(p)]^2) - (Iexstrisk%*%p[1:length(p)]^2))
                
                + (aend - aedrug))))
      
      -(lambda)*(t(p[1:length(p)]^2)%*%penaltymatrix%*%p[1:length(p)]^2)
    )
  }
  
  
  
  ll <- function(p) {                 # Negative loglikelihood without the penatlty term
    
    -(sum(log(rep(1, nrow(data)))) # rep(1, nrow(fdata1)) is used to indicate the age effect is 1
      + sum(rowsum(expostatus*(log(Mexeventsinceex%*%p[1:length(p)]^2)), indiv))
      
      -(sum(log((adrug - astart)
                
                + ((Iexendrisk%*%p[1:length(p)]^2) - (Iexstrisk%*%p[1:length(p)]^2))
                
                + (aend - aedrug))))
      
    )
  }
  
  
  
  # Cross validation score
  
  cv <- function(lambda) {
    
    p0 <- c(rep(1/(kn+2), kn+2))
    
    outopt <- optim(p0, neg.LLna, lambda=lambda, gr = NULL, method = c("BFGS"),  hessian = TRUE)
    
    cvs <- ll(outopt$par) + sum(diag((pseudoinverse(-outopt$hessian))%*%(-outopt$hessian + lambda*((8*penaltymatrix*(outopt$par%*%t(outopt$par))  ) +  (4*(diag(as.vector(penaltymatrix%*%outopt$par^2))))) )))
    
    
    return(cvs)
    
  }
  
  
  if (is.null(sp)) {
    smpar <- optim(15, cv, method=c("Brent"), lower = 0.00001, upper = 1500, control = list(reltol=1e-2))
    
  } else {
    
    smpar<-list("par"= sp, "value"=cv(sp))
  }
  
  lambda1 <- smpar$par
  smparcv <- smpar$value
  
  
  
  # Time since start of exposure for the intervals
  timesinceexp1 <- ddd - adrug
  
  for (i in 1:ncol(timesinceexp1)){
    
    timesinceexp1[,i] <- ifelse(timesinceexp1[,i]<0, 0, timesinceexp1[,i])
    timesinceexp1[,i] <- ifelse(timesinceexp1[,i]>(expo[,2]-expo[,1]), max(expo[,2]-expo[,1]), timesinceexp1[,i])
  }
  
  # Isplines for time since exposure of the cut points
  Itimesiceexp <- list()
  
  for (i in 1:ncol(ddd)){
    
    Itimesiceexp[[i]] <- ispline(timesinceexp1[,i], knots1, 4)
  }
  
  
  # Length of intervals
  
  intlength <- matrix(NA, nrow=nrow(ddd), ncol=(ncol(ddd)-1))
  
  for (i in 1:ncol(intlength)){
    intlength[,i] <- ddd[,(i+1)]-ddd[,i]
  }
  
  # Age group of the event day
  
  eventagegrp <- cut(aevent, breaks = c(min(astart), agegrp, max(aend)), labels = FALSE)
  
  eventagegrpdum <- agedummyfun(eventagegrp, agegrp = agegrp)
  
  # Exposure status of the event day
  
  expostatus <- ifelse(aevent>=adrug & aevent <= aedrug, 1, 0)
  
  #p0 <- c(rep(1/(kn+2), kn+2), rep(1, length(agegrp)))
  
  #----------------------------------------------------#
  #  Minimization of the full negative log-likelihood  #
  #              function given lambda                 #
  #----------------------------------------------------#
  
  neg.LL <- function(p, lambda) {
    
    
    denom <- matrix(0, nrow=nrow(data), ncol=ncol(intlength))
    
    for (i in 1:ncol(intlength)) {
      
      denom[,i] <- ((exp(agedummy[[i]]%*%c(0,p[(kn+3):length(p)])))*(intlength[,i]^(1-exgr[,i])))*(((Itimesiceexp[[(i+1)]]%*%p[1:(kn+2)]^2)-(Itimesiceexp[[(i)]]%*%p[1:(kn+2)]^2))^exgr[,i])  
    }
    
    
    -(sum(eventagegrpdum%*%c(0,p[(kn+3):length(p)])) # length(age) is the number of cut points used to define age groups
      
      + sum(expostatus*(log(Mexeventsinceex%*%p[1:(kn+2)]^2)))
      
      
      -(sum(log(rowSums(denom))))
      
      - (lambda)*(t(p[1:(kn+2)]^2)%*%penaltymatrix%*%p[1:(kn+2)]^2)
    )
    
  }
  
  
  p0 <- c(rep(1/(kn+2), kn+2), rep(0.6, length(agegrp)))
  
  outopt <- optim(p0, neg.LL, lambda=lambda1, gr=NULL,
                  method = c("BFGS"), lower = -Inf, upper = Inf, hessian = TRUE)
  
  
  risage <- outopt$par[(kn+3):length(outopt$par)]
  
  se <- sqrt(diag(pseudoinverse(outopt$hessian)[(kn+3):length(outopt$par),(kn+3):length(outopt$par)]))
  
  
  agename <- NULL
  for (i in 1:length(agegrp)){
    
    agename[i] <- paste("age",(i+1), sep="")
  }
  
  names(risage) <- agename
  #estimates <- cbind(RI=ris, se=se)
  
  
  timesincevaccc <- seq(0, max(timesinceex), 1) # time since vaccination at which exposure related RIs are to estimated
  
  M <- dmsplinedesign(timesincevaccc, knots1=knots1, 4, 0)
  
  
  #------------------------------------#
  
  betas <- outopt$par[1:(kn+2)]
  
  var_cov_beta <- pseudoinverse((1/2)*outopt$hessian)[1:(kn+2), 1:(kn+2)]        # variance covariance matrix of betas obtained from the hessian
  
  # var_cov_betasq <- deltamethod (list(~ x1^2, ~ x2^2), alphas, var_cov_beta, ses=FALSE) # variance covatiance matrix of beta^2
  
  var_cov_betasq <- (2*diag(betas))%*%var_cov_beta%*%(2*t(diag(betas)))
  
  variance <- rep(0, length(timesincevaccc))
  
  for (i in 1:length(timesincevaccc)) {
    variance[i] <- t(M[i,])%*%var_cov_betasq%*%M[i,]
  }
  
  sderror <- sqrt(variance)
  
  estimates <- (M%*%outopt$par[1:(kn+2)]^2)
  
  lci <- estimates - 1.96*sderror
  uci <- estimates + 1.96*sderror
  
 
  results <- list("coef" = risage, "se_age"=se, "exposure" =estimates, "timesinceexpo"= timesincevaccc, "se"=sderror, "smoothingpara"=lambda1, "cv"=smparcv)
  
  
  class(results) <- "smoothexposccs"
  return(results)
  
  
}


