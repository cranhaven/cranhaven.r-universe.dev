#--------------------------------------------------------------------#
#                 Non-parametric SCCS method                         #
# where age and exposure effects are represented by spline functions #
#--------------------------------------------------------------------#

nonparasccs <- function(indiv, astart, aend, aevent, adrug, aedrug, kn1=12, kn2=12, sp1=NULL, sp2=NULL, data){
  
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  
  fdata <- data.frame("indiv"=indiv, "startob"=astart, "endob"=aend, "st_risk"=adrug, "end_risk"=aedrug, "eventday"=aevent)
  
  fdata$st_risk <- pmax(fdata$startob, fdata$st_risk)
  fdata$st_risk <- pmin(fdata$endob, fdata$st_risk)
  
  fdata$end_risk <- pmax(fdata$startob, fdata$end_risk)
  fdata$end_risk <- pmin(fdata$endob, fdata$end_risk)
  
  fdata$st_risk <- ifelse(is.na(fdata$st_risk), fdata$endob, fdata$st_risk)
  fdata$end_risk <- ifelse(is.na(fdata$end_risk), fdata$endob, fdata$end_risk)
  fdata$end_risk <- pmin(fdata$endob, fdata$end_risk) # new 30 - Oct - 2017
  
  
  
  fdata$expostatus <- ifelse(fdata$eventday < fdata$st_risk | fdata$eventday > fdata$end_risk | fdata$st_risk==fdata$endob, 0, 1)
  fdata$timesinceex <- fdata$eventday - fdata$st_risk
  fdata$timesinceex <- ifelse(fdata$timesinceex < 0, 0, fdata$timesinceex)
  fdata$timesinceex <- ifelse(fdata$timesinceex > (fdata$end_risk - fdata$st_risk), max(fdata$end_risk - fdata$st_risk), fdata$timesinceex)
  
  knots1 <- seq(min(fdata$startob), max(fdata$endob)+0.0005, length=kn1)
  # knots1ex <- seq(0, max(fdata$end_risk - fdata$st_risk)+0.0005, length=kn2)
  
  data4 <- data.frame(fdata)
  data4$timesincevac <- data4$eventday - data4$st_risk
  data4$risklen <- data4$end_risk - data4$st_risk
  #data4$expostatus <- ifelse(data4$timesincevac <= 0 | data4$timesincevac > data4$risklen, 0, 1)
  data4$expostatus <- ifelse(data4$timesincevac < 0 | data4$timesincevac > data4$risklen, 0, 1) # new 30 - Oct - 2017
  timesincevacwithinexpo <- data4$timesincevac[data4$expostatus==1] 
  timesincevacwithinexpo<- c(timesincevacwithinexpo, 0, max(data4$risklen)+0.00001)
 
  
  knots1ex <- seq(min(timesincevacwithinexpo), max(timesincevacwithinexpo), length=kn2)
  
  
  # Design matrices related to the age effect, its 1st, 2nd, and 3rd derivatives at age at event,  start risk and end risk
  
  Maevent       = dmsplinedesign(fdata$eventday, knots1, 4, 0)
  Maendrisk     = dmsplinedesign(fdata$end_risk, knots1, 4, 0)
  Mastrisk      = dmsplinedesign(fdata$st_risk, knots1, 4, 0)
  Mad1endrisk   = dmsplinedesign(fdata$end_risk, knots1, 4, 1)
  Mad1strisk    = dmsplinedesign(fdata$st_risk, knots1, 4, 1)
  Mad2endrisk   = dmsplinedesign(fdata$end_risk, knots1, 4, 2)
  Mad2strisk    = dmsplinedesign(fdata$st_risk, knots1, 4, 2)
  Mad3endrisk   = dmsplinedesign(fdata$end_risk, knots1, 4, 3)
  Mad3strisk    =  dmsplinedesign(fdata$st_risk, knots1, 4, 3)
  Iastrisk      =  ispline(fdata$st_risk, knots1, 4)
  Iastartobs =  ispline(fdata$startob, knots1, 4)
  Iaendobs =    ispline(fdata$endob, knots1, 4)
  Iaendrisk =   ispline(fdata$end_risk, knots1, 4)
  # Design matrices related to the exposure effect and integrals
  Mexeventsinceex = dmsplinedesign(fdata$timesinceex, knots1ex, 4, 0)    # knotsex4 are knots used to produce M-spline for the exposure effect
  Iexendrisk      = ispline(fdata$end_risk-fdata$st_risk, knots1ex, 4)
  Iexstrisk       = ispline(fdata$st_risk-fdata$st_risk, knots1ex, 4)
  I1exendrisk     = integrateIspline(fdata$end_risk-fdata$st_risk, knots1ex, 4, 1)
  I1exstrisk      = integrateIspline(fdata$st_risk-fdata$st_risk, knots1ex, 4, 1)
  I2exendrisk     = integrateIspline(fdata$end_risk-fdata$st_risk, knots1ex, 4, 2)
  I2exstrisk      = integrateIspline(fdata$st_risk-fdata$st_risk, knots1ex, 4, 2)
  I3exendrisk     = integrateIspline(fdata$end_risk-fdata$st_risk, knots1ex, 4, 3)
  I3exstrisk      = integrateIspline(fdata$st_risk-fdata$st_risk, knots1ex, 4, 3)
  
  ################################
  #      Penalty Matrices        #
  ################################
  
  # Age effect #
  basisobj_age <- create.bspline.basis(knots1,kn1+2)
  penaltymatrixage <- bsplinepen(basisobj_age)
  
  # Exposure effect #
  basisobj_exposure <- create.bspline.basis(knots1ex,kn2+2)
  penaltymatrix <- bsplinepen(basisobj_exposure)
  
  
  ##---------------------------------------------------------------------##
  #                     Selection of smoothing parametrs                  #
  ##---------------------------------------------------------------------##
  
  
                       #----------------------------------------#
                       #    Age smoothing parameter selection   #   
                       #----------------------------------------#
  
  
  #-----------------------------------------------#
  # Penalized negative log likelihood function    #
  #    when there is no exposure                  #
  #-----------------------------------------------#
  
  neg.LLnex <- function(p, lambda1) {
    
    -(sum(rowsum(log(Maevent[, 1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2), fdata$indiv))
      
      -(sum(rowsum(log((Iaendobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2) - (Iastartobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)
                       
      ), fdata$indiv)))
      
      -(lambda1)*(t(c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)%*%penaltymatrixage%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)
    )
  }
  
  
  llex <- function(p) {
    
    -(sum(rowsum(log(Maevent[, 1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2), fdata$indiv))
      
      -(sum(rowsum(log((Iaendobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2) - (Iastartobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)
                       
      ), fdata$indiv)))
      
    )
  }
  
  
  # Cross validation score
  
  cv1 <- function(lambda1) {
    
    p0 <- c(rep(1/(kn1+1), kn1+1))
    
    outopt1 <- optim(p0, neg.LLnex, lambda1=lambda1, gr = NULL, method = c("BFGS"),  hessian = TRUE)
    par <- outopt1$par
    hes <- outopt1$hessian
    #p0 <- outopt1$par
    
    cvs <- llex(par) + sum(diag(pseudoinverse(-hes[1:(kn1+1), 1:(kn1+1)])%*%(-hes[1:(kn1+1), 1:(kn1+1)] + 
                      ((lambda1*(8*penaltymatrixage*((c(par[1:(ceiling((kn1+2+1)/2))], 1, par[((ceiling((kn1+2+1)/2))+1):(kn1+1)]))%*%t(c(par[1:(ceiling((kn1+2+1)/2))], 1, par[((ceiling((kn1+2+1)/2))+1):(kn1+1)]))) + 4*(diag(as.vector(penaltymatrixage%*%(c(par[1:(ceiling((kn1+2+1)/2))], 1, par[((ceiling((kn1+2+1)/2))+1):(kn1+1)]))^2)))))[-((ceiling((kn1+2+1)/2))+1),-((ceiling((kn1+2+1)/2))+1)]))))
    
    return(cvs)
    
  }
  
  
  if (is.null(sp1)) {
    smpar1 <- optim(0.00001, cv1, method=c("Brent"), lower = 0.00001, upper = 150000000, control = list(reltol=1e-2))
    
  } else {
    
    smpar1<-list("par"= sp1, "value"=cv1(sp1))
  }
  
  
  
  # End of age smoothing parameter selection lambda1 = smpar1$par
  
  
  #--------------------------------------------------------------------------------------------------------------------------#
 
  #---------------------------------------------#
  #    Exposure smoothing parameter selection   #   
  #---------------------------------------------#
  
  
  #---------------------------------------------#
  # Penalized negative log likelihood function  #
  #    when there is no age effect              #
  #---------------------------------------------#
  
  neg.LLna <- function(p, lambda) {
    
    -(sum(rowsum(log(rep(1, nrow(fdata))), fdata$indiv)) # rep(1, nrow(fdata)) is used to indicate the age effect is 1
      + sum(rowsum(fdata$expostatus*(log(Mexeventsinceex%*%p[1:length(p)]^2)), fdata$indiv))
      
      -(sum(rowsum(log((fdata$st_risk - fdata$startob)
                       
                       + ((Iexendrisk%*%p[1:length(p)]^2) - (Iexstrisk%*%p[1:length(p)]^2))
                       
                       + (fdata$endob - fdata$end_risk)), fdata$indiv)))
      
      -(lambda)*(t(p[1:length(p)]^2)%*%penaltymatrix%*%p[1:length(p)]^2)
    )
  }
  
  
  ll <- function(p) {                 # Negative loglikelihood without the penatlty term
    
    -(sum(rowsum(log(rep(1, nrow(fdata))), fdata$indiv)) # rep(1, nrow(fdata)) is used to indicate the age effect is 1
      + sum(rowsum(fdata$expostatus*(log(Mexeventsinceex%*%p[1:length(p)]^2)), fdata$indiv))
      
      -(sum(rowsum(log((fdata$st_risk - fdata$startob)
                       
                       + ((Iexendrisk%*%p[1:length(p)]^2) - (Iexstrisk%*%p[1:length(p)]^2))
                       
                       + (fdata$endob - fdata$end_risk)), fdata$indiv)))
      
      
    )
  }
  
# Cross validation score
  
  cv <- function(lambda) {
    
    p0 <- c(rep(1/(kn2+2), kn2+2))
    
    outopt <- optim(p0, neg.LLna, lambda=lambda, gr = NULL, method = c("BFGS"),  hessian = TRUE)
    
    cvs <- ll(outopt$par) + sum(diag((pseudoinverse(-outopt$hessian))%*%(-outopt$hessian + lambda*((8*penaltymatrix*(outopt$par%*%t(outopt$par))  ) +  (4*(diag(as.vector(penaltymatrix%*%outopt$par^2))))) )))
    
    
    return(cvs)
    
  }
  
  # smpar <- optim(15, cv, method=c("Brent"), lower = 0.00001, upper = 1500)
  
  
  
  if (is.null(sp2)) {
    smpar <- optim(15, cv, method=c("Brent"), lower = 0.00001, upper = 1500, control = list(reltol=1e-2))
    
  } else {
    
    smpar<-list("par"= sp2, "value"=cv(sp2))
  }
  
  
  # End of exposure smoothing paramter selection (lambda = smpar$par)
  
  #------------------------------------------------------------------------------------------------------------------------------------------#  
  
  
  #--------------------------------------------------------------#
  #             The negative log likelihood function with        #
  #              age and exposure represented by splines         #
  #--------------------------------------------------------------#
  neg.LL <- function(p, lambda1, lambda) {
    
    -(sum(rowsum(log(Maevent[, 1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2), fdata$indiv))
      + sum(rowsum(fdata$expostatus*(log(Mexeventsinceex%*%p[(kn1+2):length(p)]^2)), fdata$indiv))
      
      
      -(sum(rowsum(log(Iastrisk%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2 - (Iastartobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)
                       
                       + ((Maendrisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(Iexendrisk%*%p[(kn1+2):length(p)]^2) - (Mastrisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(Iexstrisk%*%p[(kn1+2):length(p)]^2))
                       
                       - ((Mad1endrisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I1exendrisk%*%p[(kn1+2):length(p)]^2) - (Mad1strisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I1exstrisk%*%p[(kn1+2):length(p)]^2))
                       
                       + ((Mad2endrisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I2exendrisk%*%p[(kn1+2):length(p)]^2) - (Mad2strisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I2exstrisk%*%p[(kn1+2):length(p)]^2))
                       
                       - ((Mad3endrisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I3exendrisk%*%p[(kn1+2):length(p)]^2) - (Mad3strisk[,1:(kn1+2)]%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)*(I3exstrisk%*%p[(kn1+2):length(p)]^2))
                       
                       + (Iaendobs%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2 - Iaendrisk%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)), fdata$indiv)))
      
      -(lambda1)*(t(c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)%*%penaltymatrixage%*%c(p[1:(ceiling((kn1+2+1)/2))], 1, p[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2)
      -(lambda)*(t(p[(kn1+2):length(p)]^2)%*%penaltymatrix%*%p[(kn1+2):length(p)]^2)
      
    )
  }
  
 
  ### Intial values for the final model###
  
  p0f1 <- c(rep(1/(kn1+1), kn1+1))
  
  outoptf1 <- optim(p0f1, neg.LLnex, lambda1=smpar1$par, gr = NULL, method = c("BFGS"),  hessian = TRUE)
  
  p0f2 <- c(rep(1/(kn2+2), kn2+2))
  
  outoptf2 <- optim(p0f2, neg.LLna, lambda=smpar$par, gr = NULL, method = c("BFGS"),  hessian = TRUE)
  #--------------------------------------------------------------------------------------------------------------#
  
  # p0f <- c(rep(1/(kn1+1), (kn1+1)), rep(1/(kn2+2), (kn2+2))) # Initial values for the parameters
  
  p0f <- c(outoptf1$par, outoptf2$par) 
  out <- optim(p0f, neg.LL, lambda1=smpar1$par, lambda=smpar$par, gr = NULL, method = c("BFGS"), hessian = TRUE)
  
  
  # Estimated exposure related relative incidence function
  
  timesinceex <- seq(0, max(fdata$timesinceex))
  M <- dmsplinedesign(timesinceex, knots1ex, 4, 0)
  # expriesti <- Msplineexeffect%*%(out$par[(kn1+2):length(out$par)]^2)
  
 
  
  betas <- out$par[(kn1+2):length(out$par)]
 # var_covall <- 
  var_cov_beta <- pseudoinverse((1/2)*(out$hessian))[(kn1+2):length(out$par), (kn1+2):length(out$par)]        # variance covariance matrix of betas obtained from hessian
  
  # var_cov_betasq <- deltamethod (list(~ x1^2, ~ x2^2), alphas, var_cov_beta, ses=FALSE) # variance covatiance matrix of beta^2
  
  var_cov_betasq <- (2*diag(betas))%*%var_cov_beta%*%(2*t(diag(betas)))
  
  variance <- rep(0, length(timesinceex))
  
  for (i in 1:length(timesinceex)) {
    variance[i] <- t(M[i,])%*%var_cov_betasq%*%M[i,]
  }
  
  sderror <- sqrt(variance)
  
  estimates <- (M%*%out$par[(kn1+2):length(out$par)]^2)
  
  #lci <- estimates - 1.96*sderror
  #uci <- estimates + 1.96*sderror
  
  # Estimated age related relative incidence function
  
  agessss <- seq(min(astart), max(aend))
  Msplineageeffect <- dmsplinedesign(agessss, knots1, 4, 0)
  ageriesti <- Msplineageeffect%*%(c(out$par[1:(ceiling((kn1+2+1)/2))], 1, out$par[((ceiling((kn1+2+1)/2))+1):(kn1+1)])^2) 
  
 
  # nonparaSCCSoutput <- list("exposure" = estimates, "age"=ageriesti/max(cumsum(ageriesti)), "ageaxis"=agessss, "timesinceexpo"=timesinceex, "se"=sderror, "lambda1"=smpar1$par, "cv1"=smpar1$value, "lambda2"=smpar$par, "cv2"=smpar$value)
  nonparaSCCSoutput <- list("exposure" = estimates, "age"=ageriesti/ageriesti[1], "ageaxis"=agessss, "timesinceexpo"=timesinceex, "se"=sderror, "lambda1"=smpar1$par, "cv1"=smpar1$value, "lambda2"=smpar$par, "cv2"=smpar$value)
  class(nonparaSCCSoutput) <- "nonparasccs"
  return(nonparaSCCSoutput)
}

