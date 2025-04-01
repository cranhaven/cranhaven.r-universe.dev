InDisc <- function(SCO, nfactors = 1, nquad = 30, model = "linear", approp = FALSE, display = TRUE){

  ## checks

  # check if the data is a data.frame

  if (is.data.frame(SCO)){
    SCO <- as.matrix(SCO)
  }

  # check if the data is a matrix

  f1 <- size(SCO)[1]
  f2 <- size(SCO)[2]

  if (f1 == 1 || f2 == 1){
    stop("The SCO argument has to be a matrix")
  }

  # check if nfactors > 4
  if (nfactors > 4){
    stop("The maximum number of factors allowed is 4.")
  }

  # check the number of items

  if (f2/nfactors < 20){ #binary 20, graded o continuo 15
    # stop or warning?
    warning("The number of items is probably too small for accurate PDD estimation.")
  }


  ################
  ## begin analysis

  #center SCO
  if (all(SCO == floor(SCO))){
    #integers, probably graded
    ncat <- max(max(SCO) - min(SCO)) + 1
    central <- (ncat-1)/2

    SCOc <- SCO - central

    if (ncat > 5){
      model <- "linear"
    }
  }
  else {

    if (model=="graded"){
      model = "linear"
      warning("The data appears to be continuous, linear model was selected.")
    }
    #normalize
    SCOc <- sweep(sweep(SCO,2,colMeans(SCO)),2,apply(SCO, 2, sd),"/")

  }


  #mean of each item
  mu <- transpose(colMeans(SCO))

  muc <- transpose(colMeans(SCOc))

  if (model == "linear"){

    if (nfactors==1){
      #minres using correlation
      out_fa <-psych::fa(SCO)

      #minres using covariance
      lam <- psych::fa(SCO, covar=T)$loadings

    }
    else {
      #minres using correlation
      out_fa <-psych::fa(SCO, nfactors, rotate = "oblimin")

      #minres using covariance
      lam <- psych::fa(SCO, nfactors, rotate = "oblimin", covar=T)$loadings
    }


  }

  if (model == "graded"){

    # when graded, check if min is 0
    if (min(SCO)==0){
      SCO <- SCO + 1
    }

    tmp <- max(SCO) - min(SCO)
    if (tmp ==1){
      #minres using tetrachoric correlation
      if (nfactors==1){
        out_fa<-psych::fa(SCO, cor = "tet")
      }
      else {
        out_fa<-psych::fa(SCO, nfactors, rotate = "oblimin", cor = "tet")
      }

    }
    if (tmp >1){
      #minres using polyhcoric correlation
      if (nfactors ==1){
        out_fa<-psych::fa(SCO, cor = "poly",correct = FALSE)
      }
      else {
        out_fa<-psych::fa(SCO, nfactors, rotate = "oblimin",cor = "poly",correct = FALSE)
      }
    }

    #minres using covariance
    if (nfactors == 1){
      lam <- psych::fa(SCO, covar=T)$loadings
    }
    else {
      lam <- psych::fa(SCO, nfactors, rotate = "oblimin" ,covar=T)$loadings
    }


  }

  alpha<-out_fa$loadings
  alpha <- matrix(as.numeric(alpha)[1:(nfactors*f2)],ncol = nfactors)
  gof_dof <- out_fa$dof # degrees of freedom
  gof_chi <- out_fa$chi # empirical chi square
  gof_RMSR <- out_fa$rms
  gof_TLI <- out_fa$TLI
  gof_RMSEA <- as.numeric(out_fa$RMSEA[1])

  if (nfactors > 1){
    PHI <- out_fa$Phi
  }

  #1st priorgral for estimating residual variance and nodes

  if (nfactors ==1){
    OUT<-priorgral(alpha,nquad)
  }
  else {
    OUT <- priormul(alpha,PHI,nquad)
  }


  # WHEN NON CONVERGENCY, RECOMPUTE USING A MORE STRICT PRIOR ON priorgral
  # OUT <- priorgral(alpha, nquad, 6, 4)

  vres<-OUT$resi
  nodos1<-OUT$nodth
  nodos2<-OUT$nodichi
  var_nodos<-OUT$var_nodos
  EVARI <- OUT$mvari

  # reap

  SX <- diag(matrixStats::colSds(SCOc))
  LAM <- SX %*% alpha

  if (model=="linear"){

    if (nfactors ==1){
      FAC <- reap17c(muc, SCO, lam, vres, nodos1, nodos2, display)
    }
    else {
      SX <- diag(matrixStats::colSds(SCOc))
      LAM <- SX %*% alpha
      OUT <- generbeta(alpha, LAM, muc)
      BETA <- OUT$BETA
      MDIF <- OUT$MDIF

      COM <- diag(alpha %*% PHI %*% transpose(alpha))

      FAC <- reapmulc(muc, SCOc, LAM, vres, nodos1, nodos2, nfactors, display)
    }

  }
  if (model == "graded"){

    #1st thresholds
    if (nfactors == 1){
      THRES <- t(thresholds(SCO,min(SCO),max(SCO)))
    }
    else {
      THRES <- rthres(SCO)
    }

    n <- dim(THRES)[2]
    zeros <- matrix(0,1,n)
    THRES0 <- rbind(THRES,zeros)

    if (nfactors == 1){
      FAC <- reap17g(THRES0, SCO, alpha, vres, nodos1, nodos2, display)
    }
    else{
      OUT <- betagradm(THRES, alpha)
      BETA <- OUT$BETA
      MDIF <- OUT$MDIF

      COM <- alpha %*% PHI %*% transpose(alpha)

      FAC <- reapmulg(THRES0, SCO, alpha, vres, nodos1, nodos2, nfactors, display)


    }


  }

  # WHEN NON CONVERGENCY, FIND WHO AND RECOMPUTE USING A MORE STRICT PRIOR ON priorgral

  if (any(is.na(FAC))){

    is_NaN_FAC <- is.na(FAC)

    f1 <- size(FAC)[1]
    rows_NaN<-which(is.na(FAC))
    rows_NaN <- rows_NaN[rows_NaN <= f1]

    OUT <- priorgral(alpha, nquad, 6, 4)

    vresb<-OUT$resi
    nodos1b<-OUT$nodth
    nodos2b<-OUT$nodichi

    if (model == "linear"){
      FAC_NaN <- reap17c(mu, SCO[rows_NaN,], lam, vresb, nodos1b, nodos2b, FALSE)
    }

    if (model == "graded"){
      FAC_NaN <- reap17g(THRES, SCO[rows_NaN,], alpha, vresb, nodos1b, nodos2b, FALSE)
    }

    if (any(is.na(FAC_NaN))){

      #still non convergency, more strict prior
      OUT <- priorgral(alpha, nquad, 10, 8)

      vresc<-OUT$resi
      nodos1c<-OUT$nodth
      nodos2c<-OUT$nodichi

      if (model == "linear"){
        FAC_NaN2 <- reap17c(mu, SCO[rows_NaN,], lam, vresc, nodos1c, nodos2c, FALSE)
      }

      if (model == "graded"){
        FAC_NaN2 <- reap17g(THRES, SCO[rows_NaN,], alpha, vresc, nodos1c, nodos2c, FALSE)
      }

      for (i in 1:(size(rows_NaN)[2])){
        FAC[rows_NaN[i],] <- FAC_NaN2[i,]
      }

    }

    else {
      #after the first cut, all values converge, replace the NaN values with the obtained
      for (i in 1:(size(rows_NaN)[2])){
        FAC[rows_NaN[i],] <- FAC_NaN[i,]
      }

    }

  }

  ##

  FAC <- as.matrix(FAC)
  if (nfactors==1){
    colnames(FAC) <- c("theta","PDD","PSD (theta)","PSD (PDD)","th reli", "PDD reli")
  }
  if (nfactors==2){
    colnames(FAC) <- c("theta1","theta2","PDD","PSD (theta1)","PSD (theta2)","PSD (PDD)","th1 reli","th2 reli", "PDD reli")
  }
  if (nfactors==3){
    colnames(FAC) <- c("theta1","theta2","theta3","PDD","PSD (theta1)","PSD (theta2)","PSD (theta3)","PSD (PDD)","th1 reli","th2 reli","th3 reli", "PDD reli")
  }
  if (nfactors==4){
    colnames(FAC) <- c("theta1","theta2","theta3","theta4","PDD","PSD (theta1)","PSD (theta2)","PSD (theta3)","PSD (theta4)","PSD (PDD)","th1 reli","th2 reli","th3 reli","th4 reli", "PDD reli")
  }

  theta <- FAC[,1:nfactors]
  person_var <- FAC[,nfactors+1]
  PSD_theta <- FAC[,(nfactors+2):(nfactors+nfactors+1)]
  PSD_PDD <- FAC[,nfactors+nfactors+2]
  reli_th_i<-FAC[,(nfactors+nfactors+3): (nfactors+nfactors+nfactors+2)]
  reli_PDD_i<-FAC[,size(FAC)[2]]

  ## reliability

  if (nfactors==1){
    reli_theta <- 1 - mean(PSD_theta^2)
    reli_PDD <- var_nodos / (var_nodos + mean(PSD_PDD^2))

    aver_r_theta <- mean(reli_th_i)
    aver_r_PDD <- mean(reli_PDD_i)
    cvar <- mean(person_var)
  }
  else {
    reli_theta <- 1 - colMeans(PSD_theta^2)
    reli_PDD <- var_nodos / (var_nodos + mean(PSD_PDD^2))

    #average of the individual reliabilities
    aver_r_theta <- colMeans(reli_th_i)
    aver_r_PDD <- mean(reli_PDD_i)
    cvar <- mean(person_var)
  }

  if (reli_PDD < 0){ #when the variance between person variability is extremely low
    reli_PDD <- 0
  }





  ############ appropiateness indices ############

  if (approp == TRUE){
    if (nfactors ==1){

      cvar <- mean(person_var)

      #calculate th using the mean of person variance

       if (model == "linear"){
         th0 <- reapth17c(mu, SCO, lam, vres, nodos1, cvar, display)[,1]
         OUT2 <- rlrtest(SCO, mu, lam, cvar ,vres, th0, theta, person_var)
       }
       else { #graded
         th0 <- reapth17g(THRES0, SCO, alpha, vres, nodos1, cvar, display)[,1]
         OUT2 <-rlrtesg(THRES0, SCO, alpha, cvar, vres, th0, theta, person_var)
       }

      LR <- OUT2[,1]
      S <- OUT2[,2]

      LR_stat <- mean(LR) # fit, the closer to 0, the better
      Q_Chi_square <- sum(S) # approximate chi square with N degrees of freedom

      OUT<-list("INDIES"=FAC, "alpha" = alpha, "degrees_of_freedom"=gof_dof, "Model_Chi_square"=gof_chi, "RMSR"=gof_RMSR, "TLI"=gof_TLI, "RMSEA"=gof_RMSEA, "EVARI"=EVARI, "reli_theta"=reli_theta, "aver_r_theta" = aver_r_theta, "reli_PDD"=reli_PDD, "aver_r_PDD"=aver_r_PDD,"LR_stat"=LR_stat, "Q_Chi_square"=Q_Chi_square)
    }
    else {

      N=f1
      CFACTH1 <- 1.25*theta
      cvarihat <- 1.35*(person_var - (mean(person_var)*matrix(1,N,1)))
      cvarihat <- cvarihat + matrix(1,N,1)

      FACTH <- reapmulthc(muc,SCOc,LAM,vres,cvar,nodos1,nfactors,display)

      CFACTH0 <- 1.25*FACTH[,1:nfactors]

      #now, cvar is 1

      OUT2 <- rlrtestm(SCOc,muc, LAM, 1, vres, CFACTH0,CFACTH1,cvarihat,nfactors)

      LR <- OUT2[,1]
      S <- OUT2[,2]

      LR_stat <- mean(LR) # fit, the closer to 0, the better
      Q_Chi_square <- sum(S) # approximate chi square with N degrees of freedom

      OUT<-list("INDIES"=FAC, "alpha" = alpha, "degrees_of_freedom"=gof_dof, "Model_Chi_square"=gof_chi, "RMSR"=gof_RMSR, "TLI"=gof_TLI, "RMSEA"=gof_RMSEA, "EVARI"=EVARI, "reli_theta"=reli_theta, "aver_r_theta" = aver_r_theta, "reli_PDD"=reli_PDD, "aver_r_PDD"=aver_r_PDD,"LR_stat"=LR_stat, "Q_Chi_square"=Q_Chi_square)

    }
  }
  else {

    OUT<-list("INDIES"=FAC, "alpha" = alpha, "degrees_of_freedom"=gof_dof, "Model_Chi_square"=gof_chi, "RMSR"=gof_RMSR, "TLI"=gof_TLI, "RMSEA"=gof_RMSEA, "EVARI"=EVARI, "reli_theta"=reli_theta, "aver_r_theta" = aver_r_theta, "reli_PDD"=reli_PDD, "aver_r_PDD"=aver_r_PDD)

  }

  if (display==TRUE){
    return(OUT)
  }
  else {
    invisible(OUT)
  }

}
