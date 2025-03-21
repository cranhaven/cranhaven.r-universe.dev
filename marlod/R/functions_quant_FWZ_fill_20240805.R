###Function for quantile model for selecting a type of time-dependent covariate (univariable analysis)

Quantile.FWZ=function(y, x, lod, substitue, tau, corstr, typetd, data)
{
  quantreg <- rq <- NULL
  ##################################
  # MA(1)
  ##################################
  ma=function(rho,n){
    out=diag(n)/2
    out[row(out)==col(out)+1]=rho
    out+t(out)
  }
  ##################################
  # Inverse matrix
  ##################################
  cs.inv=function(rho,n){
    inv=1/(1-rho)*diag(n)-rho/((1-rho)*(1+(n-1)*rho))*matrix(1,ncol=n,nrow=n)
    return(inv)
  }
  ar.inv=function(rho,n){
    inv=1/(1-rho^2)*(ma(-rho,n)+diag(c(0,rep(rho^2,n-2),0)))
  }
  ##################################
  # Likelihood function
  ##################################
  lk.ex=function(alph){
    #err=y-x1*betaex
    err=y-x%*%betaex
    lkh=0
    c2=(tau*(1-tau))
    for(i in 1:m){
      ni=nii[i]
      Ai.inv=diag(rep(1/cc,ni))
      Ri.inv=cs.inv(rho=alph,n=ni)
      psi=(err[dat1$id==i]<=0)-tau
      Vi.inv=Ai.inv%*%Ri.inv%*%Ai.inv
      dVi=(c2^ni)*(1+(ni-1)*alph)*(1-alph)^(ni-1)
      lkh=lkh-0.5*(log(dVi)+t(psi)%*%Vi.inv%*%psi)
    }
    return(lkh)
  }
  lk.ar=function(alph){
    #err=y-x1*betaar
    err=y-x%*%betaar
    lkh=0
    c2=(tau*(1-tau))
    for(i in 1:m){
      ni=nii[i]
      Ai.inv=diag(rep(1/cc,ni))
      Ri.inv=ar.inv(rho=alph,n=ni)
      psi=(err[dat1$id==i]<=0)-tau
      Vi.inv=Ai.inv%*%Ri.inv%*%Ai.inv
      dVi=(1-alph^2)^(ni-1)*(c2^ni)
      lkh=lkh-0.5*(log(dVi)+t(psi)%*%Vi.inv%*%psi)
    }
    return(lkh)
  }
  ##################################
  dat1 = data
  m = length(unique(dat1$id))
  nii = as.numeric(table(dat1$id))

  #Substitution method using LOD
  if(substitue=="None") cen_y <- y

  #Substitution method using LOD
  if(substitue=="LOD") cen_y <- ifelse(y>=lod,y,lod)

  #Substitution method using LOD/2
  if(substitue=="LOD2") cen_y <- ifelse(y>=lod,y,lod/2)

  #Substitution method using LOD/square root of 2
  if(substitue=="LODS2") cen_y <- ifelse(y>=lod,y,lod/sqrt(2))

  #Beta-substitution method using mean
  if(substitue=="BetaMean"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)
    y_bar <- mean(lny1)
    z <- qnorm((length(y)-length(y1))/length(y))
    f_z <- dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    sy <- sqrt((y_bar-log((length(y)-length(y1))))^2/(dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))-qnorm((length(y)-length(y1))/length(y)))^2)
    f_sy_z <- (1-pnorm(qnorm((length(y)-length(y1))/length(y))-sy/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    beta_mean <- length(y)/(length(y)-length(y1))*pnorm(z-sy)*exp(-sy*z+(sy)^2/2)
    cen_y <- ifelse(y>=lod, y, lod*beta_mean)
  }
  #Beta-substitution method using geometric mean
  if(substitue=="BetaGM"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)
    y_bar <- mean(lny1)
    z <- qnorm((length(y)-length(y1))/length(y))
    f_z <- dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    sy <- sqrt((y_bar-log((length(y)-length(y1))))^2/(dnorm(qnorm((length(y)-length(y1))/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))-qnorm((length(y)-length(y1))/length(y)))^2)
    f_sy_z <- (1-pnorm(qnorm((length(y)-length(y1))/length(y))-sy/length(y)))/(1-pnorm(qnorm((length(y)-length(y1))/length(y))))
    beta_GM <- exp((-(length(y)-(length(y)-length(y1)))*length(y))/(length(y)-length(y1))*log(f_sy_z)-sy*z-(length(y)-(length(y)-length(y1)))/(2*(length(y)-length(y1))*length(y))*(sy)^2)
    cen_y <- ifelse(y>=lod, y, lod*beta_GM)
  }
  #Multiple imputation method without covariate
  #if(substitue=="MIWOCov"){
  #  y1 <- ifelse(y>=lod,y,NA)
  #  results_Lubin1 <- impute.Lubin(chemcol=y1, dlcol=lod, K=5, Z=NULL, verbose = TRUE)
  #  cen_y <- results_Lubin1$imputed_values[,sample(x = 1:5, size = 1)]
  #}
  #Multiple imputation method with one covariate using id
  if(substitue=="MIWithID"){
    y1 <- ifelse(y>=lod,y,NA)
    results_Lubin2 <- impute.Lubin(chemcol=y1, dlcol=lod, K=5, Z=dat1$id)
    cen_y <- as.matrix(results_Lubin2$imputed_values[,sample(x = 1:5, size = 1)])
  }
  #Multiple imputation method with two covariates using id and visit
  if(substitue=="MIWithIDRM"){
    y1 <- ifelse(y>=lod,y,NA)
    Z2=NULL
    for(i in 1:m){
      Z1=as.matrix(1:nii[i])
      Z2=rbind(Z2,Z1)
    }
    results_Lubin3 <- impute.Lubin(chemcol=y1, dlcol=lod, K=5, Z=cbind(dat1$id,Z2), verbose = TRUE)
    cen_y <- as.matrix(results_Lubin3$imputed_values[,sample(x = 1:5, size = 1)])
  }
  #Multiple imputation method using QQ-plot approach
  if(substitue=="QQplot"){
    y1 <- y[which(y>=lod)]
    lny1 <- log(y1)

    obs <- rank(y)
    rank <- (obs-0.5)/length(y)
    zscore0 <- qnorm(rank)*ifelse(y>=lod,1,0)
    zscore1 <- zscore0[which(zscore0!=0)]

    data_frame0=data.frame(cbind(lny1,zscore1))
    beta_est=as.matrix(glm(lny1~zscore1, data=data_frame0, family=gaussian)$coefficients)
    lny_zscore <- beta_est[1,1] + beta_est[2,1]*qnorm(rank)
    y_zscore <- exp(lny_zscore)
    cen_y <- ifelse(y>=lod,y,y_zscore)
  }
  y <- cen_y

  betaI=summary(rq(y ~ x[,-1], tau, data = dat1), se="iid")$coef[,1]
  #betaI=summary(rq(y ~ x1 + x2, tau, data = dat1), se="iid")$coef[,1]
  VI=summary(rq(y ~ x[,-1], tau, data = dat1), se="iid")$coef[,2]^2
  #VI=summary(rq(y ~ x1 + x2, tau, data = dat1), se="iid")$coef[,2]^2

  #output_Ind = matrix(0,6,length(betaI)) #The first column is for beta0, the second for beta1, etc. Rows: 1 for the betas, 1 for each SE estimation type, and 3 for non-convergence
  #output_Ind[1,1] = tau
  #output_Ind[2,] = t(as.matrix(betaI))
  #output_Ind[3,] = sqrt(t(as.matrix(VI)))
  #output_Ind[4,] = output_Ind[2,]-qt(0.975,(m-length(betaI)+1))*output_Ind[3,]
  #output_Ind[5,] = output_Ind[2,]+qt(0.975,(m-length(betaI)+1))*output_Ind[3,]
  #output_Ind[6,] = 1-pf((output_Ind[2,]/output_Ind[3,])^2,1,m-2)
  #rownames(output_Ind)<-c("Quantile Level","Est_Ind","SE_Ind","95% CI Lower","95% CI Upper","P-value")
  #output_Ind

  ##########################################################################
  #estimates obtained from proposed estimating functions  (with correlation)
  ##########################################################################
  p=length(betaI)
  cc = sqrt(tau*(1-tau))

  if (corstr == "exchangeable" )
  {
    betaex0=betaI
    gamex=diag(VI)

    index<-0
    iter<-1
    rho.ex=0
    while(iter<=15)
    {
      betaex=betaex0
      gamI=gamex
      err=y-x%*%betaex
      Sb=matrix(0,p,1)
      DI=Ds=matrix(0,p,p)
      Vs=matrix(0,nrow=p,ncol=p)
      #B=matrix(0,nrow=p,ncol=ni)

      rr=sqrt(diag(x%*%gamI%*%t(x)))
      dd=err/rr
      dS=dnorm(dd)/rr
      sS=1-tau-pnorm(dd)

      type=as.matrix(c(1,typetd))      #Time-dependent covariate types

      for(i in 1:m){
        ni=nii[i]
        idi=c(dat1$id==i)
        Ai.inv=diag(rep(1/cc,ni))
        Si= sS[idi]
        B=matrix(0,nrow=p,ncol=ni)

        tryex=optimize(lk.ex,c(-1/(ni-1),1),maximum =TRUE)
        rho.ex=tryex$maximum
        IV=cs.inv(rho.ex,ni) #exchangeable working structure
        IV_Type1 = IV

        Ri_inv2 = IV
        Ri_inv2[upper.tri(Ri_inv2)] = 0
        IV_Type2 = Ri_inv2

        Ri_inv3 = IV
        Ri_inv3[upper.tri(Ri_inv3)] = 0
        Ri_inv3[lower.tri(Ri_inv3)] = 0
        IV_Type3 = Ri_inv3

        Ri_inv4 = IV
        Ri_inv4[lower.tri(Ri_inv4)] = 0
        IV_Type4 = Ri_inv4

        for(j in 1:p){
          if(type[j,1]==1){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type1%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==2){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type2%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==3){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type3%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==4){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type4%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
        }

        Xi=x[idi,]
        Sb=Sb+B%*%Si
        Ds=Ds+B%*%diag(dS[dat1$id==i])%*%Xi
        Vs=Vs+B%*%Si%*%t(Si)%*%t(B)
        DI=DI+t(Xi)%*%Ai.inv%*%Ai.inv%*%diag(dS[dat1$id==i])%*%Xi
      }
      D.inv=ginv(Ds)
      betaex0=betaex-D.inv%*%Sb
      gamex=D.inv%*%Vs%*%t(D.inv)
      if(max(abs(betaex0-betaex),abs(gamex-gamI))<=10^(-4)){index=1;break}
      else{iter<-iter+1}
    }
    betaE=betaex0
    VE=gamex

    output_Exch = matrix(0,7,length(betaE)) #The first column is for beta0, the second for beta1, etc. Rows: 1 for the betas, 1 for each SE estimation type, and 3 for non-convergence
    output_Exch[1,1] = tau
    output_Exch[2,1] = corstr
    output_Exch[3,] = round(t(as.matrix(betaE)),digits=4)
    output_Exch[4,] = round(sqrt(t(as.matrix(diag(VE)))),digits=4)
    output_Exch[5,] = round(as.numeric(output_Exch[3,])-qt(0.975,(m-length(betaE)+1))*as.numeric(output_Exch[4,]),digits=4)
    output_Exch[6,] = round(as.numeric(output_Exch[3,])+qt(0.975,(m-length(betaE)+1))*as.numeric(output_Exch[4,]),digits=4)
    output_Exch[7,] = round(1-pf(as.numeric(output_Exch[3,])/as.numeric(output_Exch[4,])^2,1,(m-length(betaE)+1)),digits=4)
    rownames(output_Exch)<-c("Quantile Level","Structure","Estimate","Standard Error","95% CI Lower","95% CI Upper","P-value")

    outputExch <- data.frame(output_Exch)
    colnames(outputExch)[1] <- "Intercept"
    message("Quantile level and correlation structure are provided in the column of Intercept.")
    return(outputExch)
  }

  if (corstr == "AR-1" )
  {
    betaar0=betaI
    gamar=diag(VI)
    index<-0
    iter<-1
    rho.ar=0
    while(iter<=15)
    {
      betaar=betaar0
      gamI=gamar
      err=y-x%*%betaar
      Sb=matrix(0,p,1)
      DI= Ds=matrix(0,p,p)
      Vs=matrix(0,nrow=p,ncol=p)

      rr=sqrt(diag(x%*%gamI%*%t(x)))
      dd=err/rr
      dS=dnorm(dd)/rr
      sS=1-tau-pnorm(dd)

      type=as.matrix(c(1,typetd))      #Time-dependent covariate types

      for(i in 1:m){
        ni=nii[i]
        idi=c(dat1$id==i)
        Ai.inv=diag(rep(1/cc,ni))
        Si= sS[idi]
        B=matrix(0,nrow=p,ncol=ni)

        tryar= optimize(lk.ar,c(-1,1),maximum =TRUE)
        rho.ar=tryar$maximum
        IV=ar.inv(rho.ar,ni) #AR-1 working structure
        IV_Type1 = IV

        Ri_inv2 = IV
        Ri_inv2[upper.tri(Ri_inv2)] = 0
        IV_Type2 = Ri_inv2

        Ri_inv3 = IV
        Ri_inv3[upper.tri(Ri_inv3)] = 0
        Ri_inv3[lower.tri(Ri_inv3)] = 0
        IV_Type3 = Ri_inv3

        Ri_inv4 = IV
        Ri_inv4[lower.tri(Ri_inv4)] = 0
        IV_Type4 = Ri_inv4

        for(j in 1:p){
          if(type[j,1]==1){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type1%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==2){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type2%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==3){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type3%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
          if(type[j,1]==4){
            Xi=x[dat1$id==i,j]
            Vi.inv=Ai.inv%*%IV_Type4%*%Ai.inv
            B[j,]=t(Xi)%*%Vi.inv
          }
        }

        Xi=x[idi,]
        Sb=Sb+B%*%Si
        Ds=Ds+B%*%diag(dS[dat1$id==i])%*%Xi
        Vs=Vs+B%*%Si%*%t(Si)%*%t(B)
        DI=DI+t(Xi)%*%Ai.inv%*%Ai.inv%*%diag(dS[dat1$id==i])%*%Xi
      }
      D.inv=ginv(Ds)
      betaar0=betaar-D.inv%*%Sb
      gamar=D.inv%*%Vs%*%t(D.inv)
      if(max(abs(betaar0-betaar),abs(gamar-gamI))<=10^(-4)){index=1;break}
      else{iter<-iter+1}
    }
    betaE=betaar0
    VE=gamar

    output_AR1 = matrix(0,7,length(betaE)) #The first column is for beta0, the second for beta1, etc. Rows: 1 for the betas, 1 for each SE estimation type, and 3 for non-convergence
    output_AR1[1,1] = tau
    output_AR1[2,1] = corstr
    output_AR1[3,] = round(t(as.matrix(betaE)),digits=4)
    output_AR1[4,] = round(sqrt(t(as.matrix(diag(VE)))),digits=4)
    output_AR1[5,] = round(as.numeric(output_AR1[3,])-qt(0.975,(m-length(betaE)+1))*as.numeric(output_AR1[4,]),digits=4)
    output_AR1[6,] = round(as.numeric(output_AR1[3,])+qt(0.975,(m-length(betaE)+1))*as.numeric(output_AR1[4,]),digits=4)
    output_AR1[7,] = round(1-pf(as.numeric(output_AR1[3,])/as.numeric(output_AR1[4,])^2,1,(m-length(betaE)+1)),digits=4)
    rownames(output_AR1)<-c("Quantile Level","Structure","Estimate","Standard Error","95% CI Lower","95% CI Upper","P-value")

    outputAR1 <- data.frame(output_AR1)
    colnames(outputAR1)[1] <- "Intercept"
    message("Quantile level and correlation structure are provided in the column of Intercept.")
    return(outputAR1)
  }

}
