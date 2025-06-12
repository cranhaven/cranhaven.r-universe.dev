EMmixlogbs <- function(y, x1, alpha = NULL, Abetas = NULL, medj=NULL, pii = NULL, nu=NULL, g = NULL, accuracy = 10^-6, mfamily="Normal", iter.max = 100)
{
  start.time     <- Sys.time()
  n              <- length(y)

  p           <- ncol(x1)-1
  x           <- as.matrix(x1[,2:(p+1)])


  beta0       <- Abetas[1]                  ## slope
  betas       <- as.matrix(Abetas[2:(p+1)]) ## parameters of regression
  betas0      <- as.matrix(Abetas[2:(p+1)])
  varphi      <- rep(0,g)
  mu          <- matrix(0,n,g)

  for (k in 1:g)
  {
    varphi[k]  <- beta0+medj[k]
    mu[,k]     <- x%*%betas+varphi[k]
  }

  if(mfamily=="Normal")
  {
    criterio      <- 1
    count         <- 0
    varphi0       <- varphi

    lk = lk1 = lk2 <- sum(log(d.mixed.logbs(y, pii, alpha, mu)))## log-likelihood

    while((criterio > accuracy) && (count <= iter.max))
    {
      count       <- count+1
      zij         <- matrix(0, n, g)

      for(j in 1:g)
      {
        # E-step: computing zij, hij, hij2
        d1         <- logbs.fdp(y, alpha[j], mu[,j])

        if(length(which(d1 == 0)) > 0) {d1[which(d1 == 0)] <- .Machine$double.xmin}
        d2         <- d.mixed.logbs(y, pii, alpha, mu)
        if(length(which(d2 == 0)) > 0) {d2[which(d2 == 0)] <- .Machine$double.xmin}

        #Calculo del zij
        zij[,j]    <- d1*pii[j] / d2
        u          <- rep(1, n)

        # M-step: updating pii, alpha  and delta
        pii[j]     <- (1/n)*sum(zij[,j])
        psiem      <- 2*u*sinh((y-mu[,j])/2)
        alpha[j]   <- sqrt(sum(zij[,j]*psiem^2)/sum(zij[,j]))
      }

      pii[g]      <- 1 - (sum(pii) - pii[g])

      #beta        <- nlminb(start=beta0,maxbeta,gradient=NULL,hessian=NULL,ti,pii, alpha, delta,hij,h2ij,zij,scale=1,control=list(),lower=rep(1e-3,g),upper=rep(100,g))$par
      #betas        <- optim(betas0, f=maxbeta.logbs, method="L-BFGS-B", y=y, pii=pii, alpha=alpha, varphi=varphi, x=x, zij=zij, lower=rep(-100,p),upper=rep(100,p))$par
      betas        <- optim(par=betas0,fn=maxbeta.logbs,method="BFGS",y=y, pii=pii, alpha=alpha, varphi=varphi, x=x, zij=zij)$par
      varphi       <- optim(varphi0, fn=maxbeta.logbs, method="L-BFGS-B", y=y, pii=pii, alpha=alpha, betas=betas, x=x, zij=zij, lower=rep(-100,g),upper=rep(100,g))$par


      zero.pos    <- NULL
      zero.pos    <- which(pii == 0)
      if(length(zero.pos) != 0)
      {
        pii[zero.pos] <- 1e-10
        pii[which(pii == max(pii))] <- max(pii) - sum(pii[zero.pos])
      }

      beta0       <- sum(pii*varphi)
      Abetas      <- c(beta0,betas)

      for (k in 1:g)
      {
        mu[,k]    <- x%*%betas+varphi[k]
        medj[k]   <- varphi[k]-beta0
      }


      teta        <- c(alpha, Abetas, medj, pii)#;print(teta)
      auxlog      <- d.mixed.logbs(y, pii, alpha, mu)
      lk3         <- sum(log(auxlog))

      if(count<2){criterio <- abs(lk2 - lk3)/abs(lk3)
      } else {
        tmp       <- (lk3 - lk2)/(lk2 - lk1)
        tmp2      <- lk2 + (lk3 - lk2)/(1-tmp)
        criterio  <- abs(tmp2 - lk3)
      }
      lk2         <- lk3
    }
  }


  lk            <- lk2
  end.time      <- Sys.time()
  time.taken    <- end.time - start.time
  di            <-  2
  d             <-  g*2 + (g-1) #alpha, beta, pi = g + g + (g-1)
  aic           <- -2*lk + 2*d
  bic           <- -2*lk + log(n)*d
  edc           <- -2*lk + 0.2*sqrt(n)*d
  aic_c         <- -2*lk + 2*n*d/(n-d-1)
  mml           <- (di/2)*sum(log(n*pii/12)) + (g/2)*log(n/12) + g*(di + 1)/2 - 2*lk

  ###################################################################################################
    EP               <- im.FMlogbs(y, x1, alpha, Abetas,medj, pii)$EP
    ttable           <- data.frame(cbind(c(alpha,Abetas,medj, pii[1:(g-1)]),c(EP)))
    #Row names of ttable
    namesrowAlpha    <- c(); for(i in 1:g){namesrowAlpha[i] <- paste("alpha",i,sep="")}
    namesrowAbetas   <- c(); for(i in 1:length(Abetas)){namesrowAbetas[i] <- paste("beta",i-1,sep="")}
    namesrowMedj     <- c(); for(i in 1:g){namesrowMedj[i]   <- paste("mu",i,sep="")}
    namesrowPii      <- c(); for(i in 1:(g-1)){namesrowPii[i]  <- paste("pii",i,sep="")}
    rownames(ttable) <- c(namesrowAlpha,namesrowAbetas,namesrowMedj,namesrowPii)
    colnames(ttable) <- c("Estimate","SE")
  ###################################################################################################
    result        <- list(ttable=ttable,alpha=alpha,betas=Abetas,medj=medj,pii=pii,nu=nu,betai=betas,varphi=varphi,theta=teta,iter=count,criterio = criterio, n=length(y),time = time.taken, convergence = criterio<accuracy,aic = aic, bic = bic, edc = edc,aic_c=aic_c,mml=mml,lk=lk,zij=zij)


  obj.out       <- list(result = result)
  class(obj.out)  =  "fm-logbs"
  return(obj.out)
}


