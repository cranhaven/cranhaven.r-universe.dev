#initialisation of Z only at random

#initialPoints
#Initialisation of the ICL greedy algorithm, we need to initialize the clustering Z, the estimator \theta =(\pi,w,\nu) and probablities \rho :
initialPoints <- function(Q,  ind.all, dataMatrix, nbOfZ, percentageOfPerturbation, threshold, sigma0, sigma1){
  listOfZinitiaux <- initialZ(Q,  dataMatrix, nbOfZ, percentageOfPerturbation)
  dataVec=dataMatrix[lower.tri(dataMatrix)]
  init = initialRho(ind.all, dataVec, listOfZinitiaux, sigma0, sigma1)
  return(list(init=init, dataVec=dataVec))
}

initialPoints_NIG <- function(Q,  ind.all, dataMatrix, nbOfZ, percentageOfPerturbation, threshold, sigma0){
  listOfZinitiaux <- initialZ(Q,  dataMatrix, nbOfZ, percentageOfPerturbation)
  dataVec=dataMatrix[lower.tri(dataMatrix)]
  init = initialRho_NIG(ind.all, dataVec, listOfZinitiaux, sigma0)
  return(list(init=init, dataVec=dataVec))
}


#Initialisation of clustering Z
initialZ <- function(Q,  dataMatrix, nbOfZ, percentageOfPerturbation){
  p <- nrow(dataMatrix)
  if (Q==1){
    listOfZ <- lapply(1:nbOfZ, function(x) (rep(1,p)) )
    listOfZmatrix <- lapply(1:nbOfZ, function(x) (matrix(1,1,p)) )

  }else{
    listOfZ <- vector("list")
    listOfZmatrix <- vector("list")

    k=1
    while(k<=nbOfZ){
      listOfZ[[k]] <- sample(1:Q, p, replace=TRUE)
      k<-k+1
    }

    for (k in 1:nbOfZ){
      initZ = listOfZ[[k]]
      uniq=sort(unique(initZ))
      l=length(uniq)
      if (length(l!=Q)) {          #if a group is empty, the number of groups is shifted
        for (g in 1:p ){
          x=which(uniq==initZ[g])
          initZ[g]<-x
        }
      }
      listOfZ[[k]] = initZ


      #Zvect is transformed into Zmatrix
      Zmatrix <- matrix(0, nrow=length(unique(initZ)), ncol=p)
      ind <- matrix(c(initZ,1:p),ncol=2)
      Zmatrix[ind] <- 1
      listOfZmatrix[[k]] = Zmatrix

    }
  }
  return(list(listOfZ=listOfZ, listOfZmatrix=listOfZmatrix))
}




# initialisation of theta : with Storey for w
initialTheta <- function(ind.all, dataVec, Z, Zmatrix, sigma0, sigma1){
  p=length(Z)
  Q= nrow(Zmatrix)
  N_Q = Q*(Q+1)/2

  theta <- list(nu0=c(0,sigma0))
  theta$pi  = rep(NA,Q)
  for (k in 1:Q) {theta$pi[k]= sum(Z==k)/p}

  theta$nu <- matrix(0, nrow=N_Q, ncol=2)
  sd=rep(sigma1, N_Q)
  theta$nu[,2]=sd
  theta$w <- rep(NA, N_Q)

  pvalues <- 2*(1-stats::pnorm(abs(dataVec), mean=theta$nu0[1], sd= theta$nu0[2]))
  ind=0
  for (k in 1:Q){
    for (l in k:Q){
      ind=ind+1
      mask <- if (k==l) (Zmatrix[k, ind.all[,1]]*Zmatrix[l, ind.all[,2]]) else
        (Zmatrix[k, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[k, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )   #vector of size p(p-1)/2 giving pairs in (k,l)
      I_kl <- mask * (pvalues <=0.5)
      n_kl = sum(I_kl)
      sum_E_X = sum(I_kl *  dataVec)

      #---nu_kl
      theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl

      #---w_kl
      m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
      if (m_kl == 0){
        theta$w[ind] = 0
      }else{
        theta$w[ind] =  max(c(0,1-2*sum(mask * (pvalues > 0.5))/m_kl))
      }
    }
  }
  return(theta=theta)
}

# initialisation of theta : with Storey for w
initialTheta_NIG <- function(ind.all, dataVec, Z, Zmatrix, sigma0){
  p=length(Z)
  Q= nrow(Zmatrix)
  N_Q = Q*(Q+1)/2

  theta <- list(nu0=c(0, sigma0))
  theta$pi  = rep(NA,Q)
  for (k in 1:Q) {theta$pi[k]= sum(Z==k)/p}

  theta$nu <- matrix(0, nrow=N_Q, ncol=2)
  theta$w <- rep(NA, N_Q)

  pvalues <- 2*(1-stats::pnorm(abs(dataVec), mean=theta$nu0[1], sd= theta$nu0[2]))
  ind=0
  for (k in 1:Q){
    for (l in k:Q){
      ind=ind+1
      mask <- if (k==l) (Zmatrix[k, ind.all[,1]]*Zmatrix[l, ind.all[,2]]) else
        (Zmatrix[k, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[k, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )   #vector of size p(p-1)/2 giving pairs in (k,l)
      I_kl <- mask * (pvalues <=0.5)
      n_kl = sum(I_kl)
      sum_E_X = sum(I_kl *  dataVec)
      sum_E_X2= sum(I_kl* dataVec**2)

      #---nu_kl
      theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl

      #---sigma_kl
      theta$nu[ind,2] <- if (n_kl == 0) 1 else sum_E_X2/n_kl - theta$nu[ind,1]**2
      if(theta$nu[ind,2]<=0){theta$nu[ind,2]=.Machine$double.eps}   #machine error management
      theta$nu[ind,2]=sqrt(theta$nu[ind,2])

      #---w_kl
      m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
      if (m_kl == 0){
        theta$w[ind] = 0
      }else{
        theta$w[ind] =  max(c(0,1-2*sum(mask * (pvalues > 0.5))/m_kl))
      }
    }
  }
  return(theta=theta)
}



initialRho <- function(ind.all, dataVec, listOfZinitiaux, sigma0, sigma1){

  N=length(dataVec)     # p(p-1)/2
  nbOfZ <- length(listOfZinitiaux$listOfZ)
  M = nbOfZ
  init <- list(Z=lapply(1:M, function(k) listOfZinitiaux$listOfZ[[k]]),
               Zmatrix =lapply(1:M, function(k) listOfZinitiaux$listOfZmatrix[[k]]),
               Q= lapply(1:M, function(k) rep(NA)),
               Rho=lapply(1:M, function(k) rep(NA,  N)),
               theta=lapply(1:M, function(k) list()))
  for (k in 1:M){
    init$Q[[k]] = length(unique(init$Z[[k]]))  ##
    theta <- initialTheta(ind.all, dataVec, init$Z[[k]], init$Zmatrix[[k]], sigma0=sigma0, sigma1=sigma1)    # initialisation with Storey
    init$Rho[[k]]= get_Rho_all(theta, init$Z[[k]], dataVec)
    init$theta[[k]]=SetTheta_avecRho(ind.all, dataVec, init$Rho[[k]] , init$Z[[k]], init$Zmatrix[[k]], sigma0, sigma1)
    init$Rho[[k]]= get_Rho_all(init$theta[[k]], init$Z[[k]], dataVec)
  }
  return(init)
}

initialRho_NIG <- function(ind.all, dataVec, listOfZinitiaux, sigma0){

  N=length(dataVec)        # p(p-1)/2
  nbOfZ <- length(listOfZinitiaux$listOfZ)
  M = nbOfZ
  init <- list(Z=lapply(1:M, function(k) listOfZinitiaux$listOfZ[[k]]),
               Zmatrix =lapply(1:M, function(k) listOfZinitiaux$listOfZmatrix[[k]]),
               Q= lapply(1:M, function(k) rep(NA)),
               Rho=lapply(1:M, function(k) rep(NA,  N)),
               theta=lapply(1:M, function(k) list()))
  for (k in 1:M){
    init$Q[[k]] = length(unique(init$Z[[k]]))  ##
    theta <- initialTheta_NIG(ind.all, dataVec, init$Z[[k]], init$Zmatrix[[k]], sigma0)    # initialisation with Storey
    init$Rho[[k]]= get_Rho_all(theta, init$Z[[k]], dataVec)
    init$theta[[k]]=SetTheta_avecRho_NIG(ind.all, dataVec, init$Rho[[k]] , init$Z[[k]], init$Zmatrix[[k]], sigma0)
    init$Rho[[k]]= get_Rho_all(init$theta[[k]], init$Z[[k]], dataVec)
  }
  return(init)
}





