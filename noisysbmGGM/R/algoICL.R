
# --------- Update theta ------------------------------------------------------------------------

# update w and nu when k, l implies g or h. Update from Rho

updateThetakl_avecRho <- function(ind.all, theta, dataVec, Z, Z_matrix, RhoVec, k, l){
  p=length(Z)
  Q=nrow(Z_matrix)
  ind <- convertGroupPair(k,l,Q,directed=FALSE)
  mask <- if (k==l) (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]]) else
    (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]] + Z_matrix[k, ind.all[,2]]*Z_matrix[l, ind.all[,1]] )
  I_kl <- mask * RhoVec
  n_kl = sum(I_kl)
  sum_E_X = sum(I_kl *  dataVec)
  #---nu_kl
  theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl
  #---w_kl
  m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
  theta$w[ind]<- if (m_kl == 0) 0 else  n_kl / m_kl
  return(theta)
}

updateThetakl_avecRho_NIG <- function(ind.all, theta, dataVec, Z, Z_matrix, RhoVec, k, l){
  p=length(Z)
  Q=nrow(Z_matrix)
  ind <- convertGroupPair(k,l,Q,directed=FALSE)
  mask <- if (k==l) (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]]) else
    (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]] + Z_matrix[k, ind.all[,2]]*Z_matrix[l, ind.all[,1]] )
  I_kl <- mask * RhoVec
  n_kl = sum(I_kl)
  sum_E_X = sum(I_kl *  dataVec)
  sum_E_X2= sum(I_kl* dataVec**2)
  
  #---nu_kl
  theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl
  
  #---sigma_kl
  theta$nu[ind,2] <- if (n_kl == 0) 1 else sum_E_X2/n_kl - theta$nu[ind,1]**2
  if(theta$nu[ind,2]<=0){theta$nu[ind,2]=.Machine$double.eps}
  theta$nu[ind,2]=sqrt(theta$nu[ind,2])
  
  #---w_kl
  m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
  theta$w[ind]<- if (m_kl == 0) 0 else  n_kl / m_kl
  return(theta)
}


# update theta from Rho
SetTheta_avecRho <- function(ind.all, dataVec, RhoVec, Z, Z_matrix, sigma0, sigma1){
  p=length(Z)
  Q=nrow(Z_matrix)
  N_Q = Q*(Q+1)/2
  
  theta <- list(nu0=c(0,sigma0))
  theta$pi  = rep(NA,Q)
  for (k in 1:Q) {theta$pi[k]= sum(Z==k)/p}
  theta$nu <- matrix(0, nrow=N_Q, ncol=2, byrow=TRUE)
  sd=rep(sigma1, N_Q)
  theta$nu[,2]=sd
  theta$w <- rep(NA, N_Q)
  
  ind=0
  for (k in 1:Q){
    for (l in k:Q){
      ind=ind+1
      mask <- if (k==l) (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]]) else
        (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]] + Z_matrix[k, ind.all[,2]]*Z_matrix[l, ind.all[,1]] )
      I_kl <- mask * RhoVec
      n_kl = sum(I_kl)
      sum_E_X = sum(I_kl *  dataVec)
      #---nu_kl
      theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl
      #---w_kl
      m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
      theta$w[ind]<- if (m_kl == 0) 0 else  n_kl / m_kl
    }
  }
  return(theta=theta)
}


SetTheta_avecRho_NIG <- function(ind.all, dataVec, RhoVec, Z, Z_matrix, sigma0){
  p=length(Z)
  Q=nrow(Z_matrix)
  N_Q = Q*(Q+1)/2
  
  theta <- list(nu0=c(0,sigma0))
  theta$pi  = rep(NA,Q)
  for (k in 1:Q) {theta$pi[k]= sum(Z==k)/p}
  
  theta$nu <- matrix(0, nrow=N_Q, ncol=2, byrow=TRUE)
  theta$w <- rep(NA, N_Q)
  
  ind=0
  for (k in 1:Q){
    for (l in k:Q){
      ind=ind+1
      mask <- if (k==l) (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]]) else
        (Z_matrix[k, ind.all[,1]]*Z_matrix[l, ind.all[,2]] + Z_matrix[k, ind.all[,2]]*Z_matrix[l, ind.all[,1]] )
      I_kl <- mask * RhoVec
      n_kl = sum(I_kl)
      sum_E_X = sum(I_kl *  dataVec)
      sum_E_X2= sum(I_kl* dataVec**2)
      
      #---nu_kl
      theta$nu[ind,1] <- if (n_kl == 0) 0 else sum_E_X / n_kl
      
      #---sigma_kl
      theta$nu[ind,2] <- if (n_kl == 0) 1 else sum_E_X2/n_kl - theta$nu[ind,1]**2
      if(theta$nu[ind,2]<=0){theta$nu[ind,2]=.Machine$double.eps}
      theta$nu[ind,2]=sqrt(theta$nu[ind,2])
      
      #---w_kl
      m_kl <- if (k==l) 1/2*sum(Z==k)*(sum(Z==k) - 1)  else sum(Z==k)*sum(Z==l)
      theta$w[ind]<- if (m_kl == 0) 0 else  n_kl / m_kl
    }
  }
  return(theta=theta)
}


# ---------   Update Rho and A  ---------------------------------------------------------------------



### Rho_all, A_all
get_Rho_all <- function(theta, Z_all, dataVec){
  p <- length(Z_all)
  Q= length(theta$pi)
  N=p*(p-1)/2
  RhoVec = rep(NA, N)
  for (iStar in 1:(p-1)){
    ind.ij = convertNodePair(iStar,(1:p)[-iStar],p,directed=FALSE)
    data_iStar_j <- dataVec[ind.ij]
    ind.ql = convertGroupPair(Z_all[iStar],Z_all[-iStar],Q,directed=FALSE)
    rhoNumerator <- modelDensity(data_iStar_j, theta$nu[ind.ql,1],theta$nu[ind.ql,2]) * theta$w[ind.ql]
    RhoVec[ind.ij] <- rhoNumerator / (rhoNumerator + modelDensity(data_iStar_j, theta$nu0[1], theta$nu0[2])*(1-theta$w[ind.ql]))
  }
  return(Rho=RhoVec)
}


get_Aall_threshold  <- function(RhoVec, threshold){
  A <- (RhoVec >= threshold)
  return(A=A)
}

get_Aall_qval <-function(RhoVec, theta, Z_all, dataVec, alpha){
  lvaluesVec =1-RhoVec     #vector
  qvaluesVec <- qvaluesNSBM(dataVec, Z_all, theta, lvaluesVec)
  Aestim <- (qvaluesVec < alpha)
  return(Aestim=Aestim)
}



### Rho_Star, A_Star
get_Rho_Star <- function(iStar, qStar, theta, Z_sans_iStar, dataVec){
  p <-length(Z_sans_iStar) +1
  Q= length(theta$pi)
  Rho <-  matrix(NA, nrow=1, ncol=p-1)
  ind.ij = convertNodePair(iStar,(1:p)[-iStar],p,directed=FALSE)
  data_iStar_j <- dataVec[ind.ij]
  ind.ql = convertGroupPair(qStar,Z_sans_iStar,Q,directed=FALSE)
  rhoNumerator <- modelDensity(data_iStar_j, theta$nu[ind.ql,1], theta$nu[ind.ql,2]) * theta$w[ind.ql]
  Rho <- rhoNumerator / (rhoNumerator + modelDensity(data_iStar_j, theta$nu0[1], theta$nu0[2])*(1-theta$w[ind.ql]))
  return(Rho)
}



get_AStar_threshold <- function(iStar, RhoStar, threshold){
  p=length(RhoStar) + 1
  AStar <-  matrix(0, nrow=1, ncol=p)
  AStar[,-iStar]  <- (RhoStar >=threshold)
  # cat("rho=", round(rho,1), "\n")
  return(AStar=AStar)
}




# ---------  Update if swap  ---------------------------------------------------------------------
### if iStar passes from g to h: update of Z, RhoStar, theta then Rho
swapUpdate <- function(iStar, g, h, ind.all, dataVec, Z, Zmatrix,  Rho, theta, threshold){    #iStar passe de g à h
  if(g!=Z[iStar]){
    warning("g must be Z[iStar]")
  }
  p=length(Z)
  Q=nrow(Zmatrix)
  Ztest=Z
  Ztest[iStar] = h
  Ztest_matrix <- Zmatrix
  Ztest_matrix[h,iStar] =1; Ztest_matrix[g,iStar] =0
  
  # case group g is empty
  if (sum(Ztest==g) ==0){
    thetatest <- list(nu0=theta$nu0, pi=theta$pi[-g])
    ind = convertGroupPair(g,1:Q,Q,directed=FALSE)  #numero group pair to be deleted
    thetatest$nu=matrix(theta$nu[-ind,],  ncol=2) #otherwise dim pb when only 1 group remains
    thetatest$w=theta$w[-ind]
    
    Q = Q-1
    Ztest_matrix <- matrix(Ztest_matrix[-g,], ncol=p)
    Ztest[Ztest>g] = Ztest[Ztest>g]-1
    if (h > g){ h = h - 1 }
    thetatest$pi[h] = mean(Ztest==h)   
    
    k = h; for (l in 1:Q){thetatest =  updateThetakl_avecRho(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    #updates (kl) for k=h i.e. when (kl) implies h
    
    Rhotest <-  Rho
    for (i in 1:p){
      if (Ztest[i]==h){
        ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
        data_i_j <- dataVec[ind.ij]
        ind.ql = convertGroupPair(Ztest[i],Ztest[-i],Q,directed=FALSE)
        RhoNumerator <- modelDensity(data_i_j, thetatest$nu[ind.ql,1], thetatest$nu[ind.ql,2]) * thetatest$w[ind.ql]
        Rhotest[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetatest$nu0[1],thetatest$nu0[2])*(1-thetatest$w[ind.ql]))
      }
    }
  }
  
  
  # if g does not empty: Q remains the same
  else{
    thetatest <- list(nu0=theta$nu0, pi=theta$pi)
    thetatest$pi[g] = mean(Ztest==g) ; thetatest$pi[h] = mean(Ztest==h)
    thetatest$nu=theta$nu
    thetatest$w=theta$w
    k = g; for (l in 1:Q){thetatest = updateThetakl_avecRho(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    k = h; for (l in (1:Q)[-g]){thetatest =updateThetakl_avecRho(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    
    p <- length(Z)
    Rhotest <-  Rho
    for (i in 1:p){
      if ((Ztest[i]==g)|(Ztest[i]==h)){
        ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
        data_i_j <- dataVec[ind.ij]
        ind.ql = convertGroupPair(Ztest[i],Ztest[-i],Q,directed=FALSE)
        RhoNumerator <- modelDensity(data_i_j, thetatest$nu[ind.ql,1], thetatest$nu[ind.ql,2]) * thetatest$w[ind.ql]
        Rhotest[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetatest$nu0[1], thetatest$nu0[2])*(1-thetatest$w[ind.ql]))
      }
    }
  }
  return(list(Ztest=Ztest, Ztest_matrix=Ztest_matrix, Qtest=Q, thetatest=thetatest, Rhotest=Rhotest))  # je n'ai pas changer Q en Qtest
  
}
swapUpdate_NIG <- function(iStar, g, h, ind.all, dataVec, Z, Zmatrix,  Rho, theta, threshold){    #iStar passe de g à h
  if(g!=Z[iStar]){
    warning("g must be Z[iStar]")
  }
  p=length(Z)
  Q=nrow(Zmatrix)
  Ztest=Z
  Ztest[iStar] = h
  Ztest_matrix <- Zmatrix
  Ztest_matrix[h,iStar] =1; Ztest_matrix[g,iStar] =0
  
  # case group g is empty
  if (sum(Ztest==g) ==0){
    thetatest <- list(nu0=theta$nu0, pi=theta$pi[-g])
    ind = convertGroupPair(g,1:Q,Q,directed=FALSE)  #numero group pair to be deleted
    thetatest$nu=matrix(theta$nu[-ind,],  ncol=2) #otherwise dim pb when only 1 group remains
    thetatest$w=theta$w[-ind]
    
    Q = Q-1
    Ztest_matrix <- matrix(Ztest_matrix[-g,], ncol=p)
    Ztest[Ztest>g] = Ztest[Ztest>g]-1
    if (h > g){ h = h - 1 }
    thetatest$pi[h] = mean(Ztest==h)   
    
    k = h; for (l in 1:Q){thetatest =  updateThetakl_avecRho_NIG(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    
    Rhotest <-  Rho
    for (i in 1:p){
      if (Ztest[i]==h){
        ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
        data_i_j <- dataVec[ind.ij]
        ind.ql = convertGroupPair(Ztest[i],Ztest[-i],Q,directed=FALSE)
        RhoNumerator <- modelDensity(data_i_j, thetatest$nu[ind.ql,1], thetatest$nu[ind.ql,2]) * thetatest$w[ind.ql]
        Rhotest[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetatest$nu0[1],thetatest$nu0[2])*(1-thetatest$w[ind.ql]))
      }
    }
  }
  
  
  # if g does not empty: Q remains the same
  else{
    thetatest <- list(nu0=theta$nu0, pi=theta$pi)
    thetatest$pi[g] = mean(Ztest==g) ; thetatest$pi[h] = mean(Ztest==h)
    thetatest$nu=theta$nu
    thetatest$w=theta$w
    k = g; for (l in 1:Q){thetatest = updateThetakl_avecRho_NIG(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    k = h; for (l in (1:Q)[-g]){thetatest =updateThetakl_avecRho_NIG(ind.all, thetatest, dataVec, Ztest, Ztest_matrix, Rho, k, l)}
    
    p <- length(Z)
    Rhotest <-  Rho
    for (i in 1:p){
      if ((Ztest[i]==g)|(Ztest[i]==h)){
        ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
        data_i_j <- dataVec[ind.ij]
        ind.ql = convertGroupPair(Ztest[i],Ztest[-i],Q,directed=FALSE)
        RhoNumerator <- modelDensity(data_i_j, thetatest$nu[ind.ql,1], thetatest$nu[ind.ql,2]) * thetatest$w[ind.ql]
        Rhotest[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetatest$nu0[1], thetatest$nu0[2])*(1-thetatest$w[ind.ql]))
      }
    }
    
  }
  
  return(list(Ztest=Ztest, Ztest_matrix=Ztest_matrix, Qtest=Q, thetatest=thetatest, Rhotest=Rhotest))
  
}


#    ---------   iStar : SearchOpti for nodes iStar ---------------------------------------------------------------------
SearchOpti_iStar <- function(iStar, ind.all, dataVec, Z, Zmatrix, theta, Rho, threshold, rho, tau, n0, eta0, zeta0, fast){
  
  p=length(Z)
  Q= length(theta$pi)
  g = Z[iStar]
  listDelta <- rep(NA, Q)
  listDelta[g] = 0
  seqQ=1:Q
  
  A=get_Aall_threshold(Rho, threshold)
  
  # ----------- Calculate delta
  for (h in seqQ[-g]){
    Rho_test = get_Rho_Star(iStar, h, theta, Z[-iStar], dataVec)
    A_test = get_AStar_threshold(iStar, Rho_test, threshold)
    sigma0 = theta$nu0[2]
    sigma1 =theta$nu[1,2]           # all theta$nu[,2] are equals
    listDelta[h] =delta_NSBM(ind.all, dataVec, A, A_test, Z, Zmatrix, iStar, g, h,  rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast)$delta
  }
  
  # -------- Change cluster of iStar if necessary
  h = which.max(listDelta)  # cluster which has the biggest ICL
  #  cat("g",g,"\n")
  #  cat("h",h,"\n")
  
  if (h!=g){
    # cat("iStar", iStar, "change de groupe", "\n")
    swapping = swapUpdate(iStar, g, h, ind.all, dataVec, Z, Zmatrix,  Rho, theta, threshold)
    Z=swapping$Ztest
    Zmatrix=swapping$Ztest_matrix
    Q=swapping$Q
    theta=swapping$thetatest
    Rho=swapping$Rhotest
  }
  return(list(Rho=Rho, Z=Z, Zmatrix=Zmatrix,  Q=Q, theta=theta))
}

SearchOpti_iStar_NIG <- function(iStar, ind.all, dataVec, Z, Zmatrix, theta, Rho, threshold, a,b,c,d, n0, eta0, zeta0, fast){
  
  p=length(Z)
  Q= length(theta$pi)
  g = Z[iStar]
  listDelta <- rep(NA, Q)
  listDelta[g] = 0
  seqQ=1:Q
  
  A=get_Aall_threshold(Rho, threshold)
  
  # ----------- Calculate delta
  for (h in seqQ[-g]){
    Rho_test = get_Rho_Star(iStar, h, theta, Z[-iStar], dataVec)
    A_test = get_AStar_threshold(iStar, Rho_test, threshold)
    sigma0 = theta$nu0[2]
    listDelta[h] =delta_NSBM_NIG(ind.all, dataVec, A, A_test, Z, Zmatrix, iStar, g, h, a, b, c, d, n0, eta0, zeta0, sigma0, fast)$delta
  }
  
  # -------- Change cluster of iStar if necessary
  h = which.max(listDelta)  # cluster which has the biggest ICL
  #  cat("g",g,"\n")
  #  cat("h",h,"\n")
  
  if (h!=g){
    # cat("iStar", iStar, "change de groupe", "\n")
    swapping = swapUpdate_NIG(iStar, g, h, ind.all, dataVec, Z, Zmatrix,  Rho, theta, threshold)
    Z=swapping$Ztest
    Zmatrix=swapping$Ztest_matrix
    Q=swapping$Q
    theta=swapping$thetatest
    Rho=swapping$Rhotest
  }
  return(list(Rho=Rho, Z=Z, Zmatrix=Zmatrix,  Q=Q, theta=theta))
}




#    ---------   SearchOpti "for all nodes and an initialization" -----------------------------------------------------------
SearchOpti <- function(init, dataVec, ind.all, threshold, Nbrepet, rho, tau, n0, eta0, zeta0, fast,verbatim=TRUE){
  
  #------- init
  Z=init$Z
  Zmatrix=init$Zmatrix
  Rho=init$Rho
  theta=init$theta
  Q=length(theta$pi)
  
  
  # -------------------------algo for all nodes
  p=length(Z)
  for (repet in 1:Nbrepet){
    if(verbatim){cat("-----repet", repet, "\n")}
    Nodes = 1:p
    iStarSauv = NULL  #
    while ((length(Nodes) >1)  & (Q > 1)){
      iStar=sample(Nodes, 1)
      iStarSauv=c(iStarSauv, iStar)
      
      res = SearchOpti_iStar(iStar, ind.all, dataVec, Z,  Zmatrix, theta, Rho, threshold, rho, tau, n0, eta0, zeta0, fast)
      Rho=res$Rho
      Z=res$Z
      
      Zmatrix=res$Zmatrix
      Q=res$Q
      theta=res$theta
      Nodes=setdiff(Nodes, iStar)
    }
    
    if ((length(Nodes) ==1)  & (Q > 1)){
      iStar=Nodes
      #cat("iStar=", iStar, "\n---")
      res = SearchOpti_iStar(iStar, ind.all, dataVec, Z,  Zmatrix, theta, Rho, threshold, rho, tau, n0, eta0, zeta0, fast)
      Rho=res$Rho
      Z=res$Z
      Zmatrix=res$Zmatrix
      Q=res$Q
      theta=res$theta
      Nodes=setdiff(Nodes, iStar)
      
    }
  }
  
  
  
  # --------------- keep this initialisation?
  
  A= get_Aall_threshold(Rho, threshold)
  sigma0=theta$nu0[2]
  sigma1=theta$nu[1,2]    #all theta$nu[,2] are equals
  calculICL  = evalICL(ind.all, dataVec, Z, Zmatrix, Q, A, rho, tau,  n0, eta0, zeta0, sigma0, sigma1)
  ICL=calculICL$ICL_exact_NSBM
  currentSolution =list(Rho=Rho, A=A, Z=Z, Zmatrix=Zmatrix, Q=Q, theta=theta, ICL=ICL)
  
  
  return(currentSolution)
}



SearchOpti_NIG <- function(init, dataVec, ind.all, threshold, Nbrepet, a, b, c , d, n0, eta0, zeta0, fast,verbatim=TRUE){
  
  #------- init
  Z=init$Z
  Zmatrix=init$Zmatrix
  Rho=init$Rho
  theta=init$theta
  Q=length(theta$pi)
  
  
  # -------------------------algo for all nodes
  p=length(Z)
  for (repet in 1:Nbrepet){
    if(verbatim){cat("-----repet", repet, "\n")}
    Nodes = 1:p
    iStarSauv = NULL  #
    while ((length(Nodes) >1)  & (Q > 1)){
      #  for (iStar in 1:n){
      iStar=sample(Nodes, 1)
      iStarSauv=c(iStarSauv, iStar)
      
      res = SearchOpti_iStar_NIG(iStar, ind.all, dataVec, Z,  Zmatrix, theta, Rho, threshold, a, b, c, d, n0, eta0, zeta0, fast)
      Rho=res$Rho
      Z=res$Z
      
      Zmatrix=res$Zmatrix
      Q=res$Q
      theta=res$theta
      Nodes=setdiff(Nodes, iStar)
    }
    
    if ((length(Nodes) ==1)  & (Q > 1)){
      iStar=Nodes
      #cat("iStar=", iStar, "\n---")
      res = SearchOpti_iStar_NIG(iStar, ind.all, dataVec, Z,  Zmatrix, theta, Rho, threshold,  a,b,c,d, n0, eta0, zeta0, fast)
      Rho=res$Rho
      Z=res$Z
      Zmatrix=res$Zmatrix
      Q=res$Q
      theta=res$theta
      Nodes=setdiff(Nodes, iStar)
      
    }
  }
  
  # --------------- keep this initialisation?
  A= get_Aall_threshold(Rho, threshold)
  sigma0=theta$nu0[2]
  calculICL  = evalICL_NIG(ind.all, dataVec, Z, Zmatrix, Q, A, a,b,c,d,  n0, eta0, zeta0, sigma0)
  ICL=calculICL$ICL_exact_NSBM
  currentSolution =list(Rho=Rho, A=A, Z=Z, Zmatrix=Zmatrix, Q=Q, theta=theta, ICL=ICL)
  
  return(currentSolution)
}


SearchOpti_parallel <- function(s, ListOfInit, dataMatrix, ind.all, threshold, Nbrepet, rho, tau, n0, eta0, zeta0, fast,verbatim=TRUE){
  if(verbatim){cat("s=",s,"\n---------------------------------------------------------------------")}
  dataVec=ListOfInit$dataVec
  currentInitialPoint <-  list(Z=ListOfInit$init$Z[[s]], Zmatrix=ListOfInit$init$Zmatrix[[s]], Rho=ListOfInit$init$Rho[[s]], theta=ListOfInit$init$theta[[s]])
  currentSolution <- SearchOpti(currentInitialPoint, dataVec, ind.all, threshold,  Nbrepet,  rho, tau, n0, eta0, zeta0, fast,verbatim)
  currentSolution$sBest <- s
  return(currentSolution)
}

SearchOpti_parallel_NIG <- function(s, ListOfInit, dataMatrix, ind.all, threshold, Nbrepet, a, b, c, d, n0, eta0, zeta0, fast,verbatim=TRUE){
  if(verbatim){cat("s=",s,"\n---------------------------------------------------------------------")}
  dataVec=ListOfInit$dataVec
  currentInitialPoint <-  list(Z=ListOfInit$init$Z[[s]], Zmatrix=ListOfInit$init$Zmatrix[[s]], Rho=ListOfInit$init$Rho[[s]], theta=ListOfInit$init$theta[[s]])
  currentSolution <- SearchOpti_NIG(currentInitialPoint, dataVec, ind.all, threshold,  Nbrepet, a, b, c, d, n0, eta0, zeta0, fast,verbatim)
  currentSolution$sBest <- s
  return(currentSolution)
}



# -----------   main : SearchOpti  :  for several initialisations ---------------------------------------------------------------------
mainSearchOpti <- function(dataMatrix, Qup, threshold, Nbrepet, nbCores, nbOfZ, percentageOfPerturbation, rho, tau, n0, eta0, zeta0, sigma0, sigma1, fast,verbatim=TRUE){
  p=nrow(dataMatrix)
  ind.all=listNodePairs(p, directed=FALSE)
  
  Q=Qup
  ListOfInit <- initialPoints(Q, ind.all, dataMatrix, nbOfZ, percentageOfPerturbation, threshold, sigma0, sigma1)
  M=length(ListOfInit$init$Z)
  BestICL= -Inf
  
  if (Sys.info()["sysname"]=="Windows"){nbCores <- 1}
  else { nbCores <- if (nbCores>1) min(c(round(nbCores), parallel::detectCores())) else 1}
  doParallelComputing <- (nbCores>1)
  
  if(doParallelComputing){
    ListOfSolutions <- parallel::mclapply(1:M, function(k){
      SearchOpti_parallel(k, ListOfInit, dataMatrix, ind.all, threshold, Nbrepet, rho, tau, n0, eta0, zeta0, fast,verbatim)},  mc.cores=nbCores)    
    ListOfICL <- lapply(ListOfSolutions, function(solutionThisRun) solutionThisRun$ICL)
    BestSolution <- ListOfSolutions[[which.max(ListOfICL)]]
    if(verbatim){cat("s=",which.max(ListOfICL), "\n")}
    }else{
    for (s in 1:M){
      if(verbatim){cat("s",s, "--------------------\n")}
      
      currentInitialPoint <-  list(Z=ListOfInit$init$Z[[s]], Zmatrix=ListOfInit$init$Zmatrix[[s]], Rho=ListOfInit$init$Rho[[s]], theta=ListOfInit$init$theta[[s]])
      dataVec=ListOfInit$dataVec
      currentSolution <- SearchOpti(currentInitialPoint, dataVec, ind.all, threshold, Nbrepet, rho, tau, n0, eta0, zeta0, fast,verbatim)
      currentSolution$sBest <- s
      if (currentSolution$ICL > BestICL){
        if(verbatim){cat("s better than before \n" )
          cat("sBest=", s, "\n")}
        BestSolution <-  currentSolution
        BestICL=currentSolution$ICL
      }
    }
  }
  return(BestSolution=BestSolution)
  
}


mainSearchOpti_NIG <- function(dataMatrix, Qup, threshold, Nbrepet, nbCores, nbOfZ, percentageOfPerturbation,  a,b,c,d, n0, eta0, zeta0, sigma0, fast,verbatim=TRUE){  p=nrow(dataMatrix)
  ind.all=listNodePairs(p, directed=FALSE)
  
  Q=Qup
  ListOfInit <- initialPoints_NIG(Q,  ind.all, dataMatrix, nbOfZ, percentageOfPerturbation, threshold, sigma0)
  M=length(ListOfInit$init$Z)
  BestICL= -Inf
  
  if (Sys.info()["sysname"]=="Windows"){nbCores <- 1}
  else { nbCores <- if (nbCores>1) min(c(round(nbCores), parallel::detectCores())) else 1}
  doParallelComputing <- (nbCores>1)
  
  if(doParallelComputing){
    ListOfSolutions <- parallel::mclapply(1:M, function(k){
      SearchOpti_parallel_NIG(k, ListOfInit, dataMatrix, ind.all, threshold, Nbrepet, a,b,c,d, n0, eta0, zeta0, fast,verbatim)},   mc.cores=nbCores)
    
    ListOfICL <- lapply(ListOfSolutions, function(solutionThisRun) solutionThisRun$ICL)
    BestSolution <- ListOfSolutions[[which.max(ListOfICL)]]
    if(verbatim){cat("s=",which.max(ListOfICL), "\n")}
    }else{
    for (s in 1:M){
      if(verbatim){cat("s",s, "--------------------\n")}
      currentInitialPoint <-  list(Z=ListOfInit$init$Z[[s]], Zmatrix=ListOfInit$init$Zmatrix[[s]], Rho=ListOfInit$init$Rho[[s]], theta=ListOfInit$init$theta[[s]])
      dataVec=ListOfInit$dataVec
      currentSolution <- SearchOpti_NIG(currentInitialPoint, dataVec, ind.all, threshold, Nbrepet, a,b,c,d, n0, eta0, zeta0, fast,verbatim)
      currentSolution$sBest <- s
      if (currentSolution$ICL > BestICL){
        if(verbatim){cat("s better than before \n" )
          cat("sBest=", s, "\n")}
        BestSolution <-  currentSolution
        BestICL=currentSolution$ICL
      }
    }
  }
  return(BestSolution=BestSolution)
}



# --------------Graph inference with given alpha ---------------


InferGraph <- function(Rho, theta, Z, dataVec, alpha){
  p=length(Z)
  A = matrix(0, p,p)
  Avec= get_Aall_qval(Rho, theta, Z, dataVec, alpha)
  A[lower.tri(A)]=Avec
  A <- A + t(A)
  return(A=A)
}

