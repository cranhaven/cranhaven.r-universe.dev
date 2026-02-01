partZ <- function(dataVec, I_kl, m_kl, rho, tau, n0, eta0, zeta0, sigma1){
  n_kl = sum(I_kl)
  sum_E_X = sum(I_kl *  dataVec)
  sum_E_X2 = sum(I_kl *  dataVec**2)
  sum_Ikl_4 = sum_E_X2 -2*rho*sum_E_X +(rho^2)*n_kl
  
  if (n_kl == 0){ E_X = 0 ; E_X2 = 0 }
  else{  E_X = sum_E_X / n_kl ;  E_X2 = sum_E_X2 / n_kl }
  eta_kl = eta0 + n_kl
  zeta_kl = zeta0 + m_kl - n_kl
  terme3 =  log(1+tau**2*n_kl/sigma1**2)
  terme4=  (1/(sigma1**2+tau**2*n_kl))*sum_Ikl_4
  terme5=  ((n_kl**2)*(E_X2 - E_X**2))/(sigma1**4+sigma1**2*tau**2*n_kl)
  terme6= log(sigma1)*n_kl
  terme2_SBM = lbeta(eta_kl, zeta_kl) - lbeta(eta0, zeta0)
  return (list(t3=terme3, t4=terme4, t5=terme5, t6=terme6, terme2_SBM=terme2_SBM))
}

partZ_NIG <- function(dataVec, I_kl, m_kl,a,b,c,d, n0, eta0, zeta0){
  n_kl = sum(I_kl)
  sum_E_X = sum(I_kl *  dataVec)
  sum_E_X2 = sum(I_kl *  dataVec**2)
  
  if (n_kl == 0){ E_X = 0 ; E_X2 = 0 }
  else{  E_X = sum_E_X / n_kl ;  E_X2 = sum_E_X2 / n_kl }
  eta_kl = eta0 + n_kl
  zeta_kl = zeta0 + m_kl - n_kl
  b_x=b+n_kl
  c_x=c+0.5*n_kl
  d_x=d_post_kl(a,b,c,d,E_X,E_X2, n_kl)
  
  terme_NSBM=terme_log(b_x,c_x,d_x)
  
  terme2_SBM = lbeta(eta_kl, zeta_kl) - lbeta(eta0, zeta0)
  return (list(terme_NSBM=terme_NSBM, terme2_SBM=terme2_SBM))
}

#Update de Z,Q, theta, Rho, A if we merge g et h
mergeUpdate <- function(g, h, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold){    #merging g and h which becomes g: group h disappears    #Q =Q before merge
  p=length(Z)
  
  Qmerge = Q-1
  Zmerge=Z
  Zmerge[which(Zmerge==h)]=g
  Zmerge[Zmerge>h] = Zmerge[Zmerge>h]-1
  
  Zmerge_matrix <- Zmatrix
  Zmerge_matrix[g,which(Z==h)] =1
  Zmerge_matrix <- matrix(Zmerge_matrix[-h,], ncol=p)
  
  thetamerge <- list(nu0=theta$nu0)
  thetamerge$pi=theta$pi[-h]
  thetamerge$pi[g] = mean(Zmerge==g)
  
  ind = convertGroupPair(h,1:Q,Q,directed=FALSE)  #number of group pair to be deleted
  thetamerge$nu=matrix(theta$nu[-ind,],  ncol=2) #otherwise dim pb when only 1 group remains
  thetamerge$w=theta$w[-ind]
  k = g; for (l in 1:Qmerge){thetamerge =  updateThetakl_avecRho(ind.all, thetamerge, dataVec, Zmerge, Zmerge_matrix, Rho, k, l)} 
  
  Rhomerge <-  Rho
  for (i in 1:p){
    if (Zmerge[i]==g){
      ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
      data_i_j <- dataVec[ind.ij]
      ind.ql = convertGroupPair(Zmerge[i],Zmerge[-i],Qmerge,directed=FALSE)
      RhoNumerator <- modelDensity(data_i_j, thetamerge$nu[ind.ql,1],thetamerge$nu[ind.ql,2]) * thetamerge$w[ind.ql]
      Rhomerge[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetamerge$nu0[1], thetamerge$nu0[2])*(1-thetamerge$w[ind.ql]))
    }
  }
  Amerge <- get_Aall_threshold(Rhomerge, threshold)
  return(list(Zmerge=Zmerge, Zmerge_matrix=Zmerge_matrix, Qmerge=Qmerge, thetamerge=thetamerge, Rhomerge=Rhomerge, Amerge=Amerge))
}

mergeUpdate_NIG <- function(g, h, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold){   #merging g and h which becomes g: group h disappears    #Q =Q before merge
  p=length(Z)
  
  Qmerge = Q-1
  Zmerge=Z
  Zmerge[which(Zmerge==h)]=g
  Zmerge[Zmerge>h] = Zmerge[Zmerge>h]-1
  
  Zmerge_matrix <- Zmatrix
  Zmerge_matrix[g,which(Z==h)] =1
  Zmerge_matrix <- matrix(Zmerge_matrix[-h,], ncol=p)
  
  thetamerge <- list(nu0=theta$nu0)
  thetamerge$pi=theta$pi[-h]
  thetamerge$pi[g] = mean(Zmerge==g)
  
  ind = convertGroupPair(h,1:Q,Q,directed=FALSE)  #number of group pair to be deleted
  thetamerge$nu=matrix(theta$nu[-ind,],  ncol=2) #otherwise dim pb when only 1 group remains
  thetamerge$w=theta$w[-ind]
  k = g; for (l in 1:Qmerge){thetamerge =  updateThetakl_avecRho_NIG(ind.all, thetamerge, dataVec, Zmerge, Zmerge_matrix, Rho, k, l)} 
  
  Rhomerge <-  Rho
  for (i in 1:p){
    if (Zmerge[i]==g){
      ind.ij = convertNodePair(i,(1:p)[-i],p,directed=FALSE)
      data_i_j <- dataVec[ind.ij]
      ind.ql = convertGroupPair(Zmerge[i],Zmerge[-i],Qmerge,directed=FALSE)
      RhoNumerator <- modelDensity(data_i_j, thetamerge$nu[ind.ql,1],thetamerge$nu[ind.ql,2]) * thetamerge$w[ind.ql]
      Rhomerge[ind.ij] <- RhoNumerator / (RhoNumerator + modelDensity(data_i_j, thetamerge$nu0[1],thetamerge$nu0[2])*(1-thetamerge$w[ind.ql]))
    }
  }
  Amerge <- get_Aall_threshold(Rhomerge, threshold)
  return(list(Zmerge=Zmerge, Zmerge_matrix=Zmerge_matrix, Qmerge=Qmerge, thetamerge=thetamerge, Rhomerge=Rhomerge, Amerge=Amerge))
}



delta_NSBM_Merge_Nondirige <- function(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, g, h, rho, tau, n0, eta0, zeta0, sigma0, sigma1){
  p=length(Z)
  ii =which(Z==h | Z==g)    #which(Zmerge==g)
  ind = c(outer(ii,(1:p),convertNodePair, p,directed=FALSE))
  ind = unique(ind[!is.na(ind)])   #index of pairs (ij) with i or j in the new group g
  terme2 = - sum((((0.5/(sigma0**2))*dataVec[ind]^2+log(sigma0))* (A[ind] -Amerge[ind])))
  
  terme3_merge = 0
  terme3 = 0
  terme4_merge = 0
  terme4 = 0
  terme5_merge = 0
  terme5 = 0
  terme6_merge = 0
  terme6 = 0
  terme2_SBM_merge = 0
  terme2_SBM = 0
  
  n_g =  n0 + sum(Z==g)
  n_h = n0 + sum(Z==h)
  
  mask_gg <- (Zmatrix[g, ind.all[,1]]*Zmatrix[g, ind.all[,2]])
  mask_hh <- (Zmatrix[h, ind.all[,1]]*Zmatrix[h, ind.all[,2]])
  mask_gh <- (Zmatrix[g, ind.all[,1]]*Zmatrix[h, ind.all[,2]] + Zmatrix[g, ind.all[,2]]*Zmatrix[h, ind.all[,1]] )
  mask_gUh  <-  mask_gg + mask_hh + mask_gh
  I_gg = mask_gg * A
  I_gh = mask_gh * A
  I_hh = mask_hh * A
  I_gUh_gUh = mask_gUh * Amerge
  
  
  m_gg = 1/2*(n_g-n0)*(n_g-n0-1)
  m_hh = 1/2*(n_h-n0)*(n_h-n0-1)
  m_gh = (n_g-n0)*(n_h-n0)
  m_gUh_gUh = 1/2*(n_g -n0 + n_h - n0)*(n_g -n0 + n_h - n0 -1)
  
  delta=partZ(dataVec, I_gUh_gUh, m_gUh_gUh, rho, tau, n0, eta0, zeta0, sigma1)
  terme3_merge = terme3_merge + delta$t3
  terme4_merge = terme4_merge + delta$t4
  terme5_merge = terme5_merge + delta$t5
  terme6_merge = terme6_merge + delta$t6
  terme2_SBM_merge = terme2_SBM_merge + delta$terme2_SBM
  
  delta = partZ(dataVec, I_gg, m_gg, rho, tau, n0, eta0, zeta0, sigma1)
  terme3 = terme3 + delta$t3
  terme4 = terme4 + delta$t4
  terme5 = terme5 + delta$t5
  terme6 = terme6 + delta$t6
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  delta = partZ(dataVec, I_hh, m_hh, rho, tau, n0, eta0, zeta0, sigma1)
  terme3 = terme3 + delta$t3
  terme4 = terme4 + delta$t4
  terme5 = terme5 + delta$t5
  terme6 = terme6 + delta$t6
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  delta = partZ(dataVec, I_gh, m_gh, rho, tau, n0, eta0, zeta0, sigma1)
  terme3 = terme3 + delta$t3
  terme4 = terme4 + delta$t4
  terme5 = terme5 + delta$t5
  terme6 = terme6 + delta$t6
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  
  if (Q>2){
    
    for (l in (1:Q)[-c(g,h)]){
      n_l =  n0 + sum(Z==l)
      m_gl = (n_g-n0)*(n_l-n0)   #=m_lg
      m_hl = (n_h-n0)*(n_l-n0)   #=m_lh
      
      mask_gl <- (Zmatrix[g, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[g, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )
      I_gl <- mask_gl * A
      delta = partZ(dataVec, I_gl, m_gl, rho, tau, n0, eta0, zeta0, sigma1)
      terme3 = terme3 + delta$t3
      terme4 = terme4 + delta$t4
      terme5 = terme5 + delta$t5
      terme6 = terme6 + delta$t6
      terme2_SBM = terme2_SBM + delta$terme2_SBM
      
      mask_hl <- (Zmatrix[h, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[h, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )
      I_hl = mask_hl*A
      delta = partZ(dataVec, I_hl, m_hl, rho, tau, n0, eta0, zeta0, sigma1)
      terme3 = terme3 + delta$t3
      terme4 = terme4 + delta$t4
      terme5 = terme5 + delta$t5
      terme6 = terme6 + delta$t6
      terme2_SBM = terme2_SBM + delta$terme2_SBM
      
      m_gUh_l = (n_g -n0 + n_h - n0)*(n_l-n0)
      mask_gUh_l <- mask_gl + mask_hl
      I_gUh_l = mask_gUh_l * Amerge
      delta=partZ(dataVec, I_gUh_l, m_gUh_l, rho, tau, n0, eta0, zeta0, sigma1)
      terme3_merge = terme3_merge + delta$t3
      terme4_merge = terme4_merge + delta$t4
      terme5_merge = terme5_merge + delta$t5
      terme6_merge = terme6_merge + delta$t6
      terme2_SBM_merge = terme2_SBM_merge + delta$terme2_SBM
    }
  }
  
  terme3 = -0.5* (terme3_merge - terme3)
  terme4 = -0.5* (terme4_merge - terme4)
  terme5 = -0.5 * tau**2 * (terme5_merge - terme5)
  terme6 = -(terme6_merge - terme6)
  
  delta_orange = terme3+terme4+terme5+terme6+ terme2
  terme1_SBM = lgamma(n0) +lgamma((Q-1)*n0) +lgamma(Q*n0+p) +lgamma(n_g+n_h-n0) - lgamma(Q*n0) -lgamma((Q-1)*n0+p) -lgamma(n_g) -lgamma(n_h)
  
  delta_SBM= terme1_SBM +terme2_SBM_merge - terme2_SBM
  
  delta=delta_orange+delta_SBM
  return(list(delta=delta, delta_orange=delta_orange, delta_SBM=delta_SBM, terme2=terme2))
  
}

delta_NSBM_Merge_Nondirige_NIG <- function(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, g, h,a,b,c,d, n0, eta0, zeta0, sigma0){
  p=length(Z)
  ii =which(Z==h | Z==g)    #which(Zmerge==g)
  ind = c(outer(ii,(1:p),convertNodePair, p,directed=FALSE))
  ind = unique(ind[!is.na(ind)])   #index of pairs (ij) with i or j in the new group g
  terme2 = - sum((((0.5/(sigma0**2))*dataVec[ind]^2+log(sigma0))* (A[ind] -Amerge[ind])))
  
  termeNSBM_merge = 0
  termeNSBM = 0
  terme2_SBM_merge = 0
  terme2_SBM = 0
  
  n_g =  n0 + sum(Z==g)
  n_h = n0 + sum(Z==h)
  
  mask_gg <- (Zmatrix[g, ind.all[,1]]*Zmatrix[g, ind.all[,2]])
  mask_hh <- (Zmatrix[h, ind.all[,1]]*Zmatrix[h, ind.all[,2]])
  mask_gh <- (Zmatrix[g, ind.all[,1]]*Zmatrix[h, ind.all[,2]] + Zmatrix[g, ind.all[,2]]*Zmatrix[h, ind.all[,1]] )
  mask_gUh  <-  mask_gg + mask_hh + mask_gh
  I_gg = mask_gg * A
  I_gh = mask_gh * A
  I_hh = mask_hh * A
  I_gUh_gUh = mask_gUh * Amerge
  
  
  m_gg = 1/2*(n_g-n0)*(n_g-n0-1)
  m_hh = 1/2*(n_h-n0)*(n_h-n0-1)
  m_gh = (n_g-n0)*(n_h-n0)
  m_gUh_gUh = 1/2*(n_g -n0 + n_h - n0)*(n_g -n0 + n_h - n0 -1)
  
  delta=partZ_NIG(dataVec, I_gUh_gUh, m_gUh_gUh,a,b,c,d, n0, eta0, zeta0)
  termeNSBM_merge = termeNSBM_merge + delta$terme_NSBM
  terme2_SBM_merge = terme2_SBM_merge + delta$terme2_SBM
  
  delta = partZ_NIG(dataVec, I_gg, m_gg,a,b,c,d, n0, eta0, zeta0)
  termeNSBM = termeNSBM + delta$terme_NSBM
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  delta = partZ_NIG(dataVec, I_hh, m_hh,a,b,c,d, n0, eta0, zeta0)
  termeNSBM = termeNSBM + delta$terme_NSBM
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  delta = partZ_NIG(dataVec, I_gh, m_gh,a,b,c,d, n0, eta0, zeta0)
  termeNSBM = termeNSBM + delta$terme_NSBM
  terme2_SBM = terme2_SBM + delta$terme2_SBM
  
  
  if (Q>2){
    
    for (l in (1:Q)[-c(g,h)]){
      n_l =  n0 + sum(Z==l)
      m_gl = (n_g-n0)*(n_l-n0)   #=m_lg
      m_hl = (n_h-n0)*(n_l-n0)   #=m_lh
      
      mask_gl <- (Zmatrix[g, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[g, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )
      I_gl <- mask_gl * A
      delta = partZ_NIG(dataVec, I_gl, m_gl,a,b,c,d, n0, eta0, zeta0)
      termeNSBM = termeNSBM + delta$terme_NSBM
      terme2_SBM = terme2_SBM + delta$terme2_SBM
      
      mask_hl <- (Zmatrix[h, ind.all[,1]]*Zmatrix[l, ind.all[,2]] + Zmatrix[h, ind.all[,2]]*Zmatrix[l, ind.all[,1]] )
      I_hl = mask_hl*A
      delta = partZ_NIG(dataVec, I_hl, m_hl,a,b,c,d, n0, eta0, zeta0)
      termeNSBM = termeNSBM + delta$terme_NSBM
      terme2_SBM = terme2_SBM + delta$terme2_SBM
      
      m_gUh_l = (n_g -n0 + n_h - n0)*(n_l-n0)
      mask_gUh_l <- mask_gl + mask_hl
      I_gUh_l = mask_gUh_l * Amerge
      delta=partZ_NIG(dataVec, I_gUh_l, m_gUh_l,a,b,c,d, n0, eta0, zeta0)
      termeNSBM_merge = termeNSBM_merge + delta$terme_NSBM
      terme2_SBM_merge = terme2_SBM_merge + delta$terme2_SBM
    }
  }
  
  terme_NSBM = termeNSBM_merge - termeNSBM
  
  
  delta_orange = terme_NSBM   + terme2 + Q*terme_log(b,c,d)
  terme1_SBM = lgamma(n0) +lgamma((Q-1)*n0) +lgamma(Q*n0+p) +lgamma(n_g+n_h-n0) - lgamma(Q*n0) -lgamma((Q-1)*n0+p) -lgamma(n_g) -lgamma(n_h)
  
  
  delta_SBM= terme1_SBM +terme2_SBM_merge - terme2_SBM
  
  delta=delta_orange+delta_SBM
  return(list(delta=delta, delta_orange=delta_orange, delta_SBM=delta_SBM, terme2=terme2))
  
}


Merge_Nondirige <- function(dataVec, Z, Zmatrix, Q, Rho, A, theta, threshold, rho, tau, n0, eta0, zeta0){
  if(!(Q>1)){
    stop("Q must be a greater than 1")
  }
  p <- length(Z)
  ind.all <- listNodePairs(p, directed=FALSE)
  
  SortieMerge = matrix(0, ncol=Q*(Q-1)/2, nrow=6)
  rownames(SortieMerge)=c("g","h","delta","orange","SBM", "terme2")
  count=0
  for (gg in 1:(Q-1)){
    for (hh in (gg+1):Q){
      count=count+1
      mergeTest = mergeUpdate(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
      Amerge=mergeTest$Amerge
      sigma0=theta$nu0[2]
      sigma1=theta$nu[1,2]   # all theta$nu[,2] are equals
      deltaMerge <- delta_NSBM_Merge_Nondirige(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, gg, hh, rho, tau, n0, eta0, zeta0, sigma0, sigma1)    
      SortieMerge[,count]=c(gg,hh, deltaMerge$delta, deltaMerge$delta_orange, deltaMerge$delta_SBM, deltaMerge$terme2)
    }
  }
  ord=order(SortieMerge["delta",],decreasing=TRUE)
  SortieMerge = as.matrix(SortieMerge[,ord])
  
  while ((SortieMerge["delta",1] >0)&&(Q>1)){
    gg=SortieMerge[1,1]
    hh=SortieMerge[2,1]
    merging = mergeUpdate(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
    Z=merging$Zmerge
    Zmatrix=merging$Zmerge_matrix
    Q=merging$Qmerge
    theta=merging$thetamerge
    Rho=merging$Rhomerge
    A=merging$Amerge
    
    if (Q>1){
      SortieMerge = matrix(0, ncol=Q*(Q-1)/2, nrow=6)
      rownames(SortieMerge)=c("g","h","delta","orange","SBM", "terme2")
      count=0
      for (gg in 1:(Q-1)){
        for (hh in (gg+1):Q){
          count=count+1
          mergeTest = mergeUpdate(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
          Amerge=mergeTest$Amerge
          sigma0=theta$nu0[2]
          sigma1=theta$nu[1,2]   # all theta$nu[,2] are equals
          deltaMerge <- delta_NSBM_Merge_Nondirige(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, gg, hh, rho, tau, n0, eta0, zeta0, sigma0, sigma1)     #besoin de Q en entree ?????
          SortieMerge[,count]=c(gg,hh, deltaMerge$delta, deltaMerge$delta_orange, deltaMerge$delta_SBM, deltaMerge$terme2)
        }
      }
      ord=order(SortieMerge["delta",],decreasing=TRUE)
      SortieMerge = as.matrix(SortieMerge[,ord])
    }
  }
  return(list(Zmerge=Z, Zmatrixmerge= Zmatrix, Qmerge=Q, thetamerge=theta, Rhomerge=Rho, Amerge=A, SortieMerge=SortieMerge))
}

Merge_Nondirige_NIG <- function(dataVec, Z, Zmatrix, Q, Rho, A, theta, threshold, a,b,c,d, n0, eta0, zeta0){   #Q=Qvant merge ???
  if(!(Q>1)){
    stop("Q must be a greater than 1")
  }
  p <- length(Z)
  ind.all <- listNodePairs(p, directed=FALSE)
  
  SortieMerge = matrix(0, ncol=Q*(Q-1)/2, nrow=6)
  rownames(SortieMerge)=c("g","h","delta","orange","SBM", "terme2")
  count=0
  for (gg in 1:(Q-1)){
    for (hh in (gg+1):Q){
      count=count+1
      mergeTest = mergeUpdate_NIG(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
      Amerge=mergeTest$Amerge
      sigma0=theta$nu0[2]
      deltaMerge <- delta_NSBM_Merge_Nondirige_NIG(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, gg, hh,a,b,c,d, n0, eta0, zeta0, sigma0)    
      SortieMerge[,count]=c(gg,hh, deltaMerge$delta, deltaMerge$delta_orange, deltaMerge$delta_SBM, deltaMerge$terme2)
    }
  }
  ord=order(SortieMerge["delta",],decreasing=TRUE)
  SortieMerge = as.matrix(SortieMerge[,ord])
  
  while ((SortieMerge["delta",1] >0)&&(Q>1)){
    gg=SortieMerge[1,1]
    hh=SortieMerge[2,1]
    merging = mergeUpdate_NIG(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
    Z=merging$Zmerge
    Zmatrix=merging$Zmerge_matrix
    Q=merging$Qmerge
    theta=merging$thetamerge
    Rho=merging$Rhomerge
    A=merging$Amerge
    
    if (Q>1){
      SortieMerge = matrix(0, ncol=Q*(Q-1)/2, nrow=6)
      rownames(SortieMerge)=c("g","h","delta","orange","SBM", "terme2")
      count=0
      for (gg in 1:(Q-1)){
        for (hh in (gg+1):Q){
          count=count+1
          mergeTest = mergeUpdate_NIG(gg, hh, ind.all, dataVec, Z, Zmatrix, Q, Rho, theta, threshold)
          Amerge=mergeTest$Amerge
          sigma0=theta$nu0[2]
          deltaMerge <- delta_NSBM_Merge_Nondirige_NIG(ind.all, dataVec, A, Amerge, Z, Zmatrix, Q, gg, hh, a,b,c,d, n0, eta0, zeta0, sigma0)     #besoin de Q en entree ?????
          
          SortieMerge[,count]=c(gg,hh, deltaMerge$delta, deltaMerge$delta_orange, deltaMerge$delta_SBM, deltaMerge$terme2)
        }
      }
      ord=order(SortieMerge["delta",],decreasing=TRUE)
      SortieMerge = as.matrix(SortieMerge[,ord])
    }
  }
  return(list(Zmerge=Z, Zmatrixmerge= Zmatrix, Qmerge=Q, thetamerge=theta, Rhomerge=Rho, Amerge=A, SortieMerge=SortieMerge))
}

