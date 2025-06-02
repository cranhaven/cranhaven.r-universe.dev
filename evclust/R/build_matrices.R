build_matrices<-function(F){
  
  # Function called by mass_from_P_new and pairwise_mass
  
  nbFoc<-nrow(F)
  C<-matrix(0,nbFoc,nbFoc)  # the matrix used to compute the degrees of conflict
  for(i in 1:nbFoc){
    for(j in 1:nbFoc){
      C[i,j] <- 1-max(pmin(F[i,],F[j,]))
    }
  }
  if(max(F[1,])>0) E<-matrix(0,nbFoc,nbFoc) else{ # Matrix to compute m_ij(emptyset)
    E<-matrix(0,nbFoc-1,nbFoc-1)
    E<-cbind(rep(1,nbFoc-1),E)
    E<-rbind(rep(1,nbFoc),E)
  }
  singleton<-(rowSums(F)==1) # matrix to compute m_ij(S)
  S<-diag(singleton)
  return(list(C=C,E=E,S=S))
}
