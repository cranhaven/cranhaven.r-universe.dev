setDistances<-function(x,F,g,m,alpha,distance){

# Computation of distances to centers and variance matrices in each cluster.
# Function called by cecm.

  nbFoc<-nrow(F)
  K<-ncol(F)
  n<-nrow(x)
  nbAtt<-ncol(x)
  beta<-2

  gplus<-matrix(0,nbFoc-1,nbAtt)
  for(i in 2:nbFoc){
    fi <- F[i,]
    truc <- matrix(fi,K,nbAtt)
    gplus[i-1,] <- colSums(g*truc)/sum(fi)
  }

  if(distance==0){
    S<-vector(mode="list",length=K)
    for(k in 1:K) S[[k]]<-diag(nbAtt) # distance euclidienne
  }else{
    ind<-which(rowSums(F)==1)
    S<-vector(mode="list",length=length(ind))
#    Splot<-S # matrice de var-cov utilisee pour les plots
    for(i in 1:length(ind)){
      denomSplot<-0
      indi<-ind[i]
      Sigmai=matrix(0,nbAtt,nbAtt)
      for(k in 1:n){
        omegai=matrix(F[indi,],nbFoc,K, byrow=TRUE)
        indAj=which(rowSums(pmin(omegai,F))>0)
        for(j in 1:length(indAj)){
          indj=indAj[j]
          aux <- x[k,]-gplus[indj-1,]
          Sigmai<-Sigmai+sum(F[indj,])^(alpha-1)*m[k,indj-1]^beta *(aux%*%t(aux))
#          denomSplot=denomSplot+sum(F[indj,])^(alpha-1)*m[k,indj-1]^beta
          # denominateur utilise pour le calcul de Splot (normalisation)
        } # for j
      } # for k
    Si=det(Sigmai)^(1/nbAtt)*solve(Sigmai)
#    Splot[[i]] <- Sigmai/denomSplot
    S[[i]] <- Si # variance des elements singletons uniquement
    } # for i
  } # end if


Smean<-vector(mode="list",length=nbFoc-1)
for(i in 1:(nbFoc-1)){
  aux<-matrix(0,nbAtt,nbAtt)
  for(j in 1:K) aux<-aux+F[i+1,j]*S[[j]]
  Smean[[i]]<- aux/max(sum(F[i+1,]),1) # variance de tous les elements
} #for i

# calculation of distances to centers
D<-matrix(0,n,nbFoc-1)
for(j in 1:(nbFoc-1)){
  aux<-(x - matrix(gplus[j,],n,nbAtt,byrow = TRUE))
  if(distance==0) D[,j]<-diag(aux%*%t(aux)) else D[,j]<-diag(aux%*% Smean[[j]]%*%t(aux))
} # for j

return(list(D=D,Smean=Smean))
}

