setCentersECM<-function(x,m,F,Smean,alpha,beta){

  # Computation of centers in CECM. Function called by cecm.

  nbFoc<-nrow(F)
  K<-ncol(F)
  n<-nrow(x)
  nbAtt<-ncol(x)


  card<- rowSums(F[2:nbFoc,])
  indSingleton<-which(card==1)+1

  R<-NULL
  B<-NULL
  for(l in 1:K){
    indl<-indSingleton[l]
    Rl<-NULL
    for(i in 1:n){
      Ril<-matrix(0,nbAtt,nbAtt)
      Fl<-matrix(F[indl,],nbFoc,K,byrow=TRUE)
      indAj<-which(rowSums(pmin(Fl,F))==1)-1
      for(j in 1:length(indAj)){
        Ril=Ril+card[indAj[j]]^(alpha-1)*m[i,indAj[j]]^beta * Smean[[indAj[j]]]
      }
      Rl<-rbind(Rl,Ril)
    }
    R<-cbind(R,Rl)

    Bl<-NULL
    for(k in 1:K){
      Bkl<- matrix(0,nbAtt,nbAtt)
      indk<-indSingleton[k]
      for(i in 1:n){
        Fl<-matrix(sign(F[indl,]+F[indk,]),nbFoc,K,byrow=TRUE)
        indAj<-which(rowSums(pmin(Fl,F))==sum(Fl[1,]))-1
        for(j in 1:length(indAj)){
          Bkl<-Bkl+card[indAj[j]]^(alpha-2)*m[i,indAj[j]]^beta * Smean[[indAj[j]]]
        }
      }
      Bl=rbind(Bl,Bkl)
    }
    B=cbind(B,Bl)
  }

  X<-as.vector(t(x))
  g<-solve(t(B),t(R)%*%X)
  g=matrix(g,K,nbAtt,byrow=TRUE)
}




