
update.beta <- function(Fmat=Fmat,U=U,G=G,lam=lam,
                         Z=Z,Mmat.q=Mmat.q,Mmat.q1=Mmat.q1){

  q <- ncol(Fmat)+1
  m <- ncol(G)
  n <- nrow(U)

  Ec<-rep(1,3) #equation constraint
  Fc<-1
  Gc<-diag(3) #inequation constraint
  Hc<-rep(0,3)

  Gumat<-apply(U,1,function(x) t(G)%*%x) #mxn
  #Gu.all<-matrix(Gumat,(m*n),1) #(1-lam)*

  constALS.func<-function(x){

    i<-x[1]
    fi<-x[c(2:(q-1+1))]    #Fmat.ns[i,]
    Gui<-x[c((q+1):(q+m))] #Gumat[,i]
    #take out column since Gumat is mxn matrix
    #ZM<-C.list2[[i]]%*%Mmat.q[,-1]
    ZM<-t(Z[,,i])%*%Mmat.q[,-1]

    #explanatory variable
    Amat<-rbind((sqrt(lam)*Mmat.q1[,-1]),(sqrt(1-lam)*ZM))
    #
    #objective variable
    Bmat<-c((sqrt(lam)*fi),(sqrt(1-lam)*Gui))
    #((n*(q-1))+(n*m) x 1) vector
    #browser()
    alpres<-limSolve::lsei(A =Amat,B=Bmat,G=Gc,H=Hc,E=Ec,F=Fc,type=2)$X
    return(alpres)

  }

  #sublab <- c(1:n)
  bindALS<-cbind(c(1:n),Fmat,t(Gumat))
  Alp <- matrix(0,4,n)
  Alp[c(2:4),] <- apply(bindALS,1,constALS.func)
  t(Alp)

}
