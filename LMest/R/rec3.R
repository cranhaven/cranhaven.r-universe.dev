rec3 <- function(Q,yv,PI,Pio,pim){

# preliminaries
  out = dim(Pio)
  ns = out[1]; k = out[2]; TT = out[3]
# for t=T
  if(k==1){
    U = array(1,c(k,ns,TT)); V = matrix(ns*(TT-1),k,k)
  }else{
    U = array(0,c(k,ns,TT)); V = matrix(0,k,k)
    U[,,TT] = t((yv*Q[,,TT])/pim)
    Q1 = t((yv*Q[,,TT-1])/pim)
    P1 = Pio[,,TT]
    R = matrix(1,k,ns);
    for(i in 1:ns) V = V+Q1[,i]%o%P1[i,]
# for other t
    ## Se TT = 2
    if(TT>2){
      for(t in seq(TT-1,2,-1)){
        R = PI%*%(t(Pio[,,t+1])*R)
        P1 = Pio[,,t]*t(R)
        U[,,t] = Q1*R
        Q1 = t((yv*Q[,,t-1])/pim)
        for(i in 1:ns) V = V+Q1[,i]%o%P1[i,]
      }
    } 
    V = PI*V
    R = PI%*%(t(Pio[,,2])*R)
    U[,,1] = Q1*R
  }
  out = list(U=U,V=V)
  out
}
