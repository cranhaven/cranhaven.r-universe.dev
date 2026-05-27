getNullProb <-
function(g2l.out,x0,z0){
  x.axe<-g2l.out$data$X.test
  ind<-which.min(apply(x.axe,1,function(x) sum(abs(x-x0)))) 
  zgrid<-g2l.out$z.grid[ind,]
  check.array=ifelse(length(dim(g2l.out$lpfdr))==3,1,0)
  
  if(check.array==0){
    res_vec<-g2l.out$lpfdr[ind,]
    out<-approx(x=zgrid, y=res_vec, xout=z0, method="linear", rule = 2)$y
  }else{
    res_mat<-g2l.out$lpfdr[ind,,]
    #c(length(x.test),B,ngrid)
    niter=nrow(res_mat)
    pnull0<-rep(0,niter)
    for(i in 1:niter){
      pnull0[i]<-approx(x=zgrid, y=res_mat[i,], xout=z0, method="linear", rule = 2)$y
      #if(pnull0[i]<=0){pnull0[i]=0}
    }
    out<-data.frame(prob.null=mean(pnull0),sd=IQR(pnull0)/1.3489)
    #out<-pnull0
  }
  
  return(out)
}
