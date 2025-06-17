RGF=function(data,group,rept=100000){

    mu.huber=function(y, k=1.5, tol = 1.0e-6){
      y <- y[!is.na(y)]
      n <- length(y)
      mu <- median(y)
      s <- mad(y)
      if(s == 0) stop("cannot estimate scale: MAD is zero for this sample")
      repeat{
        yy <- pmin(pmax(mu-k*s,y),mu+k*s)
        mu1 <- sum(yy)/n
        if(abs(mu-mu1) < tol*s) break
        mu <- mu1
      }
      return(mu)
    }
    vary.huber=function(y, k=1.5, tol = 1.0e-6){
      y <- y[!is.na(y)]
      n <- length(y)
      mu <- median(y)
      s <- mad(y)
      if(s == 0) stop("cannot estimate scale: MAD is zero for this sample")
      repeat{
        yy <- pmin(pmax(mu-k*s,y),mu+k*s)
        mu1 <- sum(yy)/n
        if(abs(mu-mu1) < tol*s) break
        mu <- mu1
      }
      return(s^2)
    }
    n=tapply(data, group, length)
    k=length(tapply(data, group, length))
    xbar=tapply(data, group, mu.huber)
    var=tapply(data, group, vary.huber)

    v=matrix(0,k,rept);
    b=matrix(0,k,rept);
    total1=numeric(rept);
    total2=numeric(rept);
    grand=matrix(0,k,rept);
    grandtotal=numeric(rept);
    for(i in 1:k){
      v[i,]=rchisq(rept,as.numeric(n[i])-1);
      b[i,]=(n[i]/((n[i]-1)*var[i]))*v[i,];
      total1=total1+(b[i,]*xbar[i]);
      total2=total2+b[i,];
    }
    for(i in 1:k){
      grand[i,]=b[i,]*(xbar[i]-total1/total2)^2;
      grandtotal=grandtotal+grand[i,];
    }
    U=rchisq(rept,k-1);
    pvalue=mean(U>grandtotal);
    result=matrix(c(round(pvalue,digits=4)))
    rownames(result)=c("p-value")
    colnames(result)=c("RGF test")
    return(t(result))
}

