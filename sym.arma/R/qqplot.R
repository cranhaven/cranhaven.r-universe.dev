qqplot <-
function(model, envelope=0.95, B=400)
{
   resid = model$resid.stand
   n = length(resid)
   np = model$np
   nq = model$nq
   d = model$d
   m = max(np,nq)
   index1 = model$index1
   index2 = model$index2
   mu = model$fitted.values
   varphi = model$dispersion
   scalevar = model$scalevariance
   dist = model$family
   fixed = model$fixed
   X = model$xreg
   hat = model$coefficients
   ar = ma = NULL
   if(length(hat)==(np+nq))
   {  if(np!=0) ar[1:np] = hat[1:np]
      if(np!=0 && nq!=0) ma[1:nq] = hat[(np+1):(np+nq)]
      if(np==0 && nq!=0) ma[1:nq] = hat[1:nq]
   }
   else
   {  if(np!=0) ar[1:np] = hat[(NCOL(X)+1):(NCOL(X)+np)]
      if(np!=0 && nq!=0) ma[1:nq] = hat[(NCOL(X)+np+1):(NCOL(X)+np+nq)]
      if(np==0 && nq!=0) ma[1:nq] = hat[(NCOL(X)+1):(NCOL(X)+nq)]
   }

   resp<-NULL
   e <- matrix(0,(n-m),B)
 
   if(dist=="Normal")  dist.theo <- rnorm(1000,0,1)
   if(dist=="Student")  dist.theo <- rt(1000,df=index1)
   if(dist=="Gstudent") 
   {  z <- rnorm(1000,0,1)
      T <- 1/rgamma(1000, shape = index1/2, rate = index2/2)
      dist.theo <- T^(-0.5)*z
   }
   if(dist=="ExpPower") 
   {  v <- runif(1000,-1,1)
      w <- rgamma(1000,shape=(1+(1+index1)/2))
      dist.theo <- (2*w)^((1+index1)/2)*v                           
   }
   if(dist=="LogisticII") 
   {  v <- runif(1000,0,1)
      dist.theo <- log(v/(1-v))
   }      
   if(dist=="Cauchy") 
   {   v <- rnorm(1000,0,1)
       w <- rnorm(1000,0,1)
       dist.theo <- v/w
   }
   if(dist=="LogisticI")
      stop(paste("Function not implemented for Logistic I distribution"))
   if(dist=="Glogistic")
      stop(paste("Function not implemented for Generalized Logistic distribution"))
   if(dist=="Cnormal")
      stop(paste("Function not implemented for Contamined Normal distribution"))
  
   if(envelope!="FALSE")
   {   for(i in 1:B)
       {   resp <- symarma.sim(model=list(ar=ar,ma=ma),n=n,family=dist,index1,index2,varphi=1/scalevar)         
           fit.s <- elliptical.ts(resp,order=c(np,d,nq),xreg= X[(m+1):(n+m),],include.mean=FALSE,index1=index1,index2=index2,family=dist,trace=FALSE,fixed=fixed)
 
           td <- fit.s$resid.stand
           e[,i] <- sort(td)
           e1 <- numeric(n-m)
           e2 <- numeric(n-m)
           for(i in 1:(n-m))
           {   eo <- sort(e[i,])
               e1[i] <- eo[ceiling(B*(1-envelope))]
               e2[i] <- eo[ceiling(B*envelope)]
           }
           med <- apply(e,1,mean)
        }
        faixa <- range(resid,e1,e2)
    }
    if(envelope=="FALSE") faixa <- range(resid)
    par(pty="s")
    if(dist=="Normal")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for Normal"),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="Student")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {Student-t}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="Gstudent")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {Generalized-Student-t}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="ExpPower")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {ExpPower}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="LogisticI")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {LogisticI}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="LogisticII")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {LogisticII}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(dist=="Cauchy")
       stats::qqplot(dist.theo,resid,main = expression("Q-Q plot for" ~~ {Cauchy}),xlab="Theorical Quantiles",ylab="Standardized residuals",xlim=range(dist.theo), ylim=faixa, pch=16)
    if(envelope!="FALSE")
    {  par(new=TRUE)
          stats::qqplot(dist.theo,e1,axes=F,main="",xlab="",ylab="",type="l",xlim=range(dist.theo),ylim=faixa,lty=1)
       par(new=TRUE)
          stats::qqplot(dist.theo,e2,axes=F,main="",xlab="",ylab="", type="l",xlim=range(dist.theo),ylim=faixa,lty=1)
       par(new=TRUE)
          stats::qqplot(dist.theo,med,axes=F,main="",xlab="", ylab="", type="l",xlim=range(dist.theo),ylim=faixa,lty=2)
    }
}
