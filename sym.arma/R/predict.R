predict <-
function(model, h, xreg=NULL) 
{ 
   hat = model$coefficients
   intercept = model$include.mean
   X = model$xreg
   Y = model$Y
   r.hat = model$resid.raw
   n = model$n  
   np = model$np
   nq = model$nq
   m = max(np,nq)
   if(intercept==TRUE) p = 1
   else p = 0

   ar = ma = NULL
   if(nq!=0)  ma = hat[(length(hat)-nq+1):length(hat)]
   if(np!=0)  ar = hat[((length(hat)-nq)-np+1):(length(hat)-nq)]
 
      if(length(hat)!=(p+np+nq))
      {   if(is.null(xreg)==TRUE) stop(paste("no 'xreg' argument"))
  
          if(intercept==TRUE & NCOL(X)>1)  
            if((NCOL(X)-1)!=NCOL(xreg))  stop(paste("number of regression variables in 'xreg' is different from the fitted model"))
          if(intercept==FALSE & (NCOL(X)!=NCOL(xreg)))  stop(paste("number of regression variables in 'xreg' is different from the fitted model"))

          h = NROW(xreg)
          X.ajust = matrix(nrow=NROW(X)+h,ncol=NCOL(X))
          X.ajust[1:n,]=X
          if(intercept==TRUE)  X.ajust[(n+1):(n+h),]=cbind(1,xreg)
          else                 X.ajust[(n+1):(n+h),]=xreg
          if(intercept==TRUE)  beta = cbind(hat[1:(1+NCOL(xreg))])
          else                 beta = cbind(hat[1:NCOL(xreg)])
 
          if(intercept==TRUE)  XB.fixed = cbind(1,xreg)%*%beta
          else                 XB.fixed = xreg%*%beta
          Y.ajust = Y
          rhat.ajust = r.hat
          y.predict = NULL
          for(k in 1:h)
          {   if(np!=0) start.ar = t(cbind(Y.ajust[(n+k-1):(n+k-np)])  -   X.ajust[(n+k-1):(n+k-np),]%*%beta)%*%cbind(ar)
              else      start.ar = 0
              if(nq!=0) start.ma =  rhat.ajust[(n-m+k-1):(n-m+k-nq)]%*%cbind(ma)
              else      start.ma = 0 
                 
              y.predict[k] = XB.fixed[k] + start.ar +  start.ma
          
              Y.ajust = c(Y.ajust,y.predict[k])
              rhat.ajust = c(rhat.ajust,0)
           }
      }
      if(length(hat)==(p+np+nq))
      {   if(is.null(xreg)==FALSE) warning(paste("not used the 'xreg' argument in the fitted model"))

          if(intercept==TRUE)  beta = hat[1]
          else                 beta = 0

          if(intercept==TRUE)  X.ajust = matrix(1,nrow=NROW(X)+h,ncol=1) 
          else                 X.ajust = 0

          Y.ajust = Y
          rhat.ajust = r.hat
          y.predict = NULL
          for(k in 1:h)
          {   if(np!=0) start.ar = t(cbind(Y.ajust[(n+k-1):(n+k-np)])  -   X.ajust[(n+k-1):(n+k-np)]*beta)%*%cbind(ar)
              else      start.ar = 0
              if(nq!=0) start.ma =  rhat.ajust[(n-m+k-1):(n-m+k-nq)]%*%cbind(ma)
              else      start.ma = 0 
                 
              y.predict[k] = beta + start.ar +  start.ma
          
              Y.ajust = c(Y.ajust,y.predict[k])
              rhat.ajust = c(rhat.ajust,0)
          }
      }

   print(noquote(matrix(c(y.predict), nrow=h, ncol=1, byrow=FALSE, dimnames = list(c(paste("Y[", (n+1):(n+h), "]", sep = "")), c("fit")))))
   fit <- list(pred = y.predict)
}
