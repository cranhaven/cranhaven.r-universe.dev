restricted <- function(B,DD,yy,pp,lambda,smooth,nb,center,constmat,robust)
###### expectile regression according to eilers, schnabel
# parameters:
# formula - vector of responses ~ f(vector of independent,type="which base to use") + ...
# smooth - if smoothing with schall's algorithm, asymmetric cross validation or no smoothing shall be done
# lambda - smoothing penalty, important if no smoothing is done
{  
  nterms = length(nb)
  m = length(yy)
  np = length(pp)
  
  lala <- matrix(lambda, nrow=nterms, ncol=2, dimnames=list(1:nterms,c("mean","residual")))
  vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)

    if(smooth == "schall")
    {
      dc = 1
      dw = 1
      w <- matrix(0.5,nrow=m,ncol=nterms)
      it = 1
      while(dc >= 0.01 && it < 100)# || dw != 0)
      {
        aa <- itpen.lsfit(yy, B, 0.5, lala[,1], DD, nb,constmat,robust)
        mean.coefficients <- aa$a 
        
        sig2 <- vector()
        tau2 <- vector()
        #w0 <- w
        
        l0 <- lala[,1]
        
        for(i in 1:nterms)
        {
          partbasis = (sum(nb[0:(i-1)])+1):(sum(nb[0:i]))
          if(center)
          {
            partB = B[,-1][,partbasis,drop=FALSE]
            partDD = DD[,-1][-1,][,partbasis,drop=FALSE]
            partaa = aa$a[-1][partbasis]
          }
          else
          {
            partB = B[,partbasis,drop=FALSE]
            partDD = DD[,partbasis,drop=FALSE]
            partaa = aa$a[partbasis]
          }
        
          v <- partDD %*% partaa
          z <- aa$fitted
        
          #w[,i] <- 0.5 * (yy > z) + 0.5 * (yy <= z)
          
          H = solve(t(partB)%*%(w[,i]*partB) + lala[i,1]*t(partDD)%*%partDD)
          ##H = B%*%H%*%t(B)
          ##H = diag(H)*aa$weight
          H = apply(sqrt(w[,i])*partB,1,function(x){t(x)%*%H%*%x})
        
          sig2[i] <- sum(w[,i] * (yy - z) ^ 2,na.rm=TRUE) / (m - sum(aa$diag.hat.ma))
          tau2[i] <- sum(v ^ 2) / sum(H) + 1e-06
          
          lala[i,1] <- max(sig2[i] / tau2[i], 1e-10)
        }
        
        dc <- max(abs(log10(l0)-log10(lala[,1])))
        #dw <- sum(w != w0,na.rm=TRUE)
        it = it + 1
      }
      if(it == 100)
        warning("Schall algorithm did not converge. Stopping after 100 iterations.")
    }
    else if(smooth == "acv")
    {
      acv.min = nlm(acv,p=lala[,1],yy=yy,B=B,asymmetry=0.5,DD=DD,nb=nb,constmat=constmat,robust=robust,ndigit=8,iterlim=50,gradtol=0.0001)
        
      aa <- itpen.lsfit(yy, B, 0.5, abs(acv.min$estimate), DD, nb,constmat,robust)
      mean.coefficients <- aa$a  
      lala[,1] <- abs(acv.min$estimate)
    }
    else
    {
      aa <- itpen.lsfit(yy, B, 0.5, lala[,1], DD, nb,constmat,robust)
      mean.coefficients <- aa$a   
    }

  residuals = yy-B%*%mean.coefficients

  constmat[,] = 0

  gg = itpen.lsfit(abs(residuals),B,0.5,lala[,1],DD, nb, constmat,robust)
  
  cc = NULL

  for (q in 1:np) 
  {
    ca = itpen.lsfit(residuals,gg$fitted,pp[q],NULL,matrix(0,nrow=1,ncol=1),0, matrix(0,nrow=1,ncol=1),robust)
    cc[q] = ca$a
    
    vector.a.ma.schall[,q] = mean.coefficients + ca$a*gg$a
  }
  
  return(list(vector.a.ma.schall,lala,mean.coefficients,gg$a,cc))
}
