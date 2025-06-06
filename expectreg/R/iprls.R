iprls <- function(B,DD,yy,pp,lambda,smooth,nb,center,constmat,robust,adaptive=FALSE, LSMaxCores=1)
{
  nterms = length(nb)
  m = length(yy)
  np = length(pp)
  
#  myapply <- lapply
#  if (.Platform$OS.type == "unix" && require("multicore")) 
#  {
#      if (!multicore:::isChild()) 
#      {
#          myapply <- mclapply
#      }
#  }
  
  if(length(lambda) < nterms)
      lala = rep(lambda[1],nterms)
    else
      lala = lambda
  

  dummy.reg <- function(pp,lala,smooth,yy,B,DD,nb,nterms,center,constmat,robust,amplitude)
  {
    print(paste("tau:",pp,sep=" "))

    if(smooth == "schall")
    {
      dc = 1
      dw = 1
      w <- rep(1,times=m)
      it = 1
      while((dc >= 0.01 || dw != 0) && it < 100)
      {
        aa <- itpen.lsfit(yy, B, pp, lala, DD, nb,constmat,robust,amplitude)
        vector.a.ma.schall <- aa$a 

        w0 <- w
        l0 <- lala
        
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
          
          if(any(partDD != 0))
          {
            v <- partDD %*% partaa
            z <- aa$fitted
          
            w <- as.vector(aa$weight)

            H = solve(t(partB)%*%(w*partB) + lala[i]*t(partDD)%*%partDD)
            ##H = B%*%H%*%t(B)
            #H = diag(H)*aa$weight
            H = apply(sqrt(w)*partB,1,function(x){t(x)%*%H%*%x})
          
            sig2 <- sum(w * (yy - z) ^ 2,na.rm=TRUE) / (m - sum(aa$diag.hat.ma,na.rm=TRUE))
            tau2 <- sum(v ^ 2,na.rm=TRUE) / sum(H,na.rm=TRUE) + 1e-06

            lala[i] = max(sig2 / tau2,1e-10,na.rm=TRUE)
          }
        }
        
        dc <- max(abs(log10(l0 + 1e-6)-log10(lala + 1e-6)))
        dw <- sum(w != w0,na.rm=TRUE)
        it = it + 1
      }
      if(it == 100)
        warning("Schall algorithm did not converge. Stopping after 100 iterations.")
    }
    else if(smooth == "acv")
    {
      acv.min = nlm(acv,p=lala,yy=yy,B=B,asymmetry=pp,DD=DD,nb=nb,constmat=constmat,robust=robust,amplitude=amplitude,iterlim=100)

      aa <- itpen.lsfit(yy, B, pp, abs(acv.min$estimate), DD, nb,constmat,robust,amplitude)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$estimate)
    }
    else
    {
      aa <- itpen.lsfit(yy, B, pp, lala, DD, nb,constmat,robust,amplitude)
      vector.a.ma.schall <- aa$a   
    }

    list(vector.a.ma.schall,lala,aa$weight,aa$diag.hat.ma)
  }


  amplitude = NULL
  
  if(adaptive)
  {
  	for(i in 1:2)
  	{
  	  #acv.min = nlm(acv,p=lala,yy=yy,B=B,asymmetry=0.5,DD=DD,nb=nb,constmat=constmat,robust=robust,amplitude=amplitude,iterlim=100)
      #lala <- abs(acv.min$estimate)
      #aa <- itpen.lsfit(yy, B, 0.5, abs(acv.min$estimate), DD, nb,constmat,robust,amplitude=amplitude)
    
      coefa = dummy.reg(0.5,lala,smooth,yy,B,DD,nb,nterms,center,constmat,robust,amplitude)[[1]]
    
      res = abs(yy - B %*% coefa)
      
      coefb = dummy.reg(0.5,lala,smooth,res,B,DD,nb,nterms,center,constmat,robust,amplitude=NULL)[[1]]
    
      amplitude = B %*% coefb
      
      #resamp = abs(res - amplitude)
      
      #coefc = dummy.reg(0.5,lala,smooth,resamp,B,DD,nb,nterms,center,constmat,robust,amplitude=NULL)[[1]]
      
      #amp2 = B %*% coefc
      
      #coefd = dummy.reg(0.5,lala,smooth,res,B,DD,nb,nterms,center,constmat,robust,amplitude=amp2)[[1]]
      
      #amplitude = B %*% coefd
      
      #res = abs(yy - B %*% aa$a)
  	
  	  #acv.min = nlm(acv,p=lala,yy=res,B=B,asymmetry=0.5,DD=DD,nb=nb,constmat=constmat,robust=robust,amplitude=NULL,iterlim=100)

      #aa <- itpen.lsfit(res, B, 0.5, abs(acv.min$estimate), DD, nb,constmat,robust,NULL)
    
      #amplitude = B %*% aa$a
    }
  }

  
  if (.Platform$OS.type == "unix")
    coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,B,DD,nb,nterms,center,constmat,robust,amplitude),mc.cores = max(1,min(detectCores()-1, LSMaxCores)))
  else if (.Platform$OS.type == "windows")
    coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,B,DD,nb,nterms,center,constmat,robust,amplitude),mc.cores = 1)

  lala <- matrix(lambda, nrow=nterms, ncol=np)
  vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)
  ww = matrix(NA,nrow=m,ncol=np)
  diag.hat = matrix(NA,nrow=m,ncol=np)
  

  for(i in 1:np)
  {
    vector.a.ma.schall[,i] = coef.vector[[i]][[1]]
    lala[,i] = coef.vector[[i]][[2]]
    ww[,i] = coef.vector[[i]][[3]]
    diag.hat[,i] = coef.vector[[i]][[4]]
  }

  return(list(vector.a.ma.schall,lala,amplitude,ww,diag.hat))
}


itpen.lsfit <- function(y, B, p, lambda, DD,nb,constmat,robust,amplitude=NULL)
###### Asymmetric regression with difference penalty
# parameters:
# y - response variable
# B - B-spline basis
# p - asymmetry parameter
# lambda - smoothing parameter
# DD - difference matrix 
#
# needed: ncol(B) = ncol(DD)
{
  #w1 <- 0 * y + 0.5
  n <- ncol(B)
  lambda = c(rep(0,times=n - sum(nb)),rep(lambda,times=nb))

  P <- sqrt(lambda) * DD
  augm <- rep(0, nrow(P))
  conpen <- rep(0,nrow(constmat))
  
  
  diffcon = -1
  it.con = 1
  

  my.psi.q<-function(u,q,robust,amplitude)
  {
    # function for the Huber 2 influence function at quantile q
    if(all(is.null(amplitude)))
    {
      s<-median(abs(u))/0.6745
      c<-robust #1.345
    }
    else
    {
      s =1/0.6745
      c<-abs(amplitude) * robust #1.345
    }
    
    u<-u/s
    abs(-(1-q)*c*(u< -c)+(1-q)*u*(u<0 & u>=-c)+q*u*(u>=0 & u<c)+q*c*(u>=c))
  }
  
  resi = B %*% rep(1,ncol(B))

 
    w1 = my.psi.q(y-resi,p,robust,amplitude)/(abs(y-resi) + 1e-5*(y-resi == 0))

  
  while(any(diffcon < -1e-5) && it.con < 20)
  {
    it = 1
    dw1 = 1
    
    while(dw1 != 0 && it < 100) 
    {
        model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)
        a1 <- model$coefficients
        z1 <- B %*%a1
        w01 <- w1
        #w1 <- as.vector(ifelse(y > z1, p, 1 - p))

          w1 = my.psi.q(y-z1,p,robust,amplitude)/(abs(y-z1) + 1e-5*(y-z1 == 0))

        dw1 <- sum(w1 != w01,na.rm=TRUE)     
        it = it + 1
    }
    diffcon =  constmat %*% a1
    
    if(any(diffcon < 0))
    {
      wc = which(diffcon < 0)
      conpen[wc] = conpen[wc] + 100000
    }
    it.con = it.con + 1
  }
  diag.hat.ma1 <- hat(model$qr)[1:length(y)]  

  if(it == 50)
     warning("IWLS weights did not converge after 100 iterations.")

  list(a=a1, diag.hat.ma=diag.hat.ma1, weight = w1, fitted = z1)  
}


acv <- function(penalty,yy,B,asymmetry,DD,nb,constmat,robust,amplitude=NULL)
# asymmetric cross validation
# computes the acv score for the smoothing of the regression
# score has to be minimized dependant on parameter "penalty"
# therefore a grid search can be applied to this function
# parameters:
# penalty - smoothing parameter lambda
# yy - vector of responses
# B - basis for the approximation
# p - quantile
# DD - penalization matrix
{
  aa <- itpen.lsfit(yy, B, asymmetry, abs(penalty), DD, nb, constmat,robust,amplitude)

  #H = solve(t(B)%*%(aa$weight*B) + penalty*t(DD)%*%DD)
  ##H = B%*%H%*%t(B)
  ##H = diag(H)*aa$weight
  #H = apply(sqrt(aa$weight)*B,1,function(x){t(x)%*%H%*%x})

  #H = diag(diag(aa$diag.hat.ma))

  score = aa$weight*(yy - B%*%aa$a)^2/(1-aa$diag.hat.ma)^2

  mean(score[which(is.finite(score))],na.rm=TRUE)
}
