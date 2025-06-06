ald.aft <-
function(B,DD,yy,delta,pp,lambda,smooth,nb,center,constmat,types)
{
  nterms = length(nb)
  m = length(yy)
  np = length(pp)

  
#  myapply <- lapply
#  if (.Platform$OS.type == "unix" && require("parallel")) 
#  {
#      if (!parallel:::isChild()) 
#      {
#          myapply <- mclapply
#      }
#  }
  
  if(length(lambda) < nterms)
      lala = rep(lambda[1],nterms)
    else
      lala = lambda


  dummy.reg <- function(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center,types)
  {
    message("Quantile: ",pp,"\n")

    if(smooth == "aic")
    {
    
      acv.min = nlminb(start=lala, objective = aicfun.aft, yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,delta = delta,likfun=likeli.ald, lower=0,upper=10000)


      #acv.min = multimin.init(x=lala,f=function(x) aicfun.aft(abs(x),yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat),method="nm")
# 
#       for(i in 1:s50)
#       {
#         acv.min = multimin.iterate(acv.min)
#         print(acv.min$x)
#       }

      #acv.min = RcppDE::DEoptim(aicfun, rep(0,length(lala)), rep(10000,length(lala)), control = DEoptim.control(trace=F,strategy=2), yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat)

      #aa <- asyregpen.aft(yy, B, pp, abs(acv.min$par), DD, nb, constmat)
      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$x), DD, nb, constmat,likeli.ald)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$x)#abs(acv.min$par)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "bic")
    {
      acv.min = nlminb(start=lala,objective=bicfun.aft,yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,likfun=likeli.ald,lower=0,upper=10000)

      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat,likeli.ald)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$par)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "cvgrid")
    {
    	lala = cvgrid.ald(yy,delta,B,pp,DD,nb,constmat,types)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.ald)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else if(smooth == "lcurve")
    {
     
      
    	lala = lcurve.aft(yy,delta,B,pp,DD,nb,constmat,types)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.ald)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else
    {
      aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.ald)
      vector.a.ma.schall <- aa$a  
      diag.hat = aa$diag.hat.ma 
    }
    
    list(vector.a.ma.schall,lala,diag.hat)
  }

  if (.Platform$OS.type == "unix")
    coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center,types),mc.cores = max(1,min(detectCores()-1,2)))
  else if (.Platform$OS.type == "windows")
    coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center,types),mc.cores = 1)


  lala <- matrix(lambda, nrow=nterms, ncol=np)
  vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)
  diag.hat = matrix(NA,nrow=m,ncol=np)


  for(i in 1:np)
  {

    vector.a.ma.schall[,i] = coef.vector[[i]][[1]]
    lala[,i] = coef.vector[[i]][[2]]
    diag.hat[,i] = coef.vector[[i]][[3]]
  }

  return(list(vector.a.ma.schall,lala,diag.hat))
}


