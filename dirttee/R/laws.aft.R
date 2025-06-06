#' @importFrom stats optim
laws.aft <- function(B,DD,yy,delta,pp,lambda,smooth,nb,center,constmat,types)
###### expectile regression according to eilers, schnabel
# parameters:
# formula - vector of responses ~ f(vector of independent,type="which base to use") + ...
# smooth - if smoothing with schall's algorithm, asymmetric cross validation or no smoothing shall be done
# lambda - smoothing penalty, important if no smoothing is done
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
    message("Expectile: ",pp,"\n")

    # if(smooth == "gcv")
    # {
      # #acv.min = nlm(acv,p=lala,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,iterlim=100)
      # #acv.min = optim(par=lala,acv,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,method="L-BFGS-B",lower=0,upper=10000)
      # acv.min = nlminb(start=lala,objective=acv.aft,yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,lower=0,upper=10000)

      # #aa <- asyregpen.aft(yy, B, pp, abs(acv.min$estimate), DD, nb, constmat)
      # aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat)
      # vector.a.ma.schall <- aa$a  
      # #lala <- abs(acv.min$estimate)
      # lala <- abs(acv.min$par)
     # # print(c(acv.min$par,acv.min$objective))
      # diag.hat = aa$diag.hat.ma
    # }
     if(smooth == "aic")
    {
    
      acv.min = nlminb(start=lala,objective=aicfun.aft,yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,likfun=likeli.asynorm,lower=0.1,upper=20000,control=list(iter.max=60, rel.tol=1e-4))


      #acv.min = multimin.init(x=lala,f=function(x) aicfun.aft(abs(x),yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat),method="nm")

      #for(i in 1:50)
      #{
      #  acv.min = multimin.iterate(acv.min)
      #  print(acv.min$x)
      #}

      #acv.min = RcppDE::DEoptim(aicfun, rep(0,length(lala)), rep(10000,length(lala)), control = DEoptim.control(trace=F,strategy=2), yy=yy,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat)

      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat,likeli.asynorm)
      #aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$x), DD, nb, constmat)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$par)#abs(acv.min$x)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "bic")
    {
      acv.min = nlminb(start=lala,objective=bicfun.aft,yy=yy,delta=delta,B=B,quantile=pp,DD=DD,nb=nb, constmat=constmat,likfun=likeli.asynorm,lower=0.1,upper=20000,control=list(iter.max=60, abs.tol=1e-10,rel.tol=1e-5))

      aa <- asyregpen.aft(yy,delta, B, pp, abs(acv.min$par), DD, nb, constmat,likeli.asynorm)
      vector.a.ma.schall <- aa$a  
      lala <- abs(acv.min$par)

      diag.hat = aa$diag.hat.ma
    }
    else if(smooth == "gridbic")
    {
    	lala = bicgrid.aft(yy,delta,B,pp,DD,nb,constmat,types,likeli.asynorm)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else if(smooth == "gridaic")
    {
    	lala = aicgrid.aft(yy,delta,B,pp,DD,nb,constmat,types,likeli.asynorm)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else if(smooth == "lcurve")
    {
    	lala = lcurve.aft(yy,delta,B,pp,DD,nb,constmat,types)
    	aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
    	vector.a.ma.schall <- aa$a  
        diag.hat = aa$diag.hat.ma 
    }
    else
    {
      aa <- asyregpen.aft(yy,delta, B, pp, lala, DD, nb, constmat,likeli.asynorm)
      vector.a.ma.schall <- aa$a  
      diag.hat = aa$diag.hat.ma 
    }
    
    list(vector.a.ma.schall,lala,diag.hat,aa$sigma)
  }

  
  coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = 1)
  
  #if (.Platform$OS.type == "unix")
    #coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = max(1,min(detectCores()-1,2)))
  #else if (.Platform$OS.type == "windows")
    #coef.vector = mclapply(pp,function(pp) dummy.reg(pp,lala,smooth,yy,delta,B,DD,nb,nterms,center),mc.cores = 1)


  lala <- matrix(lambda, nrow=nterms, ncol=np)
  vector.a.ma.schall <- matrix(NA, nrow=sum(nb)+(1*center),ncol=np)
  diag.hat = matrix(NA,nrow=m,ncol=np)
  sigma = rep(NA,np)


  for(i in 1:np)
  {
  	#print(i)
    vector.a.ma.schall[,i] = coef.vector[[i]][[1]]
    lala[,i] = coef.vector[[i]][[2]]
    diag.hat[,i] = coef.vector[[i]][[3]]
    sigma[i] = coef.vector[[i]][[4]]
  }

  return(list(vector.a.ma.schall,lala,diag.hat,sigma))
}


likeli.asynorm <- function(par,tau,ycens,delta,X,K)
  { 
  	-sum(delta * log(dasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau)) + (1-delta) * log(1-pasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau))) + t(par[-1])%*%K%*%par[-1]}

likeli.asynorm2 <- function(par,tau,ycens,delta,X,K)
  { delta * log(dasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau)) + (1-delta) * log(1-pasynorm(ycens,X%*%par[-1],abs(par[1])+0.0001,tau)) + rep(t(par[-1])%*%K%*%par[-1],length(ycens))/length(ycens)}
   

asyregpen.aft <- function(y,delta, B, p, lambda, DD,nb, constmat,likfun=likeli.asynorm)
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
  w1 <- 0 * y + 0.5
  n <- ncol(B)

  lambda = c(rep(0,times=n - sum(nb)),rep(lambda,times=nb))

  P <- sqrt(lambda) * DD
  augm <- rep(0, nrow(P))
  conpen <- rep(0,nrow(constmat))
  
  model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)

  #mod2 = maxLik(likeli.asynorm,start=c(sd(y),model$coefficients),method="NR",tau=p,ycens=y,delta=delta,X=B,K=t(P)%*%P,control=list(tol=1e-6,reltol=1e-6))
  #a1 = mod2$estimate
  

  
  K=t(P)%*%P
  

  
  a1 = optim(c(sd(y),model$coefficients),likfun,tau=p,ycens=y,delta=delta,X=B,K=K)$par
  
  z1 = B%*%a1[-1]
  
# #   diffcon = -1
  # it.con = 1
  
  # while(any(diffcon < -1e-5) && it.con < 20)
  # {
    # it = 1
    # dw1 = 1
    
    # while(dw1 != 0 && it < 50) 
    # {
        # model <- lsfit(x=rbind(B,P,constmat*conpen), y=c(y, augm,0*conpen), wt=c(w1,(augm+1),1*(conpen>0)), intercept=FALSE)
        # a1 <- model$coefficients
       
        # z1 <- B %*%a1
        # w01 <- w1
        # #w1 <- as.vector(ifelse(y > z1, p, 1 - p))
        # w1[] = p
        # w1[!(y > z1)] = 1-p
        # dw1 <- sum(w1 != w01,na.rm=TRUE)     
        # it = it + 1
    # }
    # diffcon =  constmat %*% a1
    
    # if(any(diffcon < 0))
    # {
      # wc = which(diffcon < 0)
      # conpen[wc] = conpen[wc] + 100000
    # }
    # it.con = it.con + 1
  # }
  # diag.hat.ma1 <- hat(model$qr)[1:length(y)]  

  # if(it == 50)
     # warning("IWLS weights did not converge after 50 iterations.")
     
  list(a=a1[-1], fitted = z1,diag.hat.ma=hat(model$qr)[1:length(y)],sigma=a1[1],delta=delta,K=K)  
}



acv.aft <- function(penalty,yy,B,quantile,DD,nb,constmat,delta)
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
  aa <- asyregpen.aft(yy, B, quantile, abs(penalty), DD, nb, constmat,likeli.asynorm)

  #H = solve(t(B)%*%(aa$weight*B) + penalty*t(DD)%*%DD)
  ##H = B%*%H%*%t(B)
  ##H = diag(H)*aa$weight
  #H = apply(sqrt(aa$weight)*B,1,function(x){t(x)%*%H%*%x})

  #H = diag(diag(aa$diag.hat.ma))

  score = -(delta * log(dasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)) + (1-delta) * log(1-pasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)))/(1-aa$diag.hat.ma)^2

  mean(score[which(is.finite(score))],na.rm=TRUE)
}


aicfun.aft <- function(penalty,yy,delta,B,quantile,DD,nb,constmat,likfun)
{
  aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

#print(dim(B))
#print(length(aa$a1))

  score = -2*likfun(c(aa$sigma,aa$a),quantile,yy,delta,B,aa$K) + 2*(1+sum(aa$diag.hat.ma))

  score
}

bicfun.aft <- function(penalty,yy,delta,B,quantile,DD,nb,constmat,likfun)
{
  aa <- asyregpen.aft(yy,delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

  score = -2*likfun(c(aa$sigma,aa$a),quantile,yy,delta,B,aa$K) + log(length(yy))*(1+sum(aa$diag.hat.ma))

  score
}

cvgrid.aft <- function(yy,delta,B,quantile,DD,nb,constmat,types)
{
	las1 = seq(-2, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    score = rep(0, nrow(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))

    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = lambdas[i,]
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likeli.asynorm)

        score[i] =  mean(-(delta * log(dasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)) + (1-delta) * log(1-pasynorm(yy,B%*%aa$a,abs(aa$sigma)+0.0001,quantile)))/(1-aa$diag.hat.ma)^2,na.rm=T)
    
    }
    
    penalty[glatterms] = lambdas[which.min(score),]

    penalty
    
}



aicgrid.aft <- function(yy,delta,B,quantile,DD,nb,constmat,types,likfun)
{
	las1 = seq(-1, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    score = rep(0, nrow(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))

    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = unlist(lambdas[i,])
#print(penalty)
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

        score[i] =  -2*likfun(c(abs(aa$sigma),aa$a),quantile,yy,delta,B,aa$K) + 2*(1+sum(aa$diag.hat.ma))
    }
    
    penalty[glatterms] = lambdas[which.min(score),]

    penalty   
}


bicgrid.aft <- function(yy,delta,B,quantile,DD,nb,constmat,types,likfun)
{
	las1 = seq(-1, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    score = rep(0, nrow(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))

    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = unlist(lambdas[i,])
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat,likfun)

        score[i] =  -2*likfun(c(abs(aa$sigma),aa$a),quantile,yy,delta,B,aa$K) + log(length(yy))*(1+sum(aa$diag.hat.ma))
    }
    
    penalty[glatterms] = lambdas[which.min(score),]

    penalty   
}



lcurve.aft <- function(yy,delta,B,quantile,DD,nb,constmat,types)
{
	las1 = seq(-1, 4, by = .75)
	glatterms = which(types != "parametric")
#print(glatterms)
	#lambdas = matrix(las1,nrow=length(las1)*length(glatterms),ncol=1)
	#if(length(glatterms) > 1)
	#for(i in 2:length(glatterms))
    #  lambdas = cbind(lambdas,rep(las1,each=i,times=length(glatterms)-i+1))
     
     lambdas_list <- list()
    for(i in 1:length(glatterms)) {
        lambdas_list[[i]] <- las1
        }
    lambdas <- expand.grid(lambdas_list)
     
    omega = rep(0, nrow(lambdas))
    theta = matrix(NA,nrow(lambdas),ncol(lambdas))
    
    lambdas = 10^lambdas
    
    penalty = rep(0,length(types))
    #print(lambdas)
    for(i in 1:nrow(lambdas))
    {
    	penalty[glatterms] = unlist(lambdas[i,])
    	aa <- asyregpen.aft(yy, delta, B, quantile, abs(penalty), DD, nb, constmat)

        omega[i] =  sum(aa$delta*(yy-aa$fitted)^2)#sum(W*(vfits-Z)^2)
    
        for(j in 1:ncol(lambdas))
        {
        partbasis = (sum(nb[0:(glatterms[j]-1)])+1):(sum(nb[0:glatterms[j]]))
        if(all(B[,1] == 1))
          {
            partDD = DD[,-1,drop=FALSE][-1,,drop=FALSE][,partbasis,drop=FALSE]
            partaa = aa$a[-1][partbasis]
          }
          else
          {
            partDD = DD[,partbasis,drop=FALSE]
            partaa = aa$a[partbasis]
          }
       theta[i,j] = t(partaa) %*% t(partDD) %*% partDD %*% partaa
       }
    }
    
    omega = log(omega)
    theta = log(theta)

    deltas = 2*(sqrt(sum((theta[1,]-theta[2,])^2) + (omega[1]-omega[2])^2))

    for(i in 2:(nrow(lambdas)-1))
    {

      deltas[i] = sqrt(sum((theta[i,]-theta[i-1,])^2) + (omega[i]-omega[i-1])^2) #+ sqrt(sum((theta[i,]-theta[i+1,])^2) + (omega[i]-omega[i+1])^2)
    }
    deltas[nrow(lambdas)] = 2*(sqrt(sum((theta[i,]-theta[i-1,])^2) + (omega[i]-omega[i-1])^2))
   # print(deltas)
    deltas = sqrt(diff(omega)^2 + rowSums(diff(theta))^2)
  #print(deltas)  
  #print(which.min(deltas))
    penalty[glatterms] = lambdas[which.min(deltas)+1,]
    
    #aa <- asyregpen.aft(yy, B, quantile, abs(penalty), DD, nb, constmat)
    
    penalty
}

