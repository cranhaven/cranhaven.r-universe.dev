#----------------------------------------------------------------------------------#
# Package: flare                                                                   #
# flare.slim(): The user interface for slim()                                      #
# Author: Xingguo Li                                                               #
# Email: <xingguo.leo@gmail.com>                                                   #
# Date: Mar 16th 2014                                                              #
# Version: 1.2.0                                                                   #
#----------------------------------------------------------------------------------#

slim <- function(X,
                 Y_rachel,
                 Y_rachel_column = 1,
                 lambda = NULL,
                 nlambda = NULL,
                 lambda.min.value = NULL,
                 lambda.min.ratio = NULL,
                 rho = 1,
                 method="dantzig",
                 q = 2,
                 res.sd = FALSE,
                 prec = 1e-5,
                 max.ite = 1e5,
                 verbose = FALSE)
{
  if(method!="dantzig" && method!="lq" && method!="lasso"){
    stop("\"method\" must be dantzig, lasso or lq.\n")
  }
  if(method=="lq"){
    if(q<1 || q>2){
      stop("q must be in [1, 2] when method = \"lq\".\n")
    }
  }
  if(verbose) {
    message("Sparse Linear Regression with L1 Regularization.\n")
  }



  n = nrow(X)  #*****************************************
  d = ncol(X)  #****************************************

  if(n==0 || d==0) {
    stop("No data input.\n")
  }

  #rachel read below***************************
  maxdf = max(n,d)
  xm=matrix(rep(colMeans(X),n),nrow=n,ncol=d,byrow=T)
  x1=X-xm
  sdxinv=1/sqrt(colSums(x1^2)/(n-1))
  xx=x1*matrix(rep(sdxinv,n),nrow=n,ncol=d,byrow=T)
  ym=NA

  #y1=Y-ym
  Y_rachel_ncol = dim(Y_rachel)[2]
  y1 = Y_rachel * matrix(rep(sdxinv,Y_rachel_ncol),nrow=d,ncol=Y_rachel_ncol)

  #rachel read above***************************
  if(res.sd == TRUE){
    sdy=sqrt(sum(y1^2)/(n-1))
    yy=y1/sdy
  }else{
    sdy = 1
    yy = y1
  }
  intercept = FALSE

  if(intercept){
    xx = cbind(rep(1, nrow(xx)), xx)
    X = cbind(rep(1, nrow(X)), X)
    d = d+1
  }

  if(!is.null(lambda)) nlambda = length(lambda)
  if(is.null(lambda)){
    if(is.null(nlambda))
      nlambda = 5
    if(method=="dantzig"){
      if(intercept)
        lambda.max = max(abs(crossprod(xx[,2:d],yy/n)))
      else
        #lambda.max = max(abs(crossprod(xx,yy/n)))
        lambda.max = max(abs(yy/n))
    }

    if(method=="dantzig"){
      if(is.null(lambda.min.ratio)){
        lambda.min.ratio = 0.5
      }
      if(is.null(lambda.min.value)){
        lambda.min.value = lambda.min.ratio*lambda.max
      }
    }else{
      if(is.null(lambda.min.value)){
        lambda.min.value = sqrt(log(d)/n)
      }else{
        if(is.null(lambda.min.ratio)){
          lambda.min.ratio = lambda.min.value/lambda.max
        }
      }
    }
    if(lambda.max<lambda.min.value){
      lambda.max = 1
      lambda.min.value = 0.4
    }
    lambda = exp(seq(log(lambda.max), log(lambda.min.value), length = nlambda))
    rm(lambda.max,lambda.min.value,lambda.min.ratio)
    gc()
  }
  if(is.null(rho))
    rho = 1
  begt=Sys.time()

  yy_run = as.matrix(yy[,Y_rachel_column])

  if(method=="dantzig"){ # dantzig
    if(d>=n)

      out = slim.dantzig.ladm.scr(yy_run, xx, lambda, nlambda, n, d, maxdf, rho, max.ite, prec, intercept, verbose)
    else
      out = slim.dantzig.ladm.scr2(yy_run, xx, lambda, nlambda, n, d, maxdf, rho, max.ite, prec, intercept, verbose)
    q = "infty"
  }

  runt=Sys.time()-begt

  df=rep(0,nlambda)
  if(intercept){
    for(i in 1:nlambda)
      df[i] = sum(out$beta[[i]][2:d]!=0)
  }else{
    for(i in 1:nlambda)
      df[i] = sum(out$beta[[i]]!=0)
  }

  est = list()
  intcpt0=matrix(0,nrow=1,ncol=nlambda)
  intcpt=matrix(0,nrow=1,ncol=nlambda)
  if(intercept){
    beta1=matrix(0,nrow=d-1,ncol=nlambda)
    for(k in 1:nlambda){
      tmp.beta = out$beta[[k]][2:d]
      beta1[,k]=sdxinv*tmp.beta*sdy
      intcpt[k] = ym-as.numeric(xm[1,]%*%beta1[,k])+out$beta[[k]][1]*sdy
      intcpt0[k] = intcpt[k]
    }
  }else{
    beta1=matrix(0,nrow=d,ncol=nlambda)
    for(k in 1:nlambda){
      tmp.beta = out$beta[[k]]
      intcpt0[k] = 0
      beta1[,k] = sdxinv*tmp.beta*sdy
      intcpt[k] = ym-as.numeric(xm[1,]%*%beta1[,k])
    }
  }

  est$beta0 = out$beta
  est$beta = beta1
  est$intercept = intcpt
  est$intercept0 = intcpt0
  est$Y = NA
  est$X = X
  est$lambda = lambda
  est$nlambda = nlambda
  est$df = df
  est$method = method
  est$q = q
  est$ite =out$ite
  est$verbose = verbose
  est$runtime = runt
  class(est) = "slim"
  return(est)
}
