## qkernel functions



setClass("qkernel",representation("function",qpar = "list"))
#setClass("qkernel",representation("function",qpar="list"),contains="cndkernel")

rbfbase<- function(sigma=1,q=0.8)
  {

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(sigma <= 0)
      stop("sigma must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      sdist <- (1 - exp(log(q)*sdist/sigma))/(1-q)

      return(sdist)
      # return(exp(sigma*(2*crossprod(x,y) - crossprod(x) - crossprod(y))))

    }
  }
  return(new("rbfqkernel",.Data=rval,qpar=list(sigma=sigma, q=q)))

  }
setClass("rbfqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))



nonlbase<- function(alpha=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(alpha <= 0)
      stop("alpha must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")

      return((exp(-log(q)*alpha*crossprod(x))+exp(-log(q)*alpha*crossprod(y))-2*exp(-log(q)*alpha*crossprod(x,y)))/(1-q)/2 )

    }
  }
  return(new("nonlqkernel",.Data=rval,qpar=list(alpha=alpha, q=q)))
}
setClass("nonlqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

laplbase<- function(sigma=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(sigma <= 0)
      stop("sigma must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1 - exp(log(q)*sqrt(sdist)/sigma))/(1-q))

    }
  }
  return(new("laplqkernel",.Data=rval,qpar=list(sigma=sigma, q=q)))
}
setClass("laplqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

ratibase<- function(c=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(c <= 0)
      stop("c must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1 - exp(log(q)*sdist/(sdist+c)))/(1-q))

    }
  }
  return(new("ratiqkernel",.Data=rval,qpar=list(c=c, q=q)))
}
setClass("ratiqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

multbase<- function(c=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(c <= 0)
      stop("c must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((q^c - exp(log(q)*sqrt(sdist+c*c)))/(1-q))

    }
  }
  return(new("multqkernel",.Data=rval,qpar=list(c=c, q=q)))
}
setClass("multqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

invbase<- function(c=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(c <= 0)
      stop("c must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((exp(-log(q)/c) - exp(-log(q)/sqrt(sdist+c*c)))/(1-q))

    }
  }
  return(new("invqkernel",.Data=rval,qpar=list(c=c, q=q)))
}
setClass("invqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

wavbase<- function(theta=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(theta <= 0)
      stop("theta must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      if(identical(x, y)) return(matrix(0))
      else{
        sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
        ssdist<-sqrt(sdist)
        return((1/q - exp(-log(q)*theta*sin(ssdist/theta)/ssdist))/(1-q))
      }
    }
  }
  return(new("wavqkernel",.Data=rval,qpar=list(theta=theta, q=q)))
}
setClass("wavqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))


powbase<- function(d=2,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1 - exp(log(q)*sdist^(d/2)))/(1-q))

    }
  }
  return(new("powqkernel",.Data=rval,qpar=list(d=d, q=q)))
}
setClass("powqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

logbase<- function(d=2, q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1 - exp(log(q)*log(sdist^(d/2)+1)))/(1-q))

    }
  }
  return(new("logqkernel",.Data=rval,qpar=list(d=d, q=q)))
}
setClass("logqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

caubase<- function(sigma=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(sigma <= 0)
      stop("sigma must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1/q - exp(-log(q)/(1+sdist/sigma)))/(1-q))

    }
  }
  return(new("cauqkernel",.Data=rval,qpar=list(sigma=sigma, q=q)))
}
setClass("cauqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

chibase<- function(gamma=1,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(gamma <= 0)
      stop("gamma must be greater than zero")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")

      n=length(x)
      s=0
      for (i in 1:n) {
        if(x[i]+y[i]!=0)
          s=s+2*(x[i]-y[i])^2/(x[i]+y[i])
      }
      return((1 - exp(log(q)*gamma*s))/(1-q))

    }
  }
  return(new("chiqkernel",.Data=rval,qpar=list(gamma=gamma,q=q)))
}
setClass("chiqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))

studbase<- function(d=2,q=0.8)
{

  rval <- function(x,y=NULL)
  {
    if(q <= 0 | q >= 1)
      stop("q must be greater than zero and less than 1")
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")
    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(1)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return((1/q - exp(-log(q)/(1+sdist^(d/2))))/(1-q))

    }
  }
  return(new("studqkernel",.Data=rval,qpar=list(d=d, q=q)))
}
setClass("studqkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("qkernel"))


########################################################################################################


########################################################################################################
## show method for qkernel functions

setMethod("show",signature(object="qkernel"),
          function(object)
          {
            switch(class(object),
                   "rbfqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "nonlqkernel" = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"alpha = ", qpar(object)$alpha," q = ", qpar(object)$q,"\n")),
                   "laplqkernel" = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "ratiqkernel" = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"c = ", qpar(object)$c," q = ", qpar(object)$q,"\n")),
                   "multqkernel" = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"c = ", qpar(object)$c," q = ", qpar(object)$q,"\n")),
                   "invqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"c = ", qpar(object)$c," q = ", qpar(object)$q,"\n")),
                   "wavqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"theta = ", qpar(object)$theta," q = ", qpar(object)$q,"\n")),
                   "powqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$d," q = ", qpar(object)$q,"\n")),
                   "logqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$d," q = ", qpar(object)$q,"\n")),
                   "cauqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "chiqkernel"  = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"gamma = ", qpar(object)$gamma," q = ", qpar(object)$q,"\n")),
                   "studqkernel" = cat(paste("qbase qkernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$d," q = ", qpar(object)$q,"\n"))
                   )
          })

## create accesor function as in "S4 Classses in 15 pages more or less", well..

if (!isGeneric("qpar")){
  if (is.function(qpar))
    fun <- qpar
  else fun <- function(object) standardGeneric("qpar")
  setGeneric("qpar",fun)
}
setMethod("qpar","qkernel", function(object) object@qpar)


#-------------------------------------------------------------#

## Functions that return usefull qkernel calculations (qkernel matrix etc.)

## qkernmatrix function takes two or three arguments
qkernmatrix <- function(qkernel, x, y=NULL)
{

  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(x,"matrix")) stop("x must be a matrix")
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix")
  n <- nrow(x)
  res1 <- matrix(rep(0,n*n), ncol = n)
  if(is.null(y)){
    for(i in 1:n) {
      for(j in i:n) {
        res1[i,j]  <- qkernel(x[i,],x[j,])
      }
    }
    res1 <- res1 + t(res1)
    diag(res1) <- diag(res1)/2
  }
  if (is(y,"matrix")){
    m<-dim(y)[1]
    res1 <- matrix(0,dim(x)[1],dim(y)[1])
    for(i in 1:n) {
      for(j in 1:m) {
        res1[i,j] <- qkernel(x[i,],y[j,])
      }
    }
  }

  return(as.qkernmatrix(res1))
}

###############################################################################
setGeneric("qkernmatrix",function(qkernel, x, y = NULL) standardGeneric("qkernmatrix"))


qkernmatrix.rbfqkernel <- function(qkernel, x, y = NULL)
{
   sigma = qpar(qkernel)$sigma
   q = qpar(qkernel)$q
   if(q <= 0 | q >= 1)
     stop("q must be greater than zero and less than 1")
   if(sigma <= 0)
     stop("sigma must be greater than zero")
   res <- Eucdist(x, y, sEuclidean = F)
   res <- (1-exp(log(q)*res/sigma))/(1-q)
   return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="rbfqkernel"),qkernmatrix.rbfqkernel)

qkernmatrix.nonlqkernel <- function(qkernel, x, y = NULL)
{

  alpha = qpar(qkernel)$alpha
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(alpha <= 0)
    stop(" alpha must be greater than zero")

  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")

  if (is(x,"matrix") && is.null(y)){
    n <- dim(x)[1]
    res <- apply(x, 1, crossprod)
    res <- matrix(res, nrow=n, ncol=n)
    res <- exp(log(1/q)*alpha*res) + exp(log(1/q)*alpha*t(res)) - 2*exp(log(1/q)*alpha*tcrossprod(x))
    diag(res) <- 0
    return(as.qkernmatrix(0.5*res/(1-q)))
  }
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
    n <- dim(x)[1]
    m <- dim(y)[1]
    dotx <- apply(x, 1, crossprod)
    doty <- apply(y, 1, crossprod)
    dotx <- matrix(dotx, nrow=n, ncol=m)
    doty <- matrix(doty, nrow=m, ncol=n)
    res <- exp(log(1/q)*alpha*dotx) + exp(log(1/q)*alpha*t(doty)) - 2*exp(log(1/q)*alpha*tcrossprod(x,y))

    return(as.qkernmatrix(0.5*res/(1-q)))
  }


}

setMethod("qkernmatrix",signature(qkernel="nonlqkernel"),qkernmatrix.nonlqkernel)

qkernmatrix.laplqkernel <- function(qkernel, x, y = NULL)
{
  sigma = qpar(qkernel)$sigma
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(sigma <= 0)
    stop("sigma must be greater than zero")
  res <- Eucdist(x, y, sEuclidean = T)
  res <- (1-exp(log(q)*res/sigma))/(1-q)
  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="laplqkernel"),qkernmatrix.laplqkernel)

qkernmatrix.ratiqkernel <- function(qkernel, x, y = NULL)
{
  c = qpar(qkernel)$c
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(c <= 0)
    stop("c must be greater than zero")
  res <- Eucdist(x, y, sEuclidean = F)
  res <- (1 - exp(log(q)*res/(res+c)))/(1-q)

  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="ratiqkernel"),qkernmatrix.ratiqkernel)

qkernmatrix.multqkernel <- function(qkernel, x, y = NULL)
{
  c = qpar(qkernel)$c
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(c <= 0)
    stop("c must be greater than zero")
  res <- Eucdist(x, y, sEuclidean = F)
  res <- (q^c - exp(log(q)*sqrt(res+c*c)))/(1-q)

  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="multqkernel"),qkernmatrix.multqkernel)

qkernmatrix.invqkernel <- function(qkernel, x, y = NULL)
{
  c = qpar(qkernel)$c
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(c <= 0)
    stop("c must be greater than zero")
  res <- Eucdist(x, y, sEuclidean = F)
  res <- (exp(-log(q)/c) - exp(-log(q)/sqrt(res+c*c)))/(1-q)

  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="invqkernel"),qkernmatrix.invqkernel)

qkernmatrix.wavqkernel <- function(qkernel, x, y = NULL)
{
  theta = qpar(qkernel)$theta
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(theta <= 0)
    stop("theta must be greater than zero")
  res0 <- Eucdist(x, y, sEuclidean = T)
  res <- (1/q - exp(-log(q)*theta*sin(res0/theta)/res0))/(1-q)
  for (i in 1:nrow(res0)) {
    for (j in 1:ncol(res0)) {
      if(res0[i,j]==0)
        res[i,j]=0
    }
  }
  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="wavqkernel"),qkernmatrix.wavqkernel)

qkernmatrix.powqkernel <- function(qkernel, x, y = NULL)
{
  d = qpar(qkernel)$d
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = T)
  res <- (1 - exp(log(q)*res^d))/(1-q)

  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="powqkernel"),qkernmatrix.powqkernel)

qkernmatrix.logqkernel <- function(qkernel, x, y = NULL)
{
  d = qpar(qkernel)$d
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = T)
  res <- (1 - exp(log(q)*log(res^d+1)))/(1-q)

  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="logqkernel"),qkernmatrix.logqkernel)

qkernmatrix.cauqkernel <- function(qkernel, x, y = NULL)
{
  sigma = qpar(qkernel)$sigma
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(sigma <= 0)
    stop("sigma must be greater than zero")
  res <- Eucdist(x, y, sEuclidean = F)
  res <- (1/q - exp(-log(q)/(1+res/sigma)))/(1-q)
  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="cauqkernel"),qkernmatrix.cauqkernel)

qkernmatrix.chiqkernel <- function(qkernel, x, y = NULL)
{
  q = qpar(qkernel)$q
  gamma = qpar(qkernel)$gamma
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(gamma <= 0)
    stop("gamma must be greater than zero")
  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")

  if (is(x,"matrix") && is.null(y))
    y <- x
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
    n<-dim(x)[1]
    m<-dim(y)[1]
    res<-matrix(1,nrow=n,ncol=m)
    for (i in 1:n) {
      for(j in 1:m) {
        s=0
        for (k in 1:dim(x)[2]) {
          if(x[i,k]+y[j,k]!=0)
            s=s+2*(x[i,k]-y[j,k])^2/(x[i,k]+y[j,k])
        }
        res[i,j]<-(1 - exp(log(q)*gamma*s))/(1-q)
      }
    }
    return(as.qkernmatrix(res))
  }
}
setMethod("qkernmatrix",signature(qkernel="chiqkernel"),qkernmatrix.chiqkernel)

qkernmatrix.studqkernel <- function(qkernel, x, y = NULL)
{
  d = qpar(qkernel)$d
  q = qpar(qkernel)$q
  if(q <= 0 | q >= 1)
    stop("q must be greater than zero and less than 1")
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = T)
  res <- (1/q - exp(-log(q)/(1+res^d)))/(1-q)
  return(as.qkernmatrix(res))
}

setMethod("qkernmatrix",signature(qkernel="studqkernel"),qkernmatrix.studqkernel)

#######################################################################################






