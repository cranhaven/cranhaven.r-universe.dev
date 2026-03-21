## CND kernel functions
## Functions for computing a cndkernel value, matrix, matrix-vector
## product and quadratic form
##
## author : yusen zhang

########################################################################################################
## Define the qkernel objects,
## functions with an additional slot for the qkernel parameter list.
## qkernel functions take two vector arguments and return a scalar (dot product)

setClass("cndkernel",representation("function",qpar = "list"))



########################################################################################################


nonlcnd<- function(alpha = 1) # alpha > 0
{

  rval <- function(x,y=NULL)
  {
    if(alpha <= 0)
      stop(" alpha must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      return((exp(alpha*crossprod(x))+exp(alpha*crossprod(y))-2*exp(alpha*crossprod(x,y)))/2)
    }
  }
  return(new("nonlkernel",.Data=rval,qpar=list(alpha = alpha)))
}
setClass("nonlkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))


polycnd<- function(d = 2, alpha = 1, c = 1) # d > 0, alpha > 0, c > 0
{

  rval <- function(x,y=NULL)
  {
    if(d <= 0)
      stop(" d must be greater than zero ")
    if(alpha <= 0)
      stop(" alpha must be greater than zero ")
    if(c <= 0)
      stop(" c must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      return(((alpha*crossprod(x)+c)^d+(alpha*crossprod(y)+c)^d-2*(alpha*crossprod(x,y)+c)^d)/2)
    }
  }
  return(new("polykernel",.Data=rval,qpar=list(d = d, alpha = alpha, c = c)))
}
setClass("polykernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

rbfcnd<- function(gamma = 1) # gamma > 0
{

  rval <- function(x,y=NULL)
  {
    if(gamma <= 0)
      stop(" gamma must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(1-exp(-sdist/gamma))
    }
  }
  return(new("rbfkernel",.Data=rval,qpar=list(gamma = gamma)))
}
setClass("rbfkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

laplcnd<- function(gamma = 1) # gamma > 0
{

  rval <- function(x,y=NULL)
  {
    if(gamma <= 0)
      stop(" gamma must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(1-exp(-sqrt(sdist)/gamma))
    }
  }
  return(new("laplkernel",.Data=rval,qpar=list(gamma = gamma)))
}
setClass("laplkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

anocnd<- function(d = 2, sigma = 1) # 0 < d <= 2, sigma > 0
{

  rval <- function(x,y=NULL)
  {
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")
    if(sigma <= 0)
      stop(" sigma must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      n=dim(x)[2]
      res<-sum(exp(- sigma * (x - y)^2))
      fres<-n-(res)^d
      return(fres)
    }
  }
  return(new("anokernel",.Data=rval,qpar=list(d = d, sigma = sigma)))
}
setClass("anokernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))


raticnd<- function(c = 1) # c > 0
{

  rval <- function(x,y=NULL)
  {
    if(c <= 0)
      stop(" c must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(sdist/(sdist+c))
    }
  }
  return(new("ratikernel",.Data=rval,qpar=list(c = c)))
}
setClass("ratikernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

multcnd<- function(c = 1) # c > 0
{

  rval <- function(x,y=NULL)
  {
    if(c <= 0)
      stop(" c must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(sqrt(sdist+c^2)-c)
    }
  }
  return(new("multkernel",.Data=rval,qpar=list(c = c)))
}
setClass("multkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

invcnd<- function(c = 1) # c > 0
{

  rval <- function(x,y=NULL)
  {
    if(c <= 0)
      stop(" c must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(1/c-1/sqrt(sdist+c^2))
    }
  }
  return(new("invkernel",.Data=rval,qpar=list(c = c)))
}
setClass("invkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

wavcnd<- function(theta = 1) # theta > 0
{

  rval <- function(x,y=NULL)
  {
    if(theta <= 0)
      stop(" theta must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      if(identical(x, y)) return(matrix(0))
      else{
        sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
        return(1-theta*sin(sqrt(sdist)/theta)/sqrt(sdist))
      }
    }
  }
  return(new("wavkernel",.Data=rval,qpar=list(theta = theta)))
}
setClass("wavkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))


powcnd<- function(d = 2) # 0 < d <= 2
{

  rval <- function(x,y=NULL)
  {
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(sdist^(d/2))
    }
  }
  return(new("powkernel",.Data=rval,qpar=list(d = d)))
}
setClass("powkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

logcnd<- function(d = 2) # 0 < d <= 2
{

  rval <- function(x,y=NULL)
  {
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(log(1 + sdist^(d/2)))
    }
  }
  return(new("logkernel",.Data=rval,qpar=list(d = d)))
}
setClass("logkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))



caucnd<- function(gamma = 1) # gamma > 0
{

  rval <- function(x,y=NULL)
  {
    if(gamma <= 0)
      stop(" gamma must be greater than zero ")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(1-1/(1+sdist/gamma))
    }
  }
  return(new("caukernel",.Data=rval,qpar=list(gamma = gamma)))
}
setClass("caukernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

chicnd<- function()
{

  rval <- function(x,y=NULL)
  {

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
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
      return(s)
    }
  }
  return(new("chikernel",.Data=rval,qpar=list()))
}
setClass("chikernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))


studcnd<- function(d = 2) # 0 < d <= 2
{

  rval <- function(x,y=NULL)
  {
    if(d <= 0 | d > 2)
      stop(" d must be greater than zero and less than or equal to 2")

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      sdist <- crossprod(x) + crossprod(y) - 2*crossprod(x,y)
      return(1-1/(1+sdist^(d/2)))
    }
  }
  return(new("studkernel",.Data=rval,qpar=list(d = d)))
}
setClass("studkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))



norcnd<- function()
{

  rval <- function(x,y=NULL)
  {

    if(!is(x,"vector")) stop("x must be a vector")
    if(!is(y,"vector")&&!is.null(y)) stop("y must a vector")
    if (is(x,"vector") && is.null(y)){
      return(0)
    }
    if (is(x,"vector") && is(y,"vector")){
      if (!length(x)==length(y))
        stop("number of dimension must be the same on both data points")
      s = Eucdist(x,y)
      return(s)
    }
  }
  return(new("norkernel",.Data=rval,qpar=list()))
}
setClass("norkernel",prototype=structure(.Data=function(){},qpar=list()),contains=c("cndkernel"))

#-----------------------------------------------------------------#




#------------------------------------------------------------------#  d = qpar(cndkernel)$d

## show method for cndkernel functions

setMethod("show",signature(object="cndkernel"),
          function(object)
          {
            switch(class(object),
                   "nonlcndkernel" = cat(paste("Non Linear cnd kernel function.", "\n","Hyperparameter :" ,"alpha = ", qpar(object)$alpha,"\n")),
                   "polycndkernel" = cat(paste("Polynomial cnd kernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$d," alpha = ", qpar(object)$alpha," c = ", qpar(object)$c,"\n")),
                   "rbfcndkernel"  = cat(paste("Gaussian kernel cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "laplcndkernel" = cat(paste("Laplacian cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "anocndkernel"  = cat(paste("ANOVA cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "raticndkernel" = cat(paste("Rational Quadratic cnd kernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "multcndkernel" = cat(paste("Multiquadric cnd kernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "invcndkernel"  = cat(paste("Inverse Multiquadric cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma,"\n")),

                   "wavcndkernel"  = cat(paste("Wave cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "powcndkernel"  = cat(paste("d cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "logcndkernel"  = cat(paste("Log cnd kernel function.", "\n","Hyperparameter :" ,"sigma = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "caucndkernel"  = cat(paste("Cauchy cnd kernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "chicndkernel"  = cat(paste("Chi-Square cnd kernel function.", "\n","Hyperparameter :" ,"d = ", qpar(object)$sigma," q = ", qpar(object)$q,"\n")),
                   "studcndkernel" = cat(paste("Generalized T-Student cnd kernel function.", "\n","Hyperparameters :","degree = ",qpar(object)$degree," scale = ", qpar(object)$scale," offset = ", qpar(object)$offset,"\n")),
                   "norkernel"  = cat(paste("Normal cnd kernel function.", "\n","Hyperparameter :" ,"\n"))

                   )
          })


#if (!isGeneric("qpar")){
#  if (is.function(qpar))
#    fun <- qpar
#  else fun <- function(object) standardGeneric("qpar")
#  setGeneric("qpar",fun)
#}

setMethod("qpar","cndkernel", function(object) object@qpar)
#########################################################################################


## Functions that return usefull cndkernel calculations (cndkernel matrix etc.)

## cndkernmatrix function takes two or three arguments

cndkernmatrix <- function(cndkernel, x, y=NULL)
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
        res1[i,j]  <- cndkernel(x[i,],x[j,])
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
        res1[i,j] <- cndkernel(x[i,],y[j,])
      }
    }
  }

  return(as.cndkernmatrix(res1))
}
###############################################################################
setGeneric("cndkernmatrix",function(cndkernel, x, y = NULL) standardGeneric("cndkernmatrix"))


cndkernmatrix.nonlkernel <- function(cndkernel, x, y = NULL) # alpha > 0
{
  alpha = qpar(cndkernel)$alpha
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
    res <- exp(alpha*res) + exp(alpha*t(res)) - 2*exp(alpha*tcrossprod(x))
    diag(res) <- 0
    return(as.cndkernmatrix(0.5*res))
  }
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
  n<-dim(x)[1]
  m<-dim(y)[1]
  dotx <- apply(x, 1, crossprod)
  doty <- apply(y, 1, crossprod)
  dotx <- matrix(dotx, nrow=n, ncol=m)
  doty <- matrix(doty, nrow=m, ncol=n)
  res <- exp(alpha*dotx) + exp(alpha*t(doty)) - 2*exp(alpha*tcrossprod(x,y))

  return(as.cndkernmatrix(0.5*res))

  }
}
setMethod("cndkernmatrix",signature(cndkernel="nonlkernel"),cndkernmatrix.nonlkernel)

cndkernmatrix.polykernel <- function(cndkernel, x, y = NULL) # d > 0, alpha > 0, c > 0
{
  d = qpar(cndkernel)$d
  alpha = qpar(cndkernel)$alpha
  c = qpar(cndkernel)$c
  if(d <= 0)
    stop(" d must be greater than zero ")
  if(alpha <= 0)
    stop(" alpha must be greater than zero ")
  if(c <= 0)
    stop(" c must be greater than zero ")

  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")
  if (is(x,"matrix") && is.null(y)){
    n <- dim(x)[1]
    res <- apply(x, 1, crossprod)
    res <- matrix(res, nrow=n, ncol=n)
    res <- (alpha*res+c)^d + (alpha*t(res)+c)^d - 2*(alpha*tcrossprod(x)+c)^d
    diag(res) <- 0
    return(as.cndkernmatrix(0.5*res))
  }
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
  n<-dim(x)[1]
  m<-dim(y)[1]
  dotx <- apply(x, 1, crossprod)
  doty <- apply(y, 1, crossprod)
  dotx <- matrix(dotx, nrow=n, ncol=m)
  doty <- matrix(doty, nrow=m, ncol=n)
  res <- (alpha*dotx+c)^d + (alpha*t(doty)+c)^d - 2*(alpha*tcrossprod(x,y)+c)^d

  return(as.cndkernmatrix(0.5*res))
  }
}
setMethod("cndkernmatrix",signature(cndkernel="polykernel"),cndkernmatrix.polykernel)

cndkernmatrix.rbfkernel <- function(cndkernel, x, y = NULL) # gamma > 0
{
  gamma = qpar(cndkernel)$gamma
  if(gamma <= 0)
    stop(" gamma must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = F)
  res <- 1-exp(- res/gamma)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="rbfkernel"),cndkernmatrix.rbfkernel)

cndkernmatrix.laplkernel <- function(cndkernel, x, y = NULL) # gamma > 0
{

  gamma = qpar(cndkernel)$gamma
  if(gamma <= 0)
    stop(" gamma must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = TRUE)
  res <- 1-exp(- res/gamma)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="laplkernel"),cndkernmatrix.laplkernel)

cndkernmatrix.anokernel <- function(cndkernel, x, y = NULL) # 0 < d <= 2, sigma > 0
{
  d = qpar(cndkernel)$d
  sigma = qpar(cndkernel)$sigma
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  if(sigma <= 0)
    stop(" sigma must be greater than zero ")

  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")
  n <- dim(x)[1]
  n1 <- dim(x)[2]
  if(is(x,"matrix") && is.null(y)){
    a <- matrix(0,  dim(x)[2], n)
    res <- matrix(0, n ,n)
    for (i in 1:n)
    {
      a[rep(TRUE,n1), rep(TRUE,n)] <- x[i,]
      res[i,]<- n1 - colSums(exp( - sigma*(a - t(x))^2))^d
    }
    return(as.cndkernmatrix(res))
  }
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2]))
      stop("matrixes must have the same number of columns")

    m <- dim(y)[1]
    b <- matrix(0, dim(x)[2],m)
    res <- matrix(0, dim(x)[1],m)
    for( i in 1:n)
    {
      b[rep(TRUE,n1), rep(TRUE,m)] <- x[i,]
      res[i,]<- n1 - colSums(exp( - sigma*(b - t(y))^2))^d
    }
    return(as.cndkernmatrix(res))
  }
  #if (is(x,"matrix") && is(y,"matrix")){
  #  if (!(dim(x)[2]==dim(y)[2])) stop("matrixes must have the same number of columns")
  #n<-dim(x)[2]
  #n1<-dim(x)[1]
  #m1<-dim(y)[1]
  #fres<-matrix(1,nrow=n1,ncol=m1)
  #for (i in 1:n1) {
  #  for(j in 1:m1) {
  #      res<-sum(exp(- sigma * (x[i, ] - y[j, ])^2))
  #      fres[i,j]<-n-(res)^d
  #  }
  #}
  #return(as.cndkernmatrix(fres))
  #}
}
setMethod("cndkernmatrix",signature(cndkernel="anokernel"),cndkernmatrix.anokernel)

cndkernmatrix.ratikernel <- function(cndkernel, x, y = NULL) # c > 0
{
  c = qpar(cndkernel)$c
  if(c <= 0)
    stop(" c must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = F)
  res <- res/(res+c)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="ratikernel"),cndkernmatrix.ratikernel)

cndkernmatrix.multkernel <- function(cndkernel, x, y = NULL) # c > 0
{
  c = qpar(cndkernel)$c
  if(c <= 0)
    stop(" c must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = F)
  res <- sqrt(res+c^2)-c

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="multkernel"),cndkernmatrix.multkernel)

cndkernmatrix.invkernel <- function(cndkernel, x, y = NULL) # c > 0
{
  c = qpar(cndkernel)$c
  if(c <= 0)
    stop(" c must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = F)
  res <- 1/c-1/sqrt(res+c^2)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="invkernel"),cndkernmatrix.invkernel)

cndkernmatrix.wavkernel <- function(cndkernel, x, y = NULL) # theta > 0
{
  theta = qpar(cndkernel)$theta
  if(theta <= 0)
    stop(" theta must be greater than zero ")

  res0 <- Eucdist(x, y, sEuclidean = T)
  res <- 1-theta*sin(res0/theta)/res0
  for (i in 1:nrow(res0)) {
    for (j in 1:ncol(res0)) {
      if(res0[i,j]==0)
        res[i,j]=0
    }
  }
  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="wavkernel"),cndkernmatrix.wavkernel)

cndkernmatrix.powkernel <- function(cndkernel, x, y = NULL) # 0 < d <= 2
{
  d = qpar(cndkernel)$d
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = TRUE)
  res <- res^d

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="powkernel"),cndkernmatrix.powkernel)

cndkernmatrix.logkernel <- function(cndkernel, x, y = NULL) # 0 < d <= 2
{
  d = qpar(cndkernel)$d
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = TRUE)
  res <- log(1+res^d)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="logkernel"),cndkernmatrix.logkernel)

cndkernmatrix.caukernel <- function(cndkernel, x, y = NULL) # gamma > 0
{
  gamma = qpar(cndkernel)$gamma
  if(gamma <= 0)
    stop(" gamma must be greater than zero ")

  res <- Eucdist(x, y, sEuclidean = F)
  res <- 1-1/(1+res/gamma)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="caukernel"),cndkernmatrix.caukernel)

cndkernmatrix.chikernel <- function(cndkernel, x, y = NULL)
{
  if(is(x,"vector"))
    x <- as.matrix(x)
  if(is(y,"vector"))
    y <- as.matrix(y)
  if(!is(y,"matrix")&&!is.null(y)) stop("y must be a matrix or a vector")

  n <- dim(x)[1]
  n1 <- dim(x)[2]
  if (is(x,"matrix") && is.null(y))
    y <- x
  if (is(x,"matrix") && is(y,"matrix")){
    if (!(dim(x)[2]==dim(y)[2]))
      stop("matrixes must have the same number of columns")

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
        res[i,j] <- s
      }
    }
    return(as.cndkernmatrix(res))
  }
}
setMethod("cndkernmatrix",signature(cndkernel="chikernel"),cndkernmatrix.chikernel)

cndkernmatrix.studkernel <- function(cndkernel, x, y = NULL) # 0 < d <= 2
{
  d = qpar(cndkernel)$d
  if(d <= 0 | d > 2)
    stop(" d must be greater than zero and less than or equal to 2")
  res <- Eucdist(x, y, sEuclidean = TRUE)
  res <- 1-1/(1+res^d)

  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="studkernel"),cndkernmatrix.studkernel)



cndkernmatrix.norkernel <- function(cndkernel, x, y = NULL)
{
  res <- Eucdist(x,y)
  return(as.cndkernmatrix(res))
}
setMethod("cndkernmatrix",signature(cndkernel="norkernel"),cndkernmatrix.norkernel)



