## It is based on the Matlab implementation by Laurens van der Maaten, 
## to carry out modifications to the qKernels and cnd kernels
## Yusen Zhang (yusenzhang@126.com), 2017/9/5



setGeneric("qkpca",function(x, ...) standardGeneric("qkpca"))

#########################################################################
setMethod("qkpca", signature(x = "formula"),
         function(x,  data = NULL, na.action = na.omit, ...)
{
    mt <- terms(x, data = data)
    if(attr(mt, "response") > 0) stop("response not allowed in formula")
    attr(mt, "intercept") <- 0
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$formula <- mf$x
    mf$... <- NULL
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    na.act <- attr(mf, "na.action")
    Terms <- attr(mf, "terms")
    x <- model.matrix(mt, mf)
    res <- qkpca(x, ...)
    ## fix up call to refer to the generic, but leave arg name as `formula'
    cl[[1]] <- as.name("qkpca")
    kcall(res) <- cl
    attr(Terms,"intercept") <- 0
    terms(res) <- Terms
    if(!is.null(na.act))
        n.action(res) <- na.act
    return(res)
  })

#########################################################################
## Matrix Interface
## input:  any matrix? qkernel or cnd kernel
## output: pcv based on qkernel or pcv based on cnd kernel
##
## qkpca(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), features = 0, th = 1e-4, na.action = na.omit)
## qkpca(x, kernel = "logbase", qpar = list(d = 2),features = 2)
setMethod("qkpca",signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), features = 0, th = 1e-4, na.action = na.omit, ...)
{
  x <- na.action(x)
  x <- as.matrix(x)
  m <- nrow(x)
  ret <- new("qkpca")

  if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
    {
      if(is(kernel,"function")) kernel <- deparse(substitute(kernel))       # if delete "function"???
      kernel <- do.call(kernel, qpar)

  }
  if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")
  if(is(kernel,"cndkernel")){
    ## Compute conditionally negative definite kernel matrix
    K <- cndkernmatrix(kernel,x)
  }
  else
    if (is(kernel,"qkernel")) K <- qkernmatrix(kernel, x)
  ## center qkernel matrix
  H <- t(t(K - colSums(K)/m) -  rowSums(K)/m) + sum(K)/m^2
  ## center onditionally negative definite kernel matrix
  H <- - H
  ## compute eigenvectors
  res <- eigen(H/m,symmetric=TRUE)              # if it should be /m??????????????
  if(features == 0)
    features <- sum(res$values > th)
  else
    if(res$values[features] < th)
      warning(paste("eigenvalues of the qkernel matrix are below threshold!"))

  pcv(ret) <- t(t(res$vectors[,1:features])/sqrt(res$values[1:features]))
  eVal(ret) <- res$values[1:features]
  names(eVal(ret)) <- paste("Comp.", 1:features, sep = "")
  rotated(ret) <- H %*% pcv(ret)
  kcall(ret) <- match.call()
  cndkernf(ret) <- kernel
  xmatrix(ret) <- x
  return(ret)
})
#########################################################################
## CND Kernel Matrix Interface

## input:  cnd kernel matrix??? cnd kernel
## output: pcv based on cnd kernel
## usage:
## cndkfunc <- logkbase(d=2)
## cndKtrain <- cndkernmatrix(cndkfunc, train)
## qkpca(x, features = 0, th = 1e-4)
setMethod("qkpca",signature(x="cndkernmatrix"),
         function(x, features = 0, th = 1e-4, ...)
{
  ret <- new("qkpca")
  m <- dim(x)[1]
  if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
  ## center cndkernel matrix
  H <- t(t(x - colSums(x)/m) -  rowSums(x)/m) + sum(x)/m^2
  ## center onditionally negative definite kernel matrix
  H <- - H
  ## compute eigenvectors
  res <- eigen(H/m,symmetric=TRUE)

  if(features == 0)
    features <- sum(res$values > th)
  else
    if(res$values[features] < th)
      warning(paste("eigenvalues of the cndkernel matrix are below threshold!"))

  pcv(ret) <- t(t(res$vectors[,1:features])/sqrt(res$values[1:features]))
  eVal(ret) <- res$values[1:features]
  names(eVal(ret)) <- paste("Comp.", 1:features, sep = "")
  rotated(ret) <- H %*% pcv(ret)
  kcall(ret) <- match.call()
  cndkernf(ret) <- "cndkernel"
  xmatrix(ret) <- x
  return(ret)
})
#########################################################################

## qkernel Matrix Interface
## input:  qkernel matrix which is the return of qkernmatrix()
## output: pcv based on qkernel

setMethod("qkpca",signature(x= "qkernmatrix"),
          function(x, features = 0, th = 1e-4, ...)
{
  ret <- new("qkpca")
  m <- dim(x)[1]
  if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
  ## center qkernel matrix
   H <- t(t(x - colSums(x)/m) -  rowSums(x)/m) + sum(x)/m^2
  ## center q - onditionally negative definite kernel matrix
   H <- - H
  ## compute eigenvectors
  res <- eigen(H/m,symmetric=TRUE)
  if(features == 0)
    features <- sum(res$values > th)
  else
    if(res$values[features] < th)
      warning(paste("eigenvalues of the qkernel matrix are below threshold!"))
  pcv(ret) <- t(t(res$vectors[,1:features])/sqrt(res$values[1:features]))
  eVal(ret) <- res$values[1:features]
  names(eVal(ret)) <- paste("Comp.", 1:features, sep = "")
  rotated(ret) <- H %*% pcv(ret)
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- "qkernel"
  return(ret)
})
#---------------------------------------------------------------------#

## project a new matrix into the feature space
setMethod("predict",signature(object="qkpca"),
function(object, x){
    if (!is.null(terms(object))){
        if(!is.matrix(x) || !is(x,"list"))
          x <- model.matrix(delete.response(terms(object)), as.data.frame(x), na.action = n.action(object))}
    else
      x  <- if (is.vector(x)) t(t(x)) else if (!is(x,"list")) x <- as.matrix(x)
    if (is.vector(x) || is.data.frame(x))
      x <- as.matrix(x)
    if (!is.matrix(x) && !is(x,"list")) stop("x must be a matrix a vector, a data frame, or a list")
    if(is(x,"matrix")){
        nr <- nrow(x)
        mr <- nrow(xmatrix(object))}
    else{
        nr <- length(x)
        mr <- length(xmatrix(object))}

    if(is.character(cndkernf(object)))
      {
        if(!is(x,"qkernmatrix") && !is(x,"cndkernmatrix")) stop("x must inherit from class 'qkernmatrix' or 'cndkernmatrix'")
        {
          ktest <- x
          ktrain <- xmatrix(object)
          }
       }
    else if (is(cndkernf(object),"qkernel"))
      {
        ktest <- qkernmatrix(cndkernf(object),x, xmatrix(object))
        ktrain <- qkernmatrix(cndkernf(object),xmatrix(object))
        }
    else if (is(cndkernf(object),"cndkernel"))
      {
        ktest <- cndkernmatrix(cndkernf(object),x, xmatrix(object))
        ktrain <- cndkernmatrix(cndkernf(object),xmatrix(object))
        }
    ## center
    ret <- t(t(ktest - rowSums(ktest)/mr) - rowSums(ktrain)/mr) + sum(ktrain)/(mr*nr)
    ## center conditionally negative definite kernel matrix
    ret <- - ret
    return(ret %*% pcv(object))
  })








