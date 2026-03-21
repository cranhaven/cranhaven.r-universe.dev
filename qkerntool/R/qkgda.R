## qkgda function
##
##Reference:
## 1. Baudat, G, and F. Anouar. "Generalized discriminant analysis using
##    a kernel approach." Neural Computation 12.10(2000):2385.
## 2. Deng Cai, Xiaofei He, and Jiawei Han. "Speed Up Kernel Discriminant
##    Analysis", The VLDB Journal, vol. 20, no. 1, pp. 21-33, January, 2011.
##
## Yusen Zhang (yusenzhang@126.com), 2017/9/5

setGeneric("qkgda",function(x, ...) standardGeneric("qkgda"))


#########################################################################
## Matrix Interface
setMethod("qkgda",signature(x="matrix"),
          function(x, label, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), features = 0, th = 1e-4, na.action = na.omit, ...)
{
  x <- na.action(x)
  x <- as.matrix(x)
  m <- nrow(x)
  ret <- new("qkgda")

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
    if (is(kernel,"qkernel")){K <- qkernmatrix(kernel, x)}

  nClass=length(unique(label))
  if(m!= length(label))
    stop("The categorical data must be assigned to one of the categories")
  if(length(setdiff(label,1:nClass))>0.5)
    stop("The categorical variables must be coded as 1, 2, ..., K!")

  ## center qkernel matrix
  H <- t(t(K - colSums(K)/m) -  rowSums(K)/m) + sum(K)/m^2
  ## center onditionally negative definite kernel matrix
  H <- - H

  ## Perform eigenvalue analysis (H = eU * eVal * eU')
  temp <- eigen(H,symmetric=TRUE)

  eVec <- temp$vectors
  eVal <- temp$values

  if(features == 0)
    features <- sum(eVal > th)
  else
    if(eVal[features] < th)
      warning(paste("Eigenvalues of the qkernel matrix are below threshold!"))

  ## Remove eigenvectors with zero

  ind=which(eVal > th)
  eVal = eVal[ind]
  eVec = eVec[,ind]
  rankH = length(ind)

  ## Recompute H matrix with only the highest eigen values/vectors
  H = eVec %*% diag(eVal) %*% t(eVec)
  ## Construct diagonal block matrix W
  blk.list = NULL
  for (i in 1:nClass){
    num_data_class = length(which(label == i))
    blk.list[[i]] = matrix(1/num_data_class, num_data_class,num_data_class)
  }
  blk<- blkdiag(blk.list)

  ## Determine target dimensionality of data
  features = pmin(features, rankH, nClass)

  ## Perform eigendecomposition of matrix (t(P) * W * P)

  KK= t(eVec) %*% blk %*% eVec
  KK=pmax(KK,t(KK))
  res <- eigen(KK)
  Beta <- res$vectors
  lambda <- res$values
  lambda <- diag(lambda)

  Beta <- Beta[, 1:features]
  ## projection on eigenvector
  mappedX <- eVec %*% diag(1/eVal) %*% Beta
  ##  Normalization of vectors mappedX
  for (i in 1:features){
    mappedX[,i] <- mappedX[,i]/as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i]))
    #  print(as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i])))
  }
  prj(ret) <- mappedX
  eVal(ret) <- lambda
  eVec(ret) <- Beta
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- kernel
  return(ret)
})
#########################################################################
## CND Kernel Matrix Interface
setMethod("qkgda",signature(x="cndkernmatrix"),
          function(x, label, features = 0, th = 1e-4, na.action = na.omit, ...)
{

  m <- nrow(x)
  ret <- new("qkgda")

  if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")


  nClass=length(unique(label))
  if(m!= length(label))
    stop("The categorical data must be assigned to one of the categories")
  if(length(setdiff(label,1:nClass))>0.5)
    stop("The categorical response must be coded as 1, 2, ..., K!")

  ## center cndkernel matrix
  H <- t(t(x - colSums(x)/m) -  rowSums(x)/m) + sum(x)/m^2
  ## center onditionally negative definite kernel matrix
  H <- - H
  ## Perform eigenvalue analysis (H = eU * eVal * eU')
  temp <- eigen(H,symmetric=TRUE)
  eVec <- temp$vectors
  eVal <- temp$values

  if(features == 0)
    features <- sum(eVal > th)
  else
    if(eVal[features] < th)
      warning(paste("Eigenvalues of the qkernel matrix are below threshold!"))

  ## Remove eigenvectors with zero

  ind=which(eVal > th)
  eVal = eVal[ind]
  eVec = eVec[,ind]
  rankH = length(ind)

  ## Recompute H matrix with only the highest eigen values/vectors
  H = eVec %*% diag(eVal) %*% t(eVec)
  ## Construct diagonal block matrix W
  blk.list = NULL
  for (i in 1:nClass){
    num_data_class = length(which(label == i))
    blk.list[[i]] = matrix(1/num_data_class, num_data_class,num_data_class)
  }
  blk<- blkdiag(blk.list)

  ## Determine target dimensionality of data
  features = pmin(features, rankH, nClass)

  ## Perform eigendecomposition of matrix (t(P) * W * P)

  KK= t(eVec) %*% blk %*% eVec
  KK=pmax(KK,t(KK))
  res <- eigen(KK)
  Beta <- res$vectors
  lambda <- res$values
  lambda <- diag(lambda)

  Beta <- Beta[, 1:features]
  ## projection on eigenvector
  mappedX <- eVec %*% diag(1/eVal) %*% Beta
  ##  Normalization of vectors mappedX
  for (i in 1:features){
    mappedX[,i] <- mappedX[,i]/as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i]))
    #  print(as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i])))
  }
  prj(ret) <- mappedX
  eVal(ret) <- lambda
  eVec(ret) <- Beta
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- "cndkernel"
  return(ret)
})

#########################################################################

## qkernel Matrix Interface
setMethod("qkgda",signature(x= "qkernmatrix"),
          function(x, label, features = 0, th = 1e-4, ...)
{
  ret <- new("qkgda")
  m <- dim(x)[1]
  if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")

  nClass=length(unique(label))
  if(m!= length(label))
    stop("The categorical data must be assigned to one of the categories")
  if(length(setdiff(label,1:nClass))>0.5)
    stop("The categorical response must be coded as 1, 2, ..., K!")

  ## center qkernel matrix
  H <- t(t(x - colSums(x)/m) -  rowSums(x)/m) + sum(x)/m^2
  ## center onditionally negative definite kernel matrix
  H <- - H
  ## Perform eigenvalue analysis (H = eU * eVal * eU')
  temp <- eigen(H,symmetric=TRUE)
  eVec <- temp$vectors
  eVal <- temp$values

  if(features == 0)
    features <- sum(eVal > th)
  else
    if(eVal[features] < th)
      warning(paste("Eigenvalues of the qkernel matrix are below threshold!"))

  ## Remove eigenvectors with zero

  ind=which(eVal > th)
  eVal = eVal[ind]
  eVec = eVec[,ind]
  rankH = length(ind)

  ## Recompute H matrix with only the highest eigen values/vectors
  H = eVec %*% diag(eVal) %*% t(eVec)
  ## Construct diagonal block matrix W
  blk.list = NULL
  for (i in 1:nClass){
    num_data_class = length(which(label == i))
    blk.list[[i]] = matrix(1/num_data_class, num_data_class,num_data_class)
  }
  blk<- blkdiag(blk.list)

  ## Determine target dimensionality of data
  features = pmin(features, rankH, nClass)

  ## Perform eigendecomposition of matrix (t(P) * W * P)
  KK= t(eVec) %*% blk %*% eVec
  KK=pmax(KK,t(KK))
  res <- eigen(KK)
  Beta <- res$vectors
  lambda <- res$values
  lambda <- diag(lambda)

  Beta <- Beta[, 1:features]
  ## projection on eigenvector
  mappedX <- eVec %*% diag(1/eVal) %*% Beta
  ##  Normalization of vectors mappedX
  for (i in 1:features){
    mappedX[,i] <- mappedX[,i]/as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i]))
  #  print(as.vector(sqrt(t(mappedX[,i]) %*% H %*% mappedX[,i])))
  }

  prj(ret) <- mappedX
  eVal(ret) <- lambda
  eVec(ret) <- Beta
  kcall(ret) <- match.call()
  xmatrix(ret) <- x
  cndkernf(ret) <- "qkernel"
  return(ret)
})
#########################################################################

## project a new matrix into the feature space
setMethod("predict",signature(object="qkgda"),
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
    return(ret %*% prj(object))
  })






