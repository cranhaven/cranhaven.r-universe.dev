

## Wraps around function in package speccalt, 
## to carry out modifications to the qKernels and cnd kernels


setGeneric("qkspecc",function(x, ...) standardGeneric("qkspecc"))

setMethod("qkspecc", signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 2, q = 0.9), Nocent=NA, normalize="symmetric", maxk=20, iterations=200, na.action = na.omit, ...)
{

	x <- na.action(x)
  x <- as.matrix(x)

  ret <- new("qkspecc")
  if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
   {
    if(is(kernel,"function")) kernel <- deparse(substitute(kernel))       # if delete "function"???
       kernel <- do.call(kernel, qpar)
  }
  if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")
  if(is(kernel,"cndkernel")){
  ## Compute conditionally negative definite kernel matrix
    cndK <- cndkernmatrix(kernel,x)
  }
  else
    if (is(kernel,"qkernel")) cndK <- qkernmatrix(kernel, x)

  cndK <- 1/(1+cndK)

  nelts <- dim(cndK)[1]
	diag(cndK) <- 0 # "zeroed" according to Zelnik-Manor and Perona 2004
	deg <- sapply(1:nelts, function(i) { # degree matrix
		return(sum(cndK[i,]))
	})

	# hack : one "zero-based" (see "Lrw" in von Luxburg (2006)) for Bartlett procedure,
	# the other for the actual clustering.
	# Lsym= W-D
	if(normalize=="none")	L <- diag(deg) - cndK

	if(normalize=="symmetric") L <- diag(nelts) - diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))

	if(normalize=="random-walk") L <- diag(nelts) - diag(1/deg) %*% cndK

	#L <- diag(deg) - cndK
	#Lsym <- diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))
	#Lrw <- diag(nelts) - diag(1/deg) %*% cndK

	eig <- eigen(L)
	ind <- order(eig$values)
	eVals <- eig$values[ind]
	eVecs <- eig$vectors[ ,ind]
	Y <- eVecs[,1:Nocent]
	if(normalize=="symmetric"){
	  sumsY <- rowSums(Y^2)
	  Y <- t(sapply(1:nelts, function(i) Y[i,] / sqrt(sumsY[i])))
	}


	#	cent <- matrix(unlist(lapply(1:Nocent,ll<- function(Lsym){colMeans(x[which(res$cluster==Lsym), ,drop=FALSE])})),ncol=dim(x)[2], byrow=TRUE)
	#	withss <- unlist(lapply(1:Nocent,ll<- function(Lsym){sum((x[which(res$cluster==Lsym),, drop=FALSE] - cent[Lsym,])^2)}))

	res <- kmeans(Y, Nocent, iterations)
	clust(ret) <- res$cluster

	eVec(ret) <- eVecs
	eVal(ret) <- eVals
	ymatrix(ret) <- Y
	kcall(ret) <- match.call()
	xmatrix(ret) <- x
	cndkernf(ret) <- kernel
	return(ret)
})
#------------------------------------------------------------------------------------------------#
setMethod("qkspecc",signature(x = "cndkernmatrix"),
          function(x, Nocent=NA, normalize="symmetric", maxk=20, iterations=200, ...)
{

  ret <- new("qkspecc")
  if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
  cndK <- 1/(1+x)
  nelts <- dim(cndK)[1]
	diag(cndK) <- 0 # "zeroed" according to Zelnik-Manor and Perona 2004
	deg <- sapply(1:nelts, function(i) { # degree matrix
		return(sum(cndK[i,]))
	})

	# hack : one "zero-based" (see "Lrw" in von Luxburg (2006)) for Bartlett procedure,
	# the other for the actual clustering.
	if(normalize=="none")	L <- diag(deg) - cndK

	if(normalize=="symmetric") L <- diag(nelts) - diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))

	if(normalize=="random-walk") L <- diag(nelts) - diag(1/deg) %*% cndK

	#L <- diag(deg) - cndK
	#Lsym <- diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))
	#Lrw <- diag(nelts) - diag(1/deg) %*% cndK

	eig <- eigen(L)
	ind <- order(eig$values)
	eVals <- eig$values[ind]
	eVecs <- eig$vectors[ ,ind]
	Y <- eVecs[,1:Nocent]
	if(normalize=="symmetric"){
	  sumsY <- rowSums(Y^2)
	  Y <- t(sapply(1:nelts, function(i) Y[i,] / sqrt(sumsY[i])))
	}
	res <- kmeans(Y, Nocent, iterations)
	clust(ret) <- res$cluster

	eVec(ret) <- eVecs
	eVal(ret) <- eVals
	ymatrix(ret) <- Y
	kcall(ret) <- match.call()
	xmatrix(ret) <- x
	cndkernf(ret) <- "cndkernel"
	return(ret)
})


#------------------------------------------------------------------------------------------------#
setMethod("qkspecc",signature(x = "qkernmatrix"),
          function(x, Nocent=NA, normalize="symmetric", maxk=20, iterations=200, ...)
{

  ret <- new("qkspecc")
  if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
  cndK <- 1/(1+x)
  nelts <- dim(cndK)[1]
	diag(cndK) <- 0 # "zeroed" according to Zelnik-Manor and Perona 2004
	deg <- sapply(1:nelts, function(i) { # degree matrix
		return(sum(cndK[i,]))
	})

	# hack : one "zero-based" (see "Lrw" in von Luxburg (2006)) for Bartlett procedure,
	# the other for the actual clustering.
	if(normalize=="none")	L <- diag(deg) - cndK

	if(normalize=="symmetric") L <- diag(nelts) - diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))

	if(normalize=="random-walk") L <- diag(nelts) - diag(1/deg) %*% cndK

	#L <- diag(deg) - cndK
	#Lsym <- diag(1/sqrt(deg)) %*% cndK %*% diag(1/sqrt(deg))
	#Lrw <- diag(nelts) - diag(1/deg) %*% cndK

	eig <- eigen(L)
	ind <- order(eig$values)
	eVals <- eig$values[ind]
	eVecs <- eig$vectors[ ,ind]
	Y <- eVecs[,1:Nocent]
	if(normalize=="symmetric"){
	  sumsY <- rowSums(Y^2)
	  Y <- t(sapply(1:nelts, function(i) Y[i,] / sqrt(sumsY[i])))
	}

	res <- kmeans(Y, Nocent, iterations)
	clust(ret) <- res$cluster

	eVec(ret) <- eVecs
	eVal(ret) <- eVals
	ymatrix(ret) <- Y
	kcall(ret) <- match.call()
	xmatrix(ret) <- x
	cndkernf(ret) <-  "qkernel"
	return(ret)
})


#------------------------------------------------------------------------------------------------#

setMethod("plot", signature(x="qkspecc"), function(x)
  {
  plot(x@eVal, xlab="clusters", ylab="Eigen values")
  abline(v=max(x@clust), col="red")
})



