

## Wraps around Isomap in package RDRToolbox, 
## to carry out modifications to the qKernels and cnd kernels


setGeneric("qkIsomap",function(x, ...) standardGeneric("qkIsomap"))

setMethod("qkIsomap",signature(x = "matrix"),
           function(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), dims = 2, k, mod = FALSE, plotResiduals = FALSE, verbose = TRUE, na.action = na.omit, ...)
 {

    x <- na.action(x)
    x <- as.matrix(x)
    ret <- new("qkIsomap")

	if(!all(is.numeric(dims)) | any(dims < 1))
        stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")

    if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
      {
        if(is(kernel,"function")) kernel <- deparse(substitute(kernel))       # if delete "function"???
        kernel <- do.call(kernel, qpar)

      }
    if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")


    if(missing(k))
  	   k = min(nrow(x),5)
  	else{
       if(k >= nrow(x))
         stop("invalid argument: more neighbours than samples")
       if(!is.numeric(k) | k <= 1)
         stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
       }
     k = round(k)
     dims = round(dims)

     num_samples = nrow(x)
     num_features = ncol(x)

  ## compute pairwise distances
	 message("Computing distance matrix ... ", appendLF = FALSE)
	 if(is(kernel,"cndkernel"))
	   ## Compute conditionally negative definite kernel matrix
	   d <- cndkernmatrix(kernel,x)
	 else if (is(kernel,"qkernel"))
	   d <- qkernmatrix(kernel, x)
    message("done")

	## build graph of shortest paths between two neighbours (using Floyd's Algorithm)
	## modified Isomap: Use k/2 nearest neighbours and k/2 farthest neighbours
	if(mod == TRUE){
		first_k = round(k/2)
		last_k = k - first_k
		message("Building graph with shortest paths (using ", first_k ," nearest and ", last_k, " farthest neighbours) ... ", appendLF = FALSE)
	}
	## original Isomap: Use k nearest neighbours only
	else{
		first_k = k
		last_k = 0
		message("Building graph with shortest paths (using ", first_k ," nearest neighbours) ... ", appendLF = FALSE)
	}
	sort_idx = apply(d,2,order)

	## set weights to "Infinite" for not neighboured points
	for(i in 1:num_samples)
		d[i,sort_idx[(first_k+2):(num_samples-last_k),i]] = Inf

	## ensure that graph matrix is symmetric
	d = pmin(d,t(d))

	## Floyd's Algorithm
	for(i in 1:num_samples){
		d_col_i = t(d[,i])
		d_col_i = t(d_col_i[rep(1, each = num_samples),])
		d_row_i = t(d[i,])
		d_row_i = d_row_i[rep(1, each = num_samples),]
		d = pmin(d, d_col_i + d_row_i)
	}

	## determine all connected components of the graph
	num_connections = rowSums(!(is.infinite(d)))
	first_connections = apply(is.infinite(d),1,which.min)
	components = unique(first_connections)
	num_components = length(components)
	message("done")
	## reduce dimension of all components seperately and merge them to a single dataset
	message("Computing low dimensional embedding ... ", appendLF = FALSE)
	mappedX = list(NULL)
	residuals = rep(0,length(dims))
	i = 1
	for(dim in dims){
		Y = matrix(0,num_samples,dim)
		for(c in 1:num_components){

			## only use the distances of points in the connected component
			comp_indices = which(first_connections == components[c])
			D = d[comp_indices,comp_indices]
			N = length(comp_indices)

			## convert graph matrix to inner products
			T = -0.5 * ( D - rowSums(D) %*% t(rep(1/N,N)) - rep(1,N) %*% t(rowSums(D))/N + sum(D)/(N^2))

      ## catch possible errors caused by too small connected components
      if(dim(T)[1] < dim)
        stop("Problem occured while computing embedding: Connected component ", c, " consists of too few samples (", dim(T)[1], ") and cannot be reduced to ", dim, " dimensions. Try Isomap with a neighbourhood parameter higher than ", k, " or with dimension parameters lower than ", dim, ".")

			## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
	   	eig_T = eigen(T)
      Y[comp_indices,] = Re(eig_T$vectors[,1:dim] * (matrix(1,N,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))
		}
		## calculate residual variances (take care of not neighboured samples (Inf values))
		if(plotResiduals == TRUE){
       tmp = as.vector(d)
       tmp[is.infinite(tmp)] = NA
			 r = 1 - cor(cbind(tmp ,as.vector(Eucdist(Y))), use="complete.obs")^2
			 residuals[i] = r[1,2]
       rm(tmp)
		}
		mappedX[[i]] = Y
		i = i+1
	}
	message("done")
	names(mappedX) = paste("dim", dims, sep="")

	## give a summary of what has been done
	if(verbose == TRUE){
		message("number of samples: ", num_samples)
		message("reduction from ", num_features, " to ", dims, " dimensions")
		message("number of connected components in graph: ", num_components)
		message("residuals for dims:,", dims, " is ", residuals)
	}

	## plot residual variances for all dimensions
	if(plotResiduals == TRUE)
		plot(dims,residuals,type="b",xlab="dimension",ylab="residual variance")
	# Store data for out-of-sample extension
	prj(ret) <- mappedX[[1]]
	dims(ret) <- dims
	Residuals(ret) <- residuals
	eVal(ret) <- eig_T$values
	eVec(ret) <- eig_T$vectors
	kcall(ret) <- match.call()
	cndkernf(ret) <- kernel
	return(ret)
})
#-----------------------------------------------------------------#
setMethod("qkIsomap",signature(x = "cndkernmatrix"),
           function(x, dims = 2, k, mod = FALSE, plotResiduals = FALSE, verbose = TRUE, na.action = na.omit, ...)
 {


  num_samples = nrow(x)
  num_features = ncol(x)
	ret <- new("qkIsomap")
  if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
	if(!all(is.numeric(dims)) | any(dims < 1))
    stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")
  if(missing(k))
  	   k = min(nrow(x),5)
	else{
     if(k >= nrow(x))  stop("invalid argument: more neighbours than samples")
     if(!is.numeric(k) | k <= 1)
         stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
       }
  k = round(k)
  dims = round(dims)

	## build graph of shortest paths between two neighbours (using Floyd's Algorithm)
	## modified Isomap: Use k/2 nearest neighbours and k/2 farthest neighbours
	if(mod == TRUE){
		first_k = round(k/2)
		last_k = k - first_k
		message("Building graph with shortest paths (using ", first_k ," nearest and ", last_k, " farthest neighbours) ... ", appendLF = FALSE)
	}
	## original Isomap: Use k nearest neighbours only
	else{
		first_k = k
		last_k = 0
		message("Building graph with shortest paths (using ", first_k ," nearest neighbours) ... ", appendLF = FALSE)
	}
	sort_idx = apply(x,2, order)

	## set weights to "Infinite" for not neighboured points
	for(i in 1:num_samples)
		x[i,sort_idx[(first_k+2):(num_samples-last_k),i]] = Inf

	## ensure that graph matrix is symmetric
	x = pmin(x,t(x))

	## Floyd's Algorithm
	for(i in 1:num_samples){
		x_col_i = t(x[,i])
		x_col_i = t(x_col_i[rep(1, each = num_samples),])
		x_row_i = t(x[i,])
		x_row_i = x_row_i[rep(1, each = num_samples),]
		x = pmin(x, x_col_i + x_row_i)
	}

	## determine all connected components of the graph
	num_connections = rowSums(!(is.infinite(x)))
	first_connections = apply(is.infinite(x),1,which.min)
	components = unique(first_connections)
	num_components = length(components)
	message("done")
	## reduce dimension of all components seperately and merge them to a single dataset
	message("Computing low dimensional embedding ... ", appendLF = FALSE)
	mappedX = list(NULL)
	residuals = rep(0,length(dims))
	i = 1
	for(dim in dims){
		Y = matrix(0,num_samples,dim)
		for(c in 1:num_components){

			## only use the distances of points in the connected component
			comp_indices = which(first_connections == components[c])
			D = x[comp_indices,comp_indices]
			N = length(comp_indices)

			## convert graph matrix to inner products
			T = -0.5 * ( D - rowSums(D) %*% t(rep(1/N,N)) - rep(1,N) %*% t(rowSums(D))/N + sum(D)/(N^2))

      ## catch possible errors caused by too small connected components
      if(dim(T)[1] < dim)
        stop("Problem occured while computing embedding: Connected component ", c, " consists of too few samples (", dim(T)[1], ") and cannot be reduced to ", dim, " dimensions. Try Isomap with a neighbourhood parameter higher than ", k, " or with dimension parameters lower than ", dim, ".")

			## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
	   	eig_T = eigen(T)
      Y[comp_indices,] = Re(eig_T$vectors[,1:dim] * (matrix(1,N,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))
		}
		if(plotResiduals == TRUE){
		  tmp = as.vector(x)
		  tmp[is.infinite(tmp)] = NA
		  r = 1 - cor(cbind(tmp ,as.vector(Eucdist(Y))), use="complete.obs")^2
		  residuals[i] = r[1,2]
		  rm(tmp)
		}
		mappedX[[i]] = Y
		i = i+1
	}
	message("done")
	names(mappedX) = paste("dim", dims, sep="")

	## give a summary of what has been done
	if(verbose == TRUE){
		message("number of samples: ", num_samples)
		message("reduction from ", num_features, " to ", dims, " dimensions")
		message("number of connected components in graph: ", num_components)
		message("residuals for dims:,", dims, " is ", residuals)
	}
	## plot residual variances for all dimensions
	if(plotResiduals == TRUE)
	  plot(dims,residuals,type="b",xlab="dimension",ylab="residual variance")
	# Store data for out-of-sample extension
	prj(ret) <- mappedX[[1]]
	dims(ret) <- dims
	Residuals(ret) <- residuals
	eVal(ret) <- eig_T$values
	eVec(ret) <- eig_T$vectors
	kcall(ret) <- match.call()
	cndkernf(ret) <- "cndkernel"
	return(ret)
})
#------------------------------------------------------------------------------#
setMethod("qkIsomap",signature(x = "qkernmatrix"),
           function(x, dims = 2, k, mod = FALSE, plotResiduals = FALSE, verbose = TRUE, na.action = na.omit, ...)
 {


  num_samples = nrow(x)
  num_features = ncol(x)
	ret <- new("qkIsomap")
  if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
	if(!all(is.numeric(dims)) | any(dims < 1))
        stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")
  if(missing(k))
  	   k = min(nrow(x),5)
  else{
       if(k >= nrow(x))  stop("invalid argument: more neighbours than samples")
       if(!is.numeric(k) | k <= 1)
         stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
       }
  k = round(k)
  dims = round(dims)

	## build graph of shortest paths between two neighbours (using Floyd's Algorithm)
	## modified Isomap: Use k/2 nearest neighbours and k/2 farthest neighbours
	if(mod == TRUE){
		first_k = round(k/2)
		last_k = k - first_k
		message("Building graph with shortest paths (using ", first_k ," nearest and ", last_k, " farthest neighbours) ... ", appendLF = FALSE)
	}
	## original Isomap: Use k nearest neighbours only
	else{
		first_k = k
		last_k = 0
		message("Building graph with shortest paths (using ", first_k ," nearest neighbours) ... ", appendLF = FALSE)
	}
	sort_idx = apply(x,2, order)

	## set weights to "Infinite" for not neighboured points
	for(i in 1:num_samples)
		x[i,sort_idx[(first_k+2):(num_samples-last_k),i]] = Inf

	## ensure that graph matrix is symmetric
	x = pmin(x,t(x))

	## Floyd's Algorithm
	for(i in 1:num_samples){
		x_col_i = t(x[,i])
		x_col_i = t(x_col_i[rep(1, each = num_samples),])
		x_row_i = t(x[i,])
		x_row_i = x_row_i[rep(1, each = num_samples),]
		x = pmin(x, x_col_i + x_row_i)
	}

	## determine all connected components of the graph
	num_connections = rowSums(!(is.infinite(x)))
	first_connections = apply(is.infinite(x),1,which.min)
	components = unique(first_connections)
	num_components = length(components)
	message("done")
	## reduce dimension of all components seperately and merge them to a single dataset
	message("Computing low dimensional embedding ... ", appendLF = FALSE)
	mappedX = list(NULL)
	residuals = rep(0,length(dims))
	i = 1
	for(dim in dims){
		Y = matrix(0,num_samples,dim)
		for(c in 1:num_components){

			## only use the distances of points in the connected component
			comp_indices = which(first_connections == components[c])
			D = x[comp_indices,comp_indices]
			N = length(comp_indices)

			## convert graph matrix to inner products
			T = -0.5 * ( D - rowSums(D) %*% t(rep(1/N,N)) - rep(1,N) %*% t(rowSums(D))/N + sum(D)/(N^2))

      ## catch possible errors caused by too small connected components
      if(dim(T)[1] < dim)
        stop("Problem occured while computing embedding: Connected component ", c, " consists of too few samples (", dim(T)[1], ") and cannot be reduced to ", dim, " dimensions. Try Isomap with a neighbourhood parameter higher than ", k, " or with dimension parameters lower than ", dim, ".")

			## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
	   	eig_T = eigen(T)
      Y[comp_indices,] = Re(eig_T$vectors[,1:dim] * (matrix(1,N,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))
		}
		if(plotResiduals == TRUE){
		  tmp = as.vector(x)
		  tmp[is.infinite(tmp)] = NA
		  r = 1 - cor(cbind(tmp ,as.vector(Eucdist(Y))), use="complete.obs")^2
		  residuals[i] = r[1,2]
		  rm(tmp)
		}
		mappedX[[i]] = Y
		i = i+1
	}
	message("done")
	names(mappedX) = paste("dim", dims, sep="")

	## give a summary of what has been done
	if(verbose == TRUE){
		message("number of samples: ", num_samples)
		message("reduction from ", num_features, " to ", dims, " dimensions")
		message("number of connected components in graph: ", num_components)
		message("residuals for dims:,", dims, " is ", residuals)
	}

	## plot residual variances for all dimensions
	if(plotResiduals == TRUE)
	  plot(dims,residuals,type="b",xlab="dimension",ylab="residual variance")
	# Store data for out-of-sample extension
	prj(ret) <- mappedX[[1]]
	dims(ret) <- dims
	Residuals(ret) <- residuals
	eVal(ret) <- eig_T$values
	eVec(ret) <- eig_T$vectors
	kcall(ret) <- match.call()
	cndkernf(ret) <- "qkernel"
	return(ret)
})

