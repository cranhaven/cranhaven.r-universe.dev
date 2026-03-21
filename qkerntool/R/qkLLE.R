

## Wraps around LLE in package RDRToolbox, 
## to carry out modifications to the qKernels and cnd kernels


setGeneric("qkLLE",function(x, ...) standardGeneric("qkLLE"))

setMethod("qkLLE",signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), dims = 2, k, na.action = na.omit, ...)
          {
            
            x <- na.action(x)
            x <- as.matrix(x)
            ret <- new("qkLLE")
            if(!all(is.numeric(dims)) | any(dims < 1))
              stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")
            if(!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
            {
              if(is(kernel,"function")) kernel <- deparse(substitute(kernel))
              kernel <- do.call(kernel, qpar)
            }
            if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")
            if(missing(k))
              k = min(nrow(x),5)
            else{
              if(k >= nrow(x))
                stop("invalid argument: more neighbours than samples")
              if(!is.numeric(k) |k <= 1)
                stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
            }

            k = round(k)
            dims = round(dims)
            num_samples = nrow(x)

            message("Computing kernmatrix ... ", appendLF = FALSE)
            ## Compute conditionally negative definite kernel matrix
            if(is(kernel,"cndkernel")) d <- cndkernmatrix(kernel,x)
            else if (is(kernel,"qkernel"))  d <- qkernmatrix(kernel, x)
            
            message("done")
            
            ## determine the k nearest neighbours
            sort_idx = apply(d,2,order)
            neighbours = sort_idx[2:(k+1),]
            message("done")
            
            ## Construct reconstruction weight matrix
            message("Computing low dimensional emmbedding (using ", k, " nearest neighbours)... ", appendLF = FALSE)
            W = matrix(0,k,num_samples)
            for(i in 1:num_samples){
              
              KK <- d[neighbours[ ,i], i]
              C = d[neighbours[,i],neighbours[,i]] - matrix(KK, k, k) - t(matrix(KK, k, k))
              ## weights W are solution of CW=1   # solveCrossprod(A,method="qr")
              #
              # W[,i] = solve(C, rep(1,k))
              s <- svd(C)
              D <- diag(s$d)
              D[diag(s$d)!=0] = 1/D[diag(s$d)!=0]
              W[,i] = rep(1,k) %*% s$u %*% D %*% t(s$v)
              
              ## rescale weights that rows sum to one (-> invariance to translations)
              W[,i] = W[,i] / sum(W[,i])
            }
            
            ## build the cost matrix M = (I-W)'(I-W)
            M = diag(1,num_samples)
            for(i in 1:num_samples){
              w = W[,i]
              n = neighbours[,i]
              M[i,n] = M[i,n] - t(w)
              M[n,i] = M[n,i] - w
              M[n,n] = M[n,n] + w %*% t(w)
            }
            
            ## low dimensional embedding Y given by the eigenvectors belonging to the (dims+1) smallest eigenvalues (first eigenvalue is trivial)
            eig_M = eigen(M)
            sweep(eig_M$vectors, 2, sqrt(colSums(eig_M$vectors^2)), "/")    ## normalize eingenvectors
            Y = eig_M$vectors[,(num_samples-1):(num_samples-dims)] * sqrt(num_samples)
            message("done")
            
            # Store data for out-of-sample extension
            prj(ret) <- Y
            dims(ret) <- dims
            eVal(ret) <- eig_M$values
            eVec(ret) <- eig_M$vectors
            kcall(ret) <- match.call()
            cndkernf(ret) <- kernel
            return(ret)
          })



setMethod("qkLLE",signature(x = "cndkernmatrix"),
          function(x, dims = 2, k, na.action = na.omit, ...)
          {
            ret <- new("qkLLE")
            if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
            if(!all(is.numeric(dims)) | any(dims < 1))
              stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")
            if(missing(k))
              k = min(nrow(x),5)
            else{
              if(k >= nrow(x))
                stop("invalid argument: more neighbours than samples")
              if(!is.numeric(k) |k <= 1)
                stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
            }
            
            k = round(k)
            dims = round(dims)
            num_samples = nrow(x)
            
            ## determine the k nearest neighbours
            sort_idx = apply(x,2,order)
            neighbours = sort_idx[2:(k+1),]
            message("done")
            
            message("Computing low dimensional emmbedding (using ", k, " nearest neighbours)... ", appendLF = FALSE)
            W = matrix(0,k,num_samples)
            for(i in 1:num_samples){
              
              KK <- x[neighbours[ ,i], i]
              C = x[neighbours[,i],neighbours[,i]] - matrix(KK, k, k) - t(matrix(KK, k, k))
              ## weights W are solution of CW=1   # solveCrossprod(A,method="qr")
              #
              # W[,i] = solve(C, rep(1,k))
              s <- svd(C)
              D <- diag(s$d)
              D[diag(s$d)!=0] = 1/D[diag(s$d)!=0]
              W[,i] = rep(1,k) %*% s$u %*% D %*% t(s$v)
              
              ## rescale weights that rows sum to one (-> invariance to translations)
              W[,i] = W[,i] / sum(W[,i])
            }
            
            ## build the cost matrix M = (I-W)'(I-W)
            M = diag(1,num_samples)
            for(i in 1:num_samples){
              w = W[,i]
              n = neighbours[,i]
              M[i,n] = M[i,n] - t(w)
              M[n,i] = M[n,i] - w
              M[n,n] = M[n,n] + w %*% t(w)
            }
            
            ## low dimensional embedding Y given by the eigenvectors belonging to the (dims+1) smallest eigenvalues (first eigenvalue is trivial)
            eig_M = eigen(M)
            sweep(eig_M$vectors, 2, sqrt(colSums(eig_M$vectors^2)), "/")    ## normalize eingenvectors
            Y = eig_M$vectors[,(num_samples-1):(num_samples-dims)] * sqrt(num_samples)
            message("done")
            
            # Store data for out-of-sample extension
            prj(ret) <- Y
            dims(ret) <- dims
            eVal(ret) <- eig_M$values
            eVec(ret) <- eig_M$vectors
            kcall(ret) <- match.call()
            cndkernf(ret) <- "cndkernel"
            return(ret)
          })


setMethod("qkLLE",signature(x = "qkernmatrix"),
          function(x, dims = 2, k, na.action = na.omit,...)
          {
            ret <- new("qkLLE")
            if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
            if(!all(is.numeric(dims)) | any(dims < 1))
              stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")
            if(missing(k))
              k = min(nrow(x),5)
            else{
              if(k >= nrow(x))
                stop("invalid argument: more neighbours than samples")
              if(!is.numeric(k) |k <= 1)
                stop("invalid argument: neighbour parameter is required to be an integer value >= 2")
            }
            
            k = round(k)
            dims = round(dims)
            num_samples = nrow(x)
            
            ## determine the k nearest neighbours
            sort_idx = apply(x,2,order)
            neighbours = sort_idx[2:(k+1),]
            message("done")
            
            message("Computing low dimensional emmbedding (using ", k, " nearest neighbours)... ", appendLF = FALSE)
            W = matrix(0,k,num_samples)
            for(i in 1:num_samples){
              
              KK <- x[neighbours[ ,i], i]
              C = x[neighbours[,i],neighbours[,i]] - matrix(KK, k, k) - t(matrix(KK, k, k))
              ## weights W are solution of CW=1   # solveCrossprod(A,method="qr")
              #
              # W[,i] = solve(C, rep(1,k))
              s <- svd(C)
              D <- diag(s$d)
              D[diag(s$d)!=0] = 1/D[diag(s$d)!=0]
              W[,i] = rep(1,k) %*% s$u %*% D %*% t(s$v)
              
              ## rescale weights that rows sum to one (-> invariance to translations)
              W[,i] = W[,i] / sum(W[,i])
            }
            
            ## build the cost matrix M = (I-W)'(I-W)
            M = diag(1,num_samples)
            for(i in 1:num_samples){
              w = W[,i]
              n = neighbours[,i]
              M[i,n] = M[i,n] - t(w)
              M[n,i] = M[n,i] - w
              M[n,n] = M[n,n] + w %*% t(w)
            }
            
            ## low dimensional embedding Y given by the eigenvectors belonging to the (dims+1) smallest eigenvalues (first eigenvalue is trivial)
            eig_M = eigen(M)
            sweep(eig_M$vectors, 2, sqrt(colSums(eig_M$vectors^2)), "/")    ## normalize eingenvectors
            Y = eig_M$vectors[,(num_samples-1):(num_samples-dims)] * sqrt(num_samples)
            message("done")
            
            # Store data for out-of-sample extension
            prj(ret) <- Y
            dims(ret) <- dims
            eVal(ret) <- eig_M$values
            eVec(ret) <- eig_M$vectors
            kcall(ret) <- match.call()
            cndkernf(ret) <- "qkernel"
            return(ret)
          })
