
## It is a code comes from the original Isomap in package RDRToolbox, 
## to carry out modifications to the qKernels and cnd kernels

setGeneric("qkMDS",function(x, ...) standardGeneric("qkMDS"))
setMethod("qkMDS",signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), dims = 2, plotResiduals = FALSE, verbose = TRUE,na.action = na.omit, ...)
{

  x <- na.action(x)
  x <- as.matrix(x)
  ret <- new("qkMDS")
  if(!all(is.numeric(dims)) | any(dims < 1))
      stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")

  if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
      {
        if(is(kernel,"function")) kernel <- deparse(substitute(kernel))
          kernel <- do.call(kernel, qpar)

      }
  if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")

  num_samples = nrow(x)
  num_features = ncol(x)

  ## compute kernmatrix
  message("Computing kernmatrix ... ", appendLF = FALSE)
  if(is(kernel,"cndkernel")) d <- cndkernmatrix(kernel,x)
  else if (is(kernel,"qkernel")) d <- qkernmatrix(kernel,x)
  message("done")

  ## reduce dimension and merge them to a single dataset
  message("Computing low dimensional embedding ... ", appendLF = FALSE)
  mappedX = list(NULL)
  residuals = rep(0,length(dims))
  i = 1
  for(dim in dims){
    Y = matrix(0,num_samples,dim)
    T = -0.5 * ( d^2 - rowSums(d^2) %*% t(rep(1/num_samples,num_samples)) - rep(1,num_samples) %*% t(rowSums(d^2))/num_samples + sum(d^2)/(num_samples^2))

    ## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
    eig_T = eigen(T)
    Y = Re(eig_T$vectors[,1:dim] * (matrix(1,num_samples,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))

    ## calculate residual variances
    if(plotResiduals == TRUE){
      tmp = as.vector(d)
      tmp[is.infinite(tmp)] = NA
      if(is(kernel,"cndkernel"))
        ## Compute conditionally negative definite kernel matrix
        pDist <- cndkernmatrix(kernel,Y)
      else if (is(kernel,"qkernel"))
        pDist <- qkernmatrix(kernel, Y)
      r = 1 - cor(cbind(tmp ,as.vector(pDist)), use="complete.obs")^2
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

setMethod("qkMDS",signature(x = "cndkernmatrix"),
          function(x, dims = 2, plotResiduals = FALSE, verbose = TRUE,na.action = na.omit, ...)
          {

            x <- na.action(x)
            x <- as.matrix(x)
            ret <- new("qkMDS")
            if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
            if(!all(is.numeric(dims)) | any(dims < 1))
              stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")

            num_samples = nrow(x)
            num_features = ncol(x)

            ## reduce dimension and merge them to a single dataset
            message("Computing low dimensional embedding ... ", appendLF = FALSE)
            mappedX = list(NULL)
            residuals = rep(0,length(dims))
            i = 1
            for(dim in dims){
              Y = matrix(0,num_samples,dim)
              T = -0.5 * ( x^2 - rowSums(x^2) %*% t(rep(1/num_samples,num_samples)) - rep(1,num_samples) %*% t(rowSums(x^2))/num_samples + sum(x^2)/(num_samples^2))

              ## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
              eig_T = eigen(T)
              Y = Re(eig_T$vectors[,1:dim] * (matrix(1,num_samples,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))

              ## calculate residual variances
              if(plotResiduals == TRUE){
                tmp = as.vector(x)
                tmp[is.infinite(tmp)] = NA
                if(is(kernel,"cndkernel"))
                  ## Compute conditionally negative definite kernel matrix
                  pDist <- cndkernmatrix(kernel,Y)
                else if (is(kernel,"qkernel"))
                  pDist <- qkernmatrix(kernel, Y)
                r = 1 - cor(cbind(tmp ,as.vector(pDist)), use="complete.obs")^2
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

setMethod("qkMDS",signature(x = "qkernmatrix"),
          function(x, dims = 2, plotResiduals = FALSE, verbose = TRUE,na.action = na.omit, ...)
          {

            x <- na.action(x)
            x <- as.matrix(x)
            ret <- new("qkMDS")
            if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
            if(!all(is.numeric(dims)) | any(dims < 1))
              stop("invalid argument: target dimension is required to be a (vector of) positive integer value(s)")

            num_samples = nrow(x)
            num_features = ncol(x)


            ## reduce dimension and merge them to a single dataset
            message("Computing low dimensional embedding ... ", appendLF = FALSE)
            mappedX = list(NULL)
            residuals = rep(0,length(dims))
            i = 1
            for(dim in dims){
              Y = matrix(0,num_samples,dim)
              T = -0.5 * ( x^2 - rowSums(x^2) %*% t(rep(1/num_samples,num_samples)) - rep(1,num_samples) %*% t(rowSums(x^2))/num_samples + sum(x^2)/(num_samples^2))

              ## embedding Y is given by the eigenvectors of T belonging to (some of) its biggest eigenvalues
              eig_T = eigen(T)
              Y = Re(eig_T$vectors[,1:dim] * (matrix(1,num_samples,1) %*%  sqrt(as.complex(eig_T$values[1:dim]))))

              ## calculate residual variances
              if(plotResiduals == TRUE){
                tmp = as.vector(x)
                tmp[is.infinite(tmp)] = NA
                if(is(kernel,"cndkernel"))
                  ## Compute conditionally negative definite kernel matrix
                  pDist <- cndkernmatrix(kernel,Y)
                else if (is(kernel,"qkernel"))
                  pDist <- qkernmatrix(kernel, Y)
                r = 1 - cor(cbind(tmp ,as.vector(pDist)), use="complete.obs")^2
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
