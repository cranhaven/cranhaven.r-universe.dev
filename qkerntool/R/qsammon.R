#come from version of matlab

setGeneric("qsammon",function(x, ...) standardGeneric("qsammon"))

setMethod("qsammon", signature(x = "matrix"),
          function(x, kernel = "rbfbase", qpar = list(sigma = 0.5, q = 0.9), dims = 2, Initialisation = 'random', MaxHalves = 20, MaxIter = 500, TolFun = 1e-7, na.action = na.omit, ...)
          {
            x <- na.action(x)
            x <- as.matrix(x)
            ret <- new("qsammon")
            N <- nrow(x)
            
            if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
            {
              if(is(kernel,"function")) kernel <- deparse(substitute(kernel))
              kernel <- do.call(kernel, qpar)
              
            }
            if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")
            if(is(kernel,"cndkernel")){
              ## Compute conditionally negative definite kernel matrix
              K <- cndkernmatrix(kernel,x)
            }
            else
              if (is(kernel,"qkernel")) K <- qkernmatrix(kernel, x)
            
            K <- sqrt(K)
            scales <- 0.5/sum(K)
            
            K <- K + diag(1,N,N)
            Dinv <- K
            Dinv=1/Dinv
            Dinv[K == 0] <- 0
            
            #Dinv <- 1/K
            if (Initialisation == 'pca') {
              UUDD <- svd(x)
              ydata <- UUDD$u[,1:dims] %*% diag(UUDD$d)[1:dims,1:dims]
            }else{
              ydata <- matrix(rnorm(N*dims,mean = 0,sd = 1),N,dims)
            }
            
            one <- matrix(1,N,dims)
            #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)
            if(is(kernel,"cndkernel")){
              ## Compute conditionally negative definite kernel matrix
              d <- sqrt(cndkernmatrix(kernel,ydata,ydata)) + diag(N)
            }
            else
              if (is(kernel,"qkernel")) d <- sqrt(qkernmatrix(kernel, ydata,ydata)) + diag(N)
            
            dinv <- d
            dinv=1/dinv
            dinv[d == 0] <- 0
            
            delta <- K - d
            E <- sum(delta^2 * Dinv)
            
            #get on with it
            for (i in 1:MaxIter) {
              #compute gradient, Hessian and search direction (note it is actually
              # 1/4 of the gradient and Hessian, but the step size is just the ratio
              #of the gradient and the diagonal of the Hessian so it doesn't matter).
              delta <- dinv - Dinv
              deltaone <- delta %*% one
              g <- delta %*% ydata - ydata*deltaone
              dinv3 <- dinv^3
              ydata2 <- ydata^2
              Hdin <- dinv3 %*% ydata2 - deltaone - 2*ydata*(dinv3 %*% ydata) + ydata2*(dinv3 %*% one)
              
              Hdin0=Hdin
              Hdin0=1/Hdin0
              Hdin0[Hdin == 0] <- 0
              
              s <- -as.vector(g) * abs(as.vector(Hdin0))
              ydata_old <- ydata
              #use step-halving procedure to ensure progress is made
              #ydata <- as.vector(ydata)
              #ydata_old <- as.vector(ydata_old)
              
              for (j in 1:MaxHalves) {
                ydata <- ydata_old + matrix(s,nrow(ydata),ncol(ydata))
                
                #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)
                if(is(kernel,"cndkernel")){
                  ## Compute conditionally negative definite kernel matrix
                  d <- cndkernmatrix(kernel,ydata, ydata)
                  d[d < 0] <- 0
                  d <- sqrt(d) + diag(N)
                }
                else
                  if (is(kernel,"qkernel")){
                    d <- qkernmatrix(kernel, ydata, ydata)
                    d[d < 0] <- 0
                    d <- sqrt(d) + diag(N)
                  }
                
                dinv <- d
                dinv=1/dinv
                dinv[d == 0] <- 0
                
                delta <- K - d
                E_new <- sum(delta^2 * Dinv)
                
                if (E_new < E) {
                  break
                }else{
                  s <- 0.5 * s
                }
              }
              #bomb out if too many halving steps are required
              if (j == MaxHalves) {
                message( 'Warning : MaxHalves exceeded.')
                break
              }
              # evaluate termination criterion
              if (abs((E - E_new)/E) < TolFun){
                message('Optimisation terminated - TolFun exceeded.')
                break
              }
              #report progress
              #show <- list()
              E <- E_new
              message( 'epoch =  :',i,' E = ', E*scales)
            }
            E <- E*scales
            
            dimRed(ret) <- ydata
            # xmatrix(ret) <- x
            kcall(ret) <- match.call()
            cndkernf(ret) <- kernel
            return(ret)
          })

#---------------------------------------------------------------------------#
setMethod("qsammon", signature(x = "cndkernmatrix"),
          function(cndkernel, x, k ,dims = 2, Initialisation = 'random', MaxHalves = 20, MaxIter = 500, TolFun = 1e-7, ...)
          {
            
            ret <- new("qsammon")
            N <- nrow(x)
            if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
            
            x <- sqrt(x)
            scales <- 0.5/sum(x)
            
            x <- x + diag(1,N,N)
            Dinv <- x
            Dinv=1/Dinv
            Dinv[x == 0] <- 0
            
            #Dinv <- 1/x
            if (Initialisation == 'pca') {
              loc <- cmdscale(x,k)
              UUDD <- svd(loc)
              ydata <- UUDD$u[,1:dims] %*% diag(UUDD$d)[1:dims,1:dims]
            }else{
              ydata <- matrix(rnorm(N*dims,mean = 0,sd = 1),N,dims)
            }
            
            one <- matrix(1,N,dims)
            #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)

            ## Compute conditionally negative definite kernel matrix
            d <- sqrt(cndkernmatrix(cndkernel,ydata,ydata)) + diag(N)
            
            
            dinv <- d
            dinv=1/dinv
            dinv[d == 0] <- 0
            
            delta <- x - d
            E <- sum(delta^2 * Dinv)
            
            #get on with it
            for (i in 1:MaxIter) {
              #compute gradient, Hessian and search direction (note it is actually
              # 1/4 of the gradient and Hessian, but the step size is just the ratio
              #of the gradient and the diagonal of the Hessian so it doesn't matter).
              delta <- dinv - Dinv
              deltaone <- delta %*% one
              g <- delta %*% ydata - ydata*deltaone
              dinv3 <- dinv^3
              ydata2 <- ydata^2
              Hdin <- dinv3 %*% ydata2 - deltaone - 2*ydata*(dinv3 %*% ydata) + ydata2*(dinv3 %*% one)
              
              Hdin0=Hdin
              Hdin0=1/Hdin0
              Hdin0[Hdin == 0] <- 0
              
              s <- -as.vector(g) * abs(as.vector(Hdin0))
              ydata_old <- ydata
              #use step-halving procedure to ensure progress is made
              #ydata <- as.vector(ydata)
              #ydata_old <- as.vector(ydata_old)
              
              for (j in 1:MaxHalves) {
                ydata <- ydata_old + matrix(s,nrow(ydata),ncol(ydata))
                
                #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)
                d <- cndkernmatrix(cndkernel,ydata, ydata)
                d[d < 0] <- 0
                d <- sqrt(d) + diag(N)
                
                dinv <- d
                dinv=1/dinv
                dinv[d == 0] <- 0
                
                delta <- x - d
                E_new <- sum(delta^2 * Dinv)
                
                if (E_new < E) {
                  break
                }else{
                  s <- 0.5 * s
                }
              }
              #bomb out if too many halving steps are required
              if (j == MaxHalves) {
                message('Warning : MaxHalves exceeded')
                break
              }
              # evaluate termination criterion
              if (abs((E - E_new)/E) < TolFun){
                message( 'Optimisation terminated - TolFun exceeded.')
                break
              }
              #report progress
              #show <- list()
              E <- E_new
              message( 'epoch =  :',i,' E = ', E*scales)
            }
            E <- E*scales
            
            dimRed(ret) <- ydata
            # xmatrix(ret) <- x
            kcall(ret) <- match.call()
            cndkernf(ret) <- "cndkernel"
            return(ret)
          })


#---------------------------------------------------------------------------#
setMethod("qsammon", signature(x = "qkernmatrix"),
          function(qkernel, x, k, dims = 2, Initialisation = 'random', MaxHalves = 20, MaxIter = 500, TolFun = 1e-7, ...)
          {
            ret <- new("qsammon")
            N <- nrow(x)
            if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
            
            x <- sqrt(x)
            scales <- 0.5/sum(x)
            
            x <- x + diag(1,N,N)
            Dinv <- x
            Dinv=1/Dinv
            Dinv[x == 0] <- 0
            
            #Dinv <- 1/x
            if (Initialisation == 'pca') {
              loc <- cmdscale(x,k)
              UUDD <- svd(loc)
              ydata <- UUDD$u[,1:dims] %*% diag(UUDD$d)[1:dims,1:dims]
            }else{
              ydata <- matrix(rnorm(N*dims,mean = 0,sd = 1),N,dims)
            }
            one <- matrix(1,N,dims)
            #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)

            d <- sqrt(qkernmatrix(qkernel, ydata,ydata)) + diag(N)
            
            dinv <- d
            dinv=1/dinv
            dinv[d == 0] <- 0
            
            delta <- x - d
            E <- sum(delta^2 * Dinv)
            
            #get on with it
            for (i in 1:MaxIter) {
              #compute gradient, Hessian and search direction (note it is actually
              # 1/4 of the gradient and Hessian, but the step size is just the ratio
              #of the gradient and the diagonal of the Hessian so it doesn't matter).
              delta <- dinv - Dinv
              deltaone <- delta %*% one
              g <- delta %*% ydata - ydata*deltaone
              dinv3 <- dinv^3
              ydata2 <- ydata^2
              Hdin <- dinv3 %*% ydata2 - deltaone - 2*ydata*(dinv3 %*% ydata) + ydata2*(dinv3 %*% one)
              
              Hdin0=Hdin
              Hdin0=1/Hdin0
              Hdin0[Hdin == 0] <- 0
              
              s <- -as.vector(g) * abs(as.vector(Hdin0))
              ydata_old <- ydata
              #use step-halving procedure to ensure progress is made
              #ydata <- as.vector(ydata)
              #ydata_old <- as.vector(ydata_old)
              
              for (j in 1:MaxHalves) {
                ydata <- ydata_old + matrix(s,nrow(ydata),ncol(ydata))
                
                #d <- Eucdist(ydata,ydata, sEuclidean = TRUE) + diag(N)
                d <- qkernmatrix(qkernel, ydata, ydata)
                d[d < 0] <- 0
                d <- sqrt(d) + diag(N)
                
                dinv <- d
                dinv=1/dinv
                dinv[d == 0] <- 0
                
                #  dinv <- 1/d
                delta <- x - d
                E_new <- sum(delta^2 * Dinv)
                
                if (E_new < E) {
                  break
                }else{
                  s <- 0.5 * s
                }
              }
              #bomb out if too many halving steps are required
              if (j == MaxHalves) {
                message('Warning : MaxHalves exceeded')
                break
              }
              # evaluate termination criterion
              if (abs((E - E_new)/E) < TolFun){
                message( 'Optimisation terminated - TolFun exceeded.')
                break
              }
              #report progress
              #show <- list()
              E <- E_new
              message( 'epoch =  :',i,' E = ', E*scales)
            }
            E <- E*scales
            
            dimRed(ret) <- ydata
            # xmatrix(ret) <- x
            kcall(ret) <- match.call()
            cndkernf(ret) <- "qkernel"
            return(ret)
          })
