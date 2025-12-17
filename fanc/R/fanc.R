fanc <- function(
  x, factors, n.obs, rho, gamma, cor.factor=FALSE, normalize=TRUE, normalize.penalty=FALSE, covmat, 
  type="MC", model="FA", control=list())
{

   ## Check error
 if (!missing(rho)) {
    if (!is.matrix(rho)){
      if(length(rho)==1){
        rho <- rep(rho, ifelse(missing(gamma), 9, length(gamma)))
        rho <- as.matrix(rho)
        rho <- t(rho)
        }else{
          stop('"rho" must be a matrix')
        }
    }
    if(nrow(rho)>1) rho <- apply(rho,2,function(x) sort(x, decreasing = TRUE))
    missingrho <- FALSE
  }else{
    rho <- NA
    missingrho <- TRUE
  }
if(!missing(gamma)){
  missinggamma <- FALSE
  gamma <- sort(gamma,decreasing=TRUE)
}
if(missing(gamma)){
  gamma <- NA
  missinggamma <- TRUE
}
if(sum(is.finite(gamma)==0) == 0 && type=="MC") warning('The maximum value of "gamma" should be "Inf"')

#missingw <- missing(w)
#if(missingw){
  if(normalize.penalty){
    if(missing(x)){
      if(eigen(covmat)$values[nrow(covmat)] > 1e-15){
        w <- 1/apply(factanal(covmat=covmat,factors=factors)$loadings,1, function(x) (sum(x^2))) #OK!!!!
      }else{
        fit.unnormalized <- fanc(covmat=covmat, factors=factors, rho=0, gamma=Inf, cor.factor=FALSE, normalize=normalize, type="MC")
        w <- 1/apply(fit.unnormalized$loadings[[1]][[1]], 1, function(x) (sum(x^2))) #OK!!!!
        #print(w)
      }
    }else{
      if(ncol(x) < nrow(x)) w <- 1/apply(factanal(x,factors)$loadings,1, function(x) (sum(x^2))) #OK!!!!
      if(ncol(x) >= nrow(x)){
        fit.unnormalized <- fanc(x, factors, rho=0, gamma=Inf, cor.factor=FALSE, normalize=normalize, type="MC")
        w <- 1/apply(fit.unnormalized$loadings[[1]][[1]], 1, function(x) (sum(x^2))) #OK!!!!
        #print(w)
      }
    }
    w <- w / mean(w)
  }else{
      if(missing(x)) w <- rep(1,ncol(covmat))
      else w <- rep(1,ncol(x))
  }
#}

  ## Organize control parameters
  con <- list(
    tol.em=1e-8, tol.cd=1e-8, tol.bfgs=1e-8, min.uniquevar=0.005,
    eta=0, zita=0, Delta=0.001, init.coef=seq(0.3, 2, length=10),
    #max.rho=ifelse(missing(max.rho), NA, max.rho),
    w=w,
    max.rho=NA,
    rho=rho,
    gamma=gamma,
    max.gamma=ifelse(type=="MC", 100, 1),
    min.gamma=ifelse(type=="MC", 1.01, ifelse(type=="prenet", 0.0001, 0.1)),     
    length.rho=ifelse(missingrho, 30, nrow(rho)), 
    length.gamma=ifelse(missinggamma, ifelse(type=="enet",10,9), length(gamma)), 
    alpha.powerseq=0.5,
    maxit.em=10000, maxit.cd=500, maxit.bfgs=500,
    cor.factor=cor.factor, min.rhozero=FALSE,
    start="warm", ncand.initial=10, ncand.initial.prenet=100, pmax_for_S=500, 
    maxit.initial=500,  
    progress=FALSE, openmp=FALSE, num.threads=0, gamma.ebic=1)
  con[names(control)] <- control
  ## Check error
  if (!missing(x)) {
    if (!is.matrix(x))
      stop('"x" must be a matrix.')
    if (!is.numeric(x))
      stop('"x" must be a matrix.')
    if (factors < 1 || factors >= ncol(x))
      stop('"factors" must be a positive integer less than the number of variables.')
  }
  if (!missing(covmat)) {
    if (!is.matrix(covmat))
      stop('"covmat" must be a covariance matrix.')
    if (factors < 1 || factors >= ncol(covmat))
      stop('"factors" must be a positive integer less than the number of variables.')
  }
  if (factors == 1 && type=="prenet") stop('1-factor model cannot be estimated when "prenet" is used.')
  if (type!="MC" && type!="prenet" && type!="enet" )
    stop('"type" must be "MC", "prenet" or "enet".')
  if (model!="FA" && model!="PPCA" )
    stop('"model" must be "FA" or "PPCA".')
  if(model=="FA") model_i <- 1
  if(model=="PPCA") model_i <- 2
  if (con$length.gamma < 1)
    stop('"length.gamma" in control must be a positive integer.')
  if (!missing(n.obs)) {
    if (n.obs < 2)
      stop('"n.obs" must be an integer greater than 2.')
  }
  if (!is.logical(cor.factor))
    stop('"cor.factor"  must be logical.')
  if (con$length.rho < 1)
    stop('"length.rho" in control must be a positive integer.')
  if (!missingrho) {
    if(sum(rho<0)>0) stop('The value of "rho" must not be negative')
    if(ncol(con$rho) != con$length.gamma) stop('The number of column of "rho" must be equal to the length of "gamma".')
  }
  if (!missinggamma) {
    if(type=="MC" && sum(gamma<=1)>0) stop('The value of "gamma" must be greater than 1')
    if(type=="prenet" || type=="enet"){
      if (sum(gamma>1)>0 || sum(gamma<=0)>0) stop('The value of "gamma" must be in (0,1]')
    }
  }
  if (!is.na(con$max.rho)) {
    if(type=="MC"){
      if (con$max.rho <= 0) stop('"max.rho"  must be a positive real value.')
      if (con$max.rho > 8.25) warning('"max.rho" is greater than 8.25. In such cases, the reparametrization of the penalty funcion may be failed')
    }
    if(type=="prenet" || type=="enet" ){
      if (length(con$max.rho) != con$length.gamma) stop('"max.rho"  must be a vector. The length of "max.rho"  must be equal to the length of gamma.')
      if (sum(con$max.rho <= 0) > 0) stop('The elements of "max.rho"  must be positive real values.')
    }
  }
  if (con$min.gamma <= 1 && type=="MC")
    stop('"min.gamma" in control must be greater than 1.')
  if (con$min.gamma <= 0 && (type=="prenet" || type=="enet"))
    stop('"min.gamma" in control must be greater than 0.')
  if (con$max.gamma > 1 && (type=="prenet" || type=="enet"))
    stop('"min.gamma" in control must be smaller or equal to 1.')
  if (con$max.gamma <= con$min.gamma)
    stop('"max.gamma"  must be a real value greater than min.gamma.')
  if (con$eta < 0)
    stop('"eta"  must be a non-negative real vaule.')
  if (con$ncand.initial < 1)
    stop('"ncand.initial" in control must be a positive integer.')
  if (con$ncand.initial.prenet< 1)
    stop('"ncand.initial.prenet" in control must be a positive integer.')
  if (con$maxit.em < 1)
    stop('"maxit.em" in control must be a positive integer.')
  if (con$maxit.cd < 1)
    stop('"maxit.cd" in control must be a positive integer.')
  if (con$maxit.bfgs < 1)
    stop('"maxit.bfgs" in control must be a positive integer.')
  if (is.na(match(con$start, c("warm", "cold"))))
    stop('"start" in control must be "warm" or "cold".')
  if (con$Delta <= 0)
    stop('"Delta" in control must be a positive real value.')
  if (con$min.uniquevar <= 0)
    stop('"min.uniquevar" in control must be a positive real value.')
  if (con$tol.em <= 0)
    stop('"tol.em" in control must be a positive real value.')
  if (con$tol.cd <= 0)
    stop('"tol.cd" in control must be a positive real value.')
  if (con$tol.bfgs <= 0)
    stop('"tol.bfgs" in control must be a positive real value.')
  if (con$zita < 0)
    stop('"zita" in control must be a non-negative real vaule.')
  if (!is.logical(normalize))
    stop('"normalize" must be logical.')
  if (!is.logical(con$min.rhozero))
    stop('"min.rhozero" in control must be logical.')
  if (!is.logical(con$progress))
    stop('"progress" in control must be logical.')
  if (!is.logical(con$openmp))
    stop('"openmp" in control must be logical.')
  if (con$openmp && con$num.threads == 0) {
    con$num.threads <- 1 #parallel::detectCores()
  }

  con$tol.em <- as.double(con$tol.em)
  con$tol.cd <- as.double(con$tol.cd)
  con$tol.bfgs <- as.double(con$tol.bfgs)
  con$min.uniquevar <- as.double(con$min.uniquevar)
  con$eta <- as.double(con$eta)
  con$zita <- as.double(con$zita)
  con$Delta <- as.double(con$Delta)
  con$init.coef <- as.double(con$init.coef)
  con$rho <- as.double(con$rho)
  con$gamma <- as.double(con$gamma)
  con$max.rho <- as.double(con$max.rho)
  con$max.gamma <- as.double(con$max.gamma)
  con$min.gamma <- as.double(con$min.gamma)
  con$w <- as.double(con$w)
  con$length.rho <- as.integer(con$length.rho)
  con$length.gamma <- as.integer(con$length.gamma)
  con$alpha.powerseq <- as.double(con$alpha.powerseq)
  con$maxit.em <- as.integer(con$maxit.em)
  con$maxit.cd <- as.integer(con$maxit.cd)
  con$maxit.bfgs <- as.integer(con$maxit.bfgs)
  con$cor.factor <- as.integer(con$cor.factor)
  con$min.rhozero <- as.integer(con$min.rhozero)
  con$start <- as.character(con$start)
  con$ncand.initial <- as.integer(con$ncand.initial)
  con$ncand.initial.prenet <- as.integer(con$ncand.initial.prenet)
  con$pmax_for_S <- as.integer(con$pmax_for_S)
  #con$trace <- as.logical(con$trace)
  con$maxit.initial <- as.integer(con$maxit.initial)
  con$progress <- as.integer(con$progress)
  con$openmp <- as.integer(con$openmp)
  con$num.threads <- as.integer(con$num.threads)
  con$model <- as.integer(model_i)


  ## Prepare data
  missing.x <- missing(x)
  if (missing.x) {
    if (missing(covmat))
      stop("input data matrix or covariance matrix is needed.")
    p <- ncol(covmat)
    N <- ifelse(missing(n.obs), p + 1, n.obs)
    x <- 1
  } else {
    x.orig <- x
    p <- ncol(x)
    N <- nrow(x)
    x <- scale(x, scale=normalize)[1:N, 1:p]
    if (normalize) x <- x / sqrt(N - 1) * sqrt(N)
    if (p <= N || p <= con$pmax_for_S) {
      covmat <- cov(x) * (N - 1) / N
    } else {
      covmat <- diag(apply(x, 2, function(x) var(x) * (N - 1) / N))
    }
    x <- x[1 : N, 1 : p] / sqrt(N)
  }

  if(type=="MC") type_i <- 1
  if(type=="prenet") type_i <- 2
  if(type=="enet") type_i <- 3


  ## Run
  rslt <- .Call("RextCall_fanc",
                as.integer(p), as.integer(factors), as.integer(N),
                as.double(covmat), as.double(x), as.integer(type_i), con)

  ## Convert Lambda to dgCMatrix
  rslt$loadings <- vector("list", con$length.gamma)
  for (gi in 1 : con$length.gamma) {
    rslt$loadings[[gi]] <- vector("list", con$length.rho)
    for (ri in 1 : con$length.rho) {
      arridx <- arrayInd(rslt$spi.loadings[[ri, gi]] + 1, .dim=c(p, factors))
      rslt$loadings[[gi]][[ri]] <-
        sparseMatrix(i=arridx[, 1], j=arridx[, 2],
                     x=rslt$spv.loadings[[ri, gi]], dims=c(p, factors))
    }
  }
  #rslt$spi.loadings <- NULL
  #rslt$spv.loadings <- NULL
  
  ## Convert fanc format (pivot diag.Psi and logF table, aggregate conv)
  rslt$uniquenesses <- array(apply(rslt$uniquenesses, 3, t),
                             dim=dim(rslt$uniquenesses)[c(2, 1, 3)])
  rslt$likelihood <- array(apply(rslt$likelihood, 3, t),
                           dim=dim(rslt$likelihood)[c(2, 1, 3)])
  rslt$convergence <- rowSums(matrix(rslt$convergence, 3))
  rslt$type <- type
  rslt$model <- model

  ## Attach rownames and colnames
  names.gamma <- paste("gamma", 1 : con$length.gamma, sep="")
  names.rho <- paste("rho", 1 : con$length.rho, sep="")
  names.var <-
    if (missing.x) {
      if (!is.null(colnames(covmat))) colnames(covmat)
      else paste("V", 1 : p, sep="")
    } else {
      if (!is.null(colnames(x))) colnames(x)
      else paste("V", 1 : p, sep="")
    }
  names.factor <- paste("Factor", 1 : factors, sep="")
  names(rslt$loadings) <- names.gamma
  for (gi in 1 : con$length.gamma) {
    names(rslt$loadings[[gi]]) <- names.rho
    for (ri in 1 : con$length.rho) {
      dimnames(rslt$loadings[[gi]][[ri]]) <- list(names.var, names.factor)
    }
  }
  dimnames(rslt$uniquenesses) <- list(names.rho, names.var, names.gamma)
  dimnames(rslt$Phi) <- list(names.factor, names.factor, names.rho, names.gamma)
  dimnames(rslt$likelihood) <-
    list(names.rho, c("logF", "penalty", "logF+penalty"), names.gamma)
  names(rslt$convergence) <- c("EM", "Coordinate descent", "BFGS")

  ## Attach other properties
  if (missing.x==FALSE) rslt$x <- x.orig
  rslt$call <- match.call()

  class(rslt) <- "fanc"
  rslt
}
