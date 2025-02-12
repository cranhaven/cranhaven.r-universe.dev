
solveEN <- function(Sigma, Gamma, alpha = 1, lambda = NULL, nlambda = 100,
                    lambda.min = .Machine$double.eps^0.5, lambda.max = NULL,
                    common.lambda = TRUE, beta0 = NULL, nsup.max = NULL,
                    scale = TRUE, sdx = NULL, tol = 1E-5, maxiter = 1000,
                    mc.cores = 1L, save.at = NULL, fileID = NULL,
                    precision.format = c("double","single"), sparse = FALSE,
                    eps = .Machine$double.eps*100, verbose = FALSE)
{
    precision.format <- match.arg(precision.format)
    alpha <- as.numeric(alpha)
    scale <- as.logical(scale)
    tol <- as.numeric(tol)
    maxiter <- as.integer(maxiter)

    if(length(dim(Gamma)) == 2L){
      Gamma <- as.matrix(Gamma)
    }else{
      Gamma <- matrix(Gamma, ncol=1L)
    }
    p <- nrow(Gamma)
    q <- ncol(Gamma)

    if((sum(dim(Sigma))/2)^2 != p^2){
      stop("'Sigma' must be a p x p matrix where p = nrow(Gamma)")
    }

    if(alpha<0 | alpha>1){ stop("Parameter 'alpha' must be a number between 0 and 1")}
    stopifnot(maxiter>0)
    if(tol < .Machine$double.eps){
      stop("Input 'tol' must be > 0")
    }
    nsup.max <- ifelse(is.null(nsup.max), p, as.integer(nsup.max))

    scaleb <- TRUE
    if(scale){
      sdx <-  sqrt(diag(Sigma))
      cov2cor2(Sigma, inplace=TRUE)     # Equal to Sigma=cov2cor(Sigma) but faster
      Gamma <- sweep(Gamma, 1L, sdx, FUN="/")
    }else{
      if(is.null(sdx)){
        scaleb <- FALSE
      }else{
        if(length(sdx) != p){
          stop("'sdx' must be a numeric vector with length(sdx) = ",p)
        }
      }
    }

    # Get lambda grid. Diagonal values in Sigma are assumed to be zero
    lambda <- set_lambda(Gamma, alpha=alpha, lambda=lambda, nlambda=nlambda,
                         lambda.min=lambda.min, lambda.max=lambda.max,
                         common.lambda=common.lambda, verbose=FALSE)
    nlambda <- nrow(lambda)

    if(ifelse(is.null(beta0),FALSE,length(beta0)!=p)){
      stop("'beta0' must be a numeric vector with length(beta0) = ",p)
    }

    flagsave <- as.logical(!is.null(save.at))
    verbose2 <- as.logical((q==1L) & verbose)
    mc.cores <- ifelse((q==1L) & (mc.cores>1L), 1L, mc.cores)
    doubleprecision <- as.logical(precision.format=="double")

    compApply <- function(task)
    {
      if(ncol(lambda)>1L){
        lambda0 <- lambda[,task]
      }else{
        lambda0 <- lambda[,1]
      }

      if(flagsave){
        filename <- paste0(file_beta,fileID[task],".bin")
      }else{
        filename <- NULL
      }

      if(sparse){
        #dyn.load("c_solveEN_sparse.so")
        res <- .Call('R_updatebeta_sparse', Sigma, Gamma[,task],
                    lambda0, alpha, beta0, tol, maxiter, nsup.max,
                    scaleb, sdx, eps, filename, doubleprecision, verbose2)
        #dyn.unload("c_solveEN_sparse.so")
      }else{
        #dyn.load("c_solveEN.so")
        res <- .Call('R_updatebeta', Sigma, Gamma[,task],
                    lambda0, alpha, beta0, tol, maxiter, nsup.max,
                    scaleb, sdx, filename, doubleprecision, verbose2)
        #dyn.unload("c_solveEN.so")
      }

      if((q>1L) & verbose){
        cat(1,file=con,append=TRUE)
        utils::setTxtProgressBar(pb, nchar(scan(con,what="character",quiet=TRUE))/q)
      }

      res$task <- task
      return(res)
    }

    file_beta <- NULL
    if(flagsave){
      stopifnot(is.character(save.at))
      file_beta <- normalizePath(paste0(save.at,"beta_i_"), mustWork=FALSE)

      if(!file.exists(dirname(file_beta))){
        dir.create(dirname(file_beta), recursive=TRUE)
      }

      if(is.null(fileID)){
        fileID <- seq(q)
      }else{
        stopifnot(length(fileID) == q)
      }
    }

    # Run the analysis for 1:ncol(Gamma)
    if((q>1L) & verbose){
      pb <- utils::txtProgressBar(style=3)
      con <- tempfile(tmpdir=tempdir())
    }
    if(mc.cores == 1L){
      out <- lapply(X=seq(q), FUN=compApply)
    }else{
      out <- parallel::mclapply(X=seq(q), FUN=compApply, mc.cores=mc.cores)
    }
    if((q>1L) & verbose) {
      close(pb); unlink(con)
    }

    # Checkpoint
    if(!all(seq(q) == unlist(lapply(out,function(x) x$task)))){
      stop("Some sub-processes failed. Something went wrong during the analysis")
    }

    out <- list(p=p, q=q, alpha=alpha, nlambda=nlambda,
                lambda = lapply(out, function(x)x$lambda),
                nsup = lapply(out, function(x)x$nsup),
                niter = lapply(out, function(x)x$niter),
                error = lapply(out, function(x)x$error),
                beta = lapply(out, function(x)x$beta)
               )

    if(q == 1L){
      out$nsup <- out$nsup[[1]]
      out$lambda <- out$lambda[[1]]
      out$niter <- out$niter[[1]]
      out$error <- out$error[[1]]
      out$beta <- as.matrix(out$beta[[1]])
    }

    if(flagsave){
      out$file_beta <- gsub("i_[0-9]+.bin$", "i_\\*.bin",
                            normalizePath(paste0(file_beta,fileID[1],".bin")))
      out$fileID <- fileID
      out$beta <- NULL
    }

    class(out) <- "LASSO"
    return(out)
}
