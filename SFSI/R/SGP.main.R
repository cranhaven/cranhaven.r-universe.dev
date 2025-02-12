
SGP.main <- function(y = NULL, X = NULL, b = NULL, K, trn, tst = NULL,
                     varU, varE, ID_geno, ID_trait, trait_names = NULL,
                     alpha = 1, lambda = NULL, nlambda = 100,
                     lambda.min = .Machine$double.eps^0.5,
                     common.lambda = TRUE, subset = NULL, tol = 1E-4,
                     maxiter = 500, tag = NULL, save.at = NULL,
                     precision.format = c("single","double"),
                     mc.cores = 1L, verbose = 2)
{

  n <- length(ID_geno)

  h2 <- varU/(varU + varE)
  if(any(diag(t(h2)) < 0.001) & (verbose>1L)){
     message("The 'heritability' is too small. Results might be doubtful")
  }

  if(is.null(tst) | (length(tst)==0)){  # If no testing set
    if(verbose>1L){
      message("No testing set was provided. \n",
              "The SGP model will be fitted to the entire data set with lambda = 0")
    }
    tst <- trn[]
    lambda <- 0
  }
  nTST <- length(tst)
  nTRN <- length(trn)

  ntraits <- length(unique(ID_trait))
  subset <- set_subset(n=nTST, subset=subset)

  labels <- NULL
  if(has_names(K)){
    labels <- rownames(K)
  }

  # Fixed effects
  BLUE <- !is.null(X) & !is.null(b)
  if(BLUE){
    if(length(dim(b)) == 2L){
      b0 <- as.matrix(b)
    }else{
      b0 <- matrix(b, ncol=ntraits)
    }
    if(nrow(b0) != ncol(X)){
       stop("Number of fixed effects 'b' must be the same as 'ncol(X)'")
    }
    if(ncol(b0) != ntraits){
       stop("Number of columns in 'b' must be the same as number of traits")
    }
    X <- X[ID_geno,,drop=FALSE]
    Xb <- unlist(lapply(1:ntraits, function(j) drop(X[ID_trait==j,,drop=FALSE]%*%b0[,j]) ))

  }else{
    Xb <- NULL
  }

  # Getting K12 <- (varU*G)[trn,tst] and K11 <- (varU*G + varE*I)[trn,trn]
  # for multitrait varU*G : Kronecker(varU,K) and varE*I : Kronecker(varE,I))
  if(ntraits ==  1L){
    K11 <- K[ID_geno[trn],ID_geno[trn]]
    K11 <- tensorEVD::Hadamard_cov(varU, K11, varE, ID_trait[trn], inplace=TRUE)
  }else{
    K11 <- tensorEVD::Hadamard_cov(varU, K, varE, ID_trait[trn], ID_geno[trn])
  }
  K12 <- tensorEVD::Hadamard(varU, K, ID_trait[trn], ID_geno[trn],
                             ID_trait[tst], ID_geno[tst], drop=FALSE)

  # Standardize matrices
  sdx <-  sqrt(diag(K11))
  cov2cor2(K11, inplace=TRUE)
  K12 <- sweep(K12, 1L, sdx, FUN = "/")

  lambda <- set_lambda(K12, alpha=alpha, lambda=lambda, nlambda=nlambda,
                         lambda.min=lambda.min, lambda.max=NULL,
                         common.lambda=common.lambda, verbose=verbose>1L)

  # Split the testing set into subsets. Only the subset provided will be fitted
  if(is.null(subset)){
    fileID <- NULL
    tst0 <- tst[]
    tt <- ""
  }else{
     sets <- sort(rep(1:subset[2],ceiling(nTST/subset[2]))[1:nTST])
     index <- which(sets == subset[1])
     fileID <- index[]
     tst0 <- tst[index]
     tt <- paste0(length(index)," (subset ",subset[1],"/",subset[2],") of ")
     K12 <- K12[ , index, drop=FALSE]
     if(ncol(lambda)==nTST){
       lambda <- lambda[ , index, drop=FALSE]
     }
  }

  if(verbose>1L){
    message("Fitting a ",ifelse(ntraits==1L,"",paste0(ntraits,"-traits ")),
            "SGP model using nTST = ",tt,nTST," and nTRN = ",nTRN," records")
  }

  out <- solveEN(Sigma=K11, Gamma=K12, alpha=alpha, lambda=lambda, nlambda=nlambda,
                 lambda.min=lambda.min, common.lambda=common.lambda, tol=tol,
                 maxiter=maxiter, save.at=save.at, fileID=fileID,
                 mc.cores=mc.cores, precision.format=precision.format,
                 scale=FALSE, sdx=sdx, verbose=verbose)

  if(is.null(y)){
    u <- yHat <- NULL
  }else{
    # Adjusted training phenotypes
    if(BLUE){
      yTRN <- y[trn] - Xb[trn]
    }else{
      yTRN <- y[trn]
    }

    if(all(!is.na(yTRN))){
      u <- predict.LASSO(out, X=yTRN)
      dimnames(u) <- list(tst0, paste0("SSI.",1:ncol(u)))
      if(BLUE){
        yHat <- sweep(u, 1L, Xb[tst0], FUN="+")  # yHat <- Xb + Zu
      }else{
        yHat <- u[]
      }

    }else{
      if(verbose>1L){
        message("Some training entries in the response vector 'y' are NA, predictions\n",
                "were not calculated. Use 'predict' method with full response data")
      }
    }
  }

  if(length(tst0) == 1L){
    out$nsup <- matrix(out$nsup, nrow=1)
    out$lambda <- matrix(out$lambda, nrow=1)
  }else{
    out$nsup <- do.call(rbind, out$nsup)
    out$lambda <- do.call(rbind, out$lambda)
  }

  out$error <- out$niter <- NULL

  tmp <- list(n=n, ntraits=ntraits, trait_names=trait_names,
              nTRN=nTRN, nTST=nTST, labels=labels, tag=tag,
              y=y, b=b, varU=varU, varE=varE, h2=h2, Xb=Xb,
              u=u, yHat=yHat, trn=trn, tst=tst,
              ID_geno=ID_geno, ID_trait=ID_trait)

  out <- do.call(c, list(tmp,out))
  class(out) <- c("SGP")
  attr(out, "type") <- "SGP"

  # Save outputs if 'save.at' is not NULL
  if(is.null(save.at)){
    return(out)
  }else{
    if(is.null(subset)){
      outfile <- normalizePath(paste0(save.at,attr(out,"type"),".RData"), mustWork=FALSE)
    }else{
      prefix <- paste0("subset_",subset[1],"_of_",subset[2],"_")
      outfile <- normalizePath(paste0(save.at,prefix,attr(out,"type"),".RData"), mustWork=FALSE)
    }
    save(out, file=outfile)
    if(verbose>1L){
      message("Results were saved at file: \n   '",outfile,"'")
    }
  }
}
