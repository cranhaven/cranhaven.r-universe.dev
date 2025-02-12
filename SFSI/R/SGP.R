
SGP <- function(y = NULL, X = NULL, b = NULL, Z = NULL, K = NULL,
                trn = NULL, tst = NULL, varU = NULL, varE = NULL,
                ID_geno = NULL, ID_trait = NULL, intercept = TRUE,
                alpha = 1, lambda = NULL, nlambda = 100,
                lambda.min = .Machine$double.eps^0.5,
                common.lambda = TRUE, subset = NULL, tol = 1E-4,
                maxiter = 500, method = c("REML","ML"), tag = NULL,
                save.at = NULL, precision.format = c("single","double"),
                mc.cores = 1L, verbose = 2)
{
  method <- match.arg(method)
  precision.format <- match.arg(precision.format)
  # y=Y0[,k]; K=G0; method='REML'; precision.format='double'
  # varU=varU0; varE=varE0; subset=c(2,5); save.at=prefix
  if(!is.null(y)){
    if(length(dim(y)) == 2){
      y <- as.matrix(y)
      if(is.null(ID_geno) & is.null(ID_trait)){
        ID_geno <- as.vector(row(y))
        ID_trait <- as.vector(col(y))
      }
    }
    y <- as.vector(y)

    if(is.null(trn) & is.null(tst)){
      trn <- which(!is.na(y))
      tst <- which(is.na(y))
      if(any(is.na(y)) & (verbose>1L)){
        message("The training set was composed of the non-NA entries in the response 'y'")
        if(length(tst)>0){
          message("The testing set was composed of the NA entries in the response 'y'")
        }
      }
    }
  }

  tmp <- set_G(n=length(y), Z=Z, K=K, ID_geno=ID_geno)
  ID_geno <- tmp$ID_geno
  K <- tmp$G
  n <- length(ID_geno)

  tmp <- set_trn_tst(n=n, trn=trn, tst=tst)
  trn <- tmp$trn
  tst <- tmp$tst

  # Obtaining ID_trait and trait_names
  tmp <- set_traits(n=n, ID_trait=ID_trait, varU=varU, varE=varE)
  ID_trait <- tmp$ID_trait
  stopifnot(length(ID_geno) == length(ID_trait))
  trait_names <- tmp$trait_names
  ntraits <- length(table(ID_trait))

  if(is.null(K) & (ntraits>1L)){
    stop("'Z' and 'K' cannot be  both NULL in a multi-trait model")
  }

  # Design matrix for fixed effects
  BLUE <- ifelse(is.null(X) & !intercept, FALSE, TRUE)
  if(!BLUE & (verbose>1L)){
    message("No intercept is estimated. Response is assumed to have mean zero")
  }
  X <- set_X(n=ifelse(is.null(K),n,nrow(K)), X=X)

  if(is.null(trn) | (length(trn)==0)){  # If no training set
     stop("No training set was provided")
  }
  # Variance components calculation
  if(is.null(varU) | is.null(varE) | (is.null(b) & BLUE)){
    if(ifelse(is.null(y),TRUE,any(is.na(y[trn])))){
      stop("A response 'y' with non-NA training entries must be provided\n",
           "  when either 'varU', 'varE', or 'b' is not provided")
    }

    if(ntraits > 1L){
      # Get a common TRN set using a built-in function
      dat0 <- get_common_trn(y=y, ID_geno=ID_geno, ID_trait=ID_trait, trn=trn)

      if(length(dat0) == 0){
        stop("A training set common to all traits is needed when either \n",
             "  'varU', 'varE', or 'b' is not provided. \n",
             "  Set 'intercept = FALSE' of 'b = 0' if a null intercept is assumed")
      }
    }
  }

  convergence <- TRUE
  if(ntraits == 1L)  # Single-trait case
  {
    if(is.null(varU) | is.null(varE))
    {
      # Get variance components and estimate fixed effects
      res <- fitBLUP(y[trn], X=X, K=K, ID_geno=ID_geno[trn], ID_trait=ID_trait[trn],
                     intercept=intercept, method=method, BLUP=FALSE, verbose=FALSE)

      varU <- res$varU
      varE <- res$varE
      b <- res$b
      convergence <- res$convergence

      if(verbose>1L){
        message("Parameter estimation from a LMM within training data (nTRN = ",length(trn),")")
        message(" Variance components:"); print(c(varU=varU,varE=varE))
        message(" Fixed effects:"); print(b)
      }
    }else{
      if(is.null(b) & BLUE){   # Only estimate fixed effects as GLS
        b <- fitBLUP(y[trn], X=X, K=K, ID_geno=ID_geno[trn], ID_trait=ID_trait[trn],
                     varU=varU, varE=varE, BLUP=FALSE, verbose=FALSE)$b
        if(verbose>1L){
          message("Parameter estimation from a LMM within training data (nTRN = ",length(trn),")")
          message(" Fixed effects:"); print(b)
        }
      }
    }

  }else{   # Multitrait case
    if(is.null(varU) | is.null(varE)){
      res <- getGenCov(dat0$y, X=X, K=K, ID_geno=dat0$ID_geno, ID_trait=dat0$ID_trait,
                       intercept=intercept, verbose=FALSE)
      varU <- res$varU
      varE <- res$varE
      b <- res$b
      convergence <- all(res$convergence)

      if(verbose>1L){
        message("Parameter estimation from LMMs within training data (nTRN = ",nrow(dat0),")")
        message(" Between-traits genetic covariance, varU:"); print(varU)
        message(" Between-traits error covariance, varE:"); print(varE)
        message(" Fixed effects:"); print(b)
      }
    }else{
      if(is.null(b) & BLUE){   # Only estimate fixed effects as GLS
        b <- fitBLUP(dat0$y, X=X, K=K, ID_geno=dat0$ID_geno, ID_trait=dat0$ID_trait,
                     BLUP=FALSE, varU=diag(varU), varE=diag(varE), verbose=FALSE)$b

        if(verbose>1L){
          message("Parameter estimation from LMMs within training data (nTRN = ",nrow(dat0),")")
          message(" Fixed effects:"); print(b)
        }
      }
    }
    if(any(eigen(varU)$values<0) & (verbose>1L)){
      message("Some eigenvalues of 'varU' are negative. Results might be doubtful")
    }
  }
  if(!convergence & (verbose>1L)){
    message("Note: Convergence was not reached in the 'GEMMA' algorithm")
  }

  SGP.main(y=y, X=X, b=b, K=K, trn=trn, tst=tst, varU=varU, varE=varE,
           ID_geno=ID_geno, ID_trait=ID_trait, trait_names=trait_names,
           alpha=alpha, lambda.min=lambda.min, common.lambda=common.lambda,
           lambda=lambda, nlambda=nlambda, subset=subset, tol=tol,
           maxiter=maxiter, save.at=save.at, precision.format=precision.format,
           mc.cores=mc.cores, tag=tag, verbose=verbose)

}
