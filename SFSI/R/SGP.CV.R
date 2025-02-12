
SGP.CV <- function(y, X = NULL, b = NULL, Z = NULL, K,
                   trn = NULL, varU = NULL, varE = NULL,
                   ID_geno = NULL, ID_trait = NULL,
                   intercept = TRUE, alpha = 1, lambda = NULL,
                   nlambda = 100, lambda.min = .Machine$double.eps^0.5,
                   common.lambda = TRUE, nfolds = 5, nCV = 1L,
                   folds = NULL, seed = NULL, subset = NULL, tol = 1E-4,
                   maxiter = 500, method = c("REML","ML"), tag = NULL,
                   save.at = NULL, mc.cores = 1L, verbose = TRUE)
{
  method <- match.arg(method)

  # K=G0; mc.cores=1; method="REML"
  if(length(dim(y)) == 2){
    y <- as.matrix(y)
    if(is.null(ID_geno) & is.null(ID_trait)){
      ID_geno <- as.vector(row(y))
      ID_trait <- as.vector(col(y))
    }
  }
  y <- as.vector(y)

  if(is.null(trn)){
    trn <- which(!is.na(y))
    if(any(is.na(y)) & verbose){
      message("The training set was composed of the non-NA entries in the response 'y'")
    }
  }

  n <- length(y)
  tmp <- set_G(n=n, Z=Z, K=K, ID_geno=ID_geno)
  ID_geno <- tmp$ID_geno
  K <- tmp$G

  trn <- set_trn_tst(n=n, trn=trn, tst=NULL)$trn

  # Obtaining ID_trait and trait_names
  tmp <- set_traits(n=n, ID_trait=ID_trait, varU=varU, varE=varE)
  ID_trait <- tmp$ID_trait
  stopifnot(length(ID_geno) == length(ID_trait))
  trait_names <- tmp$trait_names
  ntraits <- length(unique(ID_trait))

  # Set folds
  nTRN <- length(trn)
  if(any(is.na(y[trn]))){
    stop("Some training entries in the response vector are NA.\n",
         "Provide a full response vector 'y' to compute within folds accuracy")
  }
  folds <- set_folds(n=nTRN, nfolds=nfolds, nCV=nCV, seed=seed,
                     folds=folds, choices = c(2,3,4,5,10,'n'),
                     verbose=verbose)
  nfolds <- max(folds[,1])
  nCV <- ncol(folds)
  nfolds_CV <- nfolds*nCV

  subset <- set_subset(n=nTRN, nfolds=nfolds, nCV=nCV, subset=subset)
  isLOOCV <- as.logical(nfolds==nTRN)
  mc.cores2 <- ifelse(isLOOCV,1L,mc.cores)

  compApply <- function(task)
  {
    ff <- TASKS[task,'fold']
    cv <- TASKS[task,'CV']
    folds0 <- folds[,cv]   # Select the partition
    tst0 <- trn[folds0 == ff]
    trn0 <- trn[folds0 != ff]

    fm <- SGP(y=y, X=X, b=b, K=K, trn=trn0, tst=tst0, varU=varU, varE=varE,
              ID_geno=ID_geno, ID_trait=ID_trait, intercept=intercept,
              alpha=alpha, lambda=lambda, nlambda=nlambda,
              lambda.min=lambda.min, tol=tol, maxiter=maxiter, method=method,
              common.lambda=common.lambda, mc.cores=mc.cores2, verbose=verbose2)

    if((ntasks>1L) & verbose){
      cat(1,file=con,append=TRUE)
      utils::setTxtProgressBar(pb, nchar(scan(con,what="character",quiet=TRUE))/ntasks)
    }

    if(isLOOCV){
      return(fm[c("u","yHat","nsup","lambda")])
    }else{
      ss <- summary.SGP(fm, simplify=TRUE)
      ss$tst <- tst0
      attr(ss, "type") <- NULL
      remove_beta_files(fm)
      return(ss)
    }
  }

  TASKS <- expand.grid(fold=1:nfolds, CV=1:nCV)

  # Split the testing set into subsets. Only the subset provided will be fitted
  if(is.null(subset)){
    tt <- ""
  }else{
     if(isLOOCV){
       sets <- sort(rep(1:subset[2],ceiling(nTRN/subset[2]))[1:nTRN])
       TASKS <- TASKS[which(sets == subset[1]), ]
       tt <- paste0(nrow(TASKS)," (subset ",subset[1],"/",subset[2],") of ")

     }else{
       tmp <- which(TASKS$fold==subset[1] & TASKS$CV==subset[2])
       TASKS <- TASKS[tmp, ,drop=FALSE]
       tt <- paste0("fold=",subset[1],ifelse(nCV==1L,"",paste0(", CV=",subset[2]))," of ")
       subset <- c(tmp,nfolds_CV) # Change the subset indices
     }
  }

  ntasks <- nrow(TASKS)
  if(verbose){
    tmp <- range(apply(folds,2,table))
    sizefold <- paste0(" (n = ",ifelse(tmp[1]==tmp[2],tmp[1],paste(tmp,collapse="-")),")")
    tt <- paste0(tt,ifelse(nCV==1L,"",paste0(nfolds," x ",nCV," = ")),nfolds_CV)
    tt <- paste0(tt,ifelse(isLOOCV," singleton","")," folds",ifelse(nCV==1L,"","-CV"))
    message("Internal ",ifelse(isLOOCV,"LOO",paste0(nfolds,"-folds"))," (x",nCV,")",
            " cross-validation using nTRN = ",nTRN," records")
    message("Fitting a ",ifelse(ntraits==1L,"",paste0(ntraits,"-traits ")),
            "SGP model in ",tt,sizefold)
  }

  # Run the analysis for 1:nrow(TASKS)
  verbose2 <- as.logical((ntasks==1L) & verbose)
  if((ntasks>1L) & verbose){
    pb <- utils::txtProgressBar(style=3)
    con <- tempfile(tmpdir=tempdir())
  }
  if((mc.cores>1L) & isLOOCV){
    res <- parallel::mclapply(X=seq(ntasks), FUN=compApply, mc.cores=mc.cores)
  }else{
    res <- lapply(X=seq(ntasks), FUN=compApply)
  }
  if((ntasks>1L) & verbose){
    close(pb); unlink(con)
  }

  # Checkpoint
  tmp <- unlist(lapply(seq(nrow(TASKS)),function(j){
    trn[folds[,TASKS[j,'CV']] == TASKS[j,'fold']]
  }))
  if(!all(tmp == unlist(lapply(res,function(x)x$tst)))){
    stop("Some sub-processes failed. Something went wrong during the analysis")
  }
  if(!isLOOCV){
    for(j in seq(length(res))) res[[j]]$tst <- NULL
  }

  names(res) <- apply(TASKS,1,function(x){
        ifelse(nCV==1,paste0("Fold ",x[1]),paste0("Fold ",x[1],", CV ",x[2]))
      })

  out <- list(n=n, y=y, ntraits=ntraits, trn=trn, nTRN=nTRN,
              ID_geno=ID_geno, ID_trait=ID_trait, trait_names=trait_names,
              nlambda=nlambda, alpha=alpha, nfolds=nfolds, nCV=nCV,
              nfolds_CV=nfolds_CV, folds=folds, isLOOCV=isLOOCV, tag=tag, CV=res)

  class(out) <- "SGP"
  attr(out, "type") <- "SGP.CV"

  # Save outputs if 'save.at' is not NULL
  if(is.null(save.at)){
    return(out)
  }else{
    if(is.null(subset)){
      outfile <- normalizePath(paste0(save.at,attr(out,"type"),".RData"), mustWork=FALSE)
    }else{
      prefix <- paste0(save.at,"subset_",subset[1],"_of_",subset[2],"_")
      outfile <- normalizePath(paste0(prefix,attr(out,"type"),".RData"), mustWork=FALSE)
    }
    save(out, file=outfile)
    if(verbose){
      message("Results were saved at file: \n   '",outfile,"'")
    }
  }
}
