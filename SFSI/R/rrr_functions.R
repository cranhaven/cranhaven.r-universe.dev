#====================================================================
#  'check_subset', 'set_folds', 'set_trn_tst',
#  'set_lambda', 'set_X', and 'set_K'
#  are functions to set initial parameters
#====================================================================
set_G <- function(n = NULL, Z = NULL, K = NULL, ID_geno = NULL)
{
  n <- ifelse(is.null(n),0,n)

  if(is.null(Z) & is.null(K)){
    G <- NULL
  }else{
    if(!is.null(K)){
      if((sum(dim(K))/2)^2 != length(K)){
        stop("Input 'K' should be a squared symmetric matrix")
      }
    }
    if(is.null(Z)){  # Case G=K:  Z=NULL & K!=NULL
      G <- K
      tt <- paste0("G = K (",nrow(G)," x ",ncol(G),")")
    }else{
      if(is.null(K)){   #  Z!=NULL & K=NULL
        # G = ZKZ' = ZZ' with K=I
        G <- tcrossprod(Z)
        tt <- paste0("G = ZZ' (",nrow(G)," x ",ncol(G),")")
      }else{            #  Z!=NULL & K!=NULL
        # G = ZKZ'
        G <- tcrossprod(Z,tcrossprod(Z,K))
        tt <- paste0("G = ZKZ' (",nrow(G)," x ",ncol(G),")")
      }
    }
  }

  if(is.null(ID_geno) | (length(ID_geno)==0)){
    if(ifelse(!is.null(G)&(n>0),n!=dim(G)[1],FALSE)){
      stop(tt," should be of dimension equal length(y) = ",n)
    }
    n0 <- ifelse(n>0,n,dim(G)[1])
    if(!is.null(n0)){
      ID_geno <- seq(1L, n0)
    }
  }
  if(!is.null(G)){
    ID_geno <- match_ID(G, ID_geno)
    n0 <- ifelse(n>0,n,length(ID_geno))
    if(is.null(ID_geno)){
      stop("Response vector 'y' (n = ",n0,") could not be mapped to rows/columns of \n",
           "  ",tt," using 'ID_geno'")
    }
  }
  if(is.null(ID_geno)){
    stop("An index vector 'ID_geno' could not be obtained")
  }
  if(n > 0){
    stopifnot(length(ID_geno) == n)
  }

  return(list(G=G, ID_geno=ID_geno))
}

#====================================================================

set_traits <- function(n, ID_trait = NULL, varU = NULL, varE = NULL)
{
  trait_names <- NULL
  if(!is.null(varU) & !is.null(varE)){  # Both are not NULL
    stopifnot(length(varU) == length(varE))
    if((length(dim(varU))==2L) & (length(dim(varE))==2L)){
      if(has_names(varU) & has_names(varE)){
        stopifnot(rownames(varU)==colnames(varU))
        stopifnot(rownames(varE)==colnames(varE))
        stopifnot(rownames(varU)==rownames(varE))
        stopifnot(colnames(varU)==colnames(varE))
      }else{
        if(!(!has_names(varU) & !has_names(varE))){
          stop("Both 'varU' and 'varE' should either have or not dimnames")
        }
      }
      flag_varcomp <- 2L  # Matrices
    }else{
      if(!(length(varU) == 1) & (length(varE) == 1)){
        stop("Inputs 'varU' and 'varE' should be scalars")
      }
      flag_varcomp <- 1L   # Scalars
    }
  }else{
    if(!(is.null(varU) & is.null(varE))){
      stop("Both 'varU' and 'varE' should be either provided or equal to NULL")
    }
    flag_varcomp <- 0L  # Not provided
  }

  if(has_names(varU)){
    if(is.null(trait_names)){
      trait_names <- rownames(varU)
    }
  }

  # Check vectors 'ID_geno' and 'ID_trait'
  if(is.null(ID_trait)){
    if(flag_varcomp %in% c(0,1)){
      ID_trait <- rep(1L, n)
    }
    if(flag_varcomp == 2L){
      stop("'ID_trait' should be provided when 'varU' and 'varE' are matrices")
    }

  }else{
    stopifnot(length(ID_trait) == n)
    if(is.factor(ID_trait) & (flag_varcomp==0L)){
      if(is.null(trait_names)){
        trait_names <- levels(ID_trait)
      }
    }
    ID_trait <- match_ID(varU, ID_trait)
    if(is.null(ID_trait)){
      if(flag_varcomp==0){
        stop("Response vector 'y' (n = ",n,") could not be mapped to traits using 'ID_trait'")
      }else{
        m <- ifelse(length(dim(varU))==2L,dim(varU)[1],1)
        stop("Response vector 'y' (n = ",n,") could not be mapped to rows/columns of \n",
             "  'varU' and 'varE' (",m," x ",m,") using 'ID_trait'")
      }
    }
  }
  ntraits <- length(unique(ID_trait))
  if(!is.null(trait_names)){
    stopifnot(length(trait_names)==ntraits)
  }
  tt1 <- ifelse(flag_varcomp == 1L, ntraits > 1, FALSE)
  tt2 <- ifelse(flag_varcomp == 2L, ntraits != dim(varU)[1], FALSE)
  if(tt1 | tt2){
    stop("'varU' and 'varE' should be ",ntraits," x ",ntraits," matrices")
  }

  list(ID_trait=ID_trait, trait_names=trait_names)
}

#====================================================================

set_subset <- function(n, nfolds = n, nCV = 1L, subset)
{
  if(is.null(subset)){
    return(NULL)
  }else{
    subset <- as.vector(as.matrix(subset))
    if(any(is.na(subset))){
      stop("Input 'subset' shoud not be NA")
    }
    if(n==nfolds){
      if(ifelse(length(subset)==2L,(1>subset[1])|(subset[1]>subset[2])|(subset[2]>n),TRUE)){
        stop("Needed 'subset = c(m,M)' with 1 <= m <= M <= ",n)
      }
    }else{
      if((length(subset)==1L)&(nCV==1L)){
        subset <- c(subset,1L)
      }
      if(ifelse(length(subset)==2L,(!subset[1]%in%seq(nfolds))|(!subset[2]%in%seq(nCV)),TRUE)){
        stop("Needed 'subset = c(fold,CV)' with 1 <= fold <= ",nfolds," and 1 <= CV <= ",nCV)
      }
    }
    return(subset)
  }
}

#====================================================================

set_folds <- function(n, nfolds, nCV, seed, folds = NULL,
                      choices = c(2,3,4,5,10,'n'),
                      verbose = TRUE)
{
  if(!as.character(nfolds) %in% choices){
    stop("Input 'nfolds' should be one of ",paste(choices,collapse=","))
  }
  isLOOCV <- as.logical(nfolds=='n')
  nfolds <- ifelse(isLOOCV, n, as.integer(nfolds))

  if(is.null(folds)){
    if(isLOOCV){
      if(nCV > 1L){
        nCV <- 1
        if(verbose){
          message("Only 'nCV' = 1 data partition is created for LOO-CV")
        }
      }
    }else{
      if(is.null(seed)){   # Seeds for randomization
        seed <- round(seq(1E3, .Machine$integer.max/1000, length=nCV))
      }else{
        if((nCV != length(seed)) & verbose){
          message("A total of 'nCV' = 'length(seed)' = ",length(seed)," partitions were created")
        }
        nCV <- length(seed)
      }
    }
    folds <- get_folds(n=n, k=nfolds, nCV=nCV, seed=seed)
  }else{
    if(length(dim(folds)) == 2L){
      folds <- as.matrix(folds)
    }else{
      folds <- matrix(folds, ncol=1L)
    }
    storage.mode(folds) <- "integer"
    if(nrow(folds) != n){
      stop("Number of rows of 'folds' should match the size of training set = ", n)
    }
    if(any(is.na(folds))){
      stop("No 'NA' are allowed in input 'folds'")
    }
    nfolds <- apply(folds,2,max)
    #nfolds <- apply(folds,2,function(x)length(unique(x)))
    if(length(unique(nfolds)) > 1L){
      stop("Number of unique folds should be the same across columns of input 'folds'")
    }
    nfolds <- as.integer(nfolds[1])
    for(j in 1:ncol(folds)){
      tmp <- sort(unique(folds[,j]))
      if(ifelse(length(tmp)==nfolds, any(seq(nfolds)!=tmp), TRUE)){
        stop("Column ",j," in input 'folds' does not contain all folds 1:",nfolds)
      }
    }
    if((n == nfolds) & (ncol(folds)>1L)){
      folds <- folds[,1,drop=FALSE]
      if(verbose){
        message("Only the first data partition in 'folds' is used for LOO-CV")
      }
    }
    colnames(folds) <- paste0("CV",1:ncol(folds))
  }

  return(folds)
}

#====================================================================

set_trn_tst <- function(n, trn = NULL, tst = NULL)
{
  if(storage.mode(trn) == "logical"){
    if(length(trn) != n){
      stop("'trn' should be a vector with length(trn) = ",n," when is of the 'logical' type")
    }
    trn <- which(trn)
  }else{
    if(storage.mode(trn) %in% c("double","integer")){
      trn <- as.integer(trn)
    }
  }

  if(storage.mode(tst) == "logical"){
    if(length(tst) != n){
      stop("'tst' should be a vector with length(tst) = ",n," when is of the 'logical' type")
    }
    tst <- which(tst)
  }else{
    if(storage.mode(tst) %in% c("double","integer")){
      tst <- as.integer(tst)
    }
  }

  if(storage.mode(tst) %in% c("double","integer")){
    tst <- as.integer(tst)
  }else{
    if(storage.mode(tst) == "logical"){
      stopifnot(length(tst) == n)
      tst <- which(tst)
    }
  }

  if(is.null(trn)){
    trn <- seq(n)
  }
  stopifnot(storage.mode(trn) == "integer")
  if(!all(trn %in% seq(n))){
    stop("All 'trn' elements should be between 1 and ",n)
  }

  if(!is.null(tst)){
    stopifnot(storage.mode(tst) == "integer")
    if(!all(tst %in% seq(n))){
      stop("All 'tst' elements should be between 1 and ",n)
    }
  }

  return(list(trn=trn, tst=tst))
}

#====================================================================

set_lambda <- function(Gamma, alpha = 1, lambda = NULL, nlambda = 100,
                       lambda.min = .Machine$double.eps^0.5,
                       lambda.max = NULL, common.lambda = TRUE,
                       verbose = TRUE)
{
  if(length(dim(Gamma)) != 2L){
    Gamma <- matrix(Gamma, ncol=1L)
  }
  q <- ncol(Gamma)

  if(is.null(lambda))
  {
    if(common.lambda){   # Grid of lambda common to all columns of Gamma
      if(is.null(lambda.max)){
        lambda.max <- ifelse(alpha > .Machine$double.eps, max(abs(Gamma)/alpha), 5)
      }else{
        if(length(lambda.max) > 1L){
          if(verbose){
            message("Multiple values in 'lambda.max' for 'common.lambda = TRUE',",
                    " only the first one will be used")
          }
          lambda.max <- lambda.max[1]
        }
      }
      lambda <- matrix(exp(seq(log(lambda.max), log(lambda.min), length=nlambda)), ncol=1L)

    }else{     # Grid of lambda specific to each column of Gamma
      if(is.null(lambda.max)){
        lambda.max <- apply(Gamma,2,function(x){
           ifelse(alpha > .Machine$double.eps, max(abs(x)/alpha), 5)
        })
      }else{
        if(length(lambda.max) != q){
          if(length(lambda.max) == 1L){
            if(verbose){
               message("A unique value of 'lambda.max' was provided but ",q,
                       " are needed, this value will be used in common")
            }
            lambda.max <- rep(lambda.max[1], q)
          }else{
            stop("Input 'lambda.max' should be a vector with length(lambda.max) = ",q)
          }
        }
      }
      lambda <- matrix(NA, nrow=nlambda, ncol=q)
      for(k in 1:q){
        lambda[,k] <- exp(seq(log(lambda.max[k]), log(lambda.min), length=nlambda))
      }
    }
  }else{
    if(length(dim(lambda)) != 2L){
      lambda <- matrix(lambda, ncol=1L)
    }
    if(ncol(lambda) != q){
      if(ncol(lambda) == 1L){
        if(verbose){
          message("A common ",ifelse(nrow(lambda)==1,"value","grid")," of 'lambda' will be used")
        }
      }else{
        stop("Input 'lambda' should be a matrix with ncol(lambda) = ", q)
      }
    }

    if(any(apply(lambda, 2L, function(x) any(diff(x) > 0)))){
      stop("Input 'lambda' should be a matrix of decreasing numbers")
    }
  }
  return(lambda)
}

#====================================================================

set_X <- function(n, X = NULL)
{
  if(is.null(X)){   # Only an intercept
    return(stats::model.matrix(~1, data=data.frame(rep(1,n))))

  }else{
    if(length(dim(X)) == 2L){
      if(nrow(X) != n){
        stop("Input 'X' should be a matrix with nrow(X) = ",n)
      }
      return(as.matrix(X))
    }else{
      if(length(X) != n){
        stop("Input 'X' should be a vector with length(X) = ",n)
      }
      X <- stats::model.matrix(~X)
      if(ncol(X) > 2L){
        colnames(X) <- gsub("^X","",colnames(X))
      }
      return(X)
    }
  }
}

#====================================================================
# Remove files for the regression coefficients
#====================================================================
remove_beta_files <- function(object){
  if(!is.null(object$file_beta)){
    for(j in seq_along(object$fileID)){
      tmp <- paste0("i_",object$fileID[j],".bin")
      unlink(gsub("i_\\*.bin$",tmp,object$file_beta))
    }
  }
}

#====================================================================
# Obtain a list with trn design for all traits:
# ID_geno is recycled for traits sharing the same design
#====================================================================
get_trn_design <- function(trn, ID_geno, ID_trait)
{
  stopifnot(is.numeric(ID_geno) | is.integer(ID_geno))
  stopifnot(is.numeric(ID_trait) | is.integer(ID_trait))

  n0 <- table(ID_trait)
  traits <- as.numeric(names(n0))
  ntraits <- length(traits)

  ID_geno1 <- ID_geno[trn]
  ID_trait1 <- ID_trait[trn]

  tmp <- table(ID_trait1)
  if(!all(traits %in% names(tmp))){
    stop("No training set was found for trait(s) ",
         paste(traits[!traits %in% names(tmp)],collapse=","))
  }

  LIST <- lapply(seq_along(traits), function(j){
    index <- ID_trait1==traits[j]
    data.frame(trn=trn[index], ID_geno=ID_geno1[index])
  })
  #LIST <- split(data.frame(trn=trn,ID_geno=ID_geno1),ID_trait1)
  ID <- 1:ntraits
  trn_design <- list()
  cont <- 0
  while(length(ID)>0){
    list0 <- LIST[[ID[1]]]
    ii <- c(1)
    trn0 <- matrix(list0$trn)
    index0 <- order(list0$ID_geno)
    ID_geno0 <- list0$ID_geno[index0]
    index0 <- matrix(index0)
    #index0 <- matrix(match(ID_geno0,list0$ID_geno))

    if(length(ID)>1){
      for(k in 2:length(ID)){
        list1 <- LIST[[ID[k]]]
        if(nrow(list0)==nrow(list1)){
          index1 <- order(list1$ID_geno)
          if(all(ID_geno0==list1$ID_geno[index1])){
            ii <- c(ii,k)
            trn0 <- cbind(trn0, list1$trn)
            index0 <- cbind(index0, matrix(index1))
            #index0 <- cbind(index0, match(ID_geno0,list1$ID_geno))
          }
        }
      }
    }

    common <- ID[ii] # common traits
    ID <- ID[-ii]

    cont <- cont + 1
    trn_design[[cont]] <- list(traits=traits[common], n=as.vector(n0[common]),
                               trn=trn0, ID_geno=ID_geno0, index=index0)
  }

  tmp <- unlist(lapply(trn_design,function(x)x$traits))
  if(any(seq(ntraits) != tmp[order(tmp)])){
    stop("There was an error in obtaining common training sets")
  }
  trn_design
}

#====================================================================
get_common_trn <- function(y, ID_geno, ID_trait, trn, weight = TRUE)
{
  stopifnot(length(y) == length(ID_geno))
  stopifnot(length(y) == length(ID_trait))

  LIST <- data.frame(trn,ID_geno=ID_geno[trn],ID_trait=ID_trait[trn],y=y[trn])
  LIST$ID <- paste0(LIST$ID_geno,"_",LIST$ID_trait)

  levels_geno <- names(table(ID_geno))
  levels_trait <- names(table(ID_trait))
  out <- c()
  for(j in 1:length(levels_geno)){
    ID0 <- paste0(rep(levels_geno[j],length(levels_trait)),"_",levels_trait)
    if(all(ID0 %in% LIST$ID)){
      tmp <- LIST[LIST$ID %in% ID0,]
      if(nrow(tmp)>length(ID0)){
        tmp0 <- do.call(rbind,lapply(split(tmp,tmp$ID),function(dt){
                        dt0 <- dt[1, ,drop=FALSE]
                        dt0$y <- mean(dt$y)
                        if(weight){
                          dt0$y <- sqrt(nrow(dt))*dt0$y
                        }
                        dt0
                      }))
        out <- rbind(out, tmp0)
      }else{
        out <- rbind(out, tmp)
      }
    }
  }

  index <- match(trn[trn %in% out$trn],out$trn)
  if(length(index) > 0){
    out <- out[index,c("trn","ID_geno","ID_trait","y")]
    rownames(out) <- NULL
  }

  return(out)
}

#====================================================================
# Labels and breaks for the nsup axis
#    x = -log(lambda)
#    y = nsup
#====================================================================
get_breaks <- function(x, y, nbreaks.x = 6, ymin = 1)
{
  fm <- stats::smooth.spline(x,y)

  tmp <- cbind(x,stats::fitted(fm))
  tmp <- tmp[which(tmp[,2]>=ymin),]
  tmp <- tmp[abs(tmp[,2]-min(tmp[,2]))<1E-8,,drop=F]
  xmin <- mean(tmp[,1], na.rm=T)

  tmp <- seq(min(x), ifelse(xmin<=0,0,2*xmin), length=1000)
  tt <- stats::predict(fm,tmp)
  xmin <- tt$x[min(which(tt$y >= ymin))]

  breaks.x <- seq(xmin, max(x), length=nbreaks.x)
  breaks.y <- stats::predict(fm, breaks.x)$y

  return(list(breaks.x=breaks.x,breaks.y=breaks.y))
}

#====================================================================
#====================================================================
map_set <- function(i = NULL, j = NULL, n = length(i), x = NULL, y = NULL,
                    labels = NULL, xlab="x", ylab="y")
{
  if(is.null(i) | is.null(j)){
    if(n <= 0){
      stop("A positive length of the map 'n' should be specified")
    }
    if(is.null(i)){
      i <- seq(1,n)
    }
    if(is.null(j)){
      j <- rep(1, n)
    }
  }
  stopifnot(length(i) == length(j))
  n <- length(i)
  MAP <- data.frame(index=seq(n), i, j)
  MAP$i_j <- paste0(MAP$i,"_",MAP$j)
  MAP$set <- NA
  MAP$index_set <- NA

  if(!is.null(y)){
    y <- as.integer(y)
    stopifnot(all(y %in% MAP$index))
    MAP[y,"set"] <- ylab
    MAP[y,"index_set"] <- seq_along(y)
  }

  if(!is.null(x)){
    x <- as.integer(x)
    stopifnot(all(x %in% MAP$index))
    MAP[x,"set"] <- xlab
    MAP[x,"index_set"] <- seq_along(x)
  }

  if(is.null(labels)){
    MAP$label <- MAP$i
  }else{
    stopifnot(length(labels) == n)
    MAP$label <- labels[MAP$i]
  }
  MAP$label_j <- paste0(MAP$label,"_",MAP$j)
  MAP
}

#====================================================================
#====================================================================
get_summary_nsup <- function(object, tst = NULL, map = NULL,
                             eps = .Machine$double.eps)
{

  ntraits <- object$ntraits
  nlambda <- object$nlambda
  names_nsup <- paste0("nsup_",1:ntraits)
  if(is.null(map)){
    map <- map_set(i=object$ID_geno, j=object$ID_trait, n=object$n,
                   x=object$trn, y=object$tst, xlab="trn", ylab="tst")
  }
  if(is.null(tst)){
    tst <- object$tst
  }
  map_trn <- map[object$trn,]
  map_tst <- map[tst,]

  out <- expand.grid(SSI=paste0("SSI.",seq(nlambda)), trait=seq(ntraits))
  out <- data.frame(out, matrix(NA, nrow=nrow(out), ncol=ntraits))
  colnames(out) <- c("SSI","trait",names_nsup)
  ID <- paste(out$SSI,out$trait)

  #nsup0 <- matrix(NA, nrow=ntraits, ncol=ntraits)
  #for(ilambda in 1:nlambda){
  #  SSIname <- paste0("SSI.",ilambda)
  #  b <- coef.SSI(object, ilambda=ilambda)
  #  for(i in 1:ntraits){
  #    b0 <- b[map_tst$j==i,,drop=F]
  #    for(j in 1:ntraits){
  #      nsup0[i,j] <- mean(apply(b0[,map_trn$j==j,drop=F],1,function(x)sum(abs(x)>eps)))
  #    }
  #  }
  #  out[match(paste(SSIname,seq(ntraits)), ID),names_nsup] <- nsup0
  #}
  SS <- lapply(1:ntraits, function(j) matrix(0,nrow=nlambda,ncol=ntraits))
  for(iy in seq_along(tst)){
    b <- coef.SGP(object, iy=iy)
    tt <- do.call(cbind,lapply(1:ntraits, function(j){
      apply(b[map_trn$j==j,,drop=F],2,function(x)sum(abs(x)>eps))
    }))
    itrait <- map_tst[iy,"j"]
    SS[[itrait]] <- SS[[itrait]] + tt
  }
  # Take the average
  for(i in 1:ntraits){
    SS[[i]] <- SS[[i]]/sum(map_tst$j==i)
  }
  # lapply(SS,function(x)x[ilambda,])
  for(ilambda in 1:nlambda){
    SSIname <- paste0("SSI.",ilambda)
    nsup0 <- do.call(rbind,lapply(SS,function(x)x[ilambda,]))
    out[match(paste(SSIname,seq(ntraits)), ID),names_nsup] <- nsup0
  }
  return(out)
}

#====================================================================
#====================================================================

has_names <- function(A)
{
  if(length(dim(A)) == 2L){
    out <- length(unlist(dimnames(A))) == sum(dim(A))
  }else{
     out <- FALSE
  }

  out
}

#====================================================================
# This function matches an ID vector to  row/column names of a matrix
#====================================================================
match_ID <- function(A = NULL, ID, MARGIN = 1, check = TRUE){
  index <- NULL
  stopifnot(all(!is.na(ID)))

  if(is.null(A)){
    Names <- NULL
    n <- length(unique(ID))
  }else{
    if(is.scalar(A)){
      A <- as.matrix(A)
    }
    if(length(dim(A)) != 2L){
      stop("Input 'A' should be a scalar or a matrix")
    }
    Names <- dimnames(A)
    n <- dim(A)[MARGIN]
  }

  if(is.null(Names)){
    if(is.numeric(ID) | is.integer(ID)){
      index <- as.integer(ID)
    }else{ # This is new: it will create the index if character and A is NULL
      if(is.null(A)){
        if(is.factor(ID)){
          index <- as.numeric(ID)
        }else{
          if(is.character(ID)){
            index <- as.numeric(as.factor(ID))
          }
        }
      }
    }
    if(!is.null(index)){  # checkpoint
      rg <- range(index)
      if(!((1L<=rg[1]) & (rg[2]<=n))){
        index <- NULL
      }
    }
  }else{
    if((length(Names) == 2L)){
      if(check){
        if(!ifelse(length(Names[[1]])==length(Names[[2]]),all(Names[[1]]==Names[[2]]),FALSE)){
          stop("All entries in 'dimnames[[1]]' should be equal to those in 'dimnames[[2]]'")
        }
      }
      names0 <- Names[[MARGIN]]
    }else{
      names0 <- NULL
    }

    if(all(as.character(ID) %in% names0)){
      index <- match(as.character(ID), names0)
    }else{
      if(is.numeric(ID) | is.integer(ID)){
        ID <- as.integer(ID)
        rg <- range(ID)
        if((1L <= rg[1]) & (rg[2] <= n)){
          index <- ID
        }
      }
    }
  }
  return(index)
}

#====================================================================
#====================================================================

is.scalar <- function(x){
  is.atomic(x) && length(x) == 1L
}

#====================================================================
#====================================================================

capitalize <- function(string)
{
  substr(string,1,1) <- toupper(substr(string,1,1))
  string
}

#====================================================================
#====================================================================

circleFun <- function(center = c(0,0), radius = 1, n = 200)
{
    tt <- seq(0, 2*pi, length.out = n)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
}

#====================================================================
# Obtain layout to for the net.plot function
#====================================================================
get_net <- function(X, MAP, symmetric, K = NULL,
                    p.radius = 1.75, delta = .Machine$double.eps)
{

  axis_labels <- namesK <- NULL

  n <- length(unique(MAP$i))
  q <- length(unique(MAP$j))

  if(!is.null(K)){
    if(has_names(K)){
      stopifnot(all(rownames(K)==colnames(K)))
      namesK <- rownames(K)
    }
  }

  xxx <- which(MAP$set=="x")
  yyy <- which(MAP$set=="y")
  if(symmetric | length(yyy)==0){
    yyy <- xxx[]
  }

  # List of modules: Group of nodes (primary nodes) connected to other nodes (secondary nodes)
  modules <- vector("list",length(yyy))
  for(k in 1:nrow(X)){
    modules[[k]] <- NA
    if(symmetric){
      modules[[k]] <- xxx[which(abs(X[k,k:ncol(X)]) > delta) + k -1]
    }else{
      modules[[k]]  <- xxx[which(abs(X[k, ]) > delta)]
    }
  }

  if(is.null(K)){
    if(symmetric){
      eee <- c()
      gr <- igraph::make_empty_graph(n = n)
      for(j in 1:nrow(X)){
        index <- modules[[j]][-1]
        if(length(index[!is.na(index)]) > 0) eee <- c(eee, as.vector(rbind(j,index)))
      }
      gr <- igraph::add_edges(gr, eee)
      xy <- list(igraph::layout_with_fr(gr, dim=2, niter=2000))
    }else{
      gr <- igraph::make_empty_graph(n = n)
      eee <- c()
      MAP0 <- MAP[yyy,]
      uniquei <- unique(MAP0$i)
      for(j in 1:length(uniquei)){
        i0 <- uniquei[j]
        map0 <- MAP0[MAP0$i==i0, ]
        con0 <- c()
        for(k in 1:nrow(map0)){
          yi <- which(yyy == map0$index[k])
          tmp <- unique(MAP[modules[[yi]],'i'])
          con0 <- c(con0, tmp)
        }
        con0 <- con0[!is.na(con0)]
        if(length(con0) > 0){
          eee <- c(eee, as.vector(rbind(i0,con0)))
        }
      }
      gr <- igraph::add_edges(gr, eee)
      tmp <- igraph::layout_with_fr(gr, dim=2, niter=2000)
      xy <- vector('list',q)
      for(j in 1:q){
        xy[[j]] <- tmp
      }
    }

  }else{
    tmp <- svd(K, nu=2, nv=0)
    d <- tmp$d
    xy <- tmp$u[,1:2]
    xy <- lapply(1:q,function(k) xy)

    expvarPC <- paste0(" (",sprintf('%.1f',100*d/sum(d)),"%)")
    if(length(expvarPC) < 2) expvarPC <- NULL
    axis_labels <- paste0("PC ",1:2,expvarPC[1:2])
  }

  # Center and  scale to -1, 1
  xy <- lapply(xy,function(tt){
    mm0 <- apply(tt,2, function(x) (max(x)+min(x))/2)
    tt <- sweep(tt, 2L, mm0, FUN="-")
    apply(tt,2,function(x)x/max(abs(x)))
  })

  radius <- unlist(lapply(xy,function(tt){
    max(apply(tt,2,function(x)diff(range(x))/2)) # Radius
  }))

  if(q == 1L){
    xy <- do.call(rbind,xy)
    mid_point <- matrix(c(x=sum(range(xy[,1]))/2, y=sum(range(xy[,1]))/2),nrow=1)

  }else{
    radius0 <- p.radius*max(radius)
    deg <- seq(0,q-1)*(360/q)
    rad <- deg*pi/180   #  Convert to radians
    mid_point <- cbind(x=radius0*cos(rad), y=radius0*sin(rad))
    xy <- do.call(rbind,lapply(1:q,function(k) sweep(xy[[k]],2L,mid_point[k,],FUN="+")))
  }

  colnames(xy) <- c("x","y")

  return(list(xy=xy, axis_labels=axis_labels, mid_point=mid_point,
              radius=radius, modules=modules, index_module=yyy))
}

#====================================================================

mytheme <- function(){
  ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust=0.5),
      legend.background = ggplot2::element_rect(fill="gray95"),
      legend.box.spacing = ggplot2::unit(0.4, "lines"),
      legend.key.size = ggplot2::unit(0.85, "lines"),
      legend.text = ggplot2::element_text(size=8),
      #legend.justification = c(1,0),
      #legend.position = c(0.99,0.01),
      legend.title = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(t=-0.2,r=0.2,b=0.2,l=0.2,unit='line'),
      strip.text.x = ggplot2::element_text(size=8.5, margin=ggplot2::margin(t=1.3,b=1.3)),
      axis.title.y.right = ggplot2::element_text(vjust = 1.6)
    )
}

#====================================================================

.onAttach <- function(libname, pkgname){

  addsp <- function(n)paste(rep(" ",n), collapse="")

  tmp <- paste0("Version: ",utils::packageVersion(pkgname),
                " (",utils::packageDate(pkgname),")")

  packageStartupMessage("
  |===============================================================|
  |    ._______. ._______. ._______. ._______.                    |
  |    | ._____| | ._____| | ._____| |__. .__|                    |
  |    | |_____. | |___.   | |_____.    | |        Sparse         |
  |    |_____. | | .___|   |_____. |    | |        Family and     |
  |    ._____| | | |       ._____| | .__| |__.     Selection      |
  |    |_______| |_|       |_______| |_______|     Index          |
  |                                                               |
  |    ",tmp,addsp(59-nchar(tmp)),"|
  |    Authors: Marco Lopez-Cruz & Gustavo de los Campos          |
  |                                                               |
  |    Type 'citation('SFSI')' to know how to cite this package   |
  |    Type 'help(package='SFSI', help_type='html')' to see help  |
  |    Type 'browseVignettes('SFSI')' to see documentation        |
  |    Visit https://github.com/MarcooLopez/SFSI/ to see examples |
  |                                                               |
  |===============================================================|")

  suppressWarnings(out <- utils::old.packages(repos="https://cloud.r-project.org"))
  if(pkgname %in% rownames(out)){
    packageStartupMessage("
    Note: New version ",out[pkgname,"ReposVer"],
    " of this package is available on CRAN")
  }

  packageStartupMessage("
  Main changes from v1.3.1 version:
    - Function SSI() is replaced by SGP(), and SSI.CV() by SGP.CV()
    - Training and testing sets are defined using integer vectors as
      'SGP(..., trn, tst)'. In the former version they were defined using
      a TRUE/FALSE vector 'SSI(..., trn_tst)'
    - In cross-validation, training set is defined as 'SGP.CV(..., trn)'
    - Method 'fitted' is replaced by method 'predict'
  ")
}
