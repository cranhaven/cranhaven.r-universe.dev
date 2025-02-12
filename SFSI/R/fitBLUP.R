
fitBLUP <- function(y, X = NULL, Z = NULL, K = NULL, trn = NULL,
                    EVD = NULL, varU = NULL, varE = NULL,
                    ID_geno = NULL, ID_trait = NULL, intercept = TRUE,
                    BLUP = TRUE, method = c("REML","ML"),
                    interval = c(1E-9,1E9), tol = 1E-8, maxiter = 1000,
                    n.regions = 10, verbose = TRUE)
{
  method <- match.arg(method)
  dmin <- .Machine$double.eps*10
  flagEigen <- as.logical(!is.null(EVD))
  # y=yNA[,]; X=X0; EVD=NULL; method="REML"; BLUP=TRUE; interval=c(1E-9,1E9)

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

  n <- length(y) # Number of total observations
  tmp <- set_G(n=n, Z=Z, K=K, ID_geno=ID_geno)
  ID_geno <- tmp$ID_geno
  G <- tmp$G

  trn <- set_trn_tst(n=n, trn=trn)$trn
  stopifnot(all(!is.na(y[trn])))

  # Obtaining ID_trait and trait_names
  tmp <- set_traits(n=n, ID_trait=ID_trait)
  ID_trait <- tmp$ID_trait
  trait_names <- tmp$trait_names
  ntraits <- length(table(ID_trait))

  if(is.null(G) & (ntraits>1L) &!flagEigen){
    stop("'Z' and 'K' cannot be both NULL when passing multiple traits")
  }

  ni <- unlist(lapply(seq(ntraits),function(j)sum(ID_trait==j)))
  if(flagEigen){
    stopifnot(all(c("values", "vectors") %in% names(EVD)))
    if(!setequal(seq(n),trn) | !all(ni==nrow(EVD$vectors))){
      tmp <- ifelse(ntraits==1,""," within-trait")
      stop("An EVD corresponding to full data (n = ",ni[1],tmp,") should be provided")
    }
    ID_geno <- rep(NA, n)
    for(j in seq(ntraits)){
      ID_geno[ID_trait==j] <- seq(nrow(EVD$vectors))
    }
    G <- K <- Z <- NULL
  }
  stopifnot(length(ID_geno) == length(ID_trait))

  trn_des <- get_trn_design(trn=trn, ID_geno=ID_geno, ID_trait=ID_trait)

  # Track if the training set is the same across all response variables.
  # If not, EVD is performed for each group with common trn set
  uniqueTRN <- as.logical(length(trn_des)==1L)
  if((ntraits > 1L) & !uniqueTRN & !flagEigen){
    if(verbose){
      message(length(trn_des)," different training sets were found for the response variable.")
      message("Eigenvalue decomposition is applied to each common training set")
    }
  }

  BLUE <- ifelse(is.null(X) & !intercept, FALSE, TRUE)
  if(!BLUE & verbose){
    message("No intercept is estimated. Response is assumed to have mean zero")
  }
  n0 <- ifelse(flagEigen,ni[1],ifelse(is.null(G),n,nrow(G)))
  X <- set_X(n=n0, X=X)

  # If varE and varU are provided
  if(!is.null(varU) & !is.null(varE)){
    stopifnot(length(varU) == length(varE))
    ratio <- varU/varE
    if(length(ratio)==1L){
       ratio <- rep(ratio, ntraits)
    }else{
      if(length(ratio) != ntraits){
        stop("'varU' and 'varE' should be vectors of length ",ntraits)
      }
    }
  }else{
    ratio <- NULL
  }

  stopifnot(n.regions > 0)
  isREML <- as.logical(method=="REML")
  bounds <- exp(seq(log(interval[1]), log(interval[2]), length=n.regions+1))

  yHat <- u <- rep(0, n)
  out <- vector("list", ntraits)
  conty <- 0

  # Perform the analysis for all traits
  if((ntraits>1L) & verbose){
    pb <- utils::txtProgressBar(style=3)
  }
  for(tr in 1:length(trn_des))
  {
    trn_des0 <- trn_des[[tr]]
    ID_genoi <- trn_des0$ID_geno
    nTRN <- length(ID_genoi)

    if(!flagEigen)
    {
      if(is.null(Z) & is.null(K)){
        # EVD of a diagonal matrix
        EVD <- list(values=rep(1, nTRN), vectors=matrix(0, ncol=nTRN, nrow=nTRN))
        for(i in 1:nTRN){
          EVD$vectors[i,nTRN-i+1] <- 1
        }
      }else{
        EVD <- eigen(G[ID_genoi,ID_genoi], symmetric=TRUE)
      }
    }
    #cat("sum(d) = ",sum(EVD$values),"\n")
    #print(EVD$values[1:5])
    #print(EVD$vectors[1:10,1:5])

    nPC <- sum(EVD$values>dmin)
    X0 <- X[ID_genoi, ,drop=FALSE]
    UtX <- crossprod(EVD$vectors,X0)
    traits <- trn_des0$traits  # traits with a common trn set
    #print(head(trn_des0$trn))
    #print(head(trn_des0$index))
    #print(head(trn_des0$ID_geno))

    for(k in seq_along(traits))
    {
      trait <- traits[k]
      index_trait <- which(ID_trait == trait)
      ID_geno1 <- ID_geno[index_trait] # genotypes for the k trait
      n0 <- trn_des0$n[k]
      index0 <- trn_des0$index[,k]     # positions of ID_genoi in that trn
      trn0 <- trn_des0$trn[,k][index0]
      ytrn <- as.vector(y[trn0])
      ratio0 <- NULL
      if(!is.null(ratio)){
        ratio0 <- ratio[trait]
      }
      Uty <- drop(crossprod(EVD$vectors,ytrn))
      #cat("sum(Uty) = ",sum(Uty),"\n")

      #dyn.load("c_blup.so")
      res <- .Call('R_solve_mixed', n0, nTRN, nPC, ratio0, Uty, UtX,
                    EVD$values, bounds, tol, maxiter, isREML, BLUE)
      #dyn.unload("c_blup.so")
      #print(res)

      if(length(res) == 1){
        if(res == 5){
          stop("Design matrix 'X' is not full rank")
        }
      }else{
        if(BLUE){
          yHat[index_trait] <- as.vector(X[ID_geno1,,drop=FALSE]%*%res$b)
          names(res$b) <- colnames(X)
        }
        ystar <- ytrn - yHat[trn0]
        if(BLUP){
          if(flagEigen){
            # Matrix B = KZ'Hinv = U diag{ratio*d/(ratio*d+1)} U'
            dd <- res$ratio*EVD$values/(res$ratio*EVD$values + 1)
            B <- tcrossprod(sweep(EVD$vectors[,1:nPC],2L,sqrt(dd[1:nPC]),FUN="*"))
            g0 <- as.vector(B%*%ystar)
          }else{
            #  Hinv = U diag{1/(theta+d)} U' = U diag{ratio/(ratio*d+1)} U' = UDU'
            dd <- res$ratio/rep(1,length(EVD$values))
            dd[1:nPC] <- res$ratio/(res$ratio*EVD$values[1:nPC] + 1)
            H <- tcrossprod(sweep(EVD$vectors,2L,sqrt(dd),FUN="*"))
            tmp <- as.vector(H%*%ystar)

            if(is.null(Z) & is.null(K)){
              g0 <- rep(0,n0)
              g0[ID_genoi] <- tmp
            }else{
              if(is.null(Z)){    # Z=NULL, K=K:   u = K[,trn]*Hinv*(y-Xb)
                g0 <- drop(crossprod(K[ID_genoi,],tmp))
              }else{
                if(is.null(K)){  # Z=Z, K=NULL:   u = Z[trn,]'Hinv*(y-Xb)
                  g0 <- drop(crossprod(Z[ID_genoi,],tmp))
                }else{           # u = KZ[trn,]'Hinv*(y-Xb)
                  g0 <- tcrossprod(K,Z[ID_genoi,])%*%tmp
                }
              }
            }
          }

          if(is.null(Z)){
            u[index_trait] <- g0[ID_geno1]
          }else{
            u[index_trait] <- drop(Z[ID_geno1,,drop=FALSE]%*%g0)
          }
          yHat[index_trait] <- yHat[index_trait] + u[index_trait]
        }else{
          g0 <- NULL
        }

        res$g <- g0
        res$convergence <- as.logical(res$convergence>0)
        res$trait <- trait

        out[[trait]] <- res
      }

      conty <- conty + 1
      if((ntraits>1L) & verbose){
        utils::setTxtProgressBar(pb, conty/ntraits)
      }
    }
  }

  if((ntraits>1L) & verbose){
    close(pb)
  }

  # Checkpoint
  if(any(seq(ntraits) != unlist(lapply(out,function(x) x$trait)) )){
      stop("Some sub-processes failed. Something went wrong during the analysis")
  }

  if(verbose){
    status <- unlist(lapply(out, function(x)x$status))
    status <- status[!is.na(status)]
    if(any(status > 0)){
      for(k in 1:4){
        index <- which(status==k)
        if(length(index)>0){
          msg <- switch(k,
            '1'=paste0("The log Likelihood function is horizontal. No search for ratio varU/varE\n",
                     " was performed and was set to varU/varE=",interval[1]),
            '2'=paste0("Algorithm to find ratio varU/varE did not converge after ",
                     maxiter," iterations.\n Results are doubtful"),
            '3'=paste0("Ratio varU/varE is around the lower bound ",interval[1],
                     "\n Results might be doubtful"),
            '4'=paste0("Ratio varU/varE is around the upper bound ",interval[2],
                     "\n Results might be doubtful"))
          if(ntraits > 1L){
            tmp <- ifelse(length(index)<=15, paste(index,collapse=","),
                paste0(paste(index[1:3],collapse=","),",...,",paste(index[length(index)-(3:1)],collapse=",")))
            msg <- paste(msg, "for",length(index),"trait(s):",tmp)
          }
          message(msg)
        }
      }
    }
  }

  out <- list(varU=unlist(lapply(out,function(x)x$varU)),
              varE=unlist(lapply(out,function(x)x$varE)),
              h2=unlist(lapply(out,function(x)x$h2)),
              b=do.call(cbind,lapply(out,function(x)x$b)),
              g=do.call(cbind,lapply(out,function(x)x$g)),
              yHat=yHat, u=u, ID_geno=ID_geno, ID_trait=ID_trait,
              convergence=unlist(lapply(out,function(x)x$convergence)),
              method=method)

  if(ntraits == 1L){
    out$b <- drop(out$b)
    out$g <- as.vector(out$g)
  }else{
    names(out$varU) <- names(out$varE) <- names(out$h2) <- trait_names
    if(!is.null(out$b)){
      colnames(out$b) <- trait_names
    }
    if(!is.null(out$g)){
      colnames(out$g) <- trait_names
    }
  }

  return(out)
}
