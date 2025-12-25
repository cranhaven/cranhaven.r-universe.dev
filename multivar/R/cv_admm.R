#' @export
cv_admm <- function(B, W, Ak, bk, k, d, lambda1, lambda2, eps,intercept=FALSE, nfolds){

  # B <- array(0,dim = c((object@d[1]),(object@d[1]*(object@k + 1) + 1), object@nlambda1*length(object@nlambda2)))
  # W <- object@W
  # Ak <- object@Ak
  # bk <- object@Ak
  # k  <- object@k
  # d  <- object@d
  # lambda1 <- object@lambda1
  # lambda2 <- object@lambda2
  # eps     <- 1e-3
  # intercept <- object@intercept
  # nfolds    <- object@nfolds
  
  make_folds  <- function(x,nfolds) split(x, cut(seq_along(x), nfolds, labels = FALSE))
  final_tmpt <- unlist(lapply(seq_along(Ak),function(j){nrow(Ak[[j]])}))
  first_tmpt <- rep(1,length(Ak))
  subj_indx_list <- lapply(seq_along(first_tmpt), function(g){first_tmpt[g]:final_tmpt[g]})
  cv_list <- lapply(subj_indx_list,function(g){make_folds(g,nfolds)})
  MSFE <- matrix(NA, nrow = nfolds, ncol = length(lambda1)*length(lambda2))
  pb   <- txtProgressBar(1, nfolds, style=3)
  
  Y <- list()
  Z <- list()
  for (kk in 1:length(Ak)){
      Y[[kk]] <- as.vector(bk[[kk]])
      Z[[kk]] <- as.matrix(kronecker(diag(1,d[[1]]),Ak[[kk]]))
  }
  
  for(fold_id in 1:nfolds){ # fold_id <- 1

    setTxtProgressBar(pb, fold_id)

    test_idx  <- lapply(1:k,function(a){as.numeric(unlist(cv_list[[a]][fold_id]))})
    train_idx <- lapply(1:k,function(a){as.numeric(unlist(cv_list[[a]][-fold_id]))})
    Y_train <- list()
    Z_train <- list()
    Y_test  <- list()
    Z_test  <- list()
    for (kk in 1:length(Ak)){
      Y_train[[kk]] <- as.vector(bk[[kk]][train_idx[[kk]],])
      Z_train[[kk]] <- as.matrix(kronecker(diag(1,d[[1]]),Ak[[kk]][train_idx[[kk]],]))
      #Y_test[[kk]]  <- as.vector(bk[[kk]][test_idx[[kk]],])
      #Z_test[[kk]]  <- as.matrix(kronecker(diag(1,d[[1]]),Ak[[kk]][test_idx[[kk]],]))
      Y_test[[kk]]  <- bk[[kk]][test_idx[[kk]],]
      Z_test[[kk]]  <- Ak[[kk]][test_idx[[kk]],]
    }

    beta <- multivar_admm(lambda1,lambda2,Z_train,Y_train,k,d,1,nrow(Ak[[1]])+1)
    
    # for(jj in 1:100){
    #   print(paste0(jj,":",sum(beta[,,jj]!=0)))
    #   print(paste0(jj,":",sum(beta[,,jj]!=0)))
    # }

    Y_big   <- t(as.matrix(do.call("rbind",Y_test)))
    Z_big   <- t(as.matrix(cbind(as.matrix(do.call("rbind",Z_test)),multivar_bdiag(Z_test))))
    
    # Calculate h-step MSFE for each penalty parameter
    for (ii in 1:dim(beta)[3]) {
      MSFE[fold_id,ii] <- norm(Y_big- beta[,-1,ii] %*% Z_big)^2
    }
  }

  #Y    <- t(as.matrix(do.call("rbind",Y)))
  #Z    <- t(as.matrix(cbind(as.matrix(do.call("rbind",Z)),multivar_bdiag(Z))))
  beta <- multivar_admm(lambda1,lambda2,Z,Y,k,d,1,nrow(Ak[[1]])+1)
  #beta <- beta[,,which.min(colMeans(MSFE))]
  return(list(beta,MSFE))
  
  
}
