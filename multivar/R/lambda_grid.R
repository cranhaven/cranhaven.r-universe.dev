#' @export
lambda_grid <-function (B, depth, nlam, Y, Z, W, k, tol, intercept,lamadapt) {

  lambda1 <- matrix(0, nrow=nlam, ncol=dim(W)[3])
  old_lam_max <- c()  
  lambdah <- max(Y %*% t(Z))  
  
  for(i in 1:dim(W)[3]){
      
    if(lamadapt){
      lam1    <- 0
      lambdah <- max(Y %*% t(Z))                 # traditional max lambda
      BOLD <- array(0,dim=c(( dim(W[,,1])[1]),( dim(W[,,1])[2]+1),1))
      old_lam_max <- c(old_lam_max,lambdah)  
      #cnt <- 0
      while(max(abs(lambdah-lam1))>10*tol){
           #cnt <- cnt + 1
          lambda <- (lambdah+lam1)/2
          BOLD   <-  wlasso(B, Z, Y, W[,,i,drop=F], k,  dim(W[,,1])[1], lambda, eps=tol,intercept)
          param  <- BOLD[,2:dim(BOLD)[2],]
  
          if(max(abs(param))< tol){
            lambdah <- lambda
          }else{
            lam1 <- lambda
          }
  
      }

      lambda1[,i] <- exp(seq(from = log(lambdah), to = log(lambdah/depth),length = nlam))
    
    } else {
      
      lambda1[,i] <- exp(seq(from = log(lambdah), to = log(lambdah/depth),length = nlam))
      
    }
    
  }
  
  return(lambda1)
 }
