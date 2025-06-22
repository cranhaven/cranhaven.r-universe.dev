
makePositiveDefinite <- function(A, epsilon = 1e-8) {
  A_reg <- A
  diag(A_reg) <- diag(A_reg) + epsilon
  return(A_reg)
}



get_inverse_wishart_matrix2 = function(X, Y, Z, subject_id, subject_to_B, binary = FALSE){
  
  constant_cols <- apply(X, 2, function(x) stats::var(x) == 0)
  non_constant_cols <- !constant_cols
  X[, constant_cols] = scale(X[, constant_cols], center = TRUE, scale = FALSE)
  X[, non_constant_cols] <- scale(X[, non_constant_cols])
  
  if(!binary){
    suppressMessages(lmm <- lme4::lmer(Y ~ 0 + X + (0 + Z|subject_id), REML = TRUE))
    coe = as.matrix(lme4::ranef(lmm)[[1]])
    coe = (coe[names(subject_to_B),])
  }else{
    suppressMessages(lmm <- lme4::glmer(as.factor(Y) ~ 0 + X + (0 + Z|subject_id), family = stats::binomial(link = "logit")))
    coe = as.matrix(lme4::ranef(lmm)[[1]])
    coe = (coe[names(subject_to_B),])
  }
  
  co = as.matrix(Matrix::bdiag(lme4::VarCorr(lmm)))
  svd_A <- svd(co)
  tolerance = 1e-10 * max(svd_A$d)
  while(length(co) > 1 & (kappa(co) > 500 | !isPositiveDefinite(co))){
    if(tolerance > max(svd_A$d)){
      if(!isPositiveDefinite(co)){
        eigenvalues <- eigen(co)$values
        negative_eigenvalues <- eigenvalues[eigenvalues < 0]
        
        # Check if there are any negative eigenvalues
        if (length(negative_eigenvalues) > 0) {
          # Find the negative eigenvalue with the largest absolute value
          largest_abs_negative_eigenvalue <- negative_eigenvalues[which.max(abs(negative_eigenvalues))]
          diag(co) = diag(co) - largest_abs_negative_eigenvalue + 1e-10
          svd_A <- svd(co)
          tolerance = 1e-10 * max(svd_A$d)
          next
        }else{
          break
        }
      }else{
        break
      }
    }
    tolerance = tolerance * 2
    svd_A <- svd(co)
    D_truncated <- diag(svd_A$d)
    diag(D_truncated) = pmax(diag(D_truncated), tolerance)
    co <- svd_A$u %*% D_truncated %*% t(svd_A$v)
    co = makePositiveDefinite(co)
    co = (co + t(co)) / 2
    
  }
  return(list(coe = as.matrix(coe), sigma = stats::sigma(lmm), covariance = as.matrix(co)))
}

bartModelMatrix=function(X, numcut=0L, usequants=FALSE, type=7,
                         rm.const=FALSE, cont=FALSE, xinfo=NULL) {
  X.class = class(X)[1]
  
  if(X.class=='factor') {
    X.class='data.frame'
    X=data.frame(X=X)
  }
  grp=NULL
  if(X.class=='data.frame') {
    p=dim(X)[2]
    
    xnm = names(X)
    for(i in 1:p) {
      if(is.factor(X[[i]])) {
        Xtemp = nnet::class.ind(X[[i]])
        colnames(Xtemp) = paste(xnm[i],1:ncol(Xtemp),sep='')
        X[[i]]=Xtemp
        m=ncol(Xtemp)
        grp=c(grp, rep(m, m))
      } else {
        X[[i]]=cbind(X[[i]])
        colnames(X[[i]])=xnm[i]
        grp=c(grp, 1)
        ##grp=c(grp, i)
      }
    }
    Xtemp=cbind(X[[1]])
    if(p>1) for(i in 2:p) Xtemp=cbind(Xtemp, X[[i]])
    X=Xtemp
  }
  else if(X.class=='numeric' | X.class=='integer') {
    X=cbind(as.numeric(X))
    ##grp=1
  }
  else if(X.class=='NULL') return(X)
  else if(X.class!='matrix')
    stop('Expecting either a factor, a vector, a matrix or a data.frame')
  
  N <- nrow(X)
  p <- ncol(X)
  
  xinfo. <- matrix(nrow=p, ncol=numcut)
  nc <- numcut
  rm.vars <- c()
  if(length(xinfo)==0 & N>0 & p>0 & (rm.const | numcut[1]>0)) {
    for(j in 1:p) {
      X.class <- class(X[1, j])[1]
      
      if(X.class=='numeric' | X.class=='integer') {
        xs <- unique(sort(X[ , j]))
        k <- length(xs)
        nc[j] <- numcut
        
        if(k %in% 0:1) { # deal with constant variables
          rm.vars <- c(rm.vars, -j)
          nc[j] <- 1
          if(k==0) xs <- NA
        }
        else if(cont)
          xs <- seq(xs[1], xs[k], length.out=numcut+2)[-c(1, numcut+2)]
        else if(k<numcut) {
          xs <- 0.5*(xs[1:(k-1)]+xs[2:k]) #  if k < numcut, use middle point between values to split
          nc[j] <- k-1
        }
        else if(usequants) { 
          xs <- stats::quantile(X[ , j], type=type,
                         probs=(0:(numcut+1))/(numcut+1))[-c(1, numcut+2)]
          names(xs) <- NULL
        }
        else xs <-
          seq(xs[1], xs[k], length.out=numcut+2)[-c(1, numcut+2)]
      }
      else
        stop(paste0('Variables of type ', X.class, ' are not supported'))
      
      xinfo.[j, 1:nc[j] ] <- xs
    }
  }
  
  X <- data.matrix(X)
  if(length(xinfo)>0) {
    if(is.list(xinfo)) for(j in 1:p){
      
      xinfo.[j, 1:length(xinfo[[j]])] <- xinfo[[j]]
    } 
    else if(is.matrix(xinfo)) xinfo. <- xinfo
    else stop('Only a list or a matrix can be provided for xinfo')
    
    for(j in 1:p) nc[j] <- sum(!is.na(xinfo.[j, ]))
  }
  
  xinfo <- xinfo.
  
  if(rm.const && length(rm.vars)>0 &&
     !(length(rm.vars)==p && all((1:p)==(-rm.vars)))) {
    X <- X[ , rm.vars]
    nc <- nc[rm.vars]
    xinfo <- xinfo[rm.vars, ]
    grp <- grp[rm.vars]
  }
  else if(length(rm.vars)==0 || (length(rm.vars)==p && all((1:p)==(-rm.vars))))
    rm.vars <- 1:p
  
  dimnames(xinfo) <- list(dimnames(X)[[2]], NULL)
  
  if(all(numcut==0)) return(X)
  else return(list(X=X, numcut=as.integer(nc), rm.const=rm.vars,
                   xinfo=xinfo, grp=grp))
}
