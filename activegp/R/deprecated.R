#' Plugin update of Constantine's C, using ynew
#' @param C A const_C object, the result of a call to C_GP
#' @param xnew The new design point
#' @noRd
expectCup <- function(C, xnew){
  if(is.null(nrow(xnew))) xnew <- matrix(xnew, nrow = 1)
  nvar <- ncol(xnew)
  
  Cup <- C$mat
  
  kn1 <- cov_gen(xnew, C$model$X0, theta = C$model$theta, type = C$model$covtype)
  
  # for shorter expressions
  theta <- sqrt(C$model$theta/2)
  
  new_lambda <- predict(object = C$model, x = xnew, nugs.only = TRUE)$nugs/C$model$nu_hat
  vn <- drop(1 - kn1 %*% tcrossprod(C$model$Ki, kn1)) + new_lambda + C$model$eps
  
  # precomputations
  Kikn <- tcrossprod(C$model$Ki, kn1)
  
  for(i in 1:nvar) {
    for(j in i:nvar){
      wa <- drop(W_kappa_ij2(C$model$X0, xnew, theta = theta, i - 1, j - 1, ct = C$ct))  # w(X, xnew)
      wb <- drop(W_kappa_ij2(xnew, rbind(C$model$X0, xnew), theta = theta, i - 1, j - 1, ct = C$ct))  # c(w(xnew, X), w(xnew, xnew))
      w <-  wb[length(wb)]# w(xnew, xnew)
      wb <- wb[-length(wb)]
      
      Cup[i, j] <- Cup[j, i] <- C$mat[i, j] + crossprod(wa + wb, Kikn/vn) - crossprod(Kikn, C$Wij[[i]][[j]]) %*% Kikn/vn - w/vn
    }
  }
  return(Cup)
}

