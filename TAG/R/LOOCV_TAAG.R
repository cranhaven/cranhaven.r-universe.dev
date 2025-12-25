cv.TAAG = function(object, TAAG.indicator = FALSE){
  omega.est = object$omega
  lambda.est = object$lambda
  theta.est = object$s
  if(TAAG.indicator == TRUE){
    if(is.null(object$eta) == TRUE){
      stop("No eta. Is the object from TAAG?")
    }else{

      phi.est = object$phi
      eta.est = object$eta
      theta.est2 = object$nu
    }
  }else{
      delta.est = object$delta
      phi.est = 1

      theta.est2 = object$s
  }

  Input = object$X
  d <- ncol(Input)
  n <- nrow(Input)
  one <- matrix(1, nrow=n)
  Ide <- diag(1, n)

  if(is.null(object$y) == TRUE && is.null(object$ty) == TRUE){
    stop("Please include y or ty in the parTAG")
  }
  if(is.null(object$y) == TRUE){
    Y.t <- object$ty
    if(lambda.est == 0){
      object$y <- exp(Y.t)
    }else{
      object$y <- (Y.t*lambda.est + 1)^(1/lambda.est)
    }
    YY <- object$y
  }

  if(is.null(object$ty) == TRUE){
    YY <- object$y
    if(lambda.est == 0){
      object$ty <- log(YY)
    }else{
      object$ty <- (YY^(lambda.est) - 1)/lambda.est
    }
    Y.t <- object$ty
  }
  Y.t = object$ty

  R <- matrix(0, nrow=n, ncol=n)
  L <- matrix(0, nrow=n, ncol=n)
  for(i in 1:ncol(Input)){
    R <- R + (omega.est[i]*exp(-(1/(theta.est[i]^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2)))
    L <- L + (-(1/((phi.est*theta.est2[i])^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2))
  }
  L <- exp(L)
  if(TAAG.indicator == TRUE){
    R <- (1-eta.est)*R + eta.est*L + 10^(-6)*Ide
  }else{
    R <- R + 10^(delta.est)*Ide
  }
  inv.R <- rcppeigen_invert_matrix(R)
  mu.hat <-  drop((t(one) %*% inv.R %*% Y.t) / (t(one) %*% inv.R %*% one))
  TCV <- (inv.R %*% (Y.t-mu.hat)) / diag(inv.R)
  #cv transformed scale
  val.tcv <- sqrt(mean(TCV^2))

  #cv original scale
  if(lambda.est == 0){
    y.hat <- exp(Y.t - TCV)
  }else{
    y.hat <- ((Y.t - TCV)*lambda.est + 1)^(1/lambda.est)
  }

  val.cv <- sqrt(mean((YY - y.hat)^2))

  results <- list(CV=val.cv, TCV = val.tcv)
  return(results)
}



