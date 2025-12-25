check.TAAG = function(object){

  omega.est = object$omega
  lambda.est = object$lambda
  theta.est = object$s
  nu.est = object$nu
  eta.est = object$eta
  phi.est = object$phi
  Y.t = object$ty
  Input = object$X
  d <- ncol(Input)
  n <- nrow(Input)
  one <- matrix(1, nrow=n)
  Ide <- diag(1, n)
  if(lambda.est == 0){
    YY <- exp(Y.t)
  }else{
    YY <- (Y.t*lambda.est + 1)^(1/lambda.est)
  }

  #TAAG
  R <- matrix(0, nrow=n, ncol=n)
  L <- matrix(0, nrow=n, ncol=n)
  for(i in 1:ncol(Input)){
    R <- R + (omega.est[i]*exp(-(1/(theta.est[i]^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2)))
    L <- L + (-(1/((phi.est*nu.est[i])^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2))
  }
  L <- exp(L)
  R <- (1-eta.est)*R + eta.est*L + 10^(-6)*Ide
  inv.R <- rcppeigen_invert_matrix(R)
  mu.hat <-  drop((t(one) %*% inv.R %*% Y.t) / (t(one) %*% inv.R %*% one))
  tausq = t(Y.t-mu.hat)%*% inv.R %*%(Y.t-mu.hat)/n
  eigvalues <- as.vector(getEigen(R))
  Jacob <- sum(log(YY)*(lambda.est - 1))

    #-2 negative log likelihood
    mle.v.TAAG = sum(log(eigvalues)) + n*log(tausq) + (-2*Jacob)

    #CV
    TCV <- (inv.R %*% (Y.t-mu.hat)) / diag(inv.R)
    val.tcv <- sqrt(mean(TCV^2))
    if(lambda.est == 0){
      y.hat <- exp(Y.t - TCV)
    }else{
      y.hat <- ((Y.t - TCV)*lambda.est + 1)^(1/lambda.est)
    }

    cv.v.TAAG <- sqrt(mean((YY - y.hat)^2))

  #GP-mlegp
    fit.OK <- mlegp(X=Input, Z=YY, verbose = FALSE)
    nu.GP <- sqrt(1/fit.OK$beta)
    #TAAG
    L <- matrix(0, nrow=n, ncol=n)
    for(i in 1:d){
      L <- L + (-(1/((nu.GP[i])^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2))
    }
    L <- exp(L) + 10^(-6)*Ide
    inv.L <- rcppeigen_invert_matrix(L)
    mu.hat <-  drop((t(one) %*% inv.L %*% YY) / (t(one) %*% inv.L %*% one))
    tausq = t(YY- mu.hat)%*% inv.L %*%(Y.t- mu.hat)/n
    eigvalues <- as.vector(getEigen(L))
    #-2 negative log likelihood
    mle.v.GP.mlegp = sum(log(eigvalues)) + n*log(tausq)

    #CV
    CV <- (inv.L %*% (Y.t-mu.hat)) / diag(inv.L)
    cv.v.GP.mlegp <- sqrt(mean(CV^2))

  #GP-km
    temp.m <- km(formula=~1, design=Input, response=YY,
                 covtype="gauss",nugget = (10^-8), multistart = 5,
                 control = list(trace = FALSE))
    nu.GP <- sqrt(2*(coef(temp.m)$range^2))
    #TAAG
    L <- matrix(0, nrow=n, ncol=n)
    for(i in 1:d){
      L <- L + (-(1/((nu.GP[i])^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2))
    }
    L <- exp(L) + 10^(-6)*Ide
    inv.L <- rcppeigen_invert_matrix(L)
    mu.hat <-  drop((t(one) %*% inv.L %*% YY) / (t(one) %*% inv.L %*% one))
    tausq = t(YY- mu.hat)%*% inv.L %*%(Y.t- mu.hat)/n
    eigvalues <- as.vector(getEigen(L))
    #-2 negative log likelihood
    mle.v.GP.km = sum(log(eigvalues)) + n*log(tausq)

    #CV
    CV <- (inv.L %*% (Y.t-mu.hat)) / diag(inv.L)
    cv.v.GP.km <- sqrt(mean(CV^2))

   results <- matrix(c(mle.v.TAAG,  cv.v.TAAG,
                       mle.v.GP.mlegp, cv.v.GP.mlegp,
                       mle.v.GP.km, cv.v.GP.km),nrow=3,ncol=2, byrow = T)

   colnames(results) <- c("Negative Likelihood Values","Cross validation Errors")
   rownames(results) <- c("TAAG","GP_mlegp","GP_km")

   #results <- list(Table = results, Message =  "Note: For both criteria, the model with a smaller value are a better model.")
   return(results)

}






