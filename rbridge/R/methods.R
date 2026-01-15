scalar_predict <- function(object, newx, s=c("lambda.1se","lambda.min"), type){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type[1]=="response"){
    coef <- object$betas[,index]
    output <- as.numeric(cbind(1,newx) %*% coef)
  } else {
    coef <- object$betas[,index]
    output <- coef  
  }  
  return(output)
}
#' @title Make predictions from a 'cv.rbridge' object
#' @param object A 'cv.rbridge' object.
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param type It should one of "response", "nonzero" or "coefficients". The "response" is for predicted values, the "nonzero" is for exacting non-zero coefficients and the "coefficients" is for the estimated coefficients. 
#' @param ... Additional arguments for compatibility.
#' @return Among a matrix with predictions, a vector non-zero indexing or a vector of coefficients
#' @description
#' Makes predictions from a cross-validated 'cv.rbridge' model
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{coef.cv.rbridge}}
#' 
#' @author Bahadir Yuzbasi
#' maintainer Baha
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' ### Restricted Matrix and vector
#' c1 <- c(1,1,0,0,1,0,0,0)
#' R1.mat <- matrix(c1,nrow = 1, ncol = p)
#' r1.vec <- as.matrix(c(6.5),1,1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 based on first restrictions
#' model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec)
#' coef(model1,s='lambda.min')
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' @export
predict.cv.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}




#' @title Extract coefficients from a 'cv.rbridge' object
#' @param object A 'cv.rbridge' object.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param ... Additional arguments for compatibility.
#' @return A vector of coefficients
#' @description
#' Extract coefficients from a 'cv.rbridge' object.
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{predict.cv.rbridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' ### Restricted Matrix and vector
#' c1 <- c(1,1,0,0,1,0,0,0)
#' R1.mat <- matrix(c1,nrow = 1, ncol = p)
#' r1.vec <- as.matrix(c(6.5),1,1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 based on first restrictions
#' model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec)
#' coef(model1,s='lambda.min')
#' @export
coef.cv.rbridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}



#' @title Make predictions from a 'rbridge' object
#' @param object A 'rbridge' object.
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param type It should one of "response", "nonzero" or "coefficients". The "response" is for predicted values, the "nonzero" is for exacting non-zero coefficients and the "coefficients" is for the estimated coefficients. 
#' @param ... Additional arguments for compatibility.
#' @return Among a matrix with predictions, a vector non-zero indexing or a vector of coefficients
#' @description
#' Makes predictions from a cross-validated 'rbridge' model
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{coef.cv.bridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' ### Restricted Matrix and vector
#' c1 <- c(1,1,0,0,1,0,0,0)
#' R1.mat <- matrix(c1,nrow = 1, ncol = p)
#' r1.vec <- as.matrix(c(6.5),1,1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 based on first restrictions
#' model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec)
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' @export
predict.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[['lambda']])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}

#' @title Extract coefficients from a 'rbridge' object
#' @param object A 'rbridge' object.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param ... Additional arguments for compatibility.
#' @return Among a matrix with predictions, a vector non-zero indexing or a vector of coefficients
#' @description
#' Makes predictions from a cross-validated 'rbridge' model
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{predict.rbridge}}
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' ### Restricted Matrix and vector
#' c1 <- c(1,1,0,0,1,0,0,0)
#' R1.mat <- matrix(c1,nrow = 1, ncol = p)
#' r1.vec <- as.matrix(c(6.5),1,1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 based on first restrictions
#' model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec)
#' coef(model1,s='lambda.min')
#' 
#' @export
coef.rbridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}



#### Bridge
#' @title Make predictions from a 'cv.bridge' object
#' @param object A 'cv.bridge' object.
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param type It should one of "response", "nonzero" or "coefficients". The "response" is for predicted values, the "nonzero" is for exacting non-zero coefficients and the "coefficients" is for the estimated coefficients. 
#' @param ... Additional arguments for compatibility.
#' @return Among a matrix with predictions, a vector non-zero indexing or a vector of coefficients
#' @description
#' Makes predictions from a cross-validated 'cv.bridge' model
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{coef.cv.bridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' model1 <- cv.bridge(X, y, q = 1)
#' coef(model1,s='lambda.min')
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' @export
predict.cv.bridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                              type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[[5]])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}




#' @title Extract coefficients from a 'cv.bridge' object
#' @param object A 'cv.bridge' object.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param ... Additional arguments for compatibility.
#' @return  A vector of coefficients
#' @description
#' Extract coefficients from a 'cv.bridge' object.
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{predict.cv.rbridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' model1 <- cv.bridge(X, y, q = 1)
#' coef(model1,s='lambda.min')
#' @export
coef.cv.bridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}



#' @title Make predictions from a 'bridge' object
#' @param object A 'bridge' object.
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param type It should one of "response", "nonzero" or "coefficients". The "response" is for predicted values, the "nonzero" is for exacting non-zero coefficients and the "coefficients" is for the estimated coefficients. 
#' @param ... Additional arguments for compatibility.
#' @return Among a matrix with predictions, a vector non-zero indexing or a vector of coefficients
#' @description
#' Makes predictions from a cross-validated 'bridge' model
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{coef.bridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' model1 <- bridge(X, y, q = 1)
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' @export
predict.bridge = function(object, newx, s=c("lambda.min","lambda.1se"), 
                           type = c("response", "nonzero","coefficients"), ...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  index = match(object[[s]],object[[2]])
  if(type =="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if((p+1) != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- scalar_predict(object, newx, s, type)
  }else if(type =="nonzero"){
    output <- nonzeroCoef(object$betas[-1,,drop=FALSE],bystep=TRUE)
  }else {
    output <- scalar_predict(object, newx, s, type)
    output <- as.matrix(output)
  }
  return(output)
}

#' @title Extract coefficients from a 'bridge' object
#' @param object A 'bridge' object.
#' @param s Value(s) of the penalty parameter lambda at which predictions are required.
#' @param ... Additional arguments for compatibility.
#' @return  A vector of coefficients
#' @description
#' Extract coefficients from a 'bridge' object.
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{predict.bridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' model1 <- bridge(X, y, q = 1)
#' coef(model1,s='lambda.min')
#' @export
coef.bridge=function(object, s=c("lambda.1se","lambda.min"),...){
  s = match.arg(s, c("lambda.min", "lambda.1se"))
  return(predict(object, s=s, type = "coefficients"))
}



#' @title Plot a 'cv.rbridge' object function
#' @param x Design matrix.
#' @param sign.lambda Either plot against \code{log(lambda)} (default) or its negative if sign.\code{lambda=-1}.
#' @param ... Other graphical parameters to plot
#' @description
#' Plots the cross-validation curve, and upper and lower standard deviation curves, as a function of the lambda values used.
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @rdname plot.cv.rbridge
#' @export
plot.cv.rbridge=function(x,sign.lambda=1,...){
  object = x
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}



### Bridge
#' @title Plot a 'cv.bridge' object function
#' @param x Design matrix.
#' @param sign.lambda Either plot against \code{log(lambda)} (default) or its negative if sign.\code{lambda=-1}.
#' @param ... Other graphical parameters to plot
#' @description
#' Plots the cross-validation curve, and upper and lower standard deviation curves, as a function of the lambda values used.
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @rdname plot.cv.bridge
#' @export
plot.cv.bridge=function(x,sign.lambda=1,...){
  object = x
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}
