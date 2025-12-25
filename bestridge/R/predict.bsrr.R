#' make predictions from a "bsrr" object.
#'
#' Returns predictions from a fitted
#' "\code{bsrr}" object.
#'
#' @param object Output from the \code{bsrr} function.
#' @param newx New data used for prediction. If omitted, the fitted linear predictors are used.
#' @param type \code{type = "link"} gives the linear predictors for \code{"binomial"},
#' \code{"poisson"} or \code{"cox"} models; for \code{"gaussian"} models it gives the
#' fitted values. \code{type = "response"} gives the fitted probabilities for
#' \code{"binomial"}, fitted mean for \code{"poisson"} and the fitted relative-risk for
#' \code{"cox"}; for \code{"gaussian"}, \code{type = "response"} is equivalent to \code{type = "link"}
#' @param \dots Additional arguments affecting the predictions produced.
#' @return The object returned depends on the types of family.
#' @seealso \code{\link{bsrr}}.
#' @inherit bsrr return author
#' @examples
#'
#' #-------------------linear model----------------------#
#' # Generate simulated data
#' n <- 200
#' p <- 20
#' k <- 5
#' rho <- 0.4
#' seed <- 10
#' Tbeta <- rep(0, p)
#' Tbeta[1:k*floor(p/k):floor(p/k)] <- rep(1, k)
#' Data <- gen.data(n, p, k, rho, family = "gaussian", beta = Tbeta, seed = seed)
#' x <- Data$x[1:140, ]
#' y <- Data$y[1:140]
#' x_new <- Data$x[141:200, ]
#' y_new <- Data$y[141:200]
#' lambda.list <- exp(seq(log(5), log(0.1), length.out = 10))
#' lm.bsrr <- bsrr(x, y, method = "pgsection")
#'
#' pred.bsrr <- predict(lm.bsrr, newx = x_new)
#'
#' #-------------------logistic model----------------------#
#' #Generate simulated data
#' Data <- gen.data(n, p, k, rho, family = "binomial", beta = Tbeta, seed = seed)
#'
#' x <- Data$x[1:140, ]
#' y <- Data$y[1:140]
#' x_new <- Data$x[141:200, ]
#' y_new <- Data$y[141:200]
#' lambda.list <- exp(seq(log(5), log(0.1), length.out = 10))
#' logi.bsrr <- bsrr(x, y, tune="cv",
#'                  family = "binomial", lambda.list = lambda.list, method = "sequential")
#'
#' pred.bsrr <- predict(logi.bsrr, newx = x_new)
#'
#' #-------------------coxph model----------------------#
#' #Generate simulated data
#' Data <- gen.data(n, p, k, rho, family = "cox", beta = Tbeta, scal = 10)
#'
#' x <- Data$x[1:140, ]
#' y <- Data$y[1:140, ]
#' x_new <- Data$x[141:200, ]
#' y_new <- Data$y[141:200, ]
#' lambda.list <- exp(seq(log(5), log(0.1), length.out = 10))
#' cox.bsrr <- bsrr(x, y, family = "cox", lambda.list = lambda.list)
#'
#' pred.bsrr <- predict(cox.bsrr, newx = x_new)
#'
#'#-------------------group selection----------------------#
#'beta <- rep(c(rep(1,2),rep(0,3)), 4)
#'Data <- gen.data(200, 20, 5, rho=0.4, beta = beta, seed =10)
#'x <- Data$x
#'y <- Data$y
#'
#'group.index <- c(rep(1, 2), rep(2, 3), rep(3, 2), rep(4, 3),
#'                 rep(5, 2), rep(6, 3), rep(7, 2), rep(8, 3))
#'lm.groupbsrr <- bsrr(x, y, s.min = 1, s.max = 8, group.index = group.index)
#'
#'pred.groupbsrr <- predict(lm.groupbsrr, newx = x_new)
#'
#'@method predict bsrr
#'@export
#'@export predict.bsrr
predict.bsrr <- function(object, newx, type = c("link", "response"), ...)
{
  # if(!is.null(object$factor)){
  #   factor <- c(object$factor)
  #   if(!is.data.frame(newx)) newx <- as.data.frame(newx)
  #   newx[,factor] <- apply(newx[,factor,drop=FALSE], 2, function(x){
  #     return(as.factor(x))
  #   })
  #   newx <- model.matrix(~., data = newx)[,-1]
  # }
  if(missing(newx)){
    newx = object$x
  }
  if(is.null(colnames(newx))) {
    newx <- as.matrix(newx)
  }else{
    vn <- names(object$beta)
    if(any(is.na(match(vn, colnames(newx))))) stop("names of newx don't match training data!")
    newx <- as.matrix(newx[,vn])
  }
  type <- match.arg(type)
  if(object$family=="gaussian")
  {
    betas <- object$beta
    coef0 <- object$coef0

    y <- drop(newx %*% betas)+coef0
    return(y)
  }
  if(object$family == "binomial")
  {
    betas <- object$beta
    coef <- object$coef0
    # class = as.numeric(exp(newx%*%betas+coef)/(1+exp(newx%*%betas+coef))>0.5)
    # class[is.na(class)] = 1
    # if(!is.null(object$y_names))
    # {
    #   class[which(class == 0,arr.ind = T)] = object$y_names[1]
    #   class[which(class == 1,arr.ind = T)] = object$y_names[2]
    # }
    #if(sum(is.infinite(exp(newx%*%betas+coef))) > 0) print(sum(is.infinite(exp(newx%*%betas+coef))))
    if(type  == "link"){
        link <- newx%*%betas+coef
        return(drop(link))
    } else{
        prob <- ifelse(is.infinite(exp(newx%*%betas+coef)), 1, exp(newx%*%betas+coef)/(1+exp(newx%*%betas+coef)))
        return(drop(prob))
    }

  }
  if(object$family == "poisson"){
    betas <- object$beta

    eta <- newx%*%betas
    if(type == "link"){
      return(eta)
    }else{
      expeta <- exp(eta)
      return(drop(expeta))
    }

  }
  if(object$family == "cox")
  {
    betas <- object$beta

    eta <- newx%*%betas;
    if(type == "link"){
      return(eta)
    } else{
      expeta <- exp(eta)
      return(drop(expeta))
    }

  }

}

