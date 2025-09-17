ilse <- function(...) UseMethod("ilse")


# methods(generic.function = ilse)

ilse.formula <- function(formula, data=NULL, bw=NULL, k.type=NULL, method="Par.cond", ...){
  if(is.null(formula)) stop('formula must be given!')
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  if(is.null(data)){
    ## generate data from the current environment
    data <- model.frame(formula = formula, na.action=NULL)
  }
  ## obtain name of response variable
  form <- terms(formula, data=data)
  vars <- attr(form, "variables")
  resp <- row.names(attr(form, "factors"))[1]

  ## obtain design matrix
  if("(Intercept)" %in% colnames(data)) data$`(Intercept)` <- NULL
  Xmat <- model.matrix.lm(object = formula, data=data, na.action = "na.pass")
  # XYdat <- model.frame(formula = formula, data = data, na.action=NULL)
  XYdat <- cbind(data[[resp]], Xmat)
  colnames(XYdat)[1] <- resp
  data <- as.data.frame(XYdat)

  np <- dim(XYdat)
  XYdat <- as.matrix(XYdat)
  Y <- XYdat[,1]
  X <- XYdat[,-1]
  n <- length(Y)
  X <- matrix(X, nrow=np[1], ncol=np[2]-1)
  colnames(X) <- colnames(XYdat)[2:np[2]]


  cl <- match.call()
  cl[[1]] <- as.name("clse")
  if(!is.matrix(X)) X <- as.matrix(X)
  res <- ilse.numeric(Y, X, bw=bw,  k.type=k.type, method=method, ...)


  res$call <- cl
  res$formula <- formula
  res$data <- data
  class(res) <- 'ilse'
  return(res)
}



ilse.numeric <- function(Y, X,bw=NULL, k.type=NULL, method="Par.cond", max.iter=20, peps=1e-5, feps = 1e-7, arma=TRUE, verbose=FALSE, ...){
  if(is.null(Y) || is.null(X)) stop('"X","Y" must be given
                                    simutaneously!')
  if (is.null(n <- nrow(X)))
    stop("'X' must be a matrix")
  if (n == 0L)
    stop("0 (non-NA) cases")
  if(!is.null(bw) && !is.numeric(bw)) stop('"bw" must be NULL or a positive scalar!')
  if(!is.matrix(X)) X <- as.matrix(X)

  ## obtain initial values of beta
  p <- ncol(X)
  if(sum(complete.cases(X))>= 2*p){
    cc.coef <- as.numeric(lm(Y~.+0, data.frame(X))$coef) # complete cases' estimate as initial estimate
  }else{
    Xtemp <- apply(X, 2, function(x) {
      x[is.na(x)] <- mean(x, na.rm=TRUE)
      return(x)
      })
    cc.coef <- as.numeric(lm(Y~.+0, data.frame(Xtemp))$coef)
    rm(Xtemp)
  }

  if(any(is.na(cc.coef)))  cc.coef[is.na(cc.coef)] <- 1

  if(is.null(bw)){

    Xmat0 <- X
    Xmat0[is.na(X)] <- 0
    bw  <- n^(-1/3)* sd(Xmat0%*%matrix(cc.coef,p,1))
    rm(Xmat0)
  }

  bw <- ifelse(bw >1, bw, 1)



  if(is.null(k.type)) k.type <- 'gaussian'
  k.type <- tolower(k.type)

  if(arma){ # use Rcpp to evalute
      k.type1 <- switch (k.type,
        gaussian = 1,
        ekp = 2,
        biweight = 3,
        triweight = 4,
        tricube = 5,
        cosine = 6,
        uniform = 7,
        triangle = 8
      )
      res <- ilse_arma(Y, X, cc.coef, bw, k.type1, max.iter, peps, feps, verbose = verbose)
      finalm <- lm(Y~.+0, data.frame(res$hX))
      names(res$beta) <- colnames(X)
      beta.new <- res$beta
      d.fn = res$d.fn; d.par=res$d.par; iterations=res$Iterations
      Z2 <- res$hX
  }else{ # # use R to evalute

    Xmat0 <- X
    Xmat0[is.na(X)] <- 0
    beta <- cc.coef
    rss <- rss.old <- sqrt(sum((Y-Xmat0%*%matrix(beta,p,1))^2))
    k <- 0

    k <- k+1
    NA_ind <- which(apply(is.na(X), 1, sum) !=0)
    Z2  <- Xmat0  #
    IDX <- indx.comp(X)
    while (k < max.iter){

      Z1 <- X
      for(i in NA_ind){
        temp_Z1 <- kern.est(ind=i, beta=beta, Xmat=X, Y=Y,IDX=IDX,bw=bw,k.type=k.type)
        if(is.null(temp_Z1)){return(NULL)}else{
          Z1[i,] <- temp_Z1
        }
      }
      Z2[is.na(X)] <- Z1[is.na(X)]
      if(method =="Full.cond"){
        xtx <- t(Z2) %*% Z2
        xty <- t(Z1)%*%Y
        beta.new <- solve(xtx)%*%xty
      }else if (method =="Par.cond"){
        beta.new <- lm(Y~.+0, data.frame(Z2))$coef

      }

      rss.new <- sqrt(sum((Y-Xmat0%*%beta.new)^2))

      d.fn <- abs(rss.new - rss.old) / rss.old
      d.par <- max(abs(beta.new-beta)) / max(abs(beta))

      if(verbose){
        message("iter=", k, ", d.fn=", format(d.fn,scientific = TRUE, digits = 4),
                ", d.par=", format(d.par,scientific = TRUE, digits = 4))
        #message("coefs:", format(as.vector(beta.new), digits = 4),'\n')
      }
      beta <- beta.new
      # Bmat <- rbind(Bmat,matrix(beta.new, nrow=1))
      rss.old <- rss.new
      rss <- c(rss,rss.new)
      if ((d.par < peps )| (d.fn < feps) ){
        break
      }
      k <- k+1
      if(k==max.iter){warning('Iteration reaches maximum limit!')}
    }
    iterations <- k
  }

  beta.new <- as.vector(beta.new)
  names(beta.new) <- colnames(X)
  finalm <- lm(Y~.+0, data.frame(Z2))
  # row.names(Bmat) <- paste0('iter', 0:k) # Bmat = Bmat,
  res <- list(beta=beta.new, hX=Z2, d.fn=d.fn, d.par=d.par, iterations=iterations,
              residuals = finalm$residuals, fitted.values=finalm$fitted.values,
              inargs=list(bw=bw, k.type=k.type, method=method, max.iter=max.iter, peps=peps,
                          feps = feps,infor_output=verbose, arma=arma))
  class(res) <- 'ilse'
  return(res)
}



# ilse.numeric <- function(Y, X,bw=NULL, k.type=NULL, method="Par.cond", max.iter=50, peps=1e-5, feps = 1e-7,verbose=FALSE, ...){
#   if(is.null(Y) || is.null(X)) stop('"X","Y" must be given
#                                     simutaneously!')
#   if (is.null(n <- nrow(X)))
#     stop("'X' must be a matrix")
#   if (n == 0L)
#     stop("0 (non-NA) cases")
#   if(!is.null(bw) && !is.numeric(bw)) stop('"bw" must be NULL or a positive scalar!')
#   if(!is.matrix(X)) X <- as.matrix(X)
#   p <- ncol(X)
#   i.com <- which(complete.cases(X))
#
#
#   if(length(i.com)>= 2*p){
#     cc.coef <- as.numeric(lm(Y~.+0, data.frame(X), subset= i.com )$coef) # complete cases' estimate as initial estimate
#   }else{
#     Xtemp <- apply(X, 2, function(x) {
#       x[is.na(x)] <- mean(x, na.rm=TRUE)
#       return(x)
#     })
#     cc.coef <- as.numeric(lm(Y~.+0, data.frame(Xtemp))$coef)
#     rm(Xtemp)
#   }
#   Xmat0 <- X
#   Xmat0[is.na(X)] <- 0
#   if(any(is.na(cc.coef)))  cc.coef[is.na(cc.coef)] <- 1
#   if(is.null(bw)) bw  <- n^(-1/3)* sd(Xmat0%*%matrix(cc.coef,p,1))
#   bw <- ifelse(bw >1, bw, 1)
#   rm(Xmat0)
#
#   if(is.null(k.type)) k.type <- 'gaussian'
#   k.type <- tolower(k.type)
#
#   if(method == "Par.cond"){
#
#     k.type1 <- switch (k.type,
#       gaussian = 1,
#       ekp = 2,
#       biweight = 3,
#       triweight = 4,
#       tricube = 5,
#       cosine = 6,
#       uniform = 7,
#       triangle = 8
#     )
#     res <- ilse_arma(Y, X, cc.coef, bw, k.type1, max.iter, peps, feps, verbose = verbose)
#     finalm <- lm(Y~.+0, data.frame(res$hX))
#     names(res$beta) <- colnames(X)
#   }else{
#     Xmat0 <- X
#     Xmat0[is.na(X)] <- 0
#     Z2  <- Xmat0  # X^tilde
#     IDX <- indx.comp(X)
#     beta <- cc.coef
#     Bmat <- beta
#     rss <- rss.old <- sqrt(sum((Y-Xmat0%*%matrix(beta,p,1))^2))
#     k <- 0
#     if(infor_output==TRUE){
#       message("iter=", k, ", d.fn=",NA, ", d.par=", NA, "\n")
#       # message("par:", format(as.vector(beta), digits = 4),'\n')
#     }
#     k <- k+1
#     NA_ind <- which(apply(is.na(X), 1, sum) !=0)
#     while (k < max.iter){
#       # W <- Xmat0 %*% beta
#       #Z1 <- t(vapply(1:n, kern.est, FUN.VALUE=numeric(p), beta = beta, Xmat = X, Y= Y,  IDX = IDX, bw = bw, K=K, bw.type=bw.type))
#       #Z2[Xmat0==0] <- Z1[Xmat0==0]
#       Z1 <- X
#       for(i in NA_ind){
#         temp_Z1 <- kern.est(ind=i, beta=beta, Xmat=X, Y=Y,IDX=IDX,bw=bw,k.type=k.type)
#         if(is.null(temp_Z1)){return(NULL)}else{
#           Z1[i,] <- temp_Z1
#         }
#       }
#       Z2[is.na(X)] <- Z1[is.na(X)]
#
#       xtx <- t(Z2) %*% Z2
#       xty <- t(Z1)%*%Y
#       beta.new <- solve(xtx)%*%xty
#
#
#
#       rss.new <- sqrt(sum((Y-Xmat0%*%beta.new)^2))
#
#       d.fn <- abs(rss.new - rss.old) / rss.old
#       d.par <- max(abs(beta.new-beta)) / max(abs(beta))
#
#       if(infor_output==TRUE){
#         message("iter=", k, ", d.fn=", format(d.fn,scientific = TRUE, digits = 4),
#                 ", d.par=", format(d.par,scientific = TRUE, digits = 4))
#         #message("coefs:", format(as.vector(beta.new), digits = 4),'\n')
#       }
#       beta <- beta.new
#       Bmat <- rbind(Bmat,matrix(beta.new, nrow=1))
#       rss.old <- rss.new
#       rss <- c(rss,rss.new)
#       if ((d.par < peps )| (d.fn < feps) ){
#         break
#       }
#       k <- k+1
#       if(k==max.iter){warning('Iteration reaches maximum limit!')}
#     }
#     beta.new <- as.vector(beta.new)
#     names(beta.new) <- colnames(X)
#     hX <- Z2
#     finalm <- lm(Y~.+0, data.frame(hX))
#     res <- list(beta=beta.new, hX=hX, iterations=k, d.fn=d.fn, d.par=d.par)
#   }
#   res <- c(res, list(residuals = finalm$residuals, fitted.values=finalm$fitted.values,
#               inargs=list(bw=bw, k.type=k.type, method=method, max.iter=max.iter, peps=peps,
#                           feps = feps, verbose=verbose)))
#   return(res)
# }
#
