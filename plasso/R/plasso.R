#' Lasso and Post-Lasso
#' 
#' @description
#' \code{\link{plasso}} implicitly estimates a Lasso model using the \code{\link[glmnet]{glmnet}} package
#' and additionally estimates coefficient paths for a subsequent Post-Lasso model.
#'
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y Vector of outcomes
#' @param w Vector of weights
#' @param ... Pass \code{\link[glmnet]{glmnet}} options
#' 
#' @import glmnet
#' @import Matrix
#' @importFrom stats coef predict var approx
#' @importFrom graphics axis matplot text
#' @importFrom methods as
#'
#' @return List including base \code{\link[glmnet]{glmnet}} (i.e. Lasso) object and Post-Lasso coefficients.
#' \item{call}{the call that produced this}
#' \item{lasso_full}{base \code{\link[glmnet]{glmnet}} object}
#' \item{beta_plasso}{matrix of coefficients for Post-Lasso model stored in sparse column format}
#' \item{x}{Input matrix of covariates}
#' \item{y}{Matrix of outcomes}
#' \item{w}{Matrix of weights}
#'
#' @export
#' 
#' @examples
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit plasso to the data
#' \donttest{p = plasso::plasso(X,y)}
#' # plot coefficient paths for Post-Lasso model
#' \donttest{plot(p, lasso=FALSE, xvar="lambda")}
#' # plot coefficient paths for Lasso model
#' \donttest{plot(p, lasso=TRUE, xvar="lambda")}
#' # get coefficients for specific lambda approximation
#' \donttest{coef(p, s=0.05)}
#' # predict fitted values along whole lambda sequence 
#' \donttest{pred = predict(p)}
#' \donttest{head(pred$plasso)}
#'
plasso = function(x,y,
                  w=NULL,
                  ...) {
  
  this.call = match.call()
  # Handle potentially provided sample weights, otherwise create weight vector of ones
  w = handle_weights(w,nrow(x))
  # Create variable names if not provided
  if ( is.null( colnames(x) ) ) colnames(x) = sprintf("var%s",seq_len(ncol(x)))
  
  # Lasso with full estimation sample
  lasso_full = glmnet(x,y,weights=as.vector(w),family="gaussian",...)
  coef_lasso_full = coef(lasso_full)                   # Save coefficients to extract later the ones at the CV minima
  lambda = lasso_full$lambda                           # Save grid to use the same in cross-validation
  x = add_intercept(x)
  
  l = length(lambda)
  coef_plasso_full = matrix(NA,
                            nrow=dim(coef_lasso_full)[1],ncol=dim(coef_lasso_full)[2],
                            dimnames=list(rownames(coef_lasso_full),colnames(coef_lasso_full)))
  
  for (i in seq_len(l)) {
    
    nm_act = names(coef_lasso_full[,i])[which(coef_lasso_full[,i] != 0)]
    coef_plasso_full[,i] = fit_betas(x,y,w,nm_act,coef_lasso_full[,i])
    
  }
  coef_plasso_full = as(coef_plasso_full, "dgCMatrix")
  
  output = list("call"=this.call,
                "lasso_full"=lasso_full,
                "beta_plasso"=coef_plasso_full,
                "x"=x[,-1],"y"=y,"w"=w)
  
  class(output) = "plasso"
  return(output)
}


#' Print (Post-) Lasso model
#' 
#' @description
#' Printing main insights from (Post-) Lasso model.
#'
#' @param x \code{\link{plasso}} object
#' @param ... Pass generic \code{\link[base]{print}} options
#' @param digits Integer, used for number formatting
#'
#' @return Prints \code{\link[glmnet]{glmnet}}-like output.
#' 
#' @method print plasso
#'
#' @export
#'
print.plasso = function(x,...,digits=max(3, getOption("digits")-3)) {
  
  plasso = x
  cat("\nCall: ", deparse(plasso$call), "\n\n")
  out = data.frame(Df=plasso$lasso_full$df,
                   `%Dev`=round(plasso$lasso_full$dev.ratio*100, 2),
                   Lambda=signif(plasso$lasso_full$lambda, digits),
                   check.names=FALSE,row.names=seq(along=plasso$lasso_full$df))
  class(out) = c("anova",class(out))
  print(out)
}


#' Summary of (Post-) Lasso model
#' 
#' @description
#' Summary of (Post-) Lasso model.
#'
#' @param object \code{\link{plasso}} object
#' @param ... Pass generic \code{\link[base]{summary}} summary options
#'
#' @return Default \code{\link[base]{summary}} object
#' 
#' @method summary plasso
#'
#' @export
#'
summary.plasso = function(object,...) {
  
  return(summary.default(object, ...))
  
}


#' Predict for (Post-) Lasso models
#' 
#' @description
#' Prediction for (Post-) Lasso models.
#'
#' @param object Fitted \code{\link{plasso}} model object
#' @param ... Pass generic \code{\link[stats]{predict}} options
#' @param newx Matrix of new values for x at which predictions are to be made. If no value is supplied, x from fitting procedure is used. This argument is not used for type="coefficients".
#' @param type Type of prediction required. "response" returns fitted values, "coefficients" returns beta estimates.
#' @param s If Null, prediction is done for all lambda values. If a value is provided, the closest lambda value of the \code{\link{plasso}} object is used.
#' 
#' @return List object containing either fitted values or coefficients for both
#' the Lasso and Post-Lasso models associated with all values along the lambda
#' input sequence or for one specifically given lambda value.
#' \item{lasso}{Matrix with Lasso predictions or coefficients}
#' \item{plasso}{Matrix with Post-Lasso predictions or coefficients}
#'
#' @method predict plasso
#'
#' @export
#' 
#' @examples
#' 
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit plasso to the data
#' \donttest{p = plasso::plasso(X,y)}
#' # predict fitted values along whole lambda sequence 
#' \donttest{pred = predict(p)}
#' \donttest{head(pred$plasso)}
#' # get estimated coefficients for specific lambda approximation
#' \donttest{predict(p, type="coefficients", s=0.05)}
#'
predict.plasso = function(object,
                          ...,
                          newx=NULL,
                          type=c("response","coefficients"),
                          s=NULL
                          ) {
  
  plasso = object
  type = match.arg(type)
  
  if (is.null(newx)) x = plasso$x else x = newx
  if ( is.null( colnames(x) ) ) colnames(x) = sprintf("var%s",seq_len(ncol(x)))
  x = add_intercept(x)
  y = plasso$y
  lambda_names = names(plasso$lasso_full$a0)
  
  if (is.null(s)) {
    
    l = length(plasso$lasso_full$lambda)
    
    coef_lasso = as.matrix(coef(plasso$lasso_full))
    colnames(coef_lasso) = lambda_names
    coef_plasso = as.matrix(plasso$beta_plasso)
    colnames(coef_plasso) = lambda_names
    
    if (type == "coefficients") {
      
      return(list(
        "lasso"=as(coef_lasso,"dgCMatrix"),
        "plasso"=as(coef_plasso,"dgCMatrix")
        )
      )
      
    } else if (type == "response"){
      
      fit_lasso = matrix(NA,nrow=nrow(x),ncol=l,dimnames=list(NULL,lambda_names))
      fit_plasso = matrix(NA,nrow=nrow(x),ncol=l,dimnames=list(NULL,lambda_names))
      
      for (i in seq_len(l)) {
        
        fit_lasso[,i] = x %*% coef_lasso[,i]
        fit_plasso[,i] = x %*% coef_plasso[,i]
      }
      
      return(list("lasso"=fit_lasso,"plasso"=fit_plasso))
    }
      
  } else if (is.numeric(s) && length(s) == 1){
    
    abs_diff = abs(plasso$lasso_full$lambda - s)
    closest_index = which.min(abs_diff)
    closest_lambda = plasso$lasso_full$lambda[closest_index]
    
    coef_lasso = as.matrix(coef(plasso$lasso_full)[,closest_index])
    colnames(coef_lasso) = lambda_names[2]
    coef_plasso = as.matrix(plasso$beta_plasso[,closest_index])
    colnames(coef_plasso) = lambda_names[2]
    

    if (type == "coefficients") {
      
      return(list(
        "lasso"=as(coef_lasso,"dgCMatrix"),
        "plasso"=as(coef_plasso,"dgCMatrix")
        )
      )
      
    } else if (type == "response"){
      
      fit_lasso = as.matrix(x %*% coef_lasso)
      fit_plasso = as.matrix(x %*% coef_plasso)
      colnames(fit_lasso) = lambda_names[2]
      colnames(fit_plasso) = lambda_names[2]
      
      return(list("lasso"=fit_lasso,"plasso"=fit_plasso))
      
    }
  }
}


#' Extract coefficients from a \code{\link{plasso}} object
#' 
#' @description
#' Extract coefficients for both Lasso and Post-Lasso from a \code{\link{plasso}} object.
#' 
#' @param object \code{\link{plasso}} object
#' @param ... Pass generic \code{\link[stats]{coef}} options
#' @param s If Null, coefficients are returned for all lambda values. If a value is provided, the closest lambda value of the \code{\link{plasso}} object is used.
#' 
#' @return List object containing coefficients that are associated with either
#' all values along the lambda input sequence or for one specifically given
#' lambda value for both the Lasso and Post-Lasso models respectively.
#' \item{lasso}{Sparse \code{\link[Matrix]{dgCMatrix-class}} object with Lasso coefficients}
#' \item{plasso}{Sparse \code{\link[Matrix]{dgCMatrix-class}} object with Post-Lasso coefficients}
#'
#' @method coef plasso
#'
#' @export 
#' 
#' @examples
#' 
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit plasso to the data
#' \donttest{p = plasso::plasso(X,y)}
#' # get estimated coefficients along whole lambda sequence 
#' \donttest{coefs = coef(p)}
#' \donttest{head(coefs$plasso)}
#' # get estimated coefficients for specific lambda approximation
#' \donttest{coef(p, s=0.05)}
#'
coef.plasso = function(object,...,s=NULL){
  return(predict(object,...,s=s,type="coefficients"))
}


#' Plot coefficient paths
#' 
#' @description
#' Plot coefficient paths of (Post-) Lasso model.
#' 
#' @param x \code{\link{plasso}} object
#' @param ... Pass generic \code{\link[base]{plot}} options
#' @param lasso If set as True, coefficient paths for Lasso instead of Post-Lasso is plotted. Default is False.
#' @param xvar X-axis variable:
#' \code{norm} plots against the L1-norm of the coefficients,
#' \code{lambda} against the log-lambda sequence,
#' and \code{dev} against the percent deviance explained.
#' @param label If TRUE, label the curves with variable sequence numbers
#' 
#' @return Produces a coefficient profile plot of the coefficient paths for a fitted \code{\link{plasso}} object.
#'
#' @method plot plasso
#'
#' @export
#' 
#' @examples
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit plasso to the data
#' \donttest{p = plasso::plasso(X,y)}
#' # plot coefficient paths for Post-Lasso model
#' \donttest{plot(p, lasso=FALSE, xvar="lambda")}
#' # plot coefficient paths for Lasso model
#' \donttest{plot(p, lasso=TRUE, xvar="lambda")}
#'
plot.plasso = function(x,..., lasso=FALSE,xvar=c("norm","lambda","dev"),label=FALSE) {
  
  
  nzC = utils::getFromNamespace("nonzeroCoef", "glmnet")
  
  if (!lasso) {
    
    lambda = x$lasso_full$lambda
    df = x$lasso_full$df
    dev = x$lasso_full$dev.ratio
    
    beta = x$beta_plasso[-1,]
    
    which = nzC(beta)
    nwhich = length(which)
    
    switch(nwhich+1,#we add one to make switch work
           "0"={
             warning("No plot produced since all coefficients zero")
             return()
           },
           "1"=warning("1 or less nonzero coefficients; glmnet plot is not meaningful")
    )
    beta = as.matrix(beta[which,,drop=FALSE])
    xvar = match.arg(xvar)
    switch(xvar,
           "norm"={
             index=apply(abs(beta),2,sum)
             iname="L1 Norm"
             approx.f=1
           },
           "lambda"={
             index=log(lambda)
             iname="Log Lambda"
             approx.f=0
           },
           "dev"= {
             index=dev
             iname="Fraction Deviance Explained"
             approx.f=1
           }
    )

    matplot(index,t(beta),lty=1,type="l",xlab=iname,ylab="Coefficients",...)
    
    atdf = pretty(index)
    prettydf = approx(x=index,y=df,xout=atdf,rule=2,method="constant",f=approx.f)$y
    
    axis(3,at=atdf,labels=prettydf,tcl=NA)
    if(label){
      nnz = length(which)
      xpos = max(index)
      pos = 4
      if(xvar == "lambda"){
        xpos = min(index)
        pos = 2
      }
      xpos = rep(xpos,nnz)
      ypos = beta[,ncol(beta)]
      text(xpos,ypos,paste(which),cex=.5,pos=pos)
    }
    
  } else if (lasso) {
    
    plot(x$lasso_full,xvar=xvar,label=label,...)
    
  }
  
}

