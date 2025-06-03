#' Cross-Validated Lasso and Post-Lasso
#' 
#' @description
#' \code{\link{cv.plasso}} uses the \code{\link[glmnet]{glmnet}} package to estimate the coefficient paths and cross-validates least squares Lasso AND Post-Lasso.
#'
#' @param x Matrix of covariates (number of observations times number of covariates matrix)
#' @param y Vector of outcomes
#' @param w Vector of weights
#' @param kf Number of folds in k-fold cross-validation
#' @param parallel Set as TRUE for parallelized cross-validation. Default is FALSE.
#' @param ... Pass \code{\link[glmnet]{glmnet}} options
#' 
#' @import glmnet
#' @import Matrix
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import iterators
#' @importFrom stats coef predict var
#'
#' @return cv.plasso object (using a list structure) including the base \code{\link[glmnet]{glmnet}} object and cross-validation results (incl. optimal Lambda values) for both Lasso and Post-Lasso model.
#' \item{call}{the call that produced this}
#' \item{lasso_full}{base \code{\link[glmnet]{glmnet}} object}
#' \item{kf}{number of folds in k-fold cross-validation}
#' \item{cv_MSE_lasso}{cross-validated MSEs of Lasso model (for every iteration of k-fold cross-validation)}
#' \item{cv_MSE_plasso}{cross-validated MSEs of Post-Lasso model (for every iteration of k-fold cross-validation)}
#' \item{mean_MSE_lasso}{averaged cross-validated MSEs of Lasso model}
#' \item{mean_MSE_plasso}{averaged cross-validated MSEs of Post-Lasso model}
#' \item{ind_min_l}{index of MSE optimal lambda value for Lasso model}
#' \item{ind_min_pl}{index of MSE optimal lambda value for Post-Lasso model}
#' \item{lambda_min_l}{MSE optimal lambda value for Lasso model}
#' \item{lambda_min_pl}{MSE optimal lambda value for Post-Lasso model}
#' \item{names_l}{Names of active variables for MSE optimal Lasso model}
#' \item{names_pl}{Names of active variables for MSE optimal Post-Lasso model}
#' \item{coef_min_l}{Coefficients for MSE optimal Lasso model}
#' \item{coef_min_pl}{Coefficients for MSE optimal Post-Lasso model}
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
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#' # get basic summary statistics
#' \donttest{print(summary(p.cv, default=FALSE))}
#' # plot cross-validated MSE curves and number of active coefficients
#' \donttest{plot(p.cv, legend_pos="bottomleft")}
#' # get coefficients at MSE optimal lambda value for both Lasso and Post-Lasso model
#' \donttest{coef(p.cv)}
#' # get coefficients at MSE optimal lambda value according to 1-standard-error rule
#' \donttest{coef(p.cv, se_rule=-1)}
#' # predict fitted values along whole lambda sequence 
#' \donttest{pred = predict(p.cv, s="all")}
#' \donttest{head(pred$plasso)}
#'
cv.plasso = function(x,y,
                  w=NULL,
                  kf=10,
                  parallel=FALSE,
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
  
  # Get indicator for CV samples
  split = stats::runif(nrow(x))
  cvgroup = as.numeric(cut(split,stats::quantile(split,probs=seq(0,1,1/kf)),include.lowest=TRUE))  # Groups for k-fold CV
  list = seq_len(kf)                                   # Needed later in the loop to get the appropriate samples
  
  if (!parallel) {
    
    # Initialize matrices for MSE of Lasso and post-lasso for each grid point and CV fold
    cv_MSE_lasso = matrix(nrow = kf,ncol = length(lambda))
    cv_MSE_plasso = matrix(nrow = kf,ncol = length(lambda))
    
    # Start loop for cross-validation of Lasso and Post-Lasso
    for (i in list) {
      CV = CV_core(x,y,w,cvgroup,list,i,lambda,...)
      
      # Extract MSE of Lasso and Post-Lasso
      cv_MSE_lasso[i,] = CV$MSE_lasso
      cv_MSE_plasso[i,] = CV$MSE_plasso
    }
    
  } else if (parallel)  {
    
    n_cores = min(parallel::detectCores()-1,kf)
    
    cl = parallel::makeCluster(n_cores, methods=FALSE)
    doParallel::registerDoParallel(cl)
    
    para_res <- foreach::foreach(i = seq_len(kf), .combine="rbind", .inorder=FALSE) %dopar% {
                           
                           ## core CV program functionsxxx no. 9
                           CV = CV_core(x,y,w,cvgroup,list,i,lambda,...)
                           
                           ## Extract MSE of Lasso and Post-Lasso
                           cv_MSE_lasso = CV$MSE_lasso
                           cv_MSE_post_lasso = CV$MSE_plasso
                           
                           ## Matrices to be returned from cores
                           return(list(as.matrix(cv_MSE_lasso),as.matrix(cv_MSE_post_lasso)))
    }
    parallel::stopCluster(cl)
    
    para_res = as.matrix(do.call(cbind,para_res))
    cv_MSE_lasso = t(para_res[,seq_len(kf)])
    cv_MSE_plasso = t(para_res[,(kf+1):(2*kf)])
    
  }
  
  # Calculate mean MSEs over all folds
  cv_MSE_lasso[is.na(cv_MSE_lasso)] = max(cv_MSE_lasso) # can happen if glmnet does not go over the full grid
  cv_MSE_plasso[is.na(cv_MSE_plasso)] = max(cv_MSE_plasso) # and/or Post-Lasso has not full rank
  mean_MSE_lasso = colMeans(cv_MSE_lasso)
  mean_MSE_plasso = colMeans(cv_MSE_plasso)
  
  # Get grid position of minimum MSE
  ind_min_l = which.min(mean_MSE_lasso)
  ind_min_pl = which.min(mean_MSE_plasso)
  # Get variable names at minima
  names_l = names(coef_lasso_full[,ind_min_l])[which(coef_lasso_full[,ind_min_l] != 0)]
  names_pl = names(coef_lasso_full[,ind_min_pl])[which(coef_lasso_full[,ind_min_pl] != 0)]
  
  # Get Lasso coefficients at minimum of Lasso
  coef_min_l = coef_lasso_full[,ind_min_l][which(coef_lasso_full[,ind_min_l] != 0)]
  # Get Lasso coefficients at minimum of Post-Lasso
  coef_min_pl = coef_lasso_full[,ind_min_pl][which(coef_lasso_full[,ind_min_pl] != 0)]
  
  # Return names and coefficients
  output = list("call"=this.call,
                "lasso_full"=lasso_full,"kf"=kf,
                "cv_MSE_lasso"=cv_MSE_lasso,"cv_MSE_plasso"=cv_MSE_plasso,
                "mean_MSE_lasso"=mean_MSE_lasso, "mean_MSE_plasso"=mean_MSE_plasso,
                "ind_min_l"=ind_min_l,"ind_min_pl"=ind_min_pl,
                "lambda_min_l"=lambda[ind_min_l],"lambda_min_pl"=lambda[ind_min_pl],
                "names_l"=names_l,"names_pl"=names_pl,
                "coef_min_l"=coef_min_l,"coef_min_pl"=coef_min_pl,
                "x"=x,"y"=y,"w"=w)
  
  class(output) = "cv.plasso"
  return(output)
}


#' Print cross-validated (Post-) Lasso model
#' 
#' @description
#' Printing main insights from cross-validated (Post-) Lasso model.
#'
#' @param x \code{\link{cv.plasso}} object
#' @param ... Pass generic \code{\link[base]{print}} options
#' @param digits Integer, used for number formatting
#'
#' @return Prints basic statistics for different lambda values of a fitted \code{\link{plasso}} object,
#' i.e. cross-validated MSEs for both Lasso and Post-Lasso model as well as the number of active variables.
#' 
#' @method print cv.plasso
#'
#' @export
#'
print.cv.plasso = function(x,...,digits=max(3, getOption("digits")-3)) {
  
  plasso = x
  cat("\nCall: ", deparse(plasso$call), "\n\n")
  out = data.frame(Df=plasso$lasso_full$df,
                   `%Dev`=round(plasso$lasso_full$dev.ratio*100, 2),
                   Lambda=signif(plasso$lasso_full$lambda, digits),
                   MSE_lasso=plasso$mean_MSE_lasso,
                   MSE_plasso=plasso$mean_MSE_plasso,
                   check.names=FALSE,row.names=seq(along=plasso$lasso_full$df))
  class(out) = c("anova",class(out))
  print(out)
}


#' Summary of cross-validated (Post-) Lasso model
#' 
#' @description
#' Summary of cross-validated (Post-) Lasso model.
#'
#' @param object \code{\link{cv.plasso}} object
#' @param ... Pass generic \code{\link[base]{summary}} summary options
#' @param default TRUE for \code{\link[glmnet]{glmnet}}-like summary output, FALSE for more specific summary information
#'
#' @return For specific summary information: summary.cv.plasso object (using list structure) containing optimal
#' lambda values and associated MSEs for both cross-validated Lasso and Post-Lasso model.
#' For default: \code{\link[base:summary]{summaryDefault}} object.
#' 
#' @method summary cv.plasso
#'
#' @export
#'
#' @examples
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#' # get informative summary statistics
#' \donttest{print(summary(p.cv, default=FALSE))}
#' # set default=TRUE for standard summary statistics
#' \donttest{print(summary(p.cv, default=TRUE))}
#' 
summary.cv.plasso = function(object,...,default=FALSE) {
  
  plasso = object
  
  if (default) {
    
    return(summary.default(plasso, ...))
    
  } else {
  
    value = list(
      call = plasso$call,
      mean_MSE_lasso = plasso$mean_MSE_lasso,
      mean_MSE_plasso = plasso$mean_MSE_plasso,
      lasso_full = plasso$lasso_full,
      ind_min_lasso = plasso$ind_min_l,
      ind_min_plasso = plasso$ind_min_pl
    )
    class(value) = "summary.cv.plasso"
    return(value)

  }
}


#' Print summary of (Post-) Lasso model
#'
#' @description
#' Prints summary information of \code{\link{cv.plasso}} object
#'
#' @param x Summary of plasso object (either of class \code{\link{summary.cv.plasso}} or \code{\link[base]{summary}})
#' @param ... Pass generic R \code{\link[base]{print}} options
#' @param digits Integer, used for number formatting
#' 
#' @return Prints information from \code{\link{summary.cv.plasso}} object into console.
#' 
#' @method print summary.cv.plasso
#'
#' @export
#' 
print.summary.cv.plasso = function(x,...,digits=max(3L, getOption("digits") - 3L)) {
  
  if (inherits(x,'summaryDefault')) {
    
    print.summaryDefault(x, digits=digits, ...)
    
  } else if (inherits(x,'summary.cv.plasso')){
    
    cat("\nCall:\n ", paste(deparse(x$call), sep="\n", collapse = "\n"), "\n\n", sep = "")
    
    cat("Lasso:\n Minimum CV MSE Lasso: ",toString(signif(min(x$mean_MSE_lasso,na.rm=TRUE),digits)))
    cat("\n Lambda at minimum: ",toString(signif(x$lasso_full$lambda[x$ind_min_l],digits)))
    cat("\n Active variables at minimum: ",names(coef(x$lasso_full)[,x$ind_min_l])[which(coef(x$lasso_full)[,x$ind_min_l] != 0)])
    
    cat("\nPost-Lasso:\n Minimum CV MSE Post-Lasso: ",toString(signif(min(x$mean_MSE_plasso,na.rm=TRUE),digits)))
    cat("\n Lambda at minimum: ",toString(signif(x$lasso_full$lambda[x$ind_min_pl],digits)))
    cat("\n Active variables at minimum: ",names(coef(x$lasso_full)[,x$ind_min_pl])[which(coef(x$lasso_full)[,x$ind_min_pl] != 0)])
    cat("\n")
    
  }
  
}


#' Predict after cross-validated (Post-) Lasso
#' 
#' @description
#' Prediction for cross-validated (Post-) Lasso.
#'
#' @param object Fitted \code{\link{cv.plasso}} model object
#' @param ... Pass generic \code{\link[stats]{predict}} options
#' @param newx Matrix of new values for x at which predictions are to be made. If no value is supplied, x from fitting procedure is used. This argument is not used for \code{type="coefficients"}.
#' @param type Type of prediction required. \code{"response"} returns fitted values, \code{"coefficients"} returns beta estimates.
#' @param s Determines whether prediction is done for all values of lambda (\code{"all"}) or only for the optimal lambda (\code{"optimal"}) according to the standard error-rule.
#' @param se_rule If equal to 0, predictions from cross-validated MSE minimum (default). Negative values go in the direction of smaller
#' models, positive values go in the direction of larger models (e.g. \code{se_rule=-1} creates the standard 1SE rule).
#' This argument is not used for \code{s="all"}.
#' 
#' @return List object containing either fitted values or coefficients for both
#' the Lasso and Post-Lasso models respectively.
#' \item{lasso}{Matrix with Lasso predictions or coefficients}
#' \item{plasso}{Matrix with Post-Lasso predictions or coefficients}
#'
#' @method predict cv.plasso
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
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#' # predict fitted values along whole lambda sequence 
#' \donttest{pred = predict(p.cv, s="all")}
#' \donttest{head(pred$plasso)}
#' # predict fitted values for optimal lambda value (according to cross-validation) 
#' \donttest{pred_optimal = predict(p.cv, s="optimal")}
#' \donttest{head(pred_optimal$plasso)}
#' # predict fitted values for new feature set X
#' \donttest{X_new = head(X, 10)}
#' \donttest{pred_new = predict(p.cv, newx=X_new, s="optimal")}
#' \donttest{pred_new$plasso}
#' # get estimated coefficients along whole lambda sequence
#' \donttest{coefs = predict(p.cv, type="coefficients", s="all")}
#' \donttest{head(coefs$plasso)}
#' # get estimated coefficients for optimal lambda value according to 1-standard-error rule
#' \donttest{predict(p.cv, type="coefficients", s="optimal", se_rule=-1)}
#'
predict.cv.plasso = function(object,
                             ...,
                             newx=NULL,
                             type=c("response","coefficients"),
                             s=c("optimal","all"),
                             se_rule=0) {
  
  plasso = object
  # Check if type and s is valid
  type = match.arg(type)
  s = match.arg(s)
  
  x = plasso$x
  if ( is.null(newx) ) newx = x
  if ( is.null( colnames(x) ) ) colnames(x) = sprintf("var%s",seq_len(ncol(x)))
  colnames(newx) = colnames(x)
  x = add_intercept(x)
  newx = add_intercept(newx)
  
  y = plasso$y
  w = handle_weights(plasso$w,nrow(x))
  lambda_names = names(plasso$lasso_full$a0)
  
  if (s == "optimal") {
    
    oneSE_lasso = sqrt(apply(plasso$cv_MSE_lasso,2,var)/plasso$kf)
    oneSE_plasso = sqrt(apply(plasso$cv_MSE_plasso,2,var)/plasso$kf)
    oneSE_lasso[is.na(oneSE_lasso)] = 0
    oneSE_plasso[is.na(oneSE_plasso)] = 0
    ind_Xse_l = find_Xse_ind(plasso$mean_MSE_lasso,plasso$ind_min_l,oneSE_lasso,se_rule)
    ind_Xse_pl = find_Xse_ind(plasso$mean_MSE_plasso,plasso$ind_min_pl,oneSE_plasso,se_rule)
    
    coef_lasso = as.matrix(coef(plasso$lasso_full)[,ind_Xse_l])
    colnames(coef_lasso) = paste0("optimal(",se_rule,")")
    
    nm_act = names(coef(plasso$lasso_full)[,ind_Xse_pl])[which(coef(plasso$lasso_full)[,ind_Xse_pl] != 0)]
    coef_plasso = as.matrix(fit_betas(x,y,w,nm_act,coef(plasso$lasso_full)[,ind_Xse_pl]))
    colnames(coef_plasso) = paste0("optimal(",se_rule,")")
    
    if (type == "coefficients") {
      
      return(list(
        "lasso"=as(coef_lasso,"dgCMatrix"),
        "plasso"=as(coef_plasso,"dgCMatrix")
        )
      )
      
    } else if (type == "response"){
      
      # Fitted values for lasso
      fit_lasso = as.matrix(newx %*% coef_lasso)
      colnames(fit_lasso) = paste0("optimal(",se_rule,")")
      
      # Fitted values for post lasso
      fit_plasso = as.matrix(newx %*% coef_plasso)
      colnames(fit_plasso) = paste0("optimal(",se_rule,")")
      
      return(list("lasso"=fit_lasso,"plasso"=fit_plasso))
      
    }
    
  } else if (s == "all"){
    
    l = length(plasso$lasso_full$lambda)
    
    coef_lasso = as.matrix(coef(plasso$lasso_full))
    coef_plasso = matrix(NA,nrow=ncol(x),ncol=l,dimnames=list(colnames(x),colnames(coef_lasso)))
    
    for (i in seq_len(l)) {
  
      nm_act = names(coef_lasso[,i])[which(coef_lasso[,i] != 0)]
      coef_plasso[,i] = fit_betas(x,y,w,nm_act,coef_lasso[,i])
    }
    
    if (type == "coefficients") {
      
      return(list(
        "lasso"=as(coef_lasso,"dgCMatrix"),
        "plasso"=as(coef_plasso,"dgCMatrix")
      )
      )
      
    } else if (type == "response"){
      
      fit_lasso = matrix(NA,nrow=nrow(newx),ncol=l,dimnames=list(NULL,colnames(coef_lasso)))
      fit_plasso = matrix(NA,nrow=nrow(newx),ncol=l,dimnames=list(NULL,colnames(coef_lasso)))
      
      for (i in seq_len(l)) {
        
        fit_lasso[,i] = newx %*% coef_lasso[,i]
        fit_plasso[,i] = newx %*% coef_plasso[,i]
      }
      
      return(list("lasso"=fit_lasso,"plasso"=fit_plasso))
      
    }
  }
}


#' Extract coefficients from a \code{\link{cv.plasso}} object
#' 
#' @description
#' Extract coefficients for both Lasso and Post-Lasso from a \code{\link{cv.plasso}} object.
#' 
#' @param object \code{\link{cv.plasso}} object
#' @param ... Pass generic \code{\link[stats]{coef}} options
#' @param s Determines whether coefficients are extracted for all values of lambda ("all") or only for the optimal lambda ("optimal") according to the specified standard error-rule.
#' @param se_rule If equal to 0, predictions from cross-validated MSE minimum (default). Negative values go in the direction of smaller
#' models, positive values go in the direction of larger models (e.g. \code{se_rule=-1} creates the standard 1SE rule).
#' This argument is not used for \code{s="all"}.
#' 
#' @return List object containing coefficients for both the Lasso and Post-Lasso
#' models respectively.
#' \item{lasso}{Sparse \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} with Lasso coefficients}
#' \item{plasso}{Sparse \code{\link[Matrix:dgCMatrix-class]{dgCMatrix}} with Post-Lasso coefficients}
#'
#' @method coef cv.plasso
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
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#' # get estimated coefficients along whole lambda sequence
#' \donttest{coefs = coef(p.cv, s="all")}
#' \donttest{head(coefs$plasso)}
#' # get estimated coefficients for optimal lambda value according to 1-standard-error rule
#' \donttest{coef(p.cv, s="optimal", se_rule=-1)}
#'
coef.cv.plasso = function(object,...,s=c("optimal","all"),se_rule=0){
  return(predict(object,...,s=s,se_rule=se_rule,type="coefficients"))
}


#' Plot of cross-validation curves
#' 
#' @description
#' Plot of cross-validation curves.
#' 
#' @param x \code{\link{cv.plasso}} object
#' @param ... Pass generic \code{\link[base]{plot}} options
#' @param legend_pos Legend position. Only considered for joint plot (lass=FALSE).
#' @param legend_size Font size of legend
#' @param lasso If set as True, only the cross-validation curve for the Lasso model is plotted. Default is False.
#' 
#' @return Plots the cross-validation curves for both Lasso and Post-Lasso models (incl. upper and lower standard deviation curves)
#' for a fitted \code{\link{cv.plasso}} object.
#' 
#' @method plot cv.plasso
#'
#' @export
#' 
#' @examples
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#' # plot cross-validated MSE curves and number of active coefficients
#' \donttest{plot(p.cv, legend_pos="bottomleft")}
#'
plot.cv.plasso = function(x,...,
                          legend_pos=c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"),
                          legend_size=0.5,
                          lasso=FALSE) {
  
  plasso = x
  legend_pos = match.arg(legend_pos)
  
  if (!lasso) {
  
    # Standard error of folds
    oneSE_lasso = sqrt(apply(plasso$cv_MSE_lasso,2,var)/plasso$kf)
    oneSE_plasso = sqrt(apply(plasso$cv_MSE_plasso,2,var)/plasso$kf)
    oneSE_lasso[is.na(oneSE_lasso)] = 0
    oneSE_plasso[is.na(oneSE_plasso)] = 0
    
    # Calculate 1SE bands
    lasso_1se_up = plasso$mean_MSE_lasso+oneSE_lasso
    lasso_1se_low = plasso$mean_MSE_lasso-oneSE_lasso
    plasso_1se_up = plasso$mean_MSE_plasso+oneSE_plasso
    plasso_1se_low = plasso$mean_MSE_plasso-oneSE_plasso
    
    # Get ranges for the graph
    xrange = range(log(plasso$lasso_full$lambda))
    yrange = c(max(-1.7e+308,min(lasso_1se_low,plasso_1se_low)),
               min(1.7e+308,max(lasso_1se_up,plasso_1se_up)))
    
    # Plot mean lines
    ylab = "Mean-squared Error"
    xlab = expression(Log(lambda))
    graphics::plot(xrange,yrange,type="n",xlab=xlab,ylab=ylab)
    graphics::lines(log(plasso$lasso_full$lambda),plasso$mean_MSE_lasso,lwd=1.5,col="blue")
    graphics::lines(log(plasso$lasso_full$lambda),plasso$mean_MSE_plasso,lwd=1.5,col="red")
    
    # Plot upper and lower 1SE lines
    graphics::lines(log(plasso$lasso_full$lambda),lasso_1se_up,lty=2,lwd=1,col="blue")
    graphics::lines(log(plasso$lasso_full$lambda),lasso_1se_low,lty=2,lwd=1,col="blue")
    graphics::lines(log(plasso$lasso_full$lambda),plasso_1se_up,lty=2,lwd=1,col="red")
    graphics::lines(log(plasso$lasso_full$lambda),plasso_1se_low,lty=2,lwd=1,col="red")
    
    # Show location of minima
    graphics::abline(v=log(plasso$lambda_min_l),lty = 1,col="blue")
    graphics::abline(v=log(plasso$lambda_min_pl),lty = 1,col="red")
    
    # Print legend
    graphics::legend(legend_pos, c("Lasso MSE","Lasso MSE+-1SE","Post-Lasso MSE","Post-Lasso MSE+-1SE","# active coeff."),lty=c(1,2,1,2,1),
                     col=c('blue','blue','red','red','forestgreen'),ncol=1,bty ="n",cex=legend_size)
    
    oldpar = graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    
    # Open a new graph for number of coefficients to be written on existing
    graphics::par(new=TRUE)
    graphics::plot(log(plasso$lasso_full$lambda),plasso$lasso_full$df,axes=F,xlab=NA,ylab=NA,cex=1.2,type="l",col="forestgreen",lwd=1.5)
    graphics::axis(side=4)
    graphics::mtext(side=4, line=3, "# active coefficients")
    
  } else if (lasso) {
    
    plot(plasso$lasso_full,...)
    
  }
}


#' Core part for (Post-) Lasso cross-validation
#' 
#' @description
#' \code{\link{CV_core}} contains the core parts of the cross-validation for Lasso and Post-Lasso.
#' 
#' @param x Covariate matrix to be used in cross-validation
#' @param y Vector of outcomes
#' @param w Vector of weight
#' @param cvgroup Categorical with k groups to identify folds
#' @param list List 1:k
#' @param i Number of fold that is used for prediction
#' @param lambda Series of lambdas used
#' @param ... Pass \code{\link[glmnet]{glmnet}} options
#'
#' @return MSE_lasso / MSE_plasso: means squared errors for each lambda.
#'
#' @keywords internal
#'
CV_core = function(x,y,w,cvgroup,list,i,lambda,...) {
  
  # Get estimation and prediction sample for this specific fold
  x_est_cv = x[cvgroup %in% list[-i],]
  y_est_cv = y[cvgroup %in% list[-i]]
  w_est_cv = w[cvgroup %in% list[-i],]
  x_pred_cv = x[cvgroup == list[i],]
  y_pred_cv = y[cvgroup == list[i]]
  w_pred_cv = w[cvgroup == list[i],]
  
  # Normalize the weights to N
  w_est_cv = norm_w_to_n(as.matrix(w_est_cv))
  w_pred_cv = norm_w_to_n(as.matrix(w_pred_cv))
  
  # Estimate Lasso for this fold using the grid of the full sample
  lasso_cv = glmnet::glmnet(x_est_cv, y_est_cv,lambda = lambda,weights=as.vector(w_est_cv),
                    family="gaussian",...)
  coef_lasso_cv = coef(lasso_cv)                                       # Save coefficients at each grid point
  
  # Predicted values with lasso coefficients for prediction sample at each grid
  fit_lasso = predict(lasso_cv,x_pred_cv)
  if (ncol(fit_lasso) != length(lambda)) {
    fit_lasso = cbind(fit_lasso,matrix(NA,nrow(fit_lasso),(length(lambda)-ncol(fit_lasso))))
  }
  
  ## Now calculate predicted values for post Lasso at each grid
  # Initialize first matrix for fitted values
  fit_plasso = matrix(NA,nrow = nrow(fit_lasso),ncol = ncol(fit_lasso))
  
  # Figure out which coefficients are active at each Lasso grid
  act_coef = (coef_lasso_cv != 0)         # boolean matrix
  
  ## Calculate full covariance matrix X'X and X'y once and select only the relevant in the loop below (much faster)
  # First, figure out variables that were active at least once and get only these
  act_once = apply(act_coef,1,any)
  nm_all_act_coef = rownames(act_coef)[act_once]
  if (length(nm_all_act_coef) == 1) {
    warning("No variables selected in one CV, even for lowest lambda, might reconsider choice of penalty term grid")
    fit_plasso = fit_lasso
  } else {
    ## Create the covariate matrix to "manually" calculate the fitted values (much faster than the build in lm command)
    if (sum(abs(coef_lasso_cv[1,])) == 0) {   # Indicates that no intercept was used
      x_all_act = x_est_cv[,nm_all_act_coef]
    }
    else if (sum(abs(coef_lasso_cv[1,])) != 0) {    # Indicates that intercept was used
      x_all_act = add_intercept(x_est_cv[,nm_all_act_coef[2:length(nm_all_act_coef)],drop=F])
      colnames(x_all_act)[1] = "(Intercept)"
      # add intercept also to prediction sample
      x_pred_cv = add_intercept(x_pred_cv[,nm_all_act_coef[2:length(nm_all_act_coef)],drop=F])
      colnames(x_pred_cv)[1] = "(Intercept)"
    }
    else {
      stop("Something strange happens with the intercepts at the Lasso path")
    }
    # Add weights
    x_w = apply(x_all_act,2,`*`,sqrt(w_est_cv))
    y_w = y_est_cv * sqrt(w_est_cv)
    
    # Get X'X
    XtX_all = crossprod(x_w)
    # Get X'y
    Xty_all = crossprod(x_w,y_w)
    
    # Initialize vector to save chosen variable names to figure out whether new variables were added from the last to the next grid
    nm_act_coef_prev = "nobody should call a variable like this"
    
    ## Loop over the grid of Lambdas to get the Post-Lasso predictions
    for (j in seq_len(length(lambda))) {
      # Get names of active variables at this grid
      nm_act_coef = rownames(act_coef)[act_coef[,j]]
      if (identical(nm_act_coef,character(0)) == TRUE) {
        # print("No variable selected at this grid => no prediction")
        next
      }
      if (identical(nm_act_coef,nm_act_coef_prev) == TRUE & j > 1) {
        # print("Same variables selected as in previous grid => Post-Lasso predictions remain unchanged")
        fit_plasso[,j] = fit_plasso[,(j-1)]
        next
      }
      else {     # Get prediction covariate matrix for that grid
        x_ols_pred = as.matrix(x_pred_cv[,nm_act_coef])
      }
      
      # Get OLS Post-Lasso predictions for this grid point
      fit = fitted_values_cv(XtX_all,Xty_all,x_ols_pred,nm_act_coef)
      if (is.null(fit) & j == 1) {
        fit_plasso[,j] = rep(mean(y),nrow(fit_plasso))
        next
      }
      if (is.null(fit) & j > 1) {
        # cat("\n X'X not invertible at grid",toString(j),": Use last feasible coefficients")
        fit_plasso[,j] = fit_plasso[,(j-1)]
        next
      } else{
        fit_plasso[,j] = fit
      }
      
      # Update current active covariates for the check whether it changes in the next grid
      nm_act_coef_prev = nm_act_coef
    }       # end loop over grids
    
  } # end if at least one var selected
  
  # Matrix with "real" outcome values for each grid
  y_rep = matrix(rep(y_pred_cv,length(lambda)),nrow=nrow(fit_lasso),ncol=ncol(fit_lasso))
  
  # Get RMSE
  SE_lasso = (y_rep - fit_lasso)^2
  SE_plasso = (y_rep - fit_plasso)^2
  if (!is.null(w)) {                                     # Weight errors by "sampling weights"
    SE_lasso = apply(SE_lasso,2,"*",w_pred_cv)
    SE_plasso = apply(SE_plasso,2,"*",w_pred_cv)
  }
  MSE_lasso = apply(SE_lasso,2,mean)
  MSE_plasso = apply(SE_plasso,2,mean)
  
  return(list("MSE_lasso" = MSE_lasso,"MSE_plasso" = MSE_plasso))
}