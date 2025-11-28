#' @title dfgam: Degrees of Freedom Selection for GAM Models
#'
#' @aliases dfgam
#'
#' @description
#' Computes the degrees of freedom for specified non-linear
#' predictors in a GAM model. The user can choose between AIC (Akaike Information
#' Criterion), AICc (AIC corrected for small sample sizes), or BIC (Bayesian
#' Information Criterion) as the selection criteria. This function is useful for
#' determining the appropriate degrees of freedom for smoothing terms in GAMs.
#'
#' @param response The response variable as a formula.
#' @param nl.predictors A character vector specifying the non-linear predictors.
#' @param other.predictors A character vector specifying other predictors if needed.
#' @param smoother The type of smoothing term, currently only "s" is supported.
#' @param method The selection method, one of "AIC", "AICc", or "BIC".
#' @param data The data frame containing the variables.
#' @param step The step size for grid search when there are multiple non-linear predictors.
#'
#' @return
#' A list containing the following components:
#' \itemize{
#'   \item \code{fit}: The fitted GAM model.
#'   \item \code{df}: A numeric vector of degrees of freedom for each non-linear predictor.
#'   \item \code{method}: The selection method used (AIC, AICc, or BIC).
#'   \item \code{nl.predictors}: The non-linear predictors used in the model.
#'   \item \code{other.predictors}: Other predictors used in the model if specified.
#' }
#'
#' @examples
#' # Load dataset
#' library(gam)
#' data(PimaIndiansDiabetes2, package="mlbench");
#'
#' # Calculate degrees of freedom using AIC
#' df2 <- dfgam(
#'   response="diabetes",
#'   nl.predictors=c("age", "mass"),
#'   other.predictors=c("pedigree"),
#'   smoother="s",
#'   method="AIC",
#'   data=PimaIndiansDiabetes2
#' );
#' 
#' print(df2$df);
#' 
#' @keywords models nonlinear regression smooth
#' @importFrom stats as.formula binomial AIC BIC 
#' @import gam
#' @export
dfgam <- function(response, nl.predictors, other.predictors=NULL, smoother="s", method = "AIC", data, step=NULL) {
  #options(warn=-1);
  if ( missing(data) ) {stop("The argument data is missing");}
  if ( missing(response) ) {stop("The argument response is missing");}
  if ( missing(nl.predictors) ) {stop("The argument 'nl.predictors' is missing");}
  if ( missing(smoother) ) {smoother <- "s";}
  if (smoother != "s") {stop("argument 'smoother' must be 's'");}
  if ( missing(method) ) {method <- "AIC";}
  if ( method != "AIC" & method != "AICc" & method != "BIC" & method != "REML" & method != "GCV.Cp") {stop("The argument 'method' is not valid");}  # include other methods from mgcv?
  if (missing(step)) step <- 3
  if (step < 1 | step > 10) stop("'step' must be between 1 and 10")
  p0 <- match(names(data), response, nomatch=0);
  p1 <- which(p0 == 1);
  ny <- data[,p1];
  if (sum(p0) == 0) {stop("variable defined in argument 'response' is not in the dataset 'data'");}
  nnl <- length(nl.predictors);
  df <- rep(1, length(nl.predictors));
  fmla <- c();
  nl.fmla <- c();
  
  # df for nonlinear predictors 
  if(is.null(other.predictors)){
    covar <- paste("s(", nl.predictors, ")", collapse="+")
    #fmla <- as.formula( paste( names(data)[p1], "~", paste(covar, collapse = "+") ) );
    fmla <- as.formula( paste( names(data)[p1], "~", covar ) );
    
  }
  if(!is.null(other.predictors)){
    nop <- length(other.predictors);
    for (i in 1:nop) {
      p2 <- match(names(data), other.predictors[i], nomatch=0);
      if (sum(p2) == 0) {stop("Check variables in argument 'other.predictors'");}
    }
    covar1 <- paste("s(", nl.predictors, ")", collapse="+");
    covar2 <- paste(other.predictors, collapse="+");
    covar <- paste (c(covar1, covar2), collapse="+");
    fmla <- as.formula( paste( names(data)[p1]," ~ ", covar, collapse = "+") ) ;
  }
  
  if (method == "REML")  {fit <- mgcv::gam(fmla, data=data, family=binomial, method = "REML");}
  if (method == "GCV.Cp")  {fit <- mgcv::gam(fmla, data=data, family=binomial, method = "GCV.Cp");}
  if (method != "REML" & method != "GCV.Cp") {fit <- mgcv::gam(fmla, data=data, family=binomial, method = "REML");}
  df <- summary(fit)$edf
  ndf <- df
  msg <- c()
  
  # get df from AIC, AICc and BIC: starting point REML
  if (method == "AIC" | method == "AICc" | method == "BIC"){
    mat <- array(NA, dim=c(41,4,nnl)) #df, AIC, BIC, AICc     #Reduce grid to became faster!
    dfAIC <- df
    dfBIC <- df
    dfAICc <- df
    lmat <- dim(mat)[1]
    df.AIC <- df
    df.BIC <- df
    df.AICc <- df
    
    #loop for the number of steps to get a fine tunning
    for (m in 1:step){
      for(k in 1:nnl){
        auxi <- round(df[k],1) + seq(-2, 2, 0.1)
        if (auxi[1] < 1) auxi <- 1.1 + auxi - auxi[1]
        mat[,1,k] <- auxi
        for(h in 1:lmat){
          ndf <- df
          ndf[k] <- mat[h,1,k]
          
          aux1 <- paste("s(", nl.predictors, ",df=",ndf, ")", collapse="+")
          if (is.null(other.predictors)) {aux2 <- aux1}
          else {aux2 <- paste(c(aux1,covar2), collapse="+")}
          fmla3 <- as.formula( paste( names(data)[p1]," ~ ", aux2, collapse = "+") ) ;
          fit <- gam::gam(fmla3, data=data, family=binomial)
          mat[h,2,k] <- AIC(fit); mat[h,3,k] <- BIC(fit); mat[h,4,k] <- AICc(fit)
        }
        df.AIC[k] <- mat[which.min(mat[,2,k]),1,k]
        df.BIC[k] <- mat[which.min(mat[,3,k]),1,k]
        df.AICc[k] <- mat[which.min(mat[,4,k]),1,k]
        if(method == "AIC") df[k] <- df.AIC[k]
        if(method == "BIC") df[k] <- df.BIC[k]
        if(method == "AICc") df[k] <- df.AICc[k]
      }
    }
  }
  
  #return	
  res <- matrix(NA,ncol=1, nrow=nnl)
  colnames(res) <- "df"
  rownames(res) <- nl.predictors
  res[1:nnl] <- df[1:nnl]
  
  if(missing(other.predictors)){
    aux1 <- paste("s(", nl.predictors, ",df=",df, ")", collapse="+")
    fmla3 <- as.formula( paste( names(data)[p1]," ~ ", aux1, collapse = "+") ) ;
    fit <- gam(fmla3, data=data, family=binomial)
  }
  if(!missing(other.predictors)){
    auxop <- paste(other.predictors, collapse="+")
    aux1 <- paste("s(", nl.predictors, ",df=",ndf, ")", collapse="+")
    aux2 <- paste(c(aux1,auxop), collapse="+")
    fmla3 <- as.formula( paste( names(data)[p1]," ~ ", aux2, collapse = "+") ) ;
    fit <- gam(fmla3, data=data, family=binomial)
  }
  
  if(!is.null(msg)) print(msg)
  ob <- list(fit=fit, df=res, method=method, nl.predictors=nl.predictors, other.predictors=other.predictors)
  return(ob)
} # dfgam

