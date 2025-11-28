#' @title flexOR: Flexible Odds Ratio Computation for GAM Models
#'
#' @aliases flexOR
#'
#' @description
#' Computes odds ratios for predictors in GAM models.
#' It provides flexibility in specifying predictors using either a data frame,
#' a response variable, and a formula or a pre-fitted GAM model.
#' The function is useful for understanding the impact of predictors on
#' binary outcomes in GAMs.
#'
#' @usage
#' flexOR(data, response = NULL, formula = NULL)
#'
#' @param data A data frame containing the variables.
#' @param response The response variable as a character string.
#' @param formula A formula specifying the model.
#'
#' @details
#' It accepts two different ways of specifying the model: by providing
#' the data frame and response variable or by specifying the formula.
#'
#' @return
#' A list containing the following components:
#' \itemize{
#'   \item \code{dataset}: The dataset used for the analysis.
#'   \item \code{formula}: The formula used in the GAM model.
#'   \item \code{gamfit}: The fitted GAM model.
#'   \item \code{response}: The response variable used in the analysis.
#' }
#'
#' @examples
#' library(gam);
#'
#' # Load dataset
#' data(PimaIndiansDiabetes2, package="mlbench");
#' 
#' # Calculate odds ratios using flexOR
#' df_result <- flexOR(data = PimaIndiansDiabetes2, response = "diabetes", 
#' formula=~ s(age) + s(mass) + s(pedigree) + pressure + glucose)
#' print(df_result)
#' 
#' @keywords models nonlinear regression smooth
#' @importFrom stats as.formula binomial na.omit terms
#' @export
flexOR <- function(data, response=NULL, formula=NULL) {
  modelfit <- "TRUE";
  mydata2 <- deparse( substitute(data) );
  modelfit <- "FALSE";
  if ( missing(data) ) {stop("The argument data is missing");}
  if ( !is.data.frame(data) ) {stop("data must be of class data.frame");}
  if ( missing(response) ) {stop("The argument response is missing");}
  if ( missing(formula) ) {stop("The argument formula is missing");}
  mydata <- data;
  p0 <- match(names(data), response, nomatch=0);
  p1 <- which(p0==1);
  ny <- data[, p1];
  if ( !missing(response) ) {response <- ny;}
  fmla <- attr(terms(formula), "term.labels");
  ncov <- length(fmla);
  colvar <- rep(0, ncov);
  for (k in 1:ncov) {
    if ( fmla[k] %in% names(data) ) {
      colvar[k] <- which(names(data) == fmla[k]);
    } else {
      for ( j in 1:ncol(data) ) {
        if ( any( grep(names(data)[j], fmla[k]) ) ) {
          colvar[k] <- j;
        }
      }
    }
  }
  if ( any(colvar==0) ) {stop("'formula' must contain the right variables");}
  covar <- as.formula( paste( " ny ~ ", paste(fmla, collapse = "+") ) );
  fit <- gam::gam(covar, data = data, x = TRUE, family = binomial);
  a1 <- c();
  a1 <- p1;
  for (k in 1:dim(mydata)[2]) {
    if ( any( grep(names(mydata)[k], formula) ) ) {a1 <- c(a1, k);}
  }
  mydata <- mydata[, a1];
  mydata <- na.omit(mydata);
  nv <- fit$nevent;
  object <- list(
    dataset=mydata, formula=formula, gamfit=fit, response=names(data)[p1]
  );
  class(object) <- "OR";
  return(object);
} # flexOR
