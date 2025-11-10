#'
#' @name shrgamsp
#' @title Cox PH model with univariate and bivariate shared gamma frailty model.
#' @description Fit Cox PH model with univariate and bivariate shared gamma frailty model.
#'
#' @param formula A formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the \code{Surv} function.
#' @param data A dataframe contain survival time, censor, covariate etc with data in columns.
#' @param initfrailp Initial estimates for the frailty parameters. The default is c(0.5).
#' @param weights vector of case weights for gamma model. the default is NULL.
#' @param control Arguments to control the fit. The default is \code{\link{bcfrailph.control}}.
#' @param ... further arguments
#'
#' @return An object of shrgamsp contains  the following components.
#' \itemize{
#'   \item \code{coefficients} - {A vector of estimated Covariate coefficients.}
#'   \item \code{frailparest} - {A vector of estimated Frailty parameters i.e. frailty variance and correlation.}
#'   \item \code{vcov}- {Variance Covariance matrix of the Estimated Covariate coefficients obtained from the observed information matrix.}
#'   \item \code{stderr}-{A vector containing the Standard error of the Estimated parameters both covariate coefficients and  frailty parameter.}
#'   \item \code{loglik0}- Log likelihood of without frailty model.
#'   \item \code{loglik}-Log likelihood of Cox PH model with frailty.
#'   \item \code{Iloglilk}- Log likelihood of with frailty model after integrating out the frailty term.
#'   \item \code{bhaz}- an array containing unique event times and estimated baseline hazard.
#'   \item \code{X}-{Matrix of observed covariates.}
#'   \item \code{time}-{the observed survival time.}
#'   \item \code{censor}-{censoring indicator.}
#'   \item \code{resid}-{the martingale residuals.}
#'   \item \code{lin.prid}-{the vector of linear predictors.}
#'   \item \code{frail}-{estimated Frailty values.}
#'   \item \code{iteration}-{Number of outer iterations.}
#'   \item \code{e.time}-{the vector of unique event times.}
#'   \item \code{n.event}- {the number of events at each of the unique event times.}
#'   \item \code{convergence}-{an indicator of convergence . see \code{\link{nlminb}}.}
#'   }
#'
#' @export shrgamsp
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats nlminb
#' @importFrom stats constrOptim
#' @importFrom stats terms
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @importFrom stats deriv
#' @importFrom stats deriv3
#' @importFrom stats rexp
#' @importFrom stats sd
#'
#' @note This is just a \code{\link{coxph}} model with gamma frailty and the differences between
#' \code{\link{coxph}} with gamma frailty fit and \code{\link{shrgamsp}} fit is on the standard errors of the
#' covariates cofficients. Here, the standard errors of the estimated covariate coefficients and the frailty variance parameter are obtained using
#' the standard errors estimation approach given in Klein and Moeschberger (2003).
#'
#' @seealso \code{\link{bcfrailph}}
#'
#' @references
#'
#' Duchateau, L., Janssen, P. (2008) The Frailty Model. Springer, New York.
#'
#' Klein, J. P., and Moeschberger, M. L. (2003), Survival analysis: techniques for censored and truncated data, New York: Springer.
#'
#' @examples
#' set.seed(2)
#' n1=500;IID=array(1:n1)
#' X1<-runif(n1,  min=0, max=1)
#' z=rgamma(n1,shape=2,scale=0.5)
#' u1<-runif(n1,  min=0, max=1)
#' time<- 1/0.1*log(1-0.1*log(u1)/(0.0001*exp(3*X1)*z))
#' censor=rep(1,n1)
#' dataa <- data.frame(time=time, X1=X1,censor=censor,IID=IID)
#'
#' fitcoxfr=shrgamsp(Surv(time,censor)~ X1+frailty(IID) ,data=dataa)
#' fitcoxfr
#'
shrgamsp<- function(formula, data,weights=NULL,initfrailp = NULL,
control=bcfrailph.control(),...)
{
Call <- match.call()
if (missing(formula)) {stop("formula must be supplied")}
if (!inherits(formula,"formula")) stop("First argument must be a formula")
if (missing(data)) {stop("Data must be supplied")}
if (!is.data.frame(data) && !is.matrix(data)) stop("Data must be data frame or matrix.")
special <- c("cluster","frailty","strata")
Terms <- terms(formula,special ,data)
mf <- model.frame(Terms,data)
mm <- model.matrix(Terms,mf)
pos_strata_mm <- grep(c("strata"), colnames(mm))
if(length(pos_strata_mm)>0){ stop(" strata is invalid in shrgamsp")}
pos_cluster_mm <- grep(c("cluster"), colnames(mm))
pos_frailty_mm <- grep("frailty", colnames(mm))
if( (length(pos_cluster_mm)>0)& (length(pos_frailty_mm)>0) ){
stop(" Simultaneous model fit using both frailty and cluster is invalid in shrgamsp")}
pos_special_mm<-c(pos_cluster_mm,pos_frailty_mm)
frailind<-NULL
if(length(pos_special_mm)==0){
X <- mm[, -c(1), drop = FALSE]
mmatt <- attributes(mm)
attr(X, "assign") <- mmatt$assign[-c(1)]
ar=array(1:(nrow(mm)/2))
frailind<-rep(ar,each=2)}
if(length(pos_special_mm)>0){
order=sort(mm[,pos_special_mm], decreasing = FALSE, index.return = TRUE)
subject_indx=order$ix
frailind=order$x
mm<-mm[subject_indx,];mf<-mf[subject_indx,]
X <- mm[, -c(1, pos_special_mm), drop = FALSE]
mmatt <- attributes(mm)
attr(X, "assign") <- mmatt$assign[-c(1, pos_special_mm)]}
if(ncol(X)<1){stop("covariates must be included")}
if (length(weights) >0){
if (!is.null(weights) && any(!is.finite(weights))){stop("weights must be finite")}}
if (length(weights) >0){
if (length(weights)<(nrow(mm)/2)){stop("Incorrect weights length")}
if ((length(weights)>(nrow(mm)/2))&(length(weights)<nrow(mm)) ){stop("Incorrect weights length")}
if (length(weights)==(nrow(mm)/2)){weights<-c(weights,weights)}}
attr(X, "contrasts") <- mmatt$contrasts
Y <- mf[[1]]
if (!inherits(Y, "Surv")){stop("Response is not a survival object")}
type <- attr(Y, "type")
if (type != "right"){stop(paste("shrgamsp doesn't support \"", type,
"\" survival data", sep = ""))}
if (length(initfrailp) > 0) {
initfrailp <- initfrailp[1]
if (initfrailp < 1e-05) {stop("the Initial frailty parameter is at or near its boumdary")}}
fit <-shrdgamma.fit(X,Y,initfrailp,frailind,weights,control)
fit$call <- Call
fit$formula <- formula
fit$weights <- weights
class(fit) <- c("shrgamsp")
fit
}
