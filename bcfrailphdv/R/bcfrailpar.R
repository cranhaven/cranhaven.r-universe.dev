#'
#' @name bcfrailpar
#' @title Parametric bivariate correlated frailty models fit.
#' @description Fit a parametric Bivariate correlated gamma, inverse gaussian and power variance frailty models with Proportional Hazard structure.
#'
#' @param formula A formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#' @param data A dataframe contain survival time, censor, covariate etc with data in columns.
#' @param initfrailp Initial estimates for the frailty parameters. The default is c(0.5,0.5).
#' @param inithazp Initial estimates for the baseline hazard distribution parameters. The default is c(0.05) for both scale and shape parameters.
#' @param initbeta Initial estimates for the covariate coefficients if there are any included. The default is taken from coxph fit.
#' @param haz A baseline hazard distribution. Either weibull, gompertz or exponential distributions are possible.
#' @param frailty A type of frailty distribution. Either gamma, inverse gaussian \code{frailty="invgauss"}  or power variance \code{frailty="pv"} frailty distributions are possible.
#' @param comonvar An argument whether to assume common frailty variance. The default is \code{comonvar=TRUE}. If \code{comonvar=FALSE}, then only gamma frailty model is possible.
#' @param ... further arguments.
#'
#' @return An object of  that contains  the following components.
#' \itemize{
#'   \item \code{coefficients} - {A vector of estimated Covariate coefficients.}
#'   \item \code{frailparest} - {A vector of estimated Frailty parameters i.e. frailty variance and correlation.}
#'   \item \code{basehazpar} - {A vector of estimated baseline hazard parameters i.e. scale and shape.}
#'   \item \code{stderr}-{A vector containing the Standard errors of the Estimated parameters with the order of frailty parameters,baseline hazard parameters and covariate coefficients.}
#'   \item \code{vcov}- {Variance Covariance matrix of the Estimated parameters.}
#'   \item \code{loglik}-{Log likelihood of the model.}
#'   \item \code{AIC}-{AIC of the model.}
#'   \item \code{BIC}-{BIC of the model.}
#'   \item \code{iterations}-{Number of outer iterations.See\link{constrOptim} for further.}
#'   \item \code{convergence}-{An indicator of convergence. See\link{constrOptim} for further.}
#'   }
#'
#' @export bcfrailpar
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats constrOptim
#' @importFrom stats deriv
#' @importFrom stats deriv3
#' @importFrom stats terms
#' @importFrom survival Surv
#' @importFrom survival coxph
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfraildv(psize=500, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' fitbcfrail=bcfrailpar(Surv(time,censor)~ X1+frailty(PID) ,data=dataa,frailty="gamma")
#' fitbcfrail
#'
#' \donttest{
#'
#' set.seed(18)
#' simdata<-simbcfraildv(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5,0.4),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' #fit with power variance frailty distribution
#' fitbcfrail=bcfrailpar(Surv(time,censor)~ X1+frailty(PID) ,data=dataa,
#' frailty="pv")
#' fitbcfrail
#'
#' ## one can set the initial parameter for the frailty parameters
#' fitbcfrail=bcfrailpar(Surv(time,censor)~ X1+frailty(PID) ,data=dataa,initfrailp = c(0.4,0.3),
#' frailty="gamma")
#' fitbcfrail
#'
#' # Not run
#'
#' #if initial frailty parameters are in the boundary of parameter space
#' fitmoe=try(bcfrailpar(Surv(time,censor)~ X1+frailty(PID),data=dataa,
#' initfrailp=c(0.2,1)),silent = TRUE)
#'
#' #if a frailty distribution other than gamma, invgauss or pv is specified
#'
#' fitmoe=try(bcfrailpar(Surv(time,censor)~ X1,data=dataa,frailty="exp"),silent = TRUE)
#'
#' # End Not run
#' }
#'
bcfrailpar<- function(formula, data,initfrailp = NULL,
inithazp= NULL,initbeta= NULL,haz=c("weibull","gompertz","exponential"),
frailty=c("gamma","invgauss","pv"),comonvar=TRUE,...)
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
if(length(pos_strata_mm)>0){ stop(" strata is invalid in bcfrailpar")}
pos_cluster_mm <- grep(c("cluster"), colnames(mm))
pos_frailty_mm <- grep("frailty", colnames(mm))
if( (length(pos_cluster_mm)>0)& (length(pos_frailty_mm)>0) ){
stop(" Simultaneous model fit using both frailty and cluster is invalid in bcfrailpar")}
pos_special_mm<-c(pos_cluster_mm,pos_frailty_mm)
mmatt <- attributes(mm)
if(length(pos_special_mm)==0){
X <- mm[, -c(1), drop = FALSE]
if(ncol(X)>=1){
attr(X, "assign") <- mmatt$assign[-c(1)]
attr(X, "contrasts") <- mmatt$contrasts}}
if(length(pos_special_mm)>0){
uniq_clust_ind<-unique(mm[,pos_special_mm])
if(length(uniq_clust_ind)!=(nrow(mm)/2)){
stop("bcfrailpar support only bivariate frailty")}
indic1<-2*array(1:(nrow(mm)/2))-1
indic2<-2*array(1:(nrow(mm)/2))
if(sum(mm[indic1,pos_special_mm]-mm[indic2,pos_special_mm])!=0){
order=sort(mm[,pos_special_mm], decreasing = FALSE, index.return = TRUE)
subject_indx=order$ix
mm<-mm[subject_indx,];mf<-mf[subject_indx,]}
X <- mm[, -c(1, pos_special_mm), drop = FALSE]
if(ncol(X)>=1){
attr(X, "assign") <- mmatt$assign[-c(1, pos_special_mm)]
attr(X, "contrasts") <- mmatt$contrasts}}
Y <- mf[[1]]
if (!inherits(Y, "Surv")){stop("Response is not a survival object")}
type <- attr(Y, "type")
if (type != "right"){stop(paste("bcfrailpar doesn't support \"", type,
"\" survival data", sep = ""))}
if (length(haz)>1) {haz<-haz[1]}
if(!(c(haz)%in%c("weibull","gompertz","exponential"))){
stop("bcfrailpar only support weibull,gompertz or exponential baseline distributions")}
if (length(frailty)>1) {frailty<-frailty[1]}
if(!(c(frailty)%in%c("gamma","invgauss","pv"))){
stop("bcfrailpar only support gamma,inverse gaussian or power variance frailty distributions")}
if (length(inithazp) > 0) {
if (any(inithazp<=0)) {stop("the Initial hazard parameter is at or near its boumdary")}
if (length(inithazp) > 2) {inithazp=inithazp[1:2]}
if (length(inithazp) == 2) {if(haz=="exponential"){inithazp=inithazp[1]}}
if (length(inithazp) == 1) {if( (haz=="weibull")|(haz=="gompertz")){inithazp=c(inithazp,inithazp)}}}
if(ncol(X)>=1){
if (length(initbeta) > 0) {
if (length(initbeta)!= ncol(X)) {stop("the Initial covariate parameters does not match number of covariates")}}}
if(comonvar){
if (length(initfrailp) > 0) {
if (length(initfrailp)>2) {stop("the number of Initial frailty parameters is not correct")}
if (any(initfrailp<1e-05)) {stop("at least one the Initial frailty parameter is at or near its boumdary")}
if (length(initfrailp)==2) {
if (initfrailp[2]>0.99) {stop("the Initial frailty correlation parameter is at or near its boumdary")}}}
if (frailty=="pv") {fit<-fitbccv.pvpar(X,Y,initfrailp,inithazp,initbeta,haz)}
if (frailty=="invgauss") {fit<-fitbccv.invgpar(X,Y,initfrailp,inithazp,initbeta,haz)}
if (frailty=="gamma") {fit<-fitbccv.gammapar(X,Y,initfrailp,inithazp,initbeta,haz)}}else{
if (frailty!="gamma") {warning("Only gamma frailty is allowed with this setup")}
  frailty <- c("gamma")
fit0<-fitbccv.gammapar(X,Y,initfrailp=NULL,inithazp=NULL,initbeta=NULL,haz)
if (length(initfrailp) > 0) {
if (length(initfrailp)>3) {stop("the number of Initial frailty parameters is not correct")}
if (any(initfrailp<1e-05)) {stop("at least one the Initial frailty parameter is at or near its boumdary")}
if (length(initfrailp)==2) {initfrailp=c(initfrailp[1],initfrailp[1],initfrailp[2])}
if (length(initfrailp)==3) {rb=feasibleregforR(initfrailp)
if (initfrailp[3]>=rb) {stop("the Initial frailty correlation parameter is at or out of its boumdary")}}}else{
initfrailp0=fit0$frailparest
initfrailp0[initfrailp0<0.001]<-0.001
if (initfrailp0[2]>=0.95) {initfrailp0[2]=0.8}
initfrailp=c(initfrailp0[1],initfrailp0[1],initfrailp0[2])}
if (length(inithazp) == 0) {
inithazp=fit0$basehazpar;inithazp[inithazp<0.0001]<-0.0001}
if(ncol(X)>=1){
if (length(initbeta) == 0) {initbeta=fit0$coefficients}}
fit<-fitbcdv.gammapar(X,Y,initfrailp,inithazp,initbeta,haz)}
fit$call <- Call
fit$formula <- formula
fit$haz<- haz
fit$frailty <- frailty
fit$comonvar <- comonvar
class(fit) <- c("bcfrailpar")
fit
}
