#'
#' @name bcfrailph
#' @title Semi-parametric bivariate correlated frailty model.
#' @description Fit a semiparametric Bivariate correlated frailty model with Proportional Hazard structure.
#'
#' @param formula A formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#' @param data A dataframe contain survival time, censor, covariate etc with data in columns.
#' @param initfrailp Initial estimates for the frailty parameters. If not specified, initial frailty variance will be obtained from coxph with univariate frailty model and for correlation c(0.5) will be used.
#' @param frailty A type of frailty distribution to be used in fit. Either gamma or lognormal. The default is gamma.
#' @param weights vector of case weights for gamma model. the default is NULL.
#' @param control Arguments to control bcfrailph fit. The default is \code{\link{bcfrailph.control}}.
#' @param ... further arguments
#'
#' @return An object of  that contains  the following components.
#' \itemize{
#'   \item \code{coefficients} - {A vector of estimated Covariate coefficients.}
#'   \item \code{frailparest} - {A vector of estimated Frailty parameters i.e. frailty variance and correlation.}
#'   \item \code{stderr}-{A vector containing the Standard error of the Estimated parameters both covariate coefficients and  frailty parameters.}
#'   \item \code{loglilk0}- Log likelihood of without frailty model or loglik of coxph fit.
#'   \item \code{loglilk}-Log likelihood of Cox PH model with frailty.
#'   \item \code{Iloglilk}- Log likelihood of with frailty. For gamma fit it is I-likelihood or the likelihood after integrating out the frailty term.For lognormal fit it is the approximate likelihood.
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
#'   \item \code{convergence}-{an indicator, 1 if converge and 0 otherwise.}
#'   \item \code{history}-{an array containing records of estimates and other information on each iterations.}
#'   }
#'
#' @export bcfrailph
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats nlminb
#' @importFrom stats nlm
#' @importFrom stats constrOptim
#' @importFrom stats terms
#' @importFrom stats as.formula
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @importFrom stats deriv
#' @importFrom stats deriv3
#' @importFrom stats rexp
#' @importFrom stats sd
#'
#' @note Parameters of Bivariate correlated gamma frailty model was estimated using a modified EM approach given in Kifle et al (2022).
#' Parameters of Bivariate correlated lognormal frailty model is based on the penalized partial likelihood approach by Rippatti and Palmgren (2000).
#'
#' @seealso \code{\link{bcfrailph.control}},\code{\link{simbcfrailph}}
#'
#' @references
#'
#' Kifle YG, Chen DG, Haileyesus MT (2022). Multivariate Frailty Models using Survey Weights with Applications to Twins Infant Mortality in Ethiopia. Statistics and Its Interface,106(4), 1\-10.
#'
#' Rippatti, S. and Palmgren, J (2000). Estimation of multivariate frailty models using penalized partial likelihood. Biometrics, 56: 1016-1022.
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfrailph(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#'
#' fitbcfrgam=bcfrailph(Surv(time,censor)~ X1+frailty(PID) ,data=dataa,frailty="gamma")
#' fitbcfrgam
#'
#' \donttest{
#' # for lognormal
#'
#' set.seed(18)
#' simdata<-simbcfrailph(psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),
#' frailty=c("lognormal"),frailpar=c(0.5,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)),comncovar=2)
#' dataa<-simdata$data
#'
#' #fit
#' fitbcfrlogn=bcfrailph(Surv(time,censor)~ X1+X2+X3+frailty(PID) ,data=dataa,frailty="lognormal")
#' fitbcfrlogn
#' 
#' ## one can set the initial parameter for the frailty parameters
#' fitbcfrailph=bcfrailph(Surv(time,censor)~ X1+frailty(PID),data=dataa,initfrailp = c(0.1,0.5),
#' frailty="lognormal")
#' fitbcfrailph
#'
#' # Not run
#'
#' #if covariates are not included
#' fitmoe=try(bcfrailph(Surv(time,censor)~0+frailty(PID),data=dataa,
#' frailty="lognormal"),silent = TRUE)
#' 
#' fitmoe=try(bcfrailph(Surv(time,censor)~1+frailty(PID),data=dataa),silent = TRUE)
#'
#' # if control is not specified correctly.
#' # if one needs to change only max.iter to be 100,
#'
#' fitmoe=try(bcfrailph(Surv(time,censor)~ X1+frailty(PID),data=dataa,
#' control=c(max.iter=100)),silent = TRUE)
#'
#' #the correct way is
#' fitmoe=bcfrailph(Surv(time,censor)~ X1+frailty(PID),data=dataa,
#' control=bcfrailph.control(max.iter=100))
#' fitmoe
#'
#' #if initial frailty parameters are in the boundary of parameter space
#' fitmoe=try(bcfrailph(Surv(time,censor)~ X1+frailty(PID),data=dataa,
#' initfrailp=c(0.2,1)),silent = TRUE)
#' 
#' fitmoe=try(bcfrailph(Surv(time,censor)~ X1+frailty(PID),data=dataa,
#' initfrailp=c(0,0.1)),silent = TRUE)
#'
#' #if a frailty distribution other than gamma and lognormal are specified
#'
#' fitmoe=try(bcfrailph(Surv(time,censor)~ X1,data=dataa,,frailty="exp"),silent = TRUE)
#' 
#' # End Not run
#' }
#'
bcfrailph<- function(formula, data,initfrailp = NULL,frailty=c("gamma","lognormal"),
weights=NULL,control=bcfrailph.control(),...)
{
Call <- match.call()
if(any(is.na(charmatch(names(Call), c("formula","data","initfrailp","frailty",
"weights","control"),nomatch = NA_integer_)))){
warning("There is/are unused argument(s)")}
if (missing(formula)) {stop("formula must be supplied")}
if (!inherits(formula,"formula")) stop("First argument must be a formula")
if (missing(data)) {stop("Data must be supplied")}
if (!is.data.frame(data) && !is.matrix(data)) stop("Data must be data frame or matrix.")
special <- c("cluster","frailty","strata")
Terms <- terms(formula,special ,data)
mf <- model.frame(Terms,data)
mm <- model.matrix(Terms,mf)
pos_strata_mm <- grep(c("strata"), colnames(mm))
if(length(pos_strata_mm)>0){stop(" strata is invalid in bcfrailph")}
pos_cluster_mm <- grep(c("cluster"), colnames(mm))
pos_frailty_mm <- grep("frailty", colnames(mm))
pos_special_mm<-c(pos_cluster_mm,pos_frailty_mm)
if(length(pos_special_mm)==0){
warning("Since frailty variable is not specified bcfrailph fits assumed consecutive two observations as a pair")
X <- mm[, -c(1), drop = FALSE]  # extract covariate(s) matrix if frailty variable is not given
mmatt <- attributes(mm)
attr(X, "assign") <- mmatt$assign[-c(1)]}
frailind<-NULL
if(length(pos_special_mm)>0){
uniq_id<-unique(mm[,pos_special_mm])
if(length(uniq_id)>(length(mm[,pos_special_mm])/2)){
stop("Frailty variable does not suggest or indicate bivariate frailty.Provide bivariate frailty indicator")}
if(length(uniq_id)<(length(mm[,pos_special_mm])/2)){
stop("Frailty variable does not suggest or indicate bivariate frailty.Provide bivariate frailty indicator")}
indic1<-2*array(1:(nrow(mm)/2))-1;indic2<-2*array(1:(nrow(mm)/2))
# If bivariate frailty variable is specified in a way other than 1,1,2,2,...
# For example, 1,2,3,...1,2,3,..., rearrange the order in a way that 1,1,2,2,...
if(sum(mm[indic1,pos_special_mm]-mm[indic2,pos_special_mm])!=0){
order=sort(mm[,pos_special_mm], decreasing = FALSE, index.return = TRUE)
subject_indx=order$ix
mm<-mm[subject_indx,];mf<-mf[subject_indx,]}
X <- mm[, -c(1, pos_special_mm), drop = FALSE]# extract covariate(s) matrix
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
if (type != "right"){stop(paste("bcfrailph doesn't support \"", type,
"\" survival data", sep = ""))}
if(length(frailty)>1){frailty<-frailty[1]}
if((frailty!=c("gamma"))&(frailty!=c("lognormal"))){
stop(paste("bcfrailph doesn't support \"", frailty,
"\" frailty distributions", sep = ""))}
if(length(initfrailp)>0){
if((length(initfrailp)<2)|(length(initfrailp)>2)){
stop("Invalid number of initial frailty parameters")}
if(any(initfrailp<0.00001)){stop("Atleast one of the Initial frailty parameter is at or near its boumdary")}
if(initfrailp[2]>0.999){stop("Initial frailty correlation parameter is at or near its boumdary")}}
# If frailty is gamma, then fitter function fitbccv.gammasp is used
# If frailty is lognormal, then fitter function fitbccv.lognsp is used
if(frailty==c("gamma")){fit<-fitbccv.gammasp(X,Y,initfrailp,weights,control)}
if(frailty==c("lognormal")){fit<-fitbccv.lognsp(X,Y,initfrailp,control)}
fit$call <- Call
fit$formula<- formula
fit$frailty<-frailty
class(fit) <- c("bcfrailph")
fit
}
