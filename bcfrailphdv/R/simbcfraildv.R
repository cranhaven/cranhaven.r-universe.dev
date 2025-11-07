#'
#' @name simbcfraildv
#' @title Simulate data from bivariate correlated frailty models.
#' @description Simulate data from bivariate correlated gamma or lognormal frailty models with one covariate.
#'
#' @param psize pair size.
#' @param cenr censored rate. The default is zero..
#' @param beta Covariate coefficient.
#' @param frailty A type of frailty distribution to be used. Either gamma or lognormal.
#' @param frailpar vector of frailty parameters, variance and correlation respectively. The default is c(0.5,0.5,0.25) meaning both variances are 0.5 and correlation 0.25.
#' @param bhaz  A type of baseline hazard distribution to be used. it can be weibull, gompertz or exponential.
#' @param bhazpar is a \code{\link{list}} containing \code{scale} and\code{shape} of the specified baseline hazard distribution.
#' @param covartype specified the distribution from which covariate(s) are goining to be sampled. covartype can be c("B","N","U")denoting binomial, normal or uniform, respectively. For example, \code{covartype=c("B","B")} to generate two covariates both from a binomial distribution.
#' @param covarpar is a \code{\link{list}} containing parmeters of the specified covariate distribution with first and second arguments denoted by \code{fargs} and \code{sargs}, respectively. For example, if \code{covartype=c("B","U")} and \code{covarpar=list(fargs=c(1,0.3),sargs=c(0.5,1.3))}, generates two independent covariates from a binomial distribution (with parameters size=1 and probs=0.5) and from uniform distributions (with parameters min=0.3 and max=1.3).
#' @param inpcovar is a \code{\link{list}} i.e,list(covar1=x1,covar2=x2) to input covariates with both x1 and x2 is in matrix form.
#' @param inpcen is a \code{\link{list}} containing \code{cent1} and \code{cent2} denoting censoring time for the first and the second subjects in pairs respectively.
#' @param comncovar if common covariates are needed.
#'
#' @return
#' An object of class \code{simbcfraildv} that contain the following:
#'
#' \itemize{
#'
#' \item{\code{data}}  {A data frame i.e, the simulated data set. IID is individual Id, PID is pair ID, time is the simulated survival time, censor is censoring indicator and X1 denote the simulated covariate.}
#'
#' \item{\code{numberofpair}}  {The specified number of pairs.}
#'
#' \item{\code{censoredrate} } {The specified censored rate.}
#'
#' \item{\code{fraildist} } {The specified frailty distribution.}
#'
#' \item{\code{frailpar}} {The specified frailty parameters.}
#' }
#'
#'
#' @export simbcfraildv
#'
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats rbinom
#' @importFrom stats quantile
#'
#' @seealso \code{\link{bcfraildv}}
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfraildv(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' dataa<-simdata$data
#' head(dataa)
#'
#' \donttest{
#' # If data generation is from bivariate correlated lognormal frailty model,
#' set.seed(18)
#' simdata<-simbcfraildv(psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),frailty=c("lognormal"),
#' frailpar=c(0.5,0.8,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)))
#' dataa<-simdata$data
#' head(dataa)
#'
#' # If common covariate is desired, i.e., here out of the three covariates 
#' #covariate 2 is common for the pair.
#' set.seed(18)
#' simdata<-simbcfraildv(psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),frailty=c("lognormal"),
#' frailpar=c(0.5,0.8,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)),comncovar=2)
#' dataa<-simdata$data
#' head(dataa)
#'
#' # If the data generation is from bivariate correlated gamma frailty model,
#' #weibull baseline and without covariate,
#' set.seed(4)
#' simdata<-simbcfraildv(psize=300, cenr= c(0.3),beta=NULL,frailty=c("gamma"),
#' frailpar=c(0.5,0.6,0.5),bhaz=c("weibull"),bhazpar=list(shape =c(5), scale = c(0.1)))
#' dataa<-simdata$data
#' head(dataa)
#' }
#'
simbcfraildv<-function(psize, cenr= c(0),beta=c(0.5),frailty,frailpar=c(0.5,0.5,0.25),
bhaz=c("weibull"),bhazpar=list(shape =c(0.5), scale = c(0.01)),
covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)),
inpcovar=NULL,inpcen=NULL,comncovar=NULL){
Call <- match.call()
if (missing(psize)) {stop("number of pairs must be supplied")}
psize<-round(psize,digits=0)
psize=psize[1];cenr=cenr[1]
if (psize<=0) {stop(" number of pairs must be positive")}
if ((cenr<0) |(cenr>=1)) {stop(" Censoring rate must be between 0 and 1")}
if (missing(frailty)) {frailty<-c("gamma")}
if(length(frailty)>1){frailty=frailty[1]}
if(!(c(frailty)%in%c("gamma","lognormal"))){
stop("simbcfrail only support gamma or lognormal frailty distributions")}
if (length(frailpar)<2) {stop(" frailty parameters i.e. variance and correlation must be supplied")}
if (length(frailpar)==2) {frailpar<-c(frailpar[1],frailpar[1],frailpar[2])}
if (length(frailpar)>3) {frailpar<-frailpar[1:3]}
if(any(frailpar[1:2]<=0)){stop("At least one invalid frailty parameter")}
if (frailpar[3]>1) {stop(" invalid frailty correlation parameter")}
if (length(bhaz)>1) {bhaz=bhaz[1]}
if(!(c(bhaz)%in%c("weibull","gompertz","exponential"))){
stop("simbcfrail only support weibull gompertz or exponential baseline hazards")}
if (length(bhazpar$scale)==0) {stop(" baseline hazard scale parameter must be supplied")}
if (length(bhazpar$scale)>1) {bhazpar$scale<-bhazpar$scale[1]}
if(bhazpar$scale<=0){stop(" invalid baseline hazard scale parameter")}
if ((bhaz=="weibull")|(bhaz=="gompertz")){
if (length(bhazpar$shape)==0) {stop(" baseline hazard shape parameter must be supplied")}
if (length(bhazpar$shape)>1) {bhazpar$shape<-bhazpar$shape[1]}
if (bhazpar$shape<=0){stop(" invalid shape parameter for weibull or gompertz baseline hazard ")}}
inputcovar=NULL
if (length(beta)==0){covar1<-covar2<-NULL}
if (length(beta)>0){
if (length(inpcovar)==0) {
if( (length(covartype)==0)|(length(covarpar)==0)){
stop(" please provide both covariate type and its arguments")}
if((length(covarpar$fargs)==0)|(length(covarpar$sargs)==0)){
stop("covariate parameter arguments have different size")}
if(length(covarpar$fargs)!=length(covarpar$sargs)){
stop(" covariate parameter arguments have different size")}
if(length(covartype)!=length(beta)){
stop(" number of covariate does not match with the number of covariate coefficients")}
ncovar=length(covartype)
if(length(covarpar$fargs)>ncovar){covarpar$fargs<-covarpar$fargs[1:ncovar]}
if(length(covarpar$sargs)>ncovar){covarpar$sargs<-covarpar$sargs[1:ncovar]}
if(ncovar>1){
if( (length(covarpar$fargs)>1)& (length(covarpar$fargs)<ncovar)){
stop(" covariate types size does not match covariate parameter arguments size")}
if(length(covarpar$fargs)==1){
covarpar$fargs<-rep(covarpar$fargs,ncovar)
covarpar$sargs<-rep(covarpar$sargs,ncovar)}}
for(j in 1:ncovar){
if(!(covartype[j]%in%c("B","U","N"))){
stop("incorrect covariates type and types shuld be either of B,U or N")}
if(covartype[j]==c("B")){
if ((covarpar$fargs[j]-round(covarpar$fargs[j],digits=0))!=0){
stop("Incorect size parameter for binomial covariate")}
if ( (covarpar$sargs[j]<=0)|(covarpar$sargs[j]>=1)){
stop("Incorect prob parameter for binomial covariate")}}
if(covartype[j]==c("U")){
if ( covarpar$sargs[j]<=covarpar$fargs[j]){
stop("Incorect max parameter for Uniform covariate")}}
if(covartype[j]==c("N")){
if (covarpar$sargs[j]<=0){
stop("Incorect sd parameter for normally distributed covariate")}}}}
if (length(inpcovar)>0) {
if (!is.list(inpcovar)){stop("Please provide inputs covar1 and covar2 as list()")}
if ((length(inpcovar$covar1)==0)|(length(inpcovar$covar2)==0)) {
stop("Please provide both input covar1 and covar2 in matrix")}
if(length(beta)==1){
if (is.matrix(inpcovar$covar1)){
if((ncol(inpcovar$covar1)>1)|(ncol(inpcovar$covar2)>1)){
stop("covariate column size differ from number of coefficients")}
if((nrow(inpcovar$covar1)!=psize)|(nrow(inpcovar$covar2)!=psize)){
stop("covariate row size differ from pair size")}}
if (!is.matrix(inpcovar$covar1)){
if (length(inpcovar$covar1)!=psize){
stop("covar1 length is different from psize")}
if (length(inpcovar$covar1)==psize){inpcovar$covar1<-matrix(c(inpcovar$covar1),psize,1)}}
if (!is.matrix(inpcovar$covar2)){
if (length(inpcovar$covar2)!=psize){
stop("covar2 length is different from psize")}
if (length(inpcovar$covar2)==psize){inpcovar$covar2<-matrix(c(inpcovar$covar2),psize,1)}}}
if(length(beta)>1){
if (!is.matrix(inpcovar$covar1)){
stop("Please provide covariate1 or covar1 in matrix form with dimension psize*length(beta)")}
if (!is.matrix(inpcovar$covar2)){
stop("Please provide covariate2 or covar2 in matrix form with dimension psize*length(beta)")}
if((ncol(inpcovar$covar1)!=length(beta))|(ncol(inpcovar$covar2)!=length(beta))){
stop("covariate column size differ from length(beta)")}
if((nrow(inpcovar$covar1)!=psize)|(nrow(inpcovar$covar2)!=psize)){
stop("covariate row size differ from pair size")}}
covar1<-inpcovar$covar1
covar2<-inpcovar$covar2
inputcovar="TRUE"}}
if (length(inpcen)>0) {
if (!is.list(inpcen)){stop("Please provide input censor times cent1 and cent2 as list()")}
if ((length(inpcen$cent1)==0)&(length(inpcen$cent2)==0)) {
stop("Please provide both censor time inputs")}
if ((length(inpcen$cent1)==0)|(length(inpcen$cent2)==0)) {
if (length(inpcen$cent1)==0){inpcen$cent2<-inpcen$cent1}else{inpcen$cent1<-inpcen$cent2}}
if ((length(inpcen$cent1)>0)&(length(inpcen$cent2)>0)) {
if(any(c(inpcen$cent1,inpcen$cent2)<=0)){stop("Invalid censoring time censor time must be >0")}
if ((length(inpcen$cent1)>1)&(length(inpcen$cent1)<psize)) {
stop("censor time cent1 have different length than psize or 1")}
if ((length(inpcen$cent2)>1)&(length(inpcen$cent2)<psize)) {
stop("censor time cent1 have different length than psize or 1")}
if ((length(inpcen$cent1)>=(2*psize))|(length(inpcen$cent2)>=(2*psize))) {
if (length(inpcen$cent1)>=(2*psize)){centp<-inpcen$cent1[1:(2*psize)]}else{centp<-inpcen$cent2[1:(2*psize)]}
e1=array(1:psize);i2=2*e1;i1=i2-1
cent1=centp[i1];cent2=centp[i2]}
if (length(inpcen$cent1)==psize){cent1<-inpcen$cent1}
if (length(inpcen$cent2)==psize){cent2<-inpcen$cent2}
if (length(inpcen$cent1)==1){cent1<-rep(inpcen$cent1,psize)}
if (length(inpcen$cent2)==1){cent2<-rep(inpcen$cent2,psize)}}}
if (length(inpcen)==0) {cent1<-cent2<-NULL}
fpar=frailpar
if(length(comncovar)>0){comncovar=comncovar
if (comncovar>length(beta)){
stop("comncovar needs to be a number not exceed the number of covariates")}}else{comncovar=NULL}
dataa=genbcfraildv(psize,cenr,cent1,cent2,beta,frailty,fpar,bhaz,bhazpar,
covartype,covarpar,inputcovar,covar1,covar2,comncovar=comncovar)
gendat <-list(data=dataa,numberofpair=psize,
frailty=frailty,frailpar=frailpar,basehaz=bhaz,
covartype=covartype,covarcoef=beta)
gendat$call <- match.call()
class(gendat) <- c("simbcfraildv")
gendat}



