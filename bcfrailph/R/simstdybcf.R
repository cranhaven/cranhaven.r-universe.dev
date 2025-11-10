#'
#' @name simstdybcf
#' @title Simulation study for bivariate correlated frailty models.
#' @description Simulation study for bivariate correlated gamma and lognormal frailty models with and without covariates.
#'
#' @param Rep number of replications.
#' @param mfit A type of frailty model to be fit in addition to \code{bcfrailph}. mfit can be c("cox","shrg") where \code{cox} is for univariate or bivariate shared lognormal and gamma model fit using \code{\link{coxph}} and \code{shrg} is for univariate or bivariate shared gamma model fit using \code{\link{shrgamsp}}.
#' @param psize pair size.
#' @param cenr censored rate. The default is zero..
#' @param beta Covariate coefficient.
#' @param frailty A type of frailty distribution to be used. Either gamma or lognormal.
#' @param frailpar vector of frailty parameters, variance and correlation respectively. The default is c(0.5,0.25) meaning variance 0.5 and correlation 0.25.
#' @param bhaz  A type of baseline hazard distribution to be used. it can be weibull, gompertz or exponential.
#' @param bhazpar is a \code{\link{list}} containing \code{scale} and\code{shape} of the specified baseline hazard distribution.
#' @param covartype specified the distribution from which covariate(s) are goining to be sampled. covartype can be c("B","N","U")denoting binomial, normal or uniform, respectively. For example, \code{covartype=c("B","B")} to generate two covariates both from a binomial distribution.
#' @param covarpar is a \code{\link{list}} containing parmeters of the specified covariate distribution with first and second arguments denoted by \code{fargs} and \code{sargs}, respectively. For example, if \code{covartype=c("B","U")} and \code{covarpar=list(fargs=c(1,0.3),sargs=c(0.5,1.3))}, generates two independent covariates from a binomial distribution (with parameters size=1 and probs=0.5) and from uniform distributions (with parameters min=0.3 and max=1.3).
#' @param inpcovar is a \code{\link{list}} i.e,list(covar1=x1,covar2=x2) to input covariates with both x1 and x2 is in matrix form.
#' @param inpcen is a \code{\link{list}} containing \code{cent1} and \code{cent2} denoting censoring time for the first and the second subjects in pairs respectively.
#' @param comncovar if common covariates are needed.
#' @return
#' An object of class \code{simstdybcf} that contain the following:
#' \itemize{
#'
#' \item{\code{Result}} {a summary result containing true parameter, mean of estimates, mean of the standard errors of the estimates, standard deviation of estimates, and 95\% CI coverage probability.}
#'
#' \item{\code{estimates}} {a matrix containing estimates of parameters at each replications.}
#'
#' \item{\code{estimateSE}} {a matrix containing standard error of estimates at each replications.}
#'
#' \item{\code{coverage}} {a matrix containing an indicator whether the true parameter lies within a 95\% CI at each replications or not.}
#'
#' \item{\code{TMAT}} {a matrix containing the generated artificial unique event times at each replications for gamma model.}
#'
#' \item{\code{h0MAT}} {a matrix containing the estimated baseline hazards at each replications for gamma model.}
#'
#' \item{\code{h0SEMAT}} {a matrix containing SE of the estimated baseline hazards at each replications for gamma model.}}
#'
#'
#' @export simstdybcf
#'
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats rbinom
#' @importFrom stats quantile
#'
#' @seealso \code{\link{simbcfrailph}}
#'
#' @examples
#' set.seed(2)
#' sim<-simstdybcf(Rep=5,psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),
#' frailty=c("lognormal"),frailpar=c(0.5,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)),comncovar=2)
#' Res<-sim$Result
#' Res
#'
#' \donttest{
#' # In addition to bcfrailph fit, if coxph with univariate lognormal frailty model is desired to run,
#'
#' sim<-simstdybcf(Rep=5,mfit="cox",psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),
#' frailty=c("lognormal"),frailpar=c(0.5,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)),comncovar=2)
#' Res<-sim$Result # bcfrailph fit result
#' Res
#' Resc<-sim$Resultc # coxph with univariate lognormal frailty model fit result
#' Resc
#' }
#'
simstdybcf<-function(Rep,mfit=NULL,
psize, cenr= c(0),beta=c(0.5),frailty,frailpar=c(0.5,0.25),
bhaz=c("weibull"),bhazpar=list(shape =c(0.5), scale = c(0.01)),
covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)),
inpcovar=NULL,inpcen=NULL,comncovar=NULL){
Call <- match.call()
if (missing(Rep)) {Rep<-2}
if (missing(frailty)) {frailty<-c("gamma")}
if (Rep<=0) {stop(" number of replication must be positive")}
if(length(frailpar)>2){frailpar=frailpar[1:2]}
if(length(mfit)>2){stop("simstdybcf does not support three model fit")}
if(length(mfit)>0){
if(!any((c(mfit)%in%c("cox","shrg")))){
stop("simstdybcf only support coxph or shrgamsp function for shared frailty fit")}}
if(length(mfit)==0){
truepar=c(beta,frailpar)
lower=upper=NULL
Est=SEEst=COVERAGE=matrix(0,Rep,length(truepar))
if( length(beta)==1){
formula<-Surv(time, censor) ~X1+frailty(PID)
cnam=c("beta","theta","rho")}
if( length(beta)==2){
formula<-Surv(time, censor) ~X1+X2+frailty(PID)
cnam=c("beta1","beta2","theta","rho")}
if( length(beta)==3){
formula<-Surv(time, censor) ~X1+X2+X3+frailty(PID)
cnam=c("beta1","beta2","beta3","theta","rho")}
if(frailty=="gamma"){
TMAT=h0MAT=h0SEMAT=matrix(0,(2*psize),Rep)
Conver=ITER=1
HHHH=rep(0,(2*psize))
for(q in 1:Rep){
simdata<-simbcfrailph(psize=psize, cenr=cenr,beta=beta,frailty=frailty,frailpar=frailpar,
bhaz=bhaz,bhazpar=bhazpar,covartype=covartype,covarpar=covarpar,
inpcovar=inpcovar,inpcen=inpcen,comncovar=comncovar)
Y<-simdata$Y;X<-simdata$X
bcspfit<-fitbccv.gammasp(X,Y,initfrailp=NULL,weights=NULL)
Est[q,]<-c(bcspfit$coefficients,bcspfit$frailparest)
SEEst[q,]<-bcspfit$stder
Conver[q]=bcspfit$convergence
ITER[q]=bcspfit$iteration
lower=Est[q,]-zq*SEEst[q,];upper=Est[q,]+zq*SEEst[q,]
COVERAGE[q,]<-coverp(x=truepar,L=lower,R=upper)
lower=upper=NULL
hhho=hhhot=hhhose=HHHH
h0=bcspfit$bhaz[,2]
siz0=length(h0);hoi=array(1:siz0)
hhho[hoi]<-h0;hhhot[hoi]<-bcspfit$bhaz[,1];hhhose[hoi]<-bcspfit$bhaz[,3]
TMAT[,q]<-hhhot;h0MAT[,q]<-hhho;h0SEMAT[,q]<-hhhose}}
if(frailty=="lognormal"){
h0SEMAT=h0MAT=TMAT=NULL
Conver=ITER=1
for(q in 1:Rep){
simdata<-simbcfrailph(psize=psize, cenr=cenr,beta=beta,frailty=frailty,frailpar=frailpar,
bhaz=bhaz,bhazpar=bhazpar,covartype=covartype,covarpar=covarpar,
inpcovar=inpcovar,inpcen=inpcen,comncovar=comncovar)
Y<-simdata$Y;X<-simdata$X
bcspfit<-fitbccv.lognsp(X,Y,initfrailp=NULL)
Est[q,]<-c(bcspfit$coefficients,bcspfit$frailparest)
SEEst[q,]<-bcspfit$stder
Conver[q]=bcspfit$convergence
ITER[q]=bcspfit$iteration
lower=Est[q,]-zq*SEEst[q,];upper=Est[q,]+zq*SEEst[q,]
COVERAGE[q,]<-coverp(x=truepar,L=lower,R=upper)
lower=upper=NULL}}
Estmean <- apply(Est,2,mean);sdEst <- apply(Est,2,sd)
SEestmean <- apply(SEEst,2,mean);probcover <- apply(COVERAGE,2,mean)
Estmean<-round(Estmean,digits=7)
sdEst<-round(sdEst,digits=7)
SEestmean <-round(SEestmean,digits=7)
Res=cbind(truepar,Estmean,SEestmean,sdEst,probcover)
colnames(Res)<-c("True.p","M.Est","M.SE","sd.Est","coverage")
rownames(Res)<-cnam
colnames(Est)<-cnam
colnames(SEEst)<-cnam
colnames(COVERAGE)<-cnam
outp<-list(Result=Res,estimates=Est,estimateSE=SEEst,coverage=COVERAGE,
TMAT=TMAT,h0MAT=h0MAT,h0SEMAT=h0SEMAT,convergence=Conver,iter=ITER)}
if(length(mfit)==1){
truepar=c(beta,frailpar)
trueparcox=c(beta,frailpar[1])
lower=upper=NULL
Est=SEEst=COVERAGE=matrix(0,Rep,length(truepar))
Estcox=SEEstcox=matrix(0,Rep,length(trueparcox))
Conver=ITER=1
if( length(beta)==1){
formula<-Surv(time, censor) ~X1+frailty(PID)
if(frailpar[2]==1){
if(frailty=="gamma"){
formulacox<-Surv(time, censor) ~X1+frailty(PID)}else{
formulacox<-Surv(time, censor) ~X1+frailty.gaussian(PID)}}else{
if(frailty=="gamma"){
formulacox<-Surv(time, censor) ~X1+frailty(IID)}else{
formulacox<-Surv(time, censor) ~X1+frailty.gaussian(IID)}}
cnam=c("beta","theta","rho");cnamcox=c("beta","theta")}
if( length(beta)==2){
formula<-Surv(time, censor) ~X1+X2+frailty(PID)
if(frailpar[2]==1){
if(frailty=="gamma"){
formulacox<-Surv(time, censor) ~X1+X2+frailty(PID)}else{
formulacox<-Surv(time, censor) ~X1+X2+frailty.gaussian(PID)}}else{
if(frailty=="gamma"){
formulacox<-Surv(time, censor) ~X1+X2+frailty(IID)}else{
formulacox<-Surv(time, censor) ~X1+X2+frailty.gaussian(IID)}}
cnam=c("beta1","beta2","theta","rho");cnamcox=c("beta1","beta2","theta")}
if( length(beta)==3){
  formula<-Surv(time, censor) ~X1+X2+X3+frailty(PID)
  if(frailpar[2]==1){
    if(frailty=="gamma"){
      formulacox<-Surv(time, censor) ~X1+X2+X3+frailty(PID)}else{
        formulacox<-Surv(time, censor) ~X1+X2+X3+frailty.gaussian(PID)}}else{
          if(frailty=="gamma"){
            formulacox<-Surv(time, censor) ~X1+X2+X3+frailty(IID)}else{
              formulacox<-Surv(time, censor) ~X1+X2+X3+frailty.gaussian(IID)}}
  cnam=c("beta1","beta2","beta3","theta","rho");cnamcox=c("beta1","beta2","beta3","theta")}
if(frailty=="gamma"){
for(q in 1:Rep){
simdata<-simbcfrailph(psize=psize, cenr=cenr,beta=beta,frailty=frailty,frailpar=frailpar,
bhaz=bhaz,bhazpar=bhazpar,covartype=covartype,covarpar=covarpar,
inpcovar=inpcovar,inpcen=inpcen,comncovar=comncovar)
dataa<-simdata$data
Y<-simdata$Y;X<-simdata$X
bcspfit<-fitbccv.gammasp(X,Y,initfrailp=NULL,weights=NULL)
Est[q,]<-c(bcspfit$coefficients,bcspfit$frailparest)
SEEst[q,]<-bcspfit$stder
Conver[q]=bcspfit$convergence
ITER[q]=bcspfit$iteration
lower=Est[q,]-zq*SEEst[q,];upper=Est[q,]+zq*SEEst[q,]
COVERAGE[q,]<-coverp(x=truepar,L=lower,R=upper)
coxfit=coxph(formula=formulacox,data=dataa,method="breslow")
if(frailpar[2]==1){ix=coxfit$history$`frailty(PID)`$history[,1]}else{
ix=coxfit$history$`frailty(IID)`$history[,1]}
Estcox[q,]<-c(coxfit$coefficients,ix[length(ix)])
SEEstcox[q,]<-c(sqrt(diag(coxfit$var)),0)}}
if(frailty=="lognormal"){
for(q in 1:Rep){
simdata<-simbcfrailph(psize=psize, cenr=cenr,beta=beta,frailty=frailty,frailpar=frailpar,
bhaz=bhaz,bhazpar=bhazpar,covartype=covartype,covarpar=covarpar,
inpcovar=inpcovar,inpcen=inpcen,comncovar=comncovar)
dataa<-simdata$data
Y<-simdata$Y;X<-simdata$X
bcspfit<-fitbccv.lognsp(X,Y,initfrailp=NULL)
Est[q,]<-c(bcspfit$coefficients,bcspfit$frailparest)
SEEst[q,]<-bcspfit$stder
Conver[q]=bcspfit$convergence
ITER[q]=bcspfit$iteration
lower=Est[q,]-zq*SEEst[q,];upper=Est[q,]+zq*SEEst[q,]
COVERAGE[q,]<-coverp(x=truepar,L=lower,R=upper)
coxfit=coxph(formula=formulacox,data=dataa,method="breslow")
if(frailpar[2]==1){ix=coxfit$history$`frailty.gaussian(PID)`$history[,1]}else{
ix=coxfit$history$`frailty.gaussian(IID)`$history[,1]}
Estcox[q,]<-c(coxfit$coefficients,ix[length(ix)])
SEEstcox[q,]<-c(sqrt(diag(coxfit$var)),0)}}
Estmean <- apply(Est,2,mean);sdEst <- apply(Est,2,sd)
SEestmean <- apply(SEEst,2,mean);probcover <- apply(COVERAGE,2,mean)
Estmean<-round(Estmean,digits=7);sdEst<-round(sdEst,digits=7)
SEestmean <-round(SEestmean,digits=7)
Res=cbind(truepar,Estmean,SEestmean,sdEst,probcover)
colnames(Res)<-c("True.p","M.Est","M.SE","sd.Est","coverage")
rownames(Res)<-cnam
colnames(Est)<-cnam
colnames(SEEst)<-cnam
colnames(COVERAGE)<-cnam
Estmeanc <- apply(Estcox,2,mean);sdEstc <- apply(Estcox,2,sd)
SEestmeanc <- apply(SEEstcox,2,mean)
Estmeanc<-round(Estmeanc,digits=7);sdEstc<-round(sdEstc,digits=7)
SEestmeanc <-round(SEestmeanc,digits=7)
SEestmeanc[length(SEestmeanc)]<-NA
Rescox=cbind(trueparcox,Estmeanc,SEestmeanc,sdEstc)
colnames(Rescox)<-c("True.p","M.Est","M.SE","sd.Est")
rownames(Rescox)<-cnamcox
outp<-list(Result=Res,Resultc=Rescox,estimates=Est,estimateSE=SEEst,coverage=COVERAGE,convergence=Conver,iter=ITER)}
if(length(mfit)==2){
truepar=c(beta,frailpar)
trueparcox=c(beta,frailpar[1])
lower=upper=NULL
Est=SEEst=COVERAGE=matrix(0,Rep,length(truepar))
Estcox=SEEstcox=matrix(0,Rep,length(trueparcox))
Estsr=SEEstsr=COVERAGEsr=matrix(0,Rep,length(trueparcox))
if( length(beta)==1){
formula<-Surv(time, censor) ~X1+frailty(PID)
if(frailpar[2]==1){
formulasr<-Surv(time, censor) ~X1+frailty(PID)}else{
formulasr<-Surv(time, censor) ~X1+frailty(IID)}
cnam=c("beta","theta","rho")
cnamsr=c("beta","theta")}
if( length(beta)==2){
formula<-Surv(time, censor) ~X1+X2+frailty(PID)
if(frailpar[2]==1){
formulasr<-Surv(time, censor) ~X1+X2+frailty(PID)}else{
formulasr<-Surv(time, censor) ~X1+X2+frailty(IID)}
cnam=c("beta1","beta2","theta","rho")
cnamsr=c("beta1","beta2","theta")}
if( length(beta)==3){
formula<-Surv(time, censor) ~X1+X2+X3+frailty(PID)
if(frailpar[2]==1){
formulasr<-Surv(time, censor) ~X1+X2+X3+frailty(PID)}else{
formulasr<-Surv(time, censor) ~X1+X2+X3+frailty(IID)}
cnam=c("beta1","beta2","beta3","theta","rho")
cnamsr=c("beta1","beta2","beta3","theta")}
for(q in 1:Rep){
simdata<-simbcfrailph(psize=psize, cenr=cenr,beta=beta,frailty=frailty,frailpar=frailpar,
bhaz=bhaz,bhazpar=bhazpar,covartype=covartype,covarpar=covarpar,
inpcovar=inpcovar,inpcen=inpcen,comncovar=comncovar)
dataa<-simdata$data
Y<-simdata$Y;X<-simdata$X
bcspfit<-fitbccv.gammasp(X,Y,initfrailp=NULL,weights=NULL)
Est[q,]<-c(bcspfit$coefficients,bcspfit$frailparest)
SEEst[q,]<-bcspfit$stder
lower=Est[q,]-zq*SEEst[q,];upper=Est[q,]+zq*SEEst[q,]
COVERAGE[q,]<-coverp(x=truepar,L=lower,R=upper)
srspfit=shrgamsp(formula=formulasr,data=dataa)
Estsr[q,]<-c(srspfit$coefficients,srspfit$frailparest)
SEEstsr[q,]<-srspfit$stder
lower=Estsr[q,]-zq*SEEstsr[q,];upper=Estsr[q,]+zq*SEEstsr[q,]
COVERAGEsr[q,]<-coverp(x=trueparcox,L=lower,R=upper)
coxfit=coxph(formula=formulasr,data=dataa,method="breslow")
if(frailpar[2]==1){ix=coxfit$history$`frailty(PID)`$history[,1]}else{
ix=coxfit$history$`frailty(IID)`$history[,1]}
Estcox[q,]<-c(coxfit$coefficients,ix[length(ix)])
SEEstcox[q,]<-c(sqrt(diag(coxfit$var)),0)}
Estmean <- apply(Est,2,mean);sdEst <- apply(Est,2,sd)
SEestmean <- apply(SEEst,2,mean);probcover <- apply(COVERAGE,2,mean)
Estmean<-round(Estmean,digits=7);sdEst<-round(sdEst,digits=7)
SEestmean <-round(SEestmean,digits=7)
Res=cbind(truepar,Estmean,SEestmean,sdEst,probcover)
colnames(Res)<-c("True.p","M.Est","M.SE","sd.Est","coverage")
rownames(Res)<-cnam
colnames(Est)<-cnam
colnames(SEEst)<-cnam
colnames(COVERAGE)<-cnam
Estmeansr <- apply(Estsr,2,mean);sdEstsr <- apply(Estsr,2,sd)
SEestmeansr <- apply(SEEstsr,2,mean);probcoversr <- apply(COVERAGEsr,2,mean)
Estmeansr<-round(Estmeansr,digits=7);sdEstsr<-round(sdEstsr,digits=7)
SEestmeansr <-round(SEestmeansr,digits=7)
Ressr=cbind(trueparcox,Estmeansr,SEestmeansr,sdEstsr,probcoversr)
colnames(Ressr)<-c("True.p","M.Est","M.SE","sd.Est","coverage")
rownames(Ressr)<-cnamsr
colnames(Estsr)<-cnamsr
colnames(SEEstsr)<-cnamsr
colnames(COVERAGEsr)<-cnamsr
Estmeanc <- apply(Estcox,2,mean);sdEstc <- apply(Estcox,2,sd)
SEestmeanc <- apply(SEEstcox,2,mean)
Estmeanc<-round(Estmeanc,digits=7);sdEstc<-round(sdEstc,digits=7)
SEestmeanc <-round(SEestmeanc,digits=7)
SEestmeanc[length(SEestmeanc)]<-NA
Rescox=cbind(trueparcox,Estmeanc,SEestmeanc,sdEstc)
colnames(Rescox)<-c("True.p","M.Est","M.SE","sd.Est")
rownames(Rescox)<-cnamsr
outp<-list(Result=Res,Resultshr=Ressr,Resultc=Rescox,estimates=Est,estimateSE=SEEst,
coverage=COVERAGE,estimatessr=Estsr,estimateSEsr=SEEstsr)}
outp$call <- match.call()
class(outp) <- c("simstdybcf")
outp}
