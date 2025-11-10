#'
#' @name fitbccv.gammasp
#' @title Bivariate correlated gamma frailty model fitting function.
#' @description Semi-parametric Bivariate correlated gamma frailty model fitting function.
#'
#' @param initfrailp Initial estimates for the frailty parameters. If not specified, initial frailty variance will be obtained from coxph with univariate gamma frailty model and for correlation c(0.5) will be used.
#' @param X Matix of predictors. This should not include an intercept.
#' @param Y a Surv object containing 2 columns (coxph.fit).
#' @param weights vector of case weights. the default is NULL.
#' @param control Arguments to control the fit. The default is \code{\link{bcfrailph.control}}.
#' @param SE a logical statement whether standard errors are obtained from the mariginal log likelihood.The default is TRUE.

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
#' @export fitbccv.gammasp
#' @importFrom stats nlminb
#' @importFrom stats constrOptim
#' @importFrom survival Surv
#' @importFrom survival coxph
#'
#' @note This function is important especially for simulation studies as it reduced checking time.
#' Parameters of Bivariate correlated gamma frailty model was estimated using a modified EM approach given in Kifle et al (2022).
#'
#' @seealso \code{\link{bcfrailph}}
#'
#' @references
#'
#' Kifle YG, Chen DG, Haileyesus MT (2022). Multivariate Frailty Models using Survey Weights with Applications to Twins Infant Mortality in Ethiopia. Statistics and Its Interface,106(4), 1\-10.
#'
#' @examples
#' set.seed(4)
#' simdata<-simbcfrailph(psize=300, cenr= c(0.3),beta=c(2),frailty=c("gamma"),
#' frailpar=c(0.5,0.5),bhaz=c("weibull"),
#' bhazpar=list(shape =c(5), scale = c(0.1)),
#' covartype= c("B"),covarpar=list(fargs=c(1),sargs=c(0.5)))
#' Y<-simdata$Y;X<-simdata$X
#'
#' bcspfit<-fitbccv.gammasp(X=X,Y=Y,initfrailp=NULL)
#' bcspfit$coef
#' bcspfit$frailpar
#'
#'
fitbccv.gammasp<-function(X,Y,initfrailp,weights=NULL,control=bcfrailph.control(),SE=TRUE){
if(length(weights)==0){
censor=Y[, 2];time=Y[, 1]
# To obtain indexes, unique event times and others
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)
if(any(is.na(ind.haz))){
tord_diff<-as.array(diff(c(0,timeo)))
id.zero_tord <- which(apply(((tord_diff<0.0000001)&(tord_diff>0)),1, all))
if(length(id.zero_tord)>0){time[indx[id.zero_tord]]<- time[indx[id.zero_tord-1]]
order=sort(time, decreasing = FALSE, index.return = TRUE);indx=order$ix;timeo=order$x}
uniq_tim<-unique(sort(time))
ind.haz=match(time,uniq_tim)}
Y[, 1]<-time
data.n1 <- nrow(X);data.n <-data.n1/2#### data.n is the number of pairs
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI)  # Matrix of riskset indicator
uniind=array(1:length(uniq_tim))
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=censor)
n_eve <-as.vector(t(Rev))  # number of events at each unique event times
i2<-2*array(1:data.n);i1<-i2-1  # i1 is indicator of the first subjects in pairs, i2 is for the second subject
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
method = "breslow", resid=FALSE)
bet<-cph0$coefficients  # Initial covariate coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0<-cumsum(c(n_eve/svexp_bet_xo))  # Initial cumulative baseline hazard
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
cen1=censor[i1];cen2=censor[i2]
Z=W=rep(0,data.n1)
# to obtain initial estimate for frailty parameters
if(length(initfrailp)>0){newtht<-initfrailp;lik=cph0$loglik[2]}else{
datu=data.frame(time=time,censor=censor,X)
IID=array(1:data.n1);ar=array(1:data.n);PID=rep(ar,each=2)
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty(IID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients;x_bet<-X%*%bet;W=cph.fit$frail
svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
re=cph.fit$history$`frailty(IID)`$history
re1=re[,1];re2=re[,3]
tht=re1[length(re1)];lik=re2[length(re2)]
QQ=(cph.fit$loglik[2]-sum(censor*W)+sum(censor))
newtht=c(tht,0.5)
if(tht<0.0001){
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty(PID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients
x_bet<-X%*%bet
w=cph.fit$frail
W[i1]<-w;W[i2]<-w
svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
re=cph.fit$history$`frailty(PID)`$history
re1=re[,1];re2=re[,3]
tht=re1[length(re1)];lik=re2[length(re2)]
QQ=(cph.fit$loglik[2]-sum(censor*W)+sum(censor))
newtht=c(tht,0.5)
if(tht<0.0001){newtht=c(0.001,0.001)}}}
xx=matrix(c(H_bet_x[i1],H_bet_x[i2],(H_bet_x[i1]+H_bet_x[i2]),cen1,cen2),data.n,5)
ui0=matrix(0,3,2);ci0=rep(0,3);ui0[1,1]<-1;ui0[2,2]<-1
ui0[3,2]<-c(-1);ci0[3]=c(-1)
fittr0=do.call(constrOptim, args=c(list(theta=newtht,f=.Llikgammacvx,
grad=.fdLlikgammacvx,xx=xx,QQ=QQ,ui=ui0,ci=ci0)))
newtht=fittr0$par;lik=-fittr0$value
tracce=matrix(0,(control$max.iter+2),(length(bet)+3))
tracce[1,]<-c(bet,newtht,lik)
newdiff=1
# iter is for outer iteration, iter1 is for inner
iter=0
repeat{
iter=iter+1
lik0=lik
iter1=0
repeat{
iter1=iter1+1
beto=bet
e_frail<-.Expefrail(newtht=newtht,H1=H_bet_x[i1],H2=H_bet_x[i2],cen1=cen1,cen2=cen2)
Z[i1]<-e_frail$z1;Z[i2]<-e_frail$z2;W<-log(Z)
# Z is the expected frailty
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = bet, control = survival::coxph.control(),
method = "breslow", resid=FALSE)
bet<-cph1$coefficients  # beta estimates
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet+W)))
H0<-cumsum(c(n_eve/svexp_bet_xo))
H_bet_x=c(H0[ind.haz]*exp(c(x_bet)))
newdiff=max(abs(bet-beto))
if((newdiff <= 1e-08)  |  (iter1 >= 50)) break}
QQ=(cph1$loglik[2]-sum(censor*W)+sum(censor))
xx=matrix(c(H_bet_x[i1],H_bet_x[i2],(H_bet_x[i1]+H_bet_x[i2]),cen1,cen2),data.n,5)
fittr=do.call(nlminb, args=c(list(start=newtht, objective=.Llikgammacvx,
gradient = .fdLlikgammacvx,xx=xx,QQ=QQ,lower = c(0,0), upper = c(Inf,1),
control=control$nlminb_control)))
newtht=fittr$par  # estimate of frailty parameters
lik=-fittr$objective
new.diff=lik-lik0
tracce[(iter+1),]<-c(bet,newtht,lik)
if((new.diff < control$tol)  |  (iter >= control$max.iter)) break}
converg=ifelse((new.diff < control$tol),1,0)
id.lin.zero <- which(apply(tracce==0, 1, all))
if(length(id.lin.zero)>0){tracce<- tracce[-id.lin.zero, ]}
colnames(tracce)<-c(colnames(X),c("theta","Row"),c("Iloglik"))
tracce=cbind(iteration=array(0:iter),tracce)
newtht0=newtht;if(newtht[1]<=0.000001){newtht[1]=0.000001;newtht0[1]<-0.0001}
h0=diff(c(0,H0));nonzero_h0=h0[h0>0]
if(SE){ # obtain SE from the mariginal likelihood
adjj_se=.SEbcfrailcv(bet=bet,newtht=newtht0,n_eve=n_eve,etime=uniq_tim,h0=h0,
censor=censor,time=time,X=X,H=H_bet_x)
adjse=c(adjj_se$se)
vcovth=solve(adjj_se$vco)
seho=adjj_se$seho
if(any(is.na(adjse))){
adjse<-c(sqrt(diag(cph1$var)),sqrt(diag(vcovth)))
warning("frailty parameters might be at the boundary of parameter space.")}}else{
adjse=c(sqrt(diag(cph1$var)),0,0)
uqt=uniq_tim[h0>0]
seho=cbind(haz=nonzero_h0,time=uqt)}
fit <-list(coefficients=bet,frailparest= c(theta=newtht[1],Row=newtht[2]),
Iloglilk=lik,loglilk0=cph0$loglik[2],loglilk=cph1$loglik[2],resid=H_bet_x,
lin.prid=cph1$linear.predictors,stderr=adjse,iteration=iter,history=tracce,bhaz=seho,
vcov=cph1$var,frail=exp(W),e.time=uniq_tim,n.event=n_eve,time=time,censor=censor,X=X,
convergence=converg,fittheta=fittr)}else{
fit<-.bcgamfitcvw(X,Y,initfrailp,weights,control)}
fit$call <- match.call()
fit$weights <- weights
class(fit) <- c("bcfrailph")
fit
}
