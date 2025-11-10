#'
#' @name fitbccv.lognsp
#' @title Bivariate correlated lognormal frailty model fitting function.
#' @description Semi-parametric Bivariate correlated lognormal frailty model fitting function.
#'
#' @param initfrailp Initial estimates for the frailty parameters. If not specified, initial frailty variance will be obtained from coxph with univariate lognormal frailty model and for correlation c(0.5) will be used.
#' @param X Matix of predictors. This should not include an intercept.
#' @param Y a Surv object containing 2 columns (coxph.fit).
#' @param control Arguments to control the fit. The default is \code{\link{bcfrailph.control}}.

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
#' @export fitbccv.lognsp
#' @importFrom stats nlminb
#' @importFrom stats nlm
#' @importFrom survival Surv
#' @importFrom survival coxph
#'
#' @note This function is important especially for simulation studies as it reduced checking time.
#' Parameters of Bivariate correlated lognormal frailty model is based on the penalized partial likelihood approach by Rippatti and Palmgren (2000).
#' 
#' @seealso \code{\link{bcfrailph}}
#'
#' @references
#'
#' Rippatti, S. and Palmgren, J (2000). Estimation of multivariate frailty models using penalized partial likelihood. Biometrics, 56: 1016-1022.
#'
#' @examples
#' set.seed(18)
#' simdata<-simbcfrailph(psize=100, cenr= c(0.2),beta=c(1,-0.7,0.5),frailty=c("lognormal"),
#' frailpar=c(0.5,-0.25),bhaz=c("exponential"),
#' bhazpar=list(scale = c(0.1)),covartype= c("N","N","B"),
#' covarpar=list(fargs=c(0,0,1),sargs=c(1,1,0.5)),comncovar=2)
#' Y<-simdata$Y;X<-simdata$X
#' 
#' bcspfit<-fitbccv.lognsp(X=X,Y=Y,initfrailp=NULL)
#' bcspfit$coef 
#' bcspfit$frailpar
#'
fitbccv.lognsp<-function(X,Y,initfrailp,control=bcfrailph.control()){
time=Y[, 1];censor=Y[, 2]
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
ncovar_coef<-ncol(X)
uniind=array(1:length(uniq_tim))
Rev <- apply(as.array(uniind),1,tofevent,x=ind.haz,cen=censor)
n_eve <-as.vector(t(Rev)) # number of events at each unique event times
ar=array(1:data.n)
i1<-2*ar-1;i2<-2*ar  # i1 is indicator of the first subjects in pairs, i2 is for the second subject
clustind=array(1:data.n1)
uniq_clus_id=unique(clustind)
firi<-match(uniq_clus_id,clustind)
firi2=firi[-1]-1
firi3<-c(firi2,length(clustind))
tau=cbind(firi,firi3)
RI <- apply(as.array(uniq_tim),1,risskset,x=time)
RI <-t(RI) # Matrix of riskset indicator
cen1=censor[i1];cen2=censor[i2]
resinteracmat<-apply(as.matrix(X),2,interacmat,u=X)
Xoxo<-matrix(resinteracmat,data.n1,ncovar_coef^2)
cph0 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =NULL, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
bet<-cph0$coefficients# Initial covariate coefficients
x_bet<-X%*%bet
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0=cumsum(n_eve/svexp_bet_xo) # Initial cumulative baseline hazard
H_bet_x=H0[ind.haz]*c(exp(X%*%bet))
W=estimo=new.diff=NULL;new.diff=1
i3=indicmin(i1=i1,i2=i2,time)
INC<-matrix(c(i1,i2,i3),data.n,3)
tracce=matrix(0,(control$max.iter+4),(length(bet)+3))
if(length(initfrailp)>0){newtht<-initfrailp}else{
datu=data.frame(time=time,censor=censor,X)
IID=array(1:data.n1);ar=array(1:data.n);PID=rep(ar,each=2)
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty.gaussian(IID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients
re=cph.fit$history$`frailty.gaussian(IID)`$history
re1=re[,1];tht=re1[length(re1)]
if(tht<0.01){
form<-as.formula(paste(paste("Surv(time,censor)"),"~",paste(colnames(X), collapse = "+ "),
"+ frailty.gaussian(PID)",collapse=""))
cph.fit <- coxph(form,data=datu,method = "breslow")
bet=cph.fit$coefficients
re=cph.fit$history$`frailty.gaussian(PID)`$history
re1=re[,1];tht=re1[length(re1)]}
if(tht<0.01){newtht=c(0.1,0)}else{newtht=c(tht,0)}}
tracce[3,]<-c(bet,newtht,1)
paro=rep(0,(data.n1+length(bet)))
paro[1:length(bet)]=bet
fittrp=do.call(nlm, args=c(list(f=llpennlognGeni,p=paro,newtht=newtht,
censor=censor,X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,steptol= 1e-06,fscale = 1,
print.level = 0, ndigit = 12,iterlim=20,gradtol = 1e-06,check.analyticals = FALSE)))
paro=fittrp$estimate;W=paro[(1+ncovar_coef):(data.n1+ncovar_coef)];bet=paro[1:ncovar_coef]
oppD2W<-Inv.HesPPL(W=W,bet=bet,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx)
ddet=oppD2W$ddet
lik0=-data.n*log(newtht[1])-0.5*data.n*log(1-newtht[2]^2)-0.5*ddet-fittrp$minimum
bet0=bet;newtht0=newtht;W0=W
new.diff=1
iter=0
repeat{
iter=iter+1
lik=lik0;bet=bet0;newtht=newtht0;W=W0
tracce[(iter+3),]<-c(bet,newtht,new.diff)
fittrp=do.call(nlm, args=c(list(f=llpennlognGeni,p=paro,newtht=newtht,
censor=censor,X=X,RI=RI,n_eve=n_eve,ind.haz=ind.haz,i1=i1,i2=i2,steptol= 1e-06,fscale = 1,
print.level = 0, ndigit = 12,iterlim=20,gradtol = 1e-06,check.analyticals = FALSE)))
paro=fittrp$estimate;W0=paro[(1+ncovar_coef):(data.n1+ncovar_coef)]
bet0=paro[1:ncovar_coef];W1=W0[i1];W2=W0[i2]
oppD2W<-Inv.HesPPL(W=W0,bet=bet0,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx)
Hii=oppD2W$Hii;Hij=oppD2W$Hij;ddet=oppD2W$ddet
wsq=sum(W1^2+W2^2);w12=sum(W1*W2)
r0=2*(Hij+w12)/(Hii+wsq);a0=((Hii+wsq)-2*r0*(Hij+w12))/(2*data.n*(1-r0^2))
if(a0<=0.00001){a0<-0.00001};if(r0<=c(-0.999)){r0<-c(-0.999)};if(r0>=c(0.999)){r0<-c(0.999)}
newtht0=c(a0,r0)
lik0=-data.n*log(a0)-0.5*data.n*log(1-r0^2)-0.5*ddet-fittrp$minimum
new.diff=(lik0-lik)
if((new.diff < control$tol)|(iter >= control$max.iter)) break}
id.lin.zero <- which(apply(tracce==0, 1, all))
if(length(id.lin.zero)>0){tracce<- tracce[-id.lin.zero, ]}
nk=length(tracce[,1])
colnames(tracce)<-c(colnames(X),c("theta","Row"),c("reltol"))
tracce=cbind(iteration=array(0:(nk-1)),tracce)
convergence=ifelse((new.diff < control$tol),1,0)
cph1 <- survival::coxph.fit(x = X, y = Y, strata = NULL,
offset =W, init = NULL, control = survival::coxph.control(),
weights = NULL,method = "breslow", resid=FALSE)
bet=cph1$coefficients
vcov2=cph1$var;x_bet<-X%*%bet+W
svexp_bet_xo=as.vector(RI%*%(exp(x_bet)))
H0=cumsum(n_eve/svexp_bet_xo);h0=diff(c(0,H0))
H_bet_x=H0[ind.haz]*c(exp(X%*%bet))
# to obtain SE
obthes=SEbothlognph(W=W,bet=bet,newtht=newtht,X=X,RI=RI,INC=INC,
n_eve=n_eve,ind.haz=ind.haz,indx=indx,Xoxo=Xoxo)
adjse<-obthes$SE;vcov<-obthes$vcovb;setht=obthes$setht
n_eve0=as.numeric(n_eve>0)
trevntimein=n_eve0*array(1:length(n_eve0))
trevntimein1=trevntimein[trevntimein>0]
trevntime=uniq_tim[trevntimein1]
nonzero_h0<-h0[trevntimein1]
seho=cbind(unqtime=trevntime,ho=nonzero_h0)
res <-list(coefficients=bet,frailparest=newtht,
vcov2 = vcov,stderr=c(adjse,setht),loglilk0=cph0$loglik[2],loglilk=cph1$loglik[2],
X=X,time=time,censor=censor,Iloglilk=lik,resid=H_bet_x,lin.prid=cph1$linear.predictors,
frail=exp(W),iteration=iter,e.time=uniq_tim,n.event=n_eve,bhaz=seho,history=tracce,
convergence=convergence)
res$call <- match.call()
class(res) <- c("bcfrailph")
res
}

