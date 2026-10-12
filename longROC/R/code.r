
#' Sensitivity and Specificity 
#'
#' This function allows you to compute sensitivity and specificity 
#' @param X n by S matrix of longitudinal score/biomarker (NA if unmeasured)
#' @param etime n vector with follow-up times 
#' @param status n vector with event indicators 
#' @param u lower limit for evaluation of sensitivity and specificity. Defaults to vtimes[s]
#' @param tt upper limit (time-horizon) for evaluation of sensitivity
#and specificity. 
#' @param s scalar number of measurements/visits to use for each subject. s<=S
#' @param vtimes S vector with visit times 
#' @param cutoff for events. Defaults to 0
#' @param fc events are defined as fc = 1. Defaults to I(\cup X(t_j)>cutoff)
#' @keywords sensitivity 
#' @keywords specificity 
#' @export
#' 

sensspec=function(X,etime,status,u=NULL,tt,s,vtimes,cutoff=0,fc=NULL) {

if(is.null(u)) {
u=vtimes[s]}

if(is.null(fc)) {
fc=function(x,cutoff) {ifelse(any(x>cutoff,na.rm=TRUE),1,0)}
}

if(s>1) {
w=which(apply(is.na(X[,1:s]),1,sum)==0 & etime>u)
positive=apply(X[w,1:s],1,fc,cutoff=cutoff)}
if(s==1) {
w=which(!is.na(X[,1]))
positive=fc(X[w,1],cutoff=cutoff)}
negative=1-positive
n=length(w) 
if(mean(positive,na.rm=TRUE)==0 | mean(positive,na.rm=TRUE)==1) {return(c(NA,NA))}
a=survfit(Surv(etime[w]-u,status[w])~1)
b0=survfit(Surv(etime[w]-u,status[w])[positive==0]~1)
b1=survfit(Surv(etime[w]-u,status[w])[positive==1]~1)
wma=which.min(abs(a$time-tt+u))
wmb1=which.min(abs(b1$time-tt+u))
wmb0=which.min(abs(b0$time-tt+u))
sens=(1-b1$surv[wmb1])*mean(positive)/(1-a$surv[wma])
spec=b0$surv[wmb0]*mean(negative)/a$surv[wma]
c(sens,spec)
}

#' Receiver Operating Characteristic (ROC) curve 
#'
#' This function allows you to compute a time-dependent ROC curve
#' based on repeatedly measured scores or biomarkers. 
#' Events are defined as I(\cup X(t_j)>cutoff)
#' @param X n by S matrix of longitudinal score/biomarker (NA if unmeasured)
#' @param etime n vector with follow-up times 
#' @param status n vector with event indicators 
#' @param u lower limit for evaluation of sensitivity and specificity. Defaults to vtimes[s]
#' @param tt upper limit (time-horizon) for evaluation of sensitivity
#and specificity. 
#' @param s scalar number of measurements/visits to use for each subject. s<=S
#' @param vtimes S vector with visit times
#' @param fc events are defined as fc = 1. Defaults to I(\cup X(t_j)>cutoff)
#' @keywords sensitivity 
#' @keywords specificity 
#' @keywords roc 
#' @export
#' 

roc=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL) {

if(is.null(u)) {u=vtimes[s]}

so=sort(unique(as.vector(X)))
ss=sapply(so,function(cf) 
sensspec(X=X,etime=etime,status=status,u=u,tt=tt,s=s,vtimes=vtimes,cutoff=cf,fc=fc))
w=which(!is.na(ss[1,]) & !is.nan(ss[1,]) & !is.nan(ss[2,]) & !is.na(ss[2,]))
yaxis=ss[1,w]
xaxis=1-ss[2,w]
yaxis[yaxis>1]=1
yaxis[yaxis<0]=0
xaxis[xaxis>1]=1
xaxis[xaxis<0]=0
yf=isoreg(xaxis,yaxis)$yf
rbind(c(0,0),cbind(sort(xaxis),yf),c(1,1))
}

#' Area Under the Receiver Operating Characteristic (AUC) curve 
#'
#' This function allows you to compute a time-dependent Area Under the
#' ROC Curve (AUC) based on repeatedly measured scores or biomarkers. 
#' Events are defined as I(\cup X(t_j)>cutoff)
#' @param ss the output of function roc 
#' @keywords sensitivity 
#' @keywords specificity 
#' @keywords auc 
#' @export
#' 

auc=function(ss) {
res=ss[1,1]*ss[1,2]/2
for(j in 2:nrow(ss)) {
res=res+(ss[j,1]-ss[j-1,1])*(ss[j,2]-ss[j-1,2])/2
res=res+ss[j-1,2]*(ss[j,1]-ss[j-1,1])}
res
}

#' Time dependent NRI with repeatedly measured biomarkers 
#'
#' This function allows you to compute a time-dependent NRI
#' based on repeatedly measured scores or biomarkers. 
#' Events are defined as I(\cup X(t_j)>cutoff). A list with three
#' elements is returned (1/2 NRI, NRI for events, NRI for non-events).
#' @param risk1 baseline risk 
#' @param risk2 enhanced risk measure 
#' @param etime n vector with follow-up times 
#' @param status n vector with event indicators 
#' @param u lower limit for evaluation of sensitivity and specificity. Defaults to vtimes[s]
#' @param tt upper limit (time-horizon) for evaluation of sensitivity
#and specificity. 
#' @keywords sensitivity 
#' @keywords specificity 
#' @keywords nri 
#' @export
#' 

nri=function(risk1,risk2,etime,status,u,tt) {

w=which(!is.na(risk1) & !is.na(risk2) & etime>u)
a=survfit(Surv(etime[w]-u,status[w])~1)
wma=which.min(abs(a$time-tt+u))

est=(1-a$surv[wma])
den=est*(1-est)

Us=which(risk2[w]>risk1[w])
Ds=which(risk2[w]<risk1[w])

b0=survfit(Surv(etime[w]-u,status[w])[Us]~1)
b1=survfit(Surv(etime[w]-u,status[w])[Ds]~1)
wmb0=which.min(abs(b0$time-tt+u))
wmb1=which.min(abs(b1$time-tt+u))
num1=((1-b0$surv[wmb0])-est)*length(Us)/length(w)
num2=(est-(1-b1$surv[wmb1]))*length(Ds)/length(w)

nri=0.5*(num1+num2)/den

nriev=(1-b0$surv[wmb0])*length(Us)/length(w)-(1-b1$surv[wmb1])*length(Ds)/length(w)
nriev=nriev/est

nrinev=b1$surv[wmb1]*length(Ds)/length(w)-b0$surv[wmb0]*length(Us)/length(w)
nrinev=nrinev/(1-est)
list(nri=nri,nri.events=nriev,nri.nonevents=nrinev)}


butstrap=function(X,etime,status,u=NULL,tt,s,vtimes,auc1,B=50,fc=NULL) {

aucs=rep(NA,B) 
aucs[1]=auc1
n=length(etime)
for(j in 2:B) {
but=sample(n,n,replace=TRUE)
aucs[j]=auc(roc(X[but,],etime[but],status[but],u,tt,s,vtimes,fc=fc))
}
aucs=aucs[aucs!=0]
se=sd(aucs)
p=2*pnorm(-abs((auc1-0.5)/se))
ci.np=quantile(aucs,c(0.025,0.975))
ci.np[ci.np<0]=0
ci.np[ci.np>1]=1
ci.par=c(auc1-1.96*sd(aucs),auc1+1.96*sd(aucs))
ci.par[ci.par<0]=0
ci.par[ci.par>1]=1
list(p.value=p,se=se,ci.np=ci.np,ci.par=ci.par)}

butstrap.nri=function(risk1,risk2,etime,status,u,tt,nri1,wh,B=1000) {
nris=rep(NA,B) 
nris[1]=nri1
n=length(etime)
for(j in 2:B) {
but=sample(n,n,replace=TRUE)
nris[j]=nri(risk1[but],risk2[but],etime[but],status[but],u,tt)[[wh]]
}
nris=nris[!is.nan(nris) & !is.na(nris) & is.finite(nris)]
se=sd(nris)
p=2*pnorm(-abs((nri1)/se))
ci.np=quantile(nris,c(0.025,0.975))
ci.np[ci.np< -1]=-1
ci.np[ci.np> 1]=1
ci.par=c(nri1-1.96*se,nri1+1.96*se)
ci.par[ci.par< -1]=-1
ci.par[ci.par>1]=1
list(p.value=p,se=se,ci.np=ci.np,ci.par=ci.par)}

maxauc=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL) {
if(!is.array(X)) {stop("X must be an array organized as var by subject
by time")}
p=dim(X)[1]-1 
op=optim(rep(0,p),function(be) {
score=X[1,,]
for(j in 1:p) {score=score+be[j]*X[j+1,,]}
-auc(roc(score,etime,status,u,tt,s,vtimes,fc=fc))},method=ifelse(p==1,"Brent","Nelder-Mead"),lower=ifelse(p==1,-10,-Inf),upper=ifelse(p==1,10,Inf))
beta=op$par
score=X[1,,]
for(j in 1:p) {score=score+beta[j]*X[j+1,,]}
return(list(beta=beta,score=score))}

plotROC=function(ro, add=FALSE, col=NULL) {
if(is.null(col)) {col="red"}
if(!add) {
plot(ro, col = col, add = FALSE, main="ROC curve",
xlab="1-Specificity", ylab="Sensitivity",xlim=c(0,1),ylim=c(0,1))
abline(a=0,b=1)}
if(add) {lines(ro,col=col)}}

plotAUC=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL,plot=TRUE) {
if(length(tt)==1) {stop("tt must be a vector")}
aucs=sapply(tt,function(ho) auc(roc(X,etime,status,u,ho,s,vtimes,fc)))
if(plot) {plot(tt,aucs,type="l")}
return(aucs)}



