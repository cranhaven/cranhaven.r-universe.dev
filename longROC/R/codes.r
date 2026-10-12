## X is an n by Tmax matrix of longitudinal score/marker, 
## where Tmax is the maximum number of visits
## tt: time-horizon for ROC curve
## u: lower limit for recording events 
## s: vector of number of visits (s[i]<=Tmax) to use for each subject
## etime,status: event times and status at last follow-up (vectors of
## length n)
## vtimes: vector of length Tmax of visit times 
## cutoff: c. 
# f_c: I(\cup X(t_j)>c) fixed 


sensspec.s=function(X,etime,status,u=NULL,tt,s,vtimes,cutoff=0,fc=NULL) {

if(is.null(u)) {u=max(vtimes[s])}
n=length(s)
positive=rep(NA,n)
if(is.null(fc)) {
fc=function(x,cutoff) ifelse(any(x>cutoff,na.rm=TRUE),1,0)
}

for(i in 1:n) {

if(s[i]>1) {
w=sum(is.na(X[i,1:s[i]]))+ifelse(etime[i]>u,0,1)
if(w==0) {
positive[i]=fc(X[i,1:s[i]],cutoff=cutoff)}}
if(s[i]==1) {
w=!is.na(X[i,1]) & etime[i]>u
if(w) {
positive[i]=fc(X[i,1],cutoff=cutoff)}
}
}

negative=1-positive
w=which(!is.na(positive) & etime>u)
n=length(w)
if(mean(positive,na.rm=TRUE)==0 | mean(positive,na.rm=TRUE)==1) {return(c(NA,NA))}
a=survfit(Surv(etime[w]-u,status[w])~1)
b0=survfit(Surv(etime[w]-u,status[w])[positive[w]==0]~1)
b1=survfit(Surv(etime[w]-u,status[w])[positive[w]==1]~1)
wma=which.min(abs(a$time-tt+u))
wmb1=which.min(abs(b1$time-tt+u))
wmb0=which.min(abs(b0$time-tt+u))
sens=(1-b1$surv[wmb1])*mean(positive[w])/(1-a$surv[wma])
spec=b0$surv[wmb0]*mean(negative[w])/a$surv[wma]
c(sens,spec)
}

roc.s=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL) {

ss=sapply(sort(unique(as.vector(X))),function(cf) 
sensspec.s(X=X,etime=etime,status=status,u=u,tt=tt,s=s,vtimes=vtimes,cutoff=cf,fc=fc))
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

butstrap.s=function(X,etime,status,u=NULL,tt,s,vtimes,auc1,B=50,fc=NULL) {

aucs=rep(NA,B) 
aucs[1]=auc1
n=length(etime)
for(j in 2:B) {
but=sample(n,n,replace=TRUE)
aucs[j]=auc(roc.s(X[but,],etime[but],status[but],u,tt,s,vtimes,fc=fc))
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

maxauc.s=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL) {
if(!is.array(X)) {stop("X must be an array organized as var by subject
by time")}
p=dim(X)[1]-1 
op=optim(rep(0,p),function(be) {
score=X[1,,]
for(j in 1:p) {score=score+be[j]*X[j+1,,]}
-auc(roc.s(score,etime,status,u,tt,s,vtimes,fc=fc))},method=ifelse(p==1,"Brent","Nelder-Mead"),lower=ifelse(p==1,-10,-Inf),upper=ifelse(p==1,10,Inf))
beta=op$par
score=X[1,,]
for(j in 1:p) {score=score+beta[j]*X[j+1,,]}
return(list(beta=beta,score=score))}

plotAUC.s=function(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL,plot=TRUE) {
if(length(tt)==1) {stop("tt must be a vector")}
aucs=sapply(tt,function(ho) auc(roc.s(X,etime,status,u,ho,s,vtimes,fc)))
if(plot) {plot(tt,aucs,type="l")}
return(aucs)}

