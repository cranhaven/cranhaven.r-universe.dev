## ----message=FALSE------------------------------------------------------------
library(evreg)

## -----------------------------------------------------------------------------
gendat<-function(n){
  x<-rep(0,n)
  y<-x
  for(i in 1:n){
    u<-runif(1)
    if(u<0.5) x[i]<-runif(1,-3,-1) else x[i]<-runif(1,1,4)
    if(x[i]<0) y[i] <- sin(3*x[i])+ x[i]+ sqrt(0.01)* rnorm(1) 
      else y[i] <- sin(3*x[i])+ x[i]+ sqrt(0.3)* rnorm(1)
  }
  return(list(x=x,y=y))
}

## -----------------------------------------------------------------------------
n<-400
nt<-1000
set.seed(20230104)
train<-gendat(n)
test<-gendat(nt)

## ----hyperparameter tuning----------------------------------------------------
cv<-ENNreg_cv(train$x,train$y,K=30,XI=c(0,0.01,0.1),RHO=c(0,0.01,0.1),verbose=FALSE)
cv

## ----training-----------------------------------------------------------------
fit<-ENNreg(train$x,train$y,K=30,xi=cv$xi,rho=cv$rho,verbose=FALSE)

## ----prediction---------------------------------------------------------------
xt<-seq(-4,5,0.01)
pred<-predict(fit,xt)

## ----compute belief intervals-------------------------------------------------
int50<-intervals(pred,0.50)
int90<-intervals(pred,0.9)
int99<-intervals(pred,0.99)

## ----plot predictions, fig.width=5, fig.height=5------------------------------
library(ggplot2)
int<-data.frame(lwr50=int50$INTBel[,1],upr50=int50$INTBel[,2],
                lwr90=int90$INTBel[,1],upr90=int90$INTBel[,2],
                lwr99=int99$INTBel[,1],upr99=int99$INTBel[,2],
                x=xt,mux=pred$mux)
ggplot(data=as.data.frame(train), aes(x = x)) + 
  geom_point(aes(y = y)) +
  geom_ribbon(data=int, aes(ymin = lwr50, ymax = upr50,x=x),alpha=0.2)+
  geom_ribbon(data=int, aes(ymin = lwr90, ymax = upr90,x=x),alpha=0.15)+
  geom_ribbon(data=int, aes(ymin = lwr99, ymax = upr99,x=x),alpha=0.1) +
  geom_line(data=int,aes(x = x,y=mux),color="red",linewidth=1.5)

## ----predict on test set------------------------------------------------------
pred.tst<-predict(fit,test$x,test$y)

## ----compute intervals and coverage-------------------------------------------
A<-seq(0.1,0.9,0.1)
nA<-length(A)
probbel<-rep(0,nA)
plbel<-rep(0,nA)
for(i in 1:nA){
  int<-intervals(pred.tst,A[i],test$y)
  probbel[i]<-int$coverage.Bel
  plbel[i]<-int$Pl.Bel
}

## ----plot calibration curve, fig.width=5, fig.height=5------------------------
oldpar <- par(pty="s")
plot(c(0,A,1),c(0,plbel,1),type="l",lwd=2,col="blue",xlab="",ylab="")
points(c(0,A,1),c(0,plbel,1),pch=21,bg="blue",cex=1.5)
lines(c(0,A,1),c(0,probbel,1),col="red",lwd=2)
points(c(0,A,1),c(0,probbel,1),pch=22,bg="red",cex=1.5)
abline(0,1,lty=2)
title(ylab="coverage rate", line=2.2, cex.lab=1.2)
title(xlab="level", line=2.2, cex.lab=1.2)
par(oldpar)

## -----------------------------------------------------------------------------
GRFN<-list(mu=1,sig=0.5,h=1)

## ----draw belief plot, fig.width=5, fig.height=5------------------------------
x<-seq(-4,6,0.01)
plot(x,Bel(x-1,x+1,GRFN),type="l",xlab="x",ylab="Bel([x-r,x+r])",
     lwd=2,ylim=c(0,1))
lines(x,Bel(x-0.5,x+0.5,GRFN),lwd=2,lty=2)
lines(x,Bel(x-0.1,x+0.1,GRFN),lwd=2,lty=3)
legend("topright",legend=c("r=1","r=0.5","r=0.1"),lty=c(1,2,3),bty="n")

## ----draw plausibility plot, fig.width=5, fig.height=5------------------------
plot(x,Pl(x-1,x+1,GRFN),type="l",xlab="x",ylab="Pl([x-r,x+r])",lwd=2,ylim=c(0,1))
lines(x,Pl(x-0.5,x+0.5,GRFN),lwd=2,lty=2)
lines(x,pl_contour(x,GRFN),lwd=2,lty=3)
legend("topright",legend=c("r=1","r=0.5","r=0"),lty=c(1,2,3),bty="n")

## ----plot cdfs, fig.width=5, fig.height=5-------------------------------------
plot(x,Bel(-Inf,x,GRFN),type="l",xlab="x",ylab="Lower/upper cdfs",lwd=2)
lines(x,Pl(-Inf,x,GRFN),type="l",lwd=2)

## -----------------------------------------------------------------------------
GRFN1<-list(mu=0,sig=2,h=4)
GRFN2<-list(mu=1,sig=1,h=1)

## -----------------------------------------------------------------------------
GRFN12s<-combination_GRFN(GRFN1,GRFN2,soft=TRUE)
GRFN12h<-combination_GRFN(GRFN1,GRFN2,soft=FALSE)
print(GRFN12s$GRFN)
print(GRFN12h$GRFN)

## ----plot combinations, fig.width=5, fig.height=5-----------------------------
x<-seq(-4,6,0.01)
plot(x,pl_contour(x,GRFN1),type="l",xlab="x",ylab="plausibility",lwd=2,
     ylim=c(0,1),col="blue")
lines(x,pl_contour(x,GRFN2),lwd=2,lty=1,col="red")
lines(x,pl_contour(x,GRFN12s$GRFN),lwd=2,lty=2,col="green")
lines(x,pl_contour(x,GRFN12h$GRFN),lwd=2,lty=2,col="cyan")
legend("topright",legend=c("GRFN1","GRFN2","soft comb.","hard comb."),
       lty=c(1,1,2,2),bty="n",col=c("blue","red","green","cyan"))

