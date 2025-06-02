### R code from vignette source 'evclust_vignette.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("MASS")


###################################################
### code chunk number 2: butterfly
###################################################
data(butterfly, package="evclust")
x<-butterfly


###################################################
### code chunk number 3: butterfly_ecm
###################################################
library("evclust")
set.seed(210121)
clus<-ecm(x, c=2, delta=5, disp=FALSE)
summary(clus)


###################################################
### code chunk number 4: plot_butterfly
###################################################
plot(x, type="n", xlab=expression(x[1]), ylab=expression(x[2]))
text(x[,1],x[,2],1:12,cex=0.8)


###################################################
### code chunk number 5: plot_cred_butterfly
###################################################
plot(1:12, clus$mass[,1], type="l", ylim=c(0,1), xlab="objects", ylab="masses", lwd=2, col=2, cex.lab=1)
lines(1:12, clus$mass[,2], lty=2, lwd=2, col=3)
lines(1:12, clus$mass[,3], lty=3, lwd=2, col=4)
lines(1:12, clus$mass[,4], lty=4, lwd=2, col=5)
legend(0.9,0.68,c(expression(paste("m(",symbol("\306"),")")),expression(paste("m(",omega[1],")")),
                 expression(paste("m(",omega[2],")")),expression(paste("m(",Omega,")"))) ,
       lty=1:4,lwd=3,cex=1,col=2:5,bty="n")



###################################################
### code chunk number 6: fourclass
###################################################
data(fourclass, package="evclust")
x<-fourclass[,1:2]
y<-fourclass[,3]


###################################################
### code chunk number 7: fourclass_evclus
###################################################
clus.evclus<-kevclus(x, c=4, disp=FALSE)


###################################################
### code chunk number 8: plot_fourclass_evclus
###################################################
plot(clus.evclus, x)


###################################################
### code chunk number 9: plot_fourclass_evclus_Shepard
###################################################
plot(x=clus.evclus,plot_Shepard=TRUE)


###################################################
### code chunk number 10: fourclass_ecm
###################################################
clus.ecm<-ecm(x, c=4, type = "pairs", delta=3.5, disp=FALSE, Omega=FALSE)


###################################################
### code chunk number 11: fourclass_ecm1
###################################################
clus.ecm1<-ecm(x, c=4, type = "pairs", delta=3.5, a=3, disp=FALSE, 
  Omega=FALSE)


###################################################
### code chunk number 12: plot_fourclass_ecm
###################################################
plot(clus.ecm, x)


###################################################
### code chunk number 13: plot_fourclass_ecm1
###################################################
plot(clus.ecm1, x)


###################################################
### code chunk number 14: fourclass_svm
###################################################
library(kernlab)
svmfit<-ksvm(~., data=data.frame(x), type="one-svc", kernel="rbfdot",
  nu=0.2, kpar=list(sigma=0.2))
fhat<-predict(svmfit, newdata=x, type="decision")


###################################################
### code chunk number 15: fourclass_nn
###################################################
clus.nn<-nnevclus(x, k=100, c=4, n_H=20, type='pairs', fhat=fhat,
  options=c(0,1000,1e-4,10), tr=TRUE)


###################################################
### code chunk number 16: plot_fourclass_nn
###################################################
plot(clus.nn, x)


###################################################
### code chunk number 17: plot_fourclass_nn_loss
###################################################
plot(clus.nn$trace$fct[clus.nn$trace$fct>0],type="l",xlab="iteration",ylab="loss")


###################################################
### code chunk number 18: contour_NN-evclus
###################################################
nx <- 50
xmin <- apply(x,2,min) - 2
xmax <- apply(x,2,max) + 2
xx <- seq(xmin[1], xmax[1], (xmax[1] - xmin[1]) / (nx-1))
yy <- seq(xmin[2], xmax[2], (xmax[2] - xmin[2]) / (nx-1))
PL <- array(0, c(nx, nx, 4))
for(i in 1:nx){
  X1 <- matrix(c(rep(xx[i],nx),yy),nx,2)
  x1 <- data.frame(X1)
  names(x1) <- c("x1","x2")
  fhat <- predict(svmfit, newdata=x1, type="decision")
  clus.t <- predict(clus.nn, X1, fhat)
  PL[i,,] <- clus.t$pl
}


###################################################
### code chunk number 19: plot_fourclass_nn_pl1
###################################################
plot(x[,1],x[,2],col=y,pch=y,xlim=c(xmin[1],xmax[1]),ylim=c(xmin[2],xmax[2]),
     main=expression(paste("pl(",omega[1],")")),
     cex.main=1.5,xlab="",ylab="")
contour(xx,yy,PL[,,1],col=1,lty=1,lwd=1.5,add=TRUE,labcex=1.1)


###################################################
### code chunk number 20: plot_fourclass_nn_pl2
###################################################
plot(x[,1],x[,2],col=y,pch=y,xlim=c(xmin[1],xmax[1]),ylim=c(xmin[2],xmax[2]),
     main=expression(paste("pl(",omega[2],")")),
     cex.main=1.5,xlab="",ylab="")
contour(xx,yy,PL[,,2],col=1,lty=1,lwd=1.5,add=TRUE,labcex=1.1)


###################################################
### code chunk number 21: plot_fourclass_nn_pl3
###################################################
plot(x[,1],x[,2],col=y,pch=y,xlim=c(xmin[1],xmax[1]),ylim=c(xmin[2],xmax[2]),
     main=expression(paste("pl(",omega[3],")")),
     cex.main=1.5,xlab="",ylab="")
contour(xx,yy,PL[,,3],col=1,lty=1,lwd=1.5,add=TRUE,labcex=1.1)


###################################################
### code chunk number 22: plot_fourclass_nn_pl4
###################################################
plot(x[,1],x[,2],col=y,pch=y,xlim=c(xmin[1],xmax[1]),ylim=c(xmin[2],xmax[2]),
     main=expression(paste("pl(",omega[4],")")),
     cex.main=1.5,xlab="",ylab="")
contour(xx,yy,PL[,,4],col=1,lty=1,lwd=1.5,add=TRUE,labcex=1.1)


###################################################
### code chunk number 23: RI_NS
###################################################
Ptrue <- pairwise_mass(create_hard_credpart(y))
P_evclus <- pairwise_mass(clus.evclus)
RI_evclus <- credal_RI(Ptrue,P_evclus,type="c")
NS_evclus <- nonspecificity(P_evclus)
P_ecm <- pairwise_mass(clus.ecm)
RI_ecm <- credal_RI(Ptrue,P_ecm,type="c")
NS_ecm <- nonspecificity(P_ecm)
P_nn <- pairwise_mass(clus.nn)
RI_nn <- credal_RI(Ptrue,P_nn,type="c")
NS_nn <- nonspecificity(P_nn)
print(c(RI_evclus, RI_ecm, RI_nn))
print(c(NS_evclus, NS_ecm, NS_nn))


###################################################
### code chunk number 24: load_S2
###################################################
data(s2, package="evclust")
n <- nrow(s2)


###################################################
### code chunk number 25: EK-NNclus_S2
###################################################
clus.eknnclus <- EkNNclus(s2, K=200, y0=sample(500,n,replace=TRUE), 
  ntrials=5, q=0.9, disp=FALSE)
print(clus.eknnclus$N)


###################################################
### code chunk number 26: s2_distance
###################################################
Dist <- createD(s2,k=100)


###################################################
### code chunk number 27: EVCLUS_S2_1
###################################################
clus.evclus1 <- kevclus(D=Dist$D, c=15, J=Dist$J, type='simple', 
  d0=quantile(Dist$D,0.25), m0=clus.eknnclus$mass, maxit=100, epsi=1e-4, 
  disp=FALSE)


###################################################
### code chunk number 28: pairs_S2
###################################################
P <- createPairs(clus.evclus1, k=2)
print(t(P$pairs))


###################################################
### code chunk number 29: EVCLUS_S2_2
###################################################
clus.evclus2 <- kevclus(D=Dist$D, c=15, J=Dist$J, type='pairs', 
  pairs=P$pairs, d0=quantile(Dist$D,0.25), m0=P$m0, maxit=100, epsi=1e-4, 
  disp=FALSE)


###################################################
### code chunk number 30: plot_s2_eknnclus
###################################################
plot(clus.eknnclus, s2)


###################################################
### code chunk number 31: plot_s2_evclus
###################################################
plot(clus.evclus2, s2)


###################################################
### code chunk number 32: load_iris
###################################################
data("iris", package="datasets")
x<-iris[,1:4]
Y<-as.numeric(iris[,5])
n<-nrow(x)


###################################################
### code chunk number 33: iris_bootclus
###################################################
fit <- bootclus(x, param=list(G = 3))


###################################################
### code chunk number 34: show_model_iris_bootclus
###################################################
print(fit$clus$modelName)


###################################################
### code chunk number 35: plot_iris1
###################################################
plot(as.dist(fit$CI[1,,]),as.dist(fit$BelPl[1,,]),pch=20,xlab="",ylab="")
title(xlab="Lower bound of 90% CI",ylab="Belief",line=2.2,cex.lab=1.7)
abline(0,1)


###################################################
### code chunk number 36: plot_iris2
###################################################
plot(as.dist(fit$CI[2,,]),as.dist(fit$BelPl[2,,]),pch=20,xlab="",ylab="")
title(xlab="Upper bound of 90% CI",ylab="Plausibility",line=2.2,cex.lab=1.7)
abline(0,1)


###################################################
### code chunk number 37: plot_iris_partition
###################################################
col<-1:3
lower.approx <- fit$Clus$lower.approx
upper.approx <- fit$Clus$upper.approx
plot.credal <- function(x,y){
  points(x,y,col=col[fit$Clus$y.pl], pch=Y, cex=apply(fit$Clus$pl,1,max))
  for(i in (1:3)){
    icol <- col[i] 
    xx <- cbind(x[lower.approx[[i]]], y[lower.approx[[i]]])
    polygon(xx[chull(xx),], lty = 1, lwd=1.5, border=icol)
    xx <- cbind(x[upper.approx[[i]]], y[upper.approx[[i]]])
    polygon(xx[chull(xx),], lty = 2, lwd=1.5, border=icol)
  }
}
pairs(x, panel = plot.credal)


###################################################
### code chunk number 38: confusion_iris
###################################################
table(iris[,5],fit$clus$classification)


###################################################
### code chunk number 39: confusion_rough_iris
###################################################
rough_partition<-apply(fit$Clus$Y,1,paste,collapse="")
table(iris[,5],rough_partition)


###################################################
### code chunk number 40: generate_bananas
###################################################
data<-bananas(300)
x<-data$x
y<-data$y


###################################################
### code chunk number 41: generate_constraints
###################################################
const<-create_MLCL(y,400)


###################################################
### code chunk number 42: bananas_cecm1
###################################################
clus.cecm <- cecm(x, c=2, ML=const$ML, CL=const$CL, ntrials=5, xi=0.9, 
  distance=1, disp=FALSE)


###################################################
### code chunk number 43: ARI_CECM
###################################################
library("mclust")
print(adjustedRandIndex(clus.cecm$y.pl, y))


###################################################
### code chunk number 44: plot_constraints
###################################################
plot(x,pch=y,col=y,xlab=expression(x[1]),ylab=expression(x[2]))
for(k in sample(nrow(const$ML),20)) lines(x[const$ML[k,],1],x[const$ML[k,],2])
for(k in sample(nrow(const$CL),20)) lines(x[const$CL[k,],1],x[const$CL[k,],2],lty=2)


###################################################
### code chunk number 45: plot_bananas_cecm
###################################################
plot(clus.cecm,x,ytrue=y,plot_approx=FALSE,plot_protos=TRUE)


###################################################
### code chunk number 46: kpca
###################################################
library(kernlab)
rbf <- rbfdot(sigma = 0.2)
K <- kernelMatrix(rbf, x)
res.kpcca <- kpcca(K, d1=2, ML=const$ML, CL=const$CL, epsi=1e-3, 
  disp=FALSE)


###################################################
### code chunk number 47: bananas_ecm
###################################################
clus.ecm<-ecm(res.kpcca$z,c=2,disp=FALSE)


###################################################
### code chunk number 48: ARI_ECM
###################################################
print(adjustedRandIndex(clus.ecm$y.pl, y))


###################################################
### code chunk number 49: bananas_cecm2
###################################################
clus.cecm1 <- cecm(res.kpcca$z, c=2, ML=const$ML, CL=const$CL, ntrials=5, 
  xi=0.9, distance=1, disp=FALSE)
print(adjustedRandIndex(clus.cecm1$y.pl, y))


###################################################
### code chunk number 50: plot_bananas_ecm1
###################################################
plot(clus.ecm,res.kpcca$z,plot_approx=TRUE)


###################################################
### code chunk number 51: plot_bananas_ecm2
###################################################
plot(clus.ecm,x,ytrue=y,plot_approx=FALSE,plot_protos=FALSE)


