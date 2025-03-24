## ------------------------------------------------------------------------
library(energy)
library(kpcalg)
set.seed(10)
#independence
x <- runif(300)
y <- runif(300)

hsic.gamma(x,y)$p.value
hsic.perm(x,y)$p.value
dcov.gamma(x,y)$p.value
dcov.test(x,y,R=100)$p.value

#uncorelated but not dependent
z <- 10*(runif(300)-0.5)
w <- z^2 + 10*runif(300)

cor(z,w)
hsic.gamma(z,w)$p.value
hsic.perm(z,w)$p.value
dcov.gamma(z,w)$p.value
dcov.test(z,w,R=100)$p.value

## ----message=F, echo =F, fig.height=3, fig.align='center'----------------
library(graph)
adj.mat <- matrix(c(0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0),nrow=4,ncol=4)
colnames(adj.mat) <- rownames(adj.mat) <- c("x","y","z","w")
plot(as(adj.mat,"graphNEL"))

## ----message=F-----------------------------------------------------------
set.seed(10)
z <- 10*runif(300)
w <- 10*runif(300)
x <- sin(z) + runif(300)
y <- cos(z) + runif(300)
data <- cbind(x,y,z,w)

## ----message=F-----------------------------------------------------------
library(pcalg)
library(kpcalg)
#conditionally independent
test1a <- kernelCItest(x=1,y=2,S=c(3),suffStat = list(data=data,ic.method="dcc.gamma"))
test2a <- kernelCItest(x=1,y=2,S=c(3),suffStat = list(data=data,ic.method="dcc.perm"))
test3a <- kernelCItest(x=1,y=2,S=c(3),suffStat = list(data=data,ic.method="hsic.gamma"))
test4a <- kernelCItest(x=1,y=2,S=c(3),suffStat = list(data=data,ic.method="hsic.perm"))
test5a <- kernelCItest(x=1,y=2,S=c(3),suffStat = list(data=data,ic.method="hsic.clust"))
test6a <- gaussCItest( x=1,y=2,S=c(3),suffStat = list(C=cor(data),n=4))

cat("DCC (gamma test): \t\t\t",test1a,
    "\nDCC (permutation test): \t\t",test2a,
    "\nHSIC-residuals (gamma test): \t\t",test3a,
    "\nHSIC-residuals (permutation test): \t",test4a,
    "\nHSIC-cluster: \t\t\t\t",test5a,
    "\nFisher's Z test: \t\t\t",test6a)

## ----message=F-----------------------------------------------------------
#dependent
test1b <- kernelCItest(x=1,y=2,S=c(4),suffStat = list(data=data,ic.method="dcc.gamma"))
test2b <- kernelCItest(x=1,y=2,S=c(4),suffStat = list(data=data,ic.method="dcc.perm"))
test3b <- kernelCItest(x=1,y=2,S=c(4),suffStat = list(data=data,ic.method="hsic.gamma"))
test4b <- kernelCItest(x=1,y=2,S=c(4),suffStat = list(data=data,ic.method="hsic.perm"))
test5b <- kernelCItest(x=1,y=2,S=c(4),suffStat = list(data=data,ic.method="hsic.clust"))
test6b <- gaussCItest( x=1,y=2,S=c(4),suffStat = list(C=cor(data),n=4))

cat("DCC (gamma test): \t\t\t",test1b,
    "\nDCC (permutation test): \t\t",test2b,
    "\nHSIC-residuals (gamma test): \t\t",test3b,
    "\nHSIC-residuals (permutation test): \t",test4b,
    "\nHSIC-cluster: \t\t\t\t",test5b,
    "\nFisher's Z test: \t\t\t",test6b)

## ----message=F, echo =F, fig.height=5, fig.align='center'----------------
library(graph)
true <- matrix(0,9,9)
true[c(1),c(2,3)]<-true[c(3,4),5]<-true[c(6),c(7)]<-true[c(7),c(8)]<-true[7,9]<-1
colnames(true) <- rownames(true) <- paste("x",c(1:9),sep="")
plot(as(true,"graphNEL"))

## ----message=F-----------------------------------------------------------
set.seed(4)
n <- 300
data <- NULL
x1 <- 2*(runif(n)-0.5)
x2 <- x1 + runif(n)-0.5
x3 <- x1^2 + 0.6*runif(n)
x4 <- rnorm(n)
x5 <- x3 + x4^2 + 2*runif(n)
x6 <- 10*(runif(n)-0.5)
x7 <- x6^2 + 5*runif(n)
x8 <- 2*x7^2 + 1.5*rnorm(n)
x9 <- x7 + 4*runif(n)
data <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)

## ----message=F-----------------------------------------------------------
library(pcalg)
library(kpcalg)
pc <- pc(suffStat = list(C = cor(data), n = 9),
           indepTest = gaussCItest,
           alpha = 0.9,
           labels = colnames(data),
           u2pd = "relaxed",
           skel.method = "stable",
           verbose = F)
kpc1 <- kpc(suffStat = list(data=data, ic.method="dcc.perm"),
           indepTest = kernelCItest,
           alpha = 0.1,
           labels = colnames(data),
           u2pd = "relaxed",
           skel.method = "stable",
           verbose = F)
kpc2 <- kpc(suffStat = list(data=data, ic.method="hsic.gamma"),
           indepTest = kernelCItest,
           alpha = 0.1,
           labels = colnames(data),
           u2pd = "relaxed",
           skel.method = "stable",
           verbose = F)
kpc3 <- kpc(suffStat = list(data=data, ic.method="hsic.perm"),
           indepTest = kernelCItest,
           alpha = 0.1,
           labels = colnames(data),
           u2pd = "relaxed",
           skel.method = "stable",
           verbose = F)
kpc4 <- kpc(suffStat = list(data=data, ic.method="hsic.clust"),
           indepTest = kernelCItest,
           alpha = 0.1,
           labels = colnames(data),
           u2pd = "relaxed",
           skel.method = "stable",
           verbose = F)

## ----fig.height=9--------------------------------------------------------
par(mfrow=c(3,2))
plot(pc@graph,attrs=list(node=list(fontsize=5)),main="pc")
plot(kpc1@graph,attrs=list(node=list(fontsize=5)),main="dpc-perm")
plot(kpc2@graph,attrs=list(node=list(fontsize=5)),main="kpc-resid-gamma")
plot(kpc3@graph,attrs=list(node=list(fontsize=5)),main="kpc-resid-perm")
plot(kpc4@graph,attrs=list(node=list(fontsize=5)),main="kpc-clust")
plot(as(true,"graphNEL"),attrs=list(node=list(fontsize=5)),main="True DAG")

