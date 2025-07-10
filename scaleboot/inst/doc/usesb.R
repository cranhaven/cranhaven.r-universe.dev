### R code from vignette source 'usesb.Rnw'

###################################################
### code chunk number 1: usesb.Rnw:56-57 (eval = FALSE)
###################################################
## install.packages("scaleboot")


###################################################
### code chunk number 2: usesb.Rnw:70-71
###################################################
set.seed(100)


###################################################
### code chunk number 3: data1
###################################################
simdata <- function(n,y,sd) {
  m <- length(y)
  x <- matrix(rnorm(m*n,0,sd),m,n)
  t(x + (y - apply(x,1,mean)))
}

X <- simdata(100,c(0,1,1,1,1,1,1,1,1,1),10)
round(X[1:3,],3)
y <- apply(X,2,mean)
round(y,3)


###################################################
### code chunk number 4: mc1
###################################################
mc1 <- function(x) all(x[1] >= x[-1])
mc1(y)


###################################################
### code chunk number 5: boot1
###################################################
countw <- function(x,w,fn) {
  y <- apply(w*x,2,sum)/sum(w)
  fn(y)
}

w <- as.vector(rmultinom(1,100,rep(1,100)))
w
countw(X,w,mc1)


###################################################
### code chunk number 6: load
###################################################
library(scaleboot)


###################################################
### code chunk number 7: sim1
###################################################
sa <- 9^seq(-1,1,length=13)
nb <- 10000

X.sb <- scaleboot(X,nb,sa,countw,mc1)


###################################################
### code chunk number 8: ans1
###################################################
summary(X.sb)  # k = 3 (default)


###################################################
### code chunk number 9: usesb.Rnw:162-163 (eval = FALSE)
###################################################
## summary(X.sb, k=1:3)  # k = 1, 2, 3


###################################################
### code chunk number 10: ans1b
###################################################
summary(X.sb, k=1:3)  # k = 1, 2, 3


###################################################
### code chunk number 11: coef1
###################################################
X.sb



###################################################
### code chunk number 12: diag1f
###################################################
plot(X.sb,legend="topleft")


###################################################
### code chunk number 13: usesb.Rnw:237-238
###################################################
plot(X.sb,legend="topleft")


###################################################
### code chunk number 14: diag1f2
###################################################
plot(X.sb,xval="sigma",log="x",yval="pvalue",legend="topleft")


###################################################
### code chunk number 15: usesb.Rnw:250-251
###################################################
plot(X.sb,xval="sigma",log="x",yval="pvalue",legend="topleft")


###################################################
### code chunk number 16: diag1s
###################################################
plot(summary(X.sb),legend="topleft")


###################################################
### code chunk number 17: usesb.Rnw:312-313
###################################################
plot(summary(X.sb),legend="topleft")


###################################################
### code chunk number 18: usesb.Rnw:341-346 (eval = FALSE)
###################################################
## library(pvclust)
## data(lung)
## sa <- 9^seq(-1,1,length=13)
## nb <- 10000
## lung73.pvclust <- pvclust(lung,r=1/sa,nboot=nb)


###################################################
### code chunk number 19: usesb.Rnw:358-360 (eval = FALSE)
###################################################
## library(scaleboot)
## lung73.sb <- sbfit(lung73.pvclust)


###################################################
### code chunk number 20: lung73
###################################################
library(scaleboot)
data(lung73)


###################################################
### code chunk number 21: usesb.Rnw:376-385 (eval = FALSE)
###################################################
## library(snow)
## cl <- makeCluster(40)
## library(pvclust)
## data(lung)
## sa <- 9^seq(-1,1,length=13)
## nb <- 10000
## lung73.pvclust <- parPvclust(cl,lung,r=1/sa,nboot=nb)
## library(scaleboot)
## lung73.sb <- sbfit(lung73.pvclust,cluster=cl)


###################################################
### code chunk number 22: lungk3
###################################################
lung73.k3 <- sbpvclust(lung73.pvclust,lung73.sb)


###################################################
### code chunk number 23: lungplot
###################################################
library(pvclust)
plot(lung73.k3, cex=0.5, cex.pv=0.7)
pvrect(lung73.k3, pv="si") # find clusters with p>0.95. Now use SI instead of AU.


###################################################
### code chunk number 24: usesb.Rnw:404-405
###################################################
library(pvclust)
plot(lung73.k3, cex=0.5, cex.pv=0.7)
pvrect(lung73.k3, pv="si") # find clusters with p>0.95. Now use SI instead of AU.


###################################################
### code chunk number 25: lungk2
###################################################
lung73.k2 <- sbpvclust(lung73.pvclust,lung73.sb, k=2)


###################################################
### code chunk number 26: lungplot67
###################################################
plot(lung73.sb[[67]],legend="topleft")


###################################################
### code chunk number 27: usesb.Rnw:429-430
###################################################
plot(lung73.sb[[67]],legend="topleft")


###################################################
### code chunk number 28: lungpval67
###################################################
summary(lung73.sb[[67]])


###################################################
### code chunk number 29: lungext67
###################################################
plot(summary(lung73.sb[[67]]),legend="topleft")


###################################################
### code chunk number 30: usesb.Rnw:446-447
###################################################
plot(summary(lung73.sb[[67]]),legend="topleft")


###################################################
### code chunk number 31: usesb.Rnw:454-455 (eval = FALSE)
###################################################
## summary(lung73.sb[c(62,67,69,71)])


###################################################
### code chunk number 32: lungsummarys
###################################################
summary(lung73.sb[c(62,67,69,71)])


###################################################
### code chunk number 33: lungpvals
###################################################
plot(lung73.sb[c(62,67,69,71)])


###################################################
### code chunk number 34: usesb.Rnw:468-469
###################################################
plot(lung73.sb[c(62,67,69,71)])


###################################################
### code chunk number 35: usesb.Rnw:542-546 (eval = FALSE)
###################################################
## library(scaleboot)
## mam15.mt <- read.mt("mam15.mt")
## mam15.trees <- relltest(mam15.mt)
## summary(mam15.trees)


###################################################
### code chunk number 36: usesb.Rnw:591-596 (eval = FALSE)
###################################################
## library(scaleboot)
## mam15.mt <- read.mt("mam15.mt")
## mam15.ass <- read.ass("mam15.ass")
## mam15.relltest <- relltest(mam15.mt,ass=mam15.ass)
## summary(mam15.relltest)


###################################################
### code chunk number 37: lung73
###################################################
library(scaleboot)
data(mam15)


###################################################
### code chunk number 38: usesb.Rnw:611-612 (eval = FALSE)
###################################################
## mam15.trees <- mam15.relltest[1:15]


###################################################
### code chunk number 39: usesb.Rnw:619-625 (eval = FALSE)
###################################################
## library(snow)
## cl <- makeCluster(40)
## library(scaleboot)
## mam15.mt <- read.mt("mam15.mt")
## mam15.ass <- read.ass("mam15.ass")
## mam15.relltest <- relltest(mam15.mt,nb=100000,ass=mam15.ass)


###################################################
### code chunk number 40: prepmam
###################################################
library(scaleboot)
data(mam15)
mam15.trees <- mam15.relltest[1:15]



###################################################
### code chunk number 41: sortmam
###################################################
stat <- attr(mam15.trees,"stat")
o <- order(stat)
mam15.trees <- mam15.trees[o]


###################################################
### code chunk number 42: usesb.Rnw:641-642 (eval = FALSE)
###################################################
## summary(mam15.trees, k=1:3)


###################################################
### code chunk number 43: usesb.Rnw:645-646
###################################################
summary(mam15.trees, k=1:3)


###################################################
### code chunk number 44: usesb.Rnw:662-664 (eval = FALSE)
###################################################
## mam15.edges <- mam15.relltest[16:25]  # 10 edges
## summary(mam15.edges,k=1:3)


###################################################
### code chunk number 45: usesb.Rnw:667-669
###################################################
mam15.edges <- mam15.relltest[16:25]  # 10 edges
summary(mam15.edges,k=1:3)


###################################################
### code chunk number 46: mamplots
###################################################
plot(mam15.trees[1:4])


###################################################
### code chunk number 47: usesb.Rnw:685-686
###################################################
plot(mam15.trees[1:4])


