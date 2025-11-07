## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  install.package("o2plsda")

## -----------------------------------------------------------------------------
library(o2plsda)
set.seed(123)
# sample * values
X = matrix(rnorm(5000),50,100)
# sample * values
Y = matrix(rnorm(5000),50,100)
##add sample names
rownames(X) <- paste("S",1:50,sep="")
rownames(Y) <- paste("S",1:50,sep="")
## gene names
colnames(X) <- paste("Gene",1:100,sep="")
colnames(Y) <- paste("Lipid",1:100,sep="")
##scaled
X = scale(X, scale = TRUE)
Y = scale(Y, scale = TRUE)
## group factor could be omitted if you don't have any group 
group <- rep(c("Ctrl","Treat"), each = 25)

## -----------------------------------------------------------------------------
set.seed(123)
## nr_folds : cross validation k-fold (suggest 10)
## ncores : parallel paramaters for large datasets
cv <- o2cv(X,Y,1:5,1:3,1:3, group = group, nr_folds = 10)


## -----------------------------------------------------------------------------
fit <- o2pls(X,Y,1,2,1)
summary(fit)


############################################

## -----------------------------------------------------------------------------
Xl <- loadings(fit,loading="Xjoint")
Xs <- scores(fit,score="Xjoint")
plot(fit,type="score",var="Xjoint", group=group)
plot(fit,type="loading",var="Xjoint", group=group,repel=F,rotation=TRUE)

## -----------------------------------------------------------------------------
res <- oplsda(fit,group, nc=1)
plot(res,type="score", group=group,repel=TRUE)
vip <- vip(res)
plot(res,type="vip", group = group, repel = FALSE,order=TRUE)

