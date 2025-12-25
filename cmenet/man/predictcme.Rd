\name{predictcme}
\alias{predictcme}
\docType{package}
\title{
  Predict using a fitted \code{cmenet} object
}
\description{
  \code{predictcme} performs prediction at new ME settings \code{newx}, given fitted \code{cmenet} object.
}
\usage{
  predictcme(fit.cme,newx)
}
\arguments{
  \item{fit.cme}{Fitted object from \code{cmenet}.}
  \item{newx}{An \eqn{m} x \eqn{p} binary matrix for prediction (\eqn{m} new ME settings, \eqn{p} binary MEs).}
}
\examples{
  \dontrun{

library(MASS)
library(cmenet)
n <- 50 #number of observations
p <- 50 #number of main effects

## Simulate model matrix for MEs and CMEs
set.seed(1)
rho <- 0 #correlation
ones <- matrix(1,p,p)
covmtx <- rho*ones+(1-rho)*diag(p)
latmtx <- mvrnorm(n,p,mu=rep(0,p),Sigma=covmtx) #equicorrelated cov. matrix
memtx <- (latmtx>=0)-(latmtx<0) #simulate model matrix for MEs
model.mtx <- full.model.mtx(memtx)$model.mtx #generate model matrix for MEs and CMEs

## Set true model and generate response
num.act <- 2 # two siblings active
num.grp <- 4 # ... within four active groups
ind <- c()
for (ii in 1:num.grp){
  eff <- sample(seq(2*(p-1)),num.act)
  ind <- c(ind, p + eff + (ii-1)*(2*(p-1)))
}
colnames(model.mtx)[ind] # active CMEs

des.mtx <- model.mtx[,ind]
inter <- 12 #intercept
xbtrue <- inter + rowSums(des.mtx)
y <- xbtrue + rnorm(n,sd=1) #response
xme <- model.mtx[,1:p]
xcme <- model.mtx[,(p+1):ncol(model.mtx)]

## Run cv.cmenet
cv.cme <- cv.cmenet(xme, xcme, y, var.names=colnames(model.mtx))
fit.cme <- cv.cme$cme.fit
sel.cme <- cv.cme$select.idx
colnames(model.mtx)[ind] #true model
colnames(model.mtx)[sel.cme] #selected effects from cmenet
colnames(model.mtx)[setdiff(sel.cme,ind)] #selected effects not in true model
colnames(model.mtx)[setdiff(ind,sel.cme)] #true effects not in selected model

## Prediction
set.seed(1000)
ntst <- 20
latmtx <- mvrnorm(ntst,p,mu=rep(0,p),Sigma=covmtx)
memtx <- (latmtx>=0)-(latmtx<0)
tst.mtx <- full.model.mtx(memtx)$model.mtx
xbtst <- inter + rowSums(tst.mtx[,ind])
ytst <- xbtst + rnorm(ntst,sd=1)
pred.cme <- predictcme(fit.cme,newx=tst.mtx)[,which(cv.cme$lambda.sib==cv.cme$params[1]),
            which(cv.cme$lambda.cou==cv.cme$params[2])]

  }
}
