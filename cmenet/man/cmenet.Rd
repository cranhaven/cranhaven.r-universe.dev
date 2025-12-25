\name{cmenet}
\alias{cmenet}
\docType{package}
\title{
  Bi-level selection of conditional main effects (fixed parameters)
}
\description{
  \code{cmenet} performs variable selection of conditional main effects (CMEs) via a bi-level penalization framework, given fixed penalty parameters.
}
\usage{
cmenet(xme, xcme, y,
          lambda.sib=exp(seq(from=log(max.lambda),to=log(max.lambda*1e-6),length=20)),
          lambda.cou=exp(seq(from=log(max.lambda),to=log(max.lambda*1e-6),length=20)),
          max.lambda=lambda0.cme(cbind(xme,xcme),y),
          gamma=1/(0.5-tau)+0.001, tau=0.01,
          act.vec=rep(1,ncol(xme)+ncol(xcme)),
          beta0=rep(0,ncol(xme)+ncol(xcme)),
          it.max=250, lambda.flg=T)
}
\arguments{
  \item{xme}{An \eqn{n} x \eqn{p} binary model matrix for MEs.}
  \item{xcme}{An \eqn{n} x (4*choose(\eqn{p},2)) model matrix for CMEs.}
  \item{y}{An \eqn{n}-length response vector.}
  \item{lambda.sib}{Penalty vector for sibling CMEs.}
  \item{lambda.cou}{Penalty vector for cousin CMEs.}
  \item{max.lambda}{Maximum penalty value.}
  \item{gamma}{Bridge parameter in MC+ penalty.}
  \item{tau}{Coupling parameter for CMEs.}
  \item{act.vec}{A (\eqn{p}+4*choose(\eqn{p},2))-length binary vector for setting which variables are always active in optimization.}
  \item{beta0}{Initial regression coefficients.}
  \item{it.max}{Number of optimization iterations.}
  \item{lambda.flg}{Use the default option TRUE (unless within \code{cv.cmenet}).}
}
\value{
  \item{coefficients}{Array of regression coefficients (over different \code{lambda.sib} and \code{lambda.cou}).}
  \item{residuals}{Array of regression residuals (over different \code{lambda.sib} and \code{lambda.cou}).}
  \item{inter}{Matrix of intercept estimates (over different \code{lambda.sib} and \code{lambda.cou}).}
}
\references{
Mak and Wu (2018). cmenet: a new method for bi-level variable selection of conditional main effects. \emph{Journal of the American Statistical Association}, to appear.
}
\examples{
\dontrun{
    library(MASS)
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

## Run cmenet
cv.cme <- cv.cmenet(xme, xcme, y, var.names=colnames(model.mtx))
}
}
