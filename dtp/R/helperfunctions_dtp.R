#-------------------------------------------------------------------------------
# Function tr
# INPUT: y is a matrix, largeT the number of time per section, t
tr<-function(y,largeT,t){
  lt<-length(y)
  r1<-0
  for(j in 1:lt){
    r1[j]=(((largeT[j]-t[j])/(largeT[j]-t[j]+1))^0.5)*(y[j]-(1/(largeT[j]-t[j]))*(sum(y[(j+1):(j+largeT[j]-t[j])])))
  }
  r1<-t(r1)

  jj=1
  r2<-0
  for(j in 1:lt){
    if (t[j]<largeT[j]){
      r2[jj]=r1[j]
      jj=jj+1
    }
  }
  r<-r2
  r<-as.matrix(r)
  return(r)
}

#-------------------------------------------------------------------------------
# Function ginv
# INPUT: x is a matrix
ginv <- function(x, tol = sqrt(.Machine$double.eps))
{
  ## Generalized Inverse of a Matrix
  dnx <- dimnames(x)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(x)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else x,
    dimnames = dnx[2:1])
}
##************************************************************
##  REGRESS
##
##  Computes a linear regression. Uses generalized inverse if X'X is singular
##
##  Format
##  beta <- regress(y,x)
##
##  Inputs
##  y	      nxm	dependent variable(s)
##  x	      nxk	independent variables (should include constant)
##
##  Output
##  beta    kxm	Regression slope estimates
##
##************************************************************
regress <- function(y,x){
  if (qr(x)$rank==ncol(x)) beta <- qr.solve(x,y)
  if (qr(x)$rank<ncol(x)) beta <- (qr(t(x)%*%x)$qr)%*%(t(x)%*%y)
  beta
}

##************************************************************
##  GMM_LINEAR
##
##  Computes the GMM estimator of a linear model
##
##  Format
##  output <- gmm_linear(y,z,x)
##  beta <- output$beta
##
##  Inputs
##  y	      nx1	dependent variable
##  z	      nxk	rhs variables
##  x	      nxl	instruments variables (should include constant and exogenous parts of z), l>=k
##
##  Outputs
##  beta	kx1	Regression slope estimates
##  se	kx1	standard errors
##  jstat	1x1	J Statistic
##
##************************************************************

gmm_linear <- function(y,z,x){
  pihat <- regress(z,x)
  xz <- t(x)%*%z
  xy <- t(x)%*%y
  beta <- qr.solve((t(pihat)%*%xz),(t(pihat)%*%xy))
  #beta <- ginv((t(pihat)%*%xz))%*%(t(pihat)%*%xy)
  e <- y-z%*%beta
  xe <- as.matrix(x*(e%*%matrix(1,1,ncol(x))))
  g <- ginv(t(xe)%*%xe)
  v <- ginv(t(xz)%*%g%*%xz)
  beta <- v%*%(t(xz)%*%g%*%xy)
  se <- as.matrix(sqrt(diag(v)))
  m <- t(x)%*%(y-z%*%beta)
  jstat <- t(m)%*%g%*%m
  list(beta=beta,e=e,se=se,jstat=jstat)
}
#******************************
#function to use
#******************************
trimr <- function(x,rb,re) {
  x <- cbind(x)
  n <- nrow(x)
  if ((rb+re) >= n) {
    stop('Attempting to trim too much')
  }
  z <- x[(rb+1):(n-re),]
  return(z)
}
#-------------------------------------------------------------------------------
# Function lagm
# INPUT: m is a matrix, nLags the number of lags

lagm <- function(m, nLags) {
  nargin <- length(as.list(match.call())) - 1
  if (nargin != 2) {
    stop('Check function inputs')
  }
  lagM <- c()
  for(i in seq(nLags)) {
    for(j in seq(ncol(m))) {
      tmp <- c(rep(NA, i), trimr(m[,j], 0, i))
      #colnames(tmp)<-colnames(m)

      lagM <- cbind(lagM, tmp)

    }
  }
  if(ncol(m)>=2){
    colnames(lagM)<-paste(colnames(m)[1:2],c(1:ncol(lagM)),sep = "_")
  }
  else{
    colnames(lagM)<-paste(colnames(m),c(1:ncol(lagM)),sep = "_")
  }
  #seq(nLags*ncol(m))
  return(lagM)
}


#-------------------------------------------------------------------------------
# Function ptlag lagging matrix/vector for both balanced/unbalanced panel data
# INPUT: cname the variable name
#        nlags the lags number
#        time the vector of time index

ptlag<-function(cname,z, nLags, time) {
  nm<-which(colnames(z)==cname)
  m<-as.matrix(z[nm])
  index <- match(time, time)
  lagM <- c()
  for(i in seq(nLags)) {
    for(j in seq(ncol(m))) {

      tmp <- c(rep(NA, i), trimr(m[,j], 0, i))
      #colnames(tmp)<-colnames(m)

      lagM <- cbind(lagM, tmp[index])

    }
  }
  if(ncol(m)>=2){
    colnames(lagM)<-paste(colnames(m)[1:ncol(m)],rep(seq(nLags), each=ncol(m)),sep = "_")
  }
  else{
    colnames(lagM)<-paste(colnames(m),c(1:ncol(lagM)),sep = "_")
  }
  #seq(nLags*ncol(m))
  return(as.matrix(lagM))
}


#-------------------------------------------------------------------------------
# Function newframe create a new dataframe
# INPUT: data is a dataframe
#        index: panel data indexes (id,time)

newframe<-function(data,index=c("id","time")){
  ni<-nrow(unique(data[index[1]]))
  newdata<-na.omit(data)
  newdata$id<-NA
  for (i in 1:ni){
    b<-as.matrix(newdata[index[1]])
    a<-unique(b)
    a<-as.vector(a)
    newdata$id[b==a[i]]<-i
  }
  tt<-count(newdata,c('id'))
  i<-seq(length(tt$id))
  newdata$TT<-rep(tt$freq[i],tt$freq[i])
  newdata<-transform(newdata , t = ave(newdata$id,newdata$id,FUN=seq))
  return(newdata)
}

#-------------------------------------------------------------------------------
# Function makeinstru create instruments
# INPUT: ff is a data frame
#        x the variable from which we create the instruments
#        maxlags the number of lags
#        initinum the column number of the initial variable
makeinstru<-function(ff,x,maxlags,initnum){
  name<-x
  t1<-na.omit(ptlag("t",ff,1,ff$t))
  inst1<-na.omit(ptlag(name,ff,1,ff$t))
  colnames(inst1)<-name
  ff1<-as.data.frame(cbind(inst1,t1))
  inst<-ptlag(name,ff1,maxlags,ff1$t_1)
  inst<-na.replace(inst,0)
  inst2<-ptlag(name,ff,maxlags,ff$t)
  inst2<-na.replace(inst2,0)
  ff<-cbind(ff,inst2)
  nini<-paste(name,initnum,sep = "_")
  init<-ff[nini]
  list(inst=inst,inst2=inst2,ff=ff,initial=init)
}


