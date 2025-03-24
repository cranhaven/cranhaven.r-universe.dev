#' Estimating the sizes of populations who inject drugs
#' from multiple data sources using a Bayesian hierarchical model.
#'
#' This R package is for reproducing Bao L, Raftery A, Reddy A. (2015) Estimating the Sizes of Populations At Risk of HIV Infection From Multiple Data Sources Using a Bayesian Hierarchical Model, Statistics and Its Interface. This function develops an algorithm for presenting a Bayesian hierarchical
#' model for estimating the sizes of drug injected
#' populations in Bangladesh. The model incorporates multiple commonly used data sources
#' including mapping data, surveys, interventions, capture-recapture data,
#' estimates or guesstimates from organizations, and expert opinion. This function provides the posterior samples of burnin thin at-risk population sizes at the subnational level.
#'
#' @param DATA dataset from Bangladesh which used in Bao L, Raftery A, Reddy A. (2015) Estimating the Sizez of Populations At Risk of HIV Infection From Multiple Data Sources Using a Bayesian Hierarchical Model, Statistics and Its Interface.
#' @param size the number of iteration in MCMC algorithm.
#' @param burnin the number of Burn-In in MCMC algorithm.
#' @param thin keep every thin-th scan.
#' @return A matrix of posterior samples of at-risk population sizes at the sub-national level, where the rows correspond to sub-national areas and the columns correspond to MCMC iterations. 
#' @author Le Bao, Adrian E. Raftery, Kyongwon Kim
#' @details
#' This function runs MCMC algorithm for reproducing Bao L, Raftery A, Reddy A. (2015) Estimating the Sizez of Populations At Risk of HIV Infection From Multiple Data Sources Using a Bayesian Hierarchical Model, Statistics and Its Interface.
#' @references Bao L, Raftery A, Reddy A. (2015) Estimating the Sizes of Populations At Risk of HIV Infection From Multiple Data Sources Using a Bayesian Hierarchical Model, Statistics and Its Interface.
#' @seealso \code{\link{rtnorm} \link{rinvgamma} \link{dtnorm}}
#' @export
#' @importFrom msm rtnorm dtnorm
#' @importFrom MCMCpack rinvgamma
#' @importFrom stats rbeta rbinom dbinom dbeta rnbinom rpois dpois dnbinom dnorm runif rnorm cor quantile
#' @importFrom graphics par hist rect text
#' @examples #n.total=sizeestima(DATA,500000,501,100)



sizeestima <- function( DATA,size, burnin,thin) {

  data = DATA[which(DATA[,4]>-1 | DATA[,8]>-1),]
  N = data[,3]
  N.sum = sum(N)
  x = data[,5:7]
  r = data[,5]+data[,6]-data[,7]
  y = data[,4]
  z = data[,8]
  index1 = setdiff(which(data[,5]>-1), which(r>-1))	# BSS but not capture-recapture
  index2 = setdiff(which(data[,6]>-1), which(r>-1))	# NEP but not capture-recapture
  index3 = which(r>-1)						# capture recapture
  index4 = which(z>-1)						# RSA
  n.min = apply(cbind(y,x[,1:2],r,rep(-1,length(y))), 1, max, na.rm = T)+1
  y.sum = n.sum = rep(0,3)
  for (i in 1:3)	y.sum[i] = sum(data[which(data[,3+i]>0),i+3])
  option.mu = 1                   # allow heterogeneous mu?
  option.ht = 1                   # allow heterogeneous mu?
  rho = 1                         # pij = pi x pj x rho, so rho=1 means independence

  ## 		MCMC


  n.total.q = NULL
  #size = 500 * 1000
  a = b = matrix(NA, size, 4)
  n = phi = matrix(NA, size, length(N))
  p1 = p2 = theta = matrix(NA, size, length(N))
  mu = sig2 = rep(NA, size)
  zl = length(index4)
  tau02 = sig02 = (log(10)/2)^2



  # this contains the functions called by MCMC.R

  ##		Initial
  if (rho==1){
    a[1,] = 2
    b[1,1] = 2000
    b[1,2:4] = 2
    phi[1,] = rbeta(length(N), a[1,1], b[1,1])
    n[1,] = rbinom(length(N), N, phi[1,])
    index = which(n[1,]<n.min)
    n[1,index] = rnbinom(length(index), n.min[index], 0.9) + n.min[index]
    n.index = list(which(data[,4]>-1), which(data[,5]>-1), which(data[,6]>-1))
    s2 = sum((log(z[index4]/n[1,index4])-mean(log(z[index4]/n[1,index4])))^2)/(length(index4)-1)
    mu[1] = mean(log(z[index4]/n[1,index4]))
    sig2[1] = s2
    l1 = length(which(data[,4]>-1))
    l2 = length(which(data[,5]>-1))
    l3 = length(which(data[,6]>-1))
    l4 = length(N)
    if (option.ht==0){
      p1 = p2 = theta = rep(NA, size)
      S = rep(NA,3)
    }
  }

  if (rho!=1){
    a[1,] = apply(a[keep,], 2, mean)
    b[1,] = apply(b[keep,], 2, mean)
    phi[1,] = apply(phi[keep,], 2, mean)
    n[1,] = round(apply(n[keep,], 2, mean))
    s2 = sum((log(z[index4]/n[1,index4])-mean(log(z[index4]/n[1,index4])))^2)/(length(index4)-1)
    mu[1] = mean(log(z[index4]/n[1,index4]))
    sig2[1] = s2
  }

  reject = rep(0,8+length(N))
  check = 0
  set.seed(1000)
  ptm <- proc.time()
  for (t in 2:size){
    check = check+1
    # update p1, p2, theta, phi
    if (option.ht==1){
      p1[t-1,n.index[[2]]] = rbeta(l2, a[t-1,2]+x[n.index[[2]],1], b[t-1,2]+n[t-1,n.index[[2]]]-x[n.index[[2]],1])
      p2[t-1,n.index[[3]]] = rbeta(l3, a[t-1,3]+x[n.index[[3]],2], b[t-1,3]+n[t-1,n.index[[3]]]-x[n.index[[3]],2])
      theta[t-1,n.index[[1]]] = rbeta(l1, a[t-1,4]+y[n.index[[1]]], b[t-1,4]+n[t-1,n.index[[1]]]-y[n.index[[1]]])
    }
    if (option.ht==0){
      for (i in 1:3)	S[i] = sum(n[t-1,n.index[[i]]])
      theta[t-1] = rbeta(1, 1+y.sum[1], 1+S[1]-y.sum[1])
      p1[t-1] = rbeta(1, 1+y.sum[2], 1+S[2]-y.sum[2])
      p2[t-1] = rbeta(1, 1+y.sum[3], 1+S[3]-y.sum[3])
    }
    phi[t-1,] = rbeta(l4, a[t-1,1]+n[t-1,], b[t-1,1]+N-n[t-1,])

    # update n by H-M
    for (j in 1:length(N)){
      n[t,j] = rpois(1, n[t-1,j])
      if (n[t,j]<n.min[j])	n[t,j]=n[t-1,j]
      if (n[t,j]>=n.min[j]){
        if (option.ht==1){
          diff = dbinom(n[t,j], N[j], phi[t-1,j], log=T) - dpois(n[t,j], n[t-1,j], log=T) -
            dbinom(n[t-1,j], N[j], phi[t-1,j], log=T) + dpois(n[t-1,j], n[t,j], log=T)
          if (is.element(j, n.index[[1]]))
            diff = diff + dbinom(y[j], n[t,j], theta[t-1,j], log=T) - dbinom(y[j], n[t-1,j], theta[t-1,j], log=T)
          if (is.element(j, n.index[[2]]))
            diff = diff + dbinom(x[j,1], n[t,j], p1[t-1,j], log=T) - dbinom(x[j,1], n[t-1,j], p1[t-1,j], log=T)
          if (is.element(j, n.index[[3]]))
            diff = diff + dbinom(x[j,2], n[t,j], p2[t-1,j], log=T) - dbinom(x[j,2], n[t-1,j], p2[t-1,j], log=T)
          if (is.element(j, index3))
            #	diff = diff + dnbinom(n[t,j]-r[j], r[j]-1, 1-(1-p1[t-1,j])*(1-p2[t-1,j]), log=T) -
            #		 dnbinom(n[t-1,j]-r[j], r[j]-1, 1-(1-p1[t-1,j])*(1-p2[t-1,j]), log=T)
            diff = diff + dnbinom(n[t,j]-r[j], r[j]-1, p1[t-1,j]+p2[t-1,j]-p1[t-1,j]*p2[t-1,j]*rho, log=T) -
              dnbinom(n[t-1,j]-r[j], r[j]-1, p1[t-1,j]+p2[t-1,j]-p1[t-1,j]*p2[t-1,j]*rho, log=T)

          if (is.element(j, index4) & option.mu==1)
            diff = diff + dnorm(log(z[j]/n[t,j]), mu[t-1], sqrt(sig2[t-1]), log=T) -
              dnorm(log(z[j]/n[t-1,j]), mu[t-1], sqrt(sig2[t-1]), log=T)
        }
        if (option.ht==0){
          diff = dbinom(n[t,j], N[j], phi[t-1,j], log=T) - dpois(n[t,j], n[t-1,j], log=T) -
            dbinom(n[t-1,j], N[j], phi[t-1,j], log=T) + dpois(n[t-1,j], n[t,j], log=T)
          if (is.element(j, n.index[[1]]))
            diff = diff + dbinom(y[j], n[t,j], theta[t-1], log=T) - dbinom(y[j], n[t-1,j], theta[t-1], log=T)
          if (is.element(j, n.index[[2]]))
            diff = diff + dbinom(x[j,1], n[t,j], p1[t-1], log=T) - dbinom(x[j,1], n[t-1,j], p1[t-1], log=T)
          if (is.element(j, n.index[[3]]))
            diff = diff + dbinom(x[j,2], n[t,j], p2[t-1], log=T) - dbinom(x[j,2], n[t-1,j], p2[t-1], log=T)
          if (is.element(j, index3))
            diff = diff + dnbinom(n[t,j]-r[j], r[j]-1, 1-(1-p1[t-1])*(1-p2[t-1]), log=T) -
              dnbinom(n[t-1,j]-r[j], r[j]-1, 1-(1-p1[t-1])*(1-p2[t-1]), log=T)
          if (is.element(j, index4) & option.mu==1)
            diff = diff + dnorm(log(z[j]/n[t,j]), mu[t-1], sqrt(sig2[t-1]), log=T) -
              dnorm(log(z[j]/n[t-1,j]), mu[t-1], sqrt(sig2[t-1]), log=T)
        }
        if (diff<log(runif(1,0,1)))	n[t,j] = n[t-1,j]
      }
    }
    reject[which(n[t,]==n[t-1,])] = reject[which(n[t,]==n[t-1,])]+1

    # update (a,b) by H-M
    for (m in 1:(1+3*option.ht)){
      lower = log(1/(a[t-1,m]+b[t-1,m]))
      upper = log(1-1/(a[t-1,m]+b[t-1,m]))
      if (m==1) sd = 0.25
      if (m==2) sd = 1
      if (m==3) sd = 0.1
      if (m==4) sd = 0.5
      ratio.ab = exp( rtnorm(1, log(a[t-1,m]/(a[t-1,m]+b[t-1,m])), sd, lower, upper) )
      a[t,m] = (a[t-1,m]+b[t-1,m])*ratio.ab
      b[t,m] = a[t-1,m]+b[t-1,m]-a[t,m]
      if (m==1)
        diff = sum(dbeta(phi[t-1,], a[t,m], b[t,m], log=T))-sum(dbeta(phi[t-1,], a[t-1,m], b[t-1,m], log=T))
      if (m==2)
        diff = sum(dbeta(p1[t-1,n.index[[2]]], a[t,m], b[t,m], log=T))-sum(dbeta(p1[t-1,n.index[[2]]], a[t-1,m], b[t-1,m], log=T))
      if (m==3)
        diff = sum(dbeta(p2[t-1,n.index[[3]]], a[t,m], b[t,m], log=T))-sum(dbeta(p2[t-1,n.index[[3]]], a[t-1,m], b[t-1,m], log=T))
      if (m==4)
        diff = sum(dbeta(theta[t-1,n.index[[1]]], a[t,m], b[t,m], log=T))-sum(dbeta(theta[t-1,n.index[[1]]], a[t-1,m], b[t-1,m], log=T))
      diff = diff + log(a[t,m]) - dtnorm(log(ratio.ab), log(a[t-1,m]/(a[t-1,m]+b[t-1,m])), sd, lower, upper, log=T) -
        log(a[t-1,m]) + dtnorm(log(a[t-1,m]/(a[t-1,m]+b[t-1,m])), log(ratio.ab), sd, lower, upper, log=T)
      if (diff>log(runif(1,0,1))){
        a[t-1,m] = a[t,m]
        b[t-1,m] = b[t,m]
      }
      if (a[t,m]!=a[t-1,m])	reject[length(N)+m] = reject[length(N)+m]+1

      lower = log(max((a[t-1,m]+b[t-1,m])/a[t-1,m], (a[t-1,m]+b[t-1,m])/b[t-1,m]))
      upper = log(10^6)
      if (m==1) sd = 0.5
      if (m==2) sd = 1
      if (m==3) sd = 0.5
      if (m==4) sd = 0.5
      sum.ab = exp( rtnorm(1, log(a[t-1,m]+b[t-1,m]), sd, lower, upper) )
      a[t,m] = a[t-1,m]/(a[t-1,m]+b[t-1,m])*sum.ab
      b[t,m] = b[t-1,m]/(a[t-1,m]+b[t-1,m])*sum.ab
      if (m==1)
        diff = sum(dbeta(phi[t-1,], a[t,m], b[t,m], log=T))-sum(dbeta(phi[t-1,], a[t-1,m], b[t-1,m], log=T))
      if (m==2)
        diff = sum(dbeta(p1[t-1,n.index[[2]]], a[t,m], b[t,m], log=T))-sum(dbeta(p1[t-1,n.index[[2]]], a[t-1,m], b[t-1,m], log=T))
      if (m==3)
        diff = sum(dbeta(p2[t-1,n.index[[3]]], a[t,m], b[t,m], log=T))-sum(dbeta(p2[t-1,n.index[[3]]], a[t-1,m], b[t-1,m], log=T))
      if (m==4)
        diff = sum(dbeta(theta[t-1,n.index[[1]]], a[t,m], b[t,m], log=T))-sum(dbeta(theta[t-1,n.index[[1]]], a[t-1,m], b[t-1,m], log=T))
      diff = diff - dtnorm(log(sum.ab), log(a[t-1,m]+b[t-1,m]), sd, lower, upper, log=T) +
        dtnorm(log(a[t-1,m]+b[t-1,m]), log(sum.ab), sd, lower, upper, log=T)
      if (diff<log(runif(1,0,1))){
        a[t,m] = a[t-1,m]
        b[t,m] = b[t-1,m]
        reject[length(N)+4+m] = reject[length(N)+4+m]+1
      }
    }
    # update (mu,sig2) by Gibbs
    if (option.mu==1){
      tau.l = 1/(1/tau02+zl/sig2[t-1])
      mu.l = sum(log(z[index4]/n[t,index4]))/sig2[t-1]*tau.l
      mu[t] = rnorm(1, mu.l, tau.l)
      s2 = sum((log(z[index4]/n[t,index4])-mu[t])^2)
      sig2[t] = rinvgamma(1, (zl+1)/2, (sig02+s2)/2)
    }
    if ((100*t)%%size==0)	print(paste(100*t/size, "percents has been done"))
  }
  print((proc.time() - ptm)/60)


  update.n <- function(option.ht, option.mu){
    if (option.ht==1){
      diff = dbinom(n[t,j], N[j], phi[t-1,j], log=T) - dpois(n[t,j], n[t-1,j], log=T) -
        dbinom(n[t-1,j], N[j], phi[t-1,j], log=T) + dpois(n[t-1,j], n[t,j], log=T)
      if (is.element(j, n.index[[1]]))
        diff = diff + dbinom(y[j], n[t,j], theta[t-1,j], log=T) - dbinom(y[j], n[t-1,j], theta[t-1,j], log=T)
      if (is.element(j, n.index[[2]]))
        diff = diff + dbinom(x[j,1], n[t,j], p1[t-1,j], log=T) - dbinom(x[j,1], n[t-1,j], p1[t-1,j], log=T)
      if (is.element(j, n.index[[3]]))
        diff = diff + dbinom(x[j,2], n[t,j], p2[t-1,j], log=T) - dbinom(x[j,2], n[t-1,j], p2[t-1,j], log=T)
      if (is.element(j, index3))
        diff = diff + dnbinom(n[t,j]-r[j], r[j]-1, 1-(1-p1[t-1,j])*(1-p2[t-1,j]), log=T) -
          dnbinom(n[t-1,j]-r[j], r[j]-1, 1-(1-p1[t-1,j])*(1-p2[t-1,j]), log=T)
      if (is.element(j, index4) & option.mu==1)
        diff = diff + dnorm(log(z[j]/n[t,j]), mu[t-1], sqrt(sig2[t-1]), log=T) -
          dnorm(log(z[j]/n[t-1,j]), mu[t-1], sqrt(sig2[t-1]), log=T)
    }
    if (option.ht==0){
      diff = dbinom(n[t,j], N[j], phi[t-1,j], log=T) - dpois(n[t,j], n[t-1,j], log=T) -
        dbinom(n[t-1,j], N[j], phi[t-1,j], log=T) + dpois(n[t-1,j], n[t,j], log=T)
      if (is.element(j, n.index[[1]]))
        diff = diff + dbinom(y[j], n[t,j], theta[t-1], log=T) - dbinom(y[j], n[t-1,j], theta[t-1], log=T)
      if (is.element(j, n.index[[2]]))
        diff = diff + dbinom(x[j,1], n[t,j], p1[t-1], log=T) - dbinom(x[j,1], n[t-1,j], p1[t-1], log=T)
      if (is.element(j, n.index[[3]]))
        diff = diff + dbinom(x[j,2], n[t,j], p2[t-1], log=T) - dbinom(x[j,2], n[t-1,j], p2[t-1], log=T)
      if (is.element(j, index3))
        diff = diff + dnbinom(n[t,j]-r[j], r[j]-1, 1-(1-p1[t-1])*(1-p2[t-1]), log=T) -
          dnbinom(n[t-1,j]-r[j], r[j]-1, 1-(1-p1[t-1])*(1-p2[t-1]), log=T)
      if (is.element(j, index4) & option.mu==1)
        diff = diff + dnorm(log(z[j]/n[t,j]), mu[t-1], sqrt(sig2[t-1]), log=T) -
          dnorm(log(z[j]/n[t-1,j]), mu[t-1], sqrt(sig2[t-1]), log=T)
    }
    return(diff)
  }


  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE, breaks=20)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  }
  panel.pearson <- function(x, y, ...) {
    horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
    vertical <- (par("usr")[3] + par("usr")[4]) / 2;
    text(horizontal, vertical, format(cor(x,y), digits=2), cex=3)
  }

  keep = seq(burnin, size, thin)
  N.r = DATA$PopSize[-which(DATA$NASROB>-1 | DATA$RSA>-1)]
  phi.r = n.r = NULL
  for (i in 1:length(N.r)){
    phi.i = rbeta(length(keep), a[keep,1],b[keep,1])
    n.i = rbinom(length(keep), N.r[i], phi.i)
    phi.r = cbind(phi.r, phi.i)
    n.r = cbind(n.r, n.i)
  }
 
  return(cbind(n[keep,], n.r))

}

