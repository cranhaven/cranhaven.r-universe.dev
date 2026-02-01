
#----------------------------Random NSBM--------------------------------------------------------

#' return a random NSBM
#' @name rnsbm
#' @export
#' @param p (interger) number of node in the network
#' @param theta =(pi;w;nu0;nu) parameter of the model
#' @param modelFamily the distribution family of the noise under the null hypothesis,
#' which can be "Gauss" (Gaussian), "Gamma", or "Poisson", by default it's 'Gauss'
#'
#' @return X the noisy matrix
#' @return theta
#' @return latentZ the latent clustering
#' @return latentA the latent adjacency matrix
rnsbm <-  function(p, theta, modelFamily='Gauss'){
  N <-  p*(p-1)/2
  P=length(theta$pi)
  w=vecToMatrix(theta$w,P)
  if(modelFamily=='Gauss'|| modelFamily=='Gamma'){
    nu1=vecToMatrix(theta$nu[,1],P)
    nu2=vecToMatrix(theta$nu[,2],P)
  }else{
    nu=vecToMatrix(theta$nu,P)
  }
  Q <- length(theta$pi)
  
  #' latent variables
  #' we strat by sampling the latent variable Z which is the vector containing the family of each nodes
  Z <- sample(1:Q, p, replace=TRUE, prob=theta$pi)
  
  #' adjacency matrix
  #' then we sample the adjacency matrix, conditionally to Z the coordinate of A follow a binomial
  #' of a parameter contain in theta$w
  A <- matrix(0, p, p)
  for (i in 1:(p-1)){
    A[i,(i+1):p] <- stats::rbinom(p-i, 1,w[Z[i],Z[(i+1):p]])
  }
  
  #' noisy observations under the null
  #' we create a matrix (n,n) X and we initialize all its entry (half of them is undirected)
  #' with a sampling of the law under the null
  X <- matrix(0, p, p)
  nonZero <- upper.tri(diag(p))
  if (modelFamily=='Gauss'){
    X[nonZero] <- stats::rnorm(N, theta$nu0[1], theta$nu0[2])
  }
  if (modelFamily=='Gamma'){
    X[nonZero] <- stats::rgamma(N, theta$nu0[1], theta$nu0[2])
  }
  if (modelFamily=='Poisson'){
    X[nonZero] <- stats::rpois(N, theta$nu0)
  }
  
  #' then for each entry where A is none zero we sample it according to the law under the alternative
  m <-  p-1
  
  for (i in 1:m){
    nonzeroind <- which(A[i,]!=0)
    L <- length(nonzeroind)
    if (L>=1){
      if (modelFamily=='Gauss'){
        X[i, nonzeroind] <- stats::rnorm(L, nu1[Z[i],Z[nonzeroind]], nu2[Z[i],Z[nonzeroind]])
      }
      if (modelFamily=='Gamma'){
        X[i, nonzeroind] <- stats::rgamma(L, nu1[Z[i],Z[nonzeroind]], nu2[Z[i],Z[nonzeroind]])
      }
      if (modelFamily=='Poisson'){
        X[i, nonzeroind] <- stats::rpois(L, nu[Z[i],Z[nonzeroind]])
      }
    }
  }
  
  A <- A + t(A)
  X <- X + t(X)
  return(list(dataMatrix=X, theta=theta, latentZ=Z, latentAdj=A))
}




#-------------------------Other Auxiliary function----------------------
convertNodePair <- function(i,j,p, directed=FALSE){
  if (sum((i>p) | (j>p))>0){
    stop("Your index is out of range")
  }
  dyads= ifelse(i==j, NA, c(0,cumsum((p-1):1))[pmin(i,j)] + abs(j-i))
  return(dyads)
}



listNodePairs <- function(p, directed=FALSE){
  # if (!exists('N'))
  N <- if (directed) p*(p-1) else p*(p-1)/2
  index <- matrix(0,N,2)
  if (directed){ # directed
    index[,1] <- rep(1:p,each=p-1)
    k <- (1:p^2)[-seq(1,p^2,by=p+1)]
    index[,2] <- rep(1:p,p)[k]
  }else { # undirected
    index[,1] <- rep(1:(p-1),times=(p-1):1)
    toto <- c()
    for (k in 1:(p-2)){
      toto <- c(toto, k*(p-1)+ 1:k)
    }
    index[,2] <- rep(2:p,p-1)[-toto]
  }
  return(index)
}

listNodePairs2 <- function(p){
  N <- p*(p+1)/2
  index <- matrix(0,N,2)
  index[,1] <- rep(1:p,times=p:1)
  toto <- c()
  for (k in 1:(p-1)){
    toto <- c(toto, k*p + 1:k)
  }
  index[,2] <- rep(1:p,p)[-toto]
  return(index)
}


convertGroupPair <- function(q, l, Q, directed){
  if (directed){
    index <- (q-1)*Q+l
  } else { # undirected
    qp <- pmin(q,l)
    lp <- pmax(q,l)
    index <- (2*Q-qp+2)*(qp-1)/2 +lp-qp+1
  }
  return(index)
}


#' matrixToVec
#'
#' @param X a SYMETRIC matrix
#'
#' @return a vector  contenting the coefficient of the upper triangle of the matrix X
#' from left to right and from top to bottom.
#' @export
matrixToVec<-function(X){
  p=length(X[1,])
  Toreturn=rep(0,p*(p+1)/2)
  l=1
  for(i in (1:p)){
    for(j in (i:p)){
      Toreturn[l]=X[i,j]
      l=l+1
    }
  }
  return(Toreturn)
}

#' vecToMatrix
#'
#' @param X a vector
#' @param p (integer) the dimension of the square matrix returned by the function
#' be careful the length of the vector X must be equal to p(p+1)/2
#'
#' @return a p by p symetric matrix whose upper triangle coefficients from left to right and
#' from top to bottom are the entries of the vector X
#' @export
vecToMatrix<-function(X,p){
  index=listNodePairs2(p)
  Toreturn=matrix(0,p,p)
  N=p*(p+1)/2
  for(i in 1:N){
    Toreturn[index[i,1],index[i,2]]=X[i]}
  Toreturn=(Toreturn+t(Toreturn))
  diag(Toreturn)=diag(Toreturn)/2
  return(Toreturn)
}

modelDensity <- function(x, mu, sigma, modelFamily='Gauss'){
  res <- stats::dnorm(x, mu, sigma)
  res[res<=.Machine$double.eps] <- .Machine$double.eps
  res[res>=.Machine$double.xmax] <- .Machine$double.xmax
  return(res)
}



#' Evalute the adjusted Rand index
#'
#' Compute the adjusted Rand index to compare two partitions
#' @name ARI
#' @export
#' @details
#' the partitions may be provided as n-vectors containing the cluster memeberships
#'     of n entities, or by Qxn - matrices whose entries are all
#'     0 and 1 where 1 indicates the cluster membership
#' @param x vector (of length n) or matrix (with n columns) providing a partition
#' @param y vector or matrix providing a partition
#'
#' @return the value of the adjusted Rand index
#'
#' @examples
#' clust1 <- c(1,2,1,2)
#' clust2 <- c(2,1,2,1)
#' ARI(clust1, clust2)
#'
#' clust3 <- matrix(c(1,1,0,0, 0,0,1,1), nrow=2, byrow=TRUE)
#' clust4 <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,1), nrow=3, byrow=TRUE)
#' ARI(clust3, clust4)
ARI <- function(x, y) {
  if (is.matrix(x))
    x <- apply(x,2,which.max)
  if (is.matrix(y))
    y <- apply(y,2,which.max)
  # first, get crosstabs
  ctab <- table(x,y)
  # now calculate 4 intermediary sums
  cellsum <- sum(ctab*(ctab-1)/2)
  totsum <- sum(ctab)*(sum(ctab)-1)/2
  # use matrix multiplication to get row and column marginal sums
  rows <- ctab %*% rep(1,ncol(ctab))
  rowsum <- sum(rows*(rows-1)/2)
  cols <- rep(1,nrow(ctab)) %*% ctab
  colsum <- sum(cols*(cols-1)/2)
  # now put them together
  adj.rand <- (cellsum - (rowsum*colsum/totsum))/(.5*(rowsum +colsum)-(rowsum*colsum/totsum))
  return(adj.rand)
}


q_delta_ql <- function(theta, q, l, t, modelFamily='Gauss'){
  # for a given (q,l)
  Q=length(theta$pi)
  ind.ql = convertGroupPair(q,l,Q,directed=FALSE)
  mu0 <- theta$nu0[1]
  sigma0 <- theta$nu0[2]
  mu <- theta$nu[ind.ql, 1]
  sigma <- theta$nu[ind.ql, 2]
  nb_t <- length(t)
  res <- matrix(0, nb_t, 2)
  res[t==1,] <- 1
  ind_t <- (t>0)&(t<1)
  if (length(ind_t)>0){
    a <- sigma^(-2)-sigma0^(-2) # scalar
    b <- -2*(mu/sigma^2 -mu0/sigma0^2)    # scalar
    cVec <- mu^2/sigma^2 -mu0^2/sigma0^2 + 2*log(sigma/sigma0*(1/theta$w[ind.ql]-1)*(1/t-1))    # vector
    if(a!=0){
      res[ind_t,] <- res[ind_t,] + (a<0)
      ind_2 <- (b^2>(4*a*cVec))&ind_t
      if(sum(ind_2)>0){
        z <- (-b+matrix(sqrt(pmax(0,b^2-4*a*cVec[ind_2])),ncol=1)%*%c(1,-1))/(2*a) # matrix sum(ind_2) x 2
        res[ind_2,2] <- res[ind_2,2] + stats::pnorm(z[,1], mu, sigma) - stats::pnorm(z[,2], mu, sigma)
        res[ind_2,1] <- res[ind_2,1] + stats::pnorm(z[,1], mu0, sigma0) - stats::pnorm(z[,2], mu0, sigma0)
      }
    }else{
      if (b!=0){
        res[ind_t,2] <- if (b<0) 1-stats::pnorm(-cVec[ind_t]/b, mu, sigma) else stats::pnorm(-cVec[ind_t]/b, mu, sigma)
        res[ind_t,1] <- if (b<0) 1-stats::pnorm(-cVec[ind_t]/b, mu0, sigma0) else stats::pnorm(-cVec[ind_t]/b, mu0, sigma0)
      }else{
        res[ind_t,] <- 1*(t[ind_t] >= 1- theta$w[ind.ql])
      }
    }
  }
  return(res)
}


qvaluesNSBM <- function(dataVec, Z, theta, lvalues){
  Q <- length(theta$pi)
  num <- den <- result <- rep(0, length(dataVec))
  ind <- 0
  
  for (q in 1:Q){
    for (l in q:Q){
      ind <- ind + 1
      q01_lvalues_seq <- theta$pi[q]*theta$pi[l]*q_delta_ql(theta, q, l, lvalues)
      f <- 1 + (q!=l)
      num <- num + f*(1-theta$w[ind]) * q01_lvalues_seq[,1]
      den <- den + f*theta$w[ind]*q01_lvalues_seq[,2]
    }
  }
  den <- den + num
  ind <- (den!=0)
  result[ind] <- num[ind]/den[ind]
  
  return(result)
}



# --------------Data Simulation  ---------------
simuData<-function(n,p,graph, g=NULL, prob=NULL, theta=NULL, u=NULL, v=NULL){
  Z=NULL
  
  #Band
  if (graph=="band3"){
    if(is.null(g)) g=3 
    L = huge::huge.generator(n=n, d=p, graph = "band", g=g, u=u, v=v)
    A.true=as.matrix(L$theta)
    X=L$data
  }
  # Hub
  if (graph=="hub"){
    if(is.null(g)) g=p/10
    L = huge::huge.generator(n=n, d=p, graph = "hub", g=g, u=u, v=v)
    A.true=as.matrix(L$theta)
    X=L$data
  }
  # scale-free
  if (graph=="scale-free"){
    L = huge::huge.generator(n=n, d=p, graph = "scale-free", u=u, v=v)
    A.true=as.matrix(L$theta)
    X=L$data
  }
  #SBM2 5  communities
  if(graph=="SBM5communautes"){
    if(is.null(u)) u=0.1
    if(is.null(v)) v = 0.3
    if(is.null(theta)) theta=list(pi=c(1/5,1/5,1/5,1/5,1/5),w=cbind(c(0.1,0.01,0.01,0.001,0.001),c(0.01,0.1,0.01,0.001,0.001),c(0.01,0.01,0.1,0.001,0.001),c(0.001,0.001,0.001,0.1,0.01),c(0.001,0.001,0.001,0.01,0.1)))
    
    Q<-length(theta$pi)
    Z <- sample(1:Q, p, replace=TRUE, prob=theta$pi)
    A <- matrix(0, p, p)
    for (i in 1:(p-1)){
      A[i,(i+1):p] <- stats::rbinom(p-i, 1, theta$w[Z[i],Z[(i+1):p]])
    }
    A.true <- A + t(A)
    Omega <- A.true*v
    diag(Omega) = abs(min(eigen(Omega)$values)) + 0.1 + u
    Sigma = stats::cov2cor(solve(Omega))
    X = MASS::mvrnorm(n, rep(0, p), Sigma)
  }
  return(list(X=X,A.true=A.true,Z=Z))
}


simuData_maxDeg<-function(n, p, maxdegree, power=2){
  u=0.1
  v=0.3
  prob=c(1:maxdegree)**(-power)
  c=sum(prob)
  prob=prob/c
  degs<-sample(1:maxdegree,p, prob=prob, replace=TRUE)
  if(sum(degs)%%2==1){
    degs[1]=degs[1]+1
  }
  #G.true <- igraph::sample_degseq(degs, method="simple.no.multiple.uniform")
  G.true <- igraph::sample_degseq(degs, method="simple.no.multiple")
  A.true=igraph::as_adjacency_matrix(G.true)
  Omega <- A.true*v
  diag(Omega) = abs(min(eigen(Omega)$values)) + 0.1 + u
  Sigma = stats::cov2cor(solve(Omega))
  X = MASS::mvrnorm(n, rep(0, p), Sigma)
  
  
  return(list(X=X,A.true=as.matrix(A.true)))
}





