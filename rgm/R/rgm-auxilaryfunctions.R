#' Rotate Locations
#'
#' Rotates locations to align with the mean vector direction.
#'
#' @param loc Matrix of locations to rotate.
#'
#' @return Matrix of rotated locations.
#' @examples
#' # Example usage with a 2-column matrix representing locations.
#' loc <- matrix(rnorm(20), ncol = 2)
#' rotated_loc <- rot(loc)
#' @export
rot<-function(loc){
  x.mn<-apply(loc,2,mean)
  alpha<-atan(x.mn[2]/x.mn[1])+(x.mn[1]<0)*pi
  phi<-pi/2-alpha
  angles<-apply(loc,1,function(x){atan(x[2]/x[1])+(x[1]<0)*pi})
  r<-apply(loc,1,function(x){sqrt(x[1]^2+x[2]^2)})
  new.loc<-r*cbind(sin(phi+angles),cos(phi+angles))
  return(new.loc)
}

#' Bayesian Probit Regression (BPR)
#'
#' Performs Bayesian Probit Regression given the predictors and response.
#'
#' @param y Vector of binary responses.
#' @param X Matrix of predictors.
#' @param offset Optional offset for the linear predictor.
#' @param theta Initial values for the regression coefficients.
#' @param theta_0 Prior mean for the regression coefficients.
#' @param N_sim Number of simulations to perform.
#'
#' @return A matrix of simulated values for the regression coefficients.
#' @export
bpr <- function(y, X, offset = 0, theta, theta_0 = c(0, 0, 0), N_sim = 1) {


  # Dimensions of theta
  D <- ncol(X)

  # Number of observations
  n <- length(y)
  N1 <- sum(y)
  N0 <- n - N1

  # Conjugate prior on the coefficients theta ~ N(theta_0, Q_0)
  Q_0 <- diag(10, D)

  # Initialize parameters
  z <- rep(NA, n)

  # Matrix storing samples of the theta parameter
  theta_chain <- matrix(0, nrow = N_sim, ncol = D)

  # ---------------------------------
  # Gibbs sampling algorithm
  # ---------------------------------

  # Compute posterior variance of theta
  prec_0 <- MASS::ginv(Q_0)
  V <- MASS::ginv(prec_0 + t(X) %*% X)

  for (t in 1:N_sim) {
    # Update Mean of z
    mu_z <- X %*% theta + offset
    # Draw latent variable z from its full conditional: z | theta, y, X
    if (N0 > 0) {
      z[y == 0] <- truncnorm::rtruncnorm(N0, mean = mu_z[y == 0], sd = 1, a = -Inf, b = 0)
    }
    if (N1 > 0) {
      z[y == 1] <- truncnorm::rtruncnorm(N1, mean = mu_z[y == 1], sd = 1, a = 0, b = Inf)
    }

    # Compute posterior mean of theta
    M <- V %*% (prec_0 %*% theta_0 + t(X) %*% (z - offset))
    # Draw variable theta from its full conditional: theta | z, X
    theta <- rmvnorm(1, M, V)

    # Store the theta draws
    theta_chain[t, ] <- theta
  }

  return(theta_chain)
}

#' Graph MCMC Sampler
#'
#' Performs Markov Chain Monte Carlo (MCMC) sampling on a graph model.
#'
#' @param G Graph adjacency matrix.
#' @param X Optional matrix of covariates.
#' @param iter Number of MCMC iterations to perform.
#' @param alpha Initial values for alpha parameters.
#' @param theta Initial values for theta parameters.
#' @param loc Initial locations for nodes in the graph.
#' @param burnin Number of burn-in iterations.
#'
#' @return A list containing samples of alpha, loc, and possibly theta.
#' @export
Gmcmc<-function(G, X=NULL, iter=1000,alpha=NULL,theta=NULL,loc=NULL, burnin=0)
{
  B<-ncol(G)
  n.edge<-nrow(G)
  p<-(sqrt(1+8*n.edge)+1)/2
  m<-matrix(1:p,ncol=p,nrow = p)
  e1<-t(m)[lower.tri(m)]
  e2<-m[lower.tri(m)]
  if(is.null(loc))
    cloc<-matrix(stats::rnorm(B*2),ncol=2)
  else
    cloc <- loc
  if(is.null(alpha))
    alpha<-stats::rnorm(B)

  dim.cond<-ncol(cloc)
  cloc.save<-array(dim = c(B,ncol(cloc), iter-burnin))
  alpha.save<-matrix(0,nrow=B,ncol= iter-burnin)

  Z <- X

  if(!is.null(Z))
  {
    Z<-as.matrix(Z)
    if(is.null(theta))
      beta<-as.matrix(rep(0,ncol(Z)))
    else
      beta<-as.matrix(theta)
    beta.save<-matrix(0,nrow=ncol(Z),ncol=iter-burnin)
  }

  #################################################################

  for (k in 1:iter){
    y<-as.vector(G)

    #####################################################
    for (b in 1:B){
      # update the latent condition locations
      X<-apply(cloc,2,rep,each=n.edge)*rep(G[,b],B)
      X[(b-1)*n.edge+(1:n.edge),]<-matrix(apply(G,1,function(g,cloc,b){colSums(cloc * g)-cloc[b,]*g[b]},cloc=cloc,b=b),nrow=n.edge,ncol=dim.cond,byrow=T)
      hlp2<-NULL
      for (bb in 1:B){
        if (bb==b){
          hlp2<-c(hlp2,rep(0,n.edge))
        } else {
          hlp3<-apply(G,1,function(g,cloc,bb,b){crossprod(colSums(cloc * g)-cloc[b,]*g[b]-cloc[bb,]*g[bb],cloc[bb,])},cloc=cloc,bb=bb,b=b)
          hlp2<-c(hlp2,hlp3)
        }
      }
      offset<-hlp2+rep(alpha,each=n.edge)
      if(!is.null(Z))
        offset<-hlp2+rep(alpha,each=n.edge)+ rep(Z%*%beta,B)
      cloc[b,]<-bpr(y,X,offset,theta = cloc[b,],theta_0 = rep(0,dim.cond))
    }

    #####################################

    dist.cond<-matrix(ncol=B,nrow=n.edge)
    for (b in 1:B){
      #updating condition-specific intercept
      dist.cond[,b]<-apply(G,1,function(g,cloc,b){crossprod(colSums(cloc * g)-cloc[b,]*g[b],cloc[b,])},cloc=cloc,b=b)
      offset<-dist.cond[,b]
      if(!is.null(Z))
        offset<-dist.cond[,b]+ Z%*%beta
      y<-G[,b]
      X<-as.matrix(rep(1,length(y)))
      alpha[b]<-bpr(y,X,offset,theta = alpha[b],theta_0 = 0)
    }


    if(!is.null(Z)){
      y<-as.vector(G)
      X<-apply(Z,2,rep,B)
      offset<-c(dist.cond)+rep(alpha,each=n.edge)
      beta<-bpr(y,X,offset,theta = beta,theta_0 = rep(0,length(beta)))
      beta<-t(beta)
    }

    if (k>burnin){
      cloc.save[,,k-burnin]<-cloc
      alpha.save[,k-burnin]<-alpha
      if(!is.null(Z))
        beta.save[,k-burnin]<-beta
    }
  }

  #################################################################


  if(is.null(Z))
    return(list(alpha=alpha.save,loc=cloc.save))
  else
    return(list(alpha=alpha.save,theta=beta.save,loc=cloc.save))
}





#' Sample Data
#'
#' This function generates sample data based on the provided parameters and truncation points.
#'
#' @param data A list of matrices representing the data.
#' @param K A list of matrices representing the precision matrices for each data matrix in `data`.
#' @param tpoints A list containing two lists of matrices for lower and upper truncation points, respectively.
#'
#' @return A list of matrices with the sampled data.
#' @export
sample.data<-function (data, K, tpoints)
{
  B <- length(data)
  p <- ncol(data[[1]])
  for (i in 1:B) {
    for (j in 1:p) {
      S <- solve(K[[i]])
      S22i <- solve(S[-j, -j])
      S12 <- S[j, -j]
      S11 <- S[j, j]
      mu.j <- t(S12) %*% S22i %*% t(data[[i]][, -j])
      var.j <- S11 - t(S12) %*% S22i %*% as.matrix(S12)
      data[[i]][, j] <- truncnorm::rtruncnorm(length(mu.j), a = tpoints[[i]][[1]][,j], b = tpoints[[i]][[2]][, j], mean = mu.j,sd = sqrt(var.j))
    }
  }
  return(data)
}



