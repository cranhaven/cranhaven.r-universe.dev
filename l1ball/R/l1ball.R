#' @name l1ball
##' @title Fit the L1 prior
#' @description This package provides an implementation of the Gibbs sampler, for using l1-ball prior with the regression likelihood  \eqn{y_i = X_i\theta+ \epsilon_i, \epsilon_i\sim {N}(0,\sigma^2)}.
#' @param y A data vector, n by 1
#' @param X A design matrix, n by p
#' @param b_w The parameter in \eqn{Beta(1, p^{b_w})} for \eqn{w}, default \eqn{b_w=1}
#' @param step Number of steps to run the Markov Chain Monte Carlo
#' @param burnin Number of burn-ins
#' @param b_lam The parameter in \eqn{\lambda_i \sim Inverse-Gamma(1, b_\lambda)}, default \eqn{b_\lambda=10^{-3}}. To increase the level of shrinkage, use smaller \eqn{b_\lambda}.
#' @return The posterior sample collected from the Markov Chain:\itemize{
#'\item trace_theta: \eqn{\theta}
#'\item trace_NonZero: The non-zero indicator \eqn{1(\theta_i\neq 0)}
#'\item trace_Lam: \eqn{\lambda_i}
#'\item trace_Sigma: \eqn{\sigma^2}
#'}
#' @import VGAM
#' @importFrom VGAM rinv.gaussian
#' @importFrom VGAM rgumbel
#' @importFrom stats quantile
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @examples
#'n = 60
#'p = 100
#'X <- matrix(rnorm(n*p),n,p)
#'d = 5
#'w0 <- c(rep(0, p-d), rnorm(d)*0.1+1)
#'y = X%*% w0 + rnorm(n,0,.1)
#'trace <- l1ball(y,X,steps=2000,burnin = 2000)
#'plot(colMeans(trace$trace_theta))
#' @export l1ball



library(VGAM)
Sample_a <- function(theta, lam, sigma2, NonZero_indicator, p){
	a = numeric(p)
	zero_idx = NonZero_indicator==0
	non_zero_idx = NonZero_indicator==1
	if (sum(non_zero_idx)>0){
		ig_m = lam[non_zero_idx] * sqrt(sigma2) / abs(theta[non_zero_idx])
		a[non_zero_idx] = 1/rinv.gaussian(sum(non_zero_idx), ig_m, 1)
	}
	a[zero_idx] = rgamma(sum(zero_idx), shape = .5, rate = .5)
	return(a)
}

a_density <- function(a, theta, lam, sigma2){
	return(-log(a)/2 - theta ^2 /lam^2/2/sigma2/a-a/2)
}

SampleNonZeroIndicator <- function(p, mu, lam, a, theta, sigma2, NonZero_indicator, a_eps= 1E-10){
	p_NonZero = -mu/lam/sqrt(sigma2)
	p_Zero = log(1-exp(p_NonZero))

	sigma = sqrt(sigma2)
	w1 = p_Zero + a_density(a_eps,theta,lam,sigma2)
	w2 = p_NonZero + a_density(a, theta, lam, sigma2)
	new_p = t(cbind(w1, w2)) + matrix(rgumbel(p*2),nrow=2,ncol=p)
	NonZero =  apply(new_p, 2, which.max) ==2
	#print( sum(NonZero)/sum(NonZero_indicator))
	if (runif(1) < sum(NonZero)/sum(NonZero_indicator)){
		s <- NonZero
	}
	else{
		s <- NonZero_indicator
	}
	return(s)
}


chol_ <- function(x){
  if(length(x)>0){
    return (chol(x))
    }
  else{
    return(sqrt(x))
  }

}


SampleTheta <- function(X2, Xy, lam, a, sigma2, NonZero_indicator, p){
	a_star = numeric(p)
	idx = which(NonZero_indicator==1)
	a_star[idx] = a[idx]*lam[idx]^2
	theta = numeric(p)
	if (sum(NonZero_indicator)>1){
		a_starlam_sub_inv = diag(1/(a_star[idx]*lam[idx]^2) + 1E-14) # add 1E-14 to make the cholesky work
		A = (X2[idx,idx]+a_starlam_sub_inv)
		theta0 = rnorm(sum(NonZero_indicator))
		LA = as.matrix(chol_(A))

		# theta[idx] = solve(t(LA), theta0) * sqrt(sigma2) + solve(t(LA), solve(LA, Xy[idx]))
		theta[idx] = solve(t(LA), theta0) * sqrt(sigma2) + chol2inv((LA)) %*% Xy[idx]
	}
	if (sum(NonZero_indicator)== 1){

	  a_starlam_sub_inv = (1/(a_star[idx]*lam[idx]^2) + 1E-14) # add 1E-14 to make the cholesky work
	  A = (X2[idx,idx]+a_starlam_sub_inv)
	  theta0 = rnorm(sum(NonZero_indicator))
	  LA = sqrt(A)
	  # print(LA)
	  # print(Xy[idx])
	  # print(A)

	  theta[idx] = theta0/LA * sqrt(sigma2) + Xy[idx]/A

	}

	return(theta)
}

ExpCDF <- function(x,lam){
	return(1-exp(-x/lam))
}

ExpInvCDF <- function(y, lam){
	return(-log(1-y)*lam)
}

SampleT <- function(mu, lam, NonZero_indicator, theta, sigma2,p){
	m = lam*sqrt(sigma2)
	t = ExpInvCDF(ExpCDF(mu, m)*runif(p),m)-mu
	t[NonZero_indicator]=abs(theta)[NonZero_indicator]
	return(t)
}

SampleLam <- function(t, mu, sigma2, p, b_lam= 1E-2){
	a=1
	beta = t+mu
	lam = 1/rgamma( n = p, shape = a+1, rate = abs(beta)/sqrt(sigma2)+b_lam )
	return(lam)
}

# SampleSigma2 <- function(X, y, theta, sigma2, t, mu, lam, n, p, eps_change = 1E-2, ub= Inf){
#   beta = t+mu
# 	ig_m = lam* sqrt(sigma2)/beta
# 	a_beta = 1/rwald(p,ig_m,1)
# 	b1 = sum((y-X%*%theta)^2)/2 + sum(beta^2/lam^2/a_beta)/2
# 	a1 = n/2+ p/2 + 1
# 	return( c(1/rgamma(1, a1, rate = b1),1))
# }

SampleSigma2 <- function(X, y, theta, sigma2, t, mu, lam, n, p, eps_change = 1E-2, ub= Inf){

  beta = t+mu

  ss2 = sum((y-X%*%theta)^2)
  s_beta = sum(abs(beta)/lam)

  Compute_l_sigma2<- function(sigma2){
    - ((n+p)/2) * log(sigma2) - ss2/sigma2/2 - s_beta/sqrt(sigma2) - p*sigma2
    # test:using exponential prior on sigma \sigma \sim Exp(p) to prevent sigma2 increases too much

    }

  forward_lb = max(c(sigma2-eps_change,0))
  forward_ub = min( c(sigma2+eps_change, ub))

  forward_density = -log(forward_ub-forward_lb)
  sigma2_new = runif(1,forward_lb, forward_ub)

  backward_lb = max(c(sigma2_new - eps_change, 0))
  backward_ub = min(c(sigma2_new+eps_change,ub))
  backward_density = -log(backward_ub-backward_lb)

  if (log(runif(1))< Compute_l_sigma2(sigma2_new)+backward_density- Compute_l_sigma2(sigma2)-forward_density){
    sigma2 = sigma2_new
    accept =1
  }
  else{
    accept = 0
  }

  return( c(sigma2, accept))

}

SampleMu <- function(p, lam, NonZero_indicator, sigma2, w, b_w, eps_change = 1E-2,  b_lam= 1E-2){

	ComputeMu <- function(w, sigma2){
		a=1
		b= b_lam*  sqrt(sigma2)
		mu = (w ^ (-1.0/a)- 1)*b
		return(mu)
	}

	Compute_h_w <- function(w){
		mu = ComputeMu(w, sigma2)
		p_NonZero = -mu/lam/sqrt(sigma2)
		p_Zero = log((1-exp(p_NonZero)))

		return(sum(p_NonZero*NonZero_indicator + p_Zero*(1.0-NonZero_indicator))+(p^b_w-1)*log(1-w))
	}
	forward_lb = max(c(w-eps_change,0))
	forward_ub = min(c(w+eps_change, 1))
	forward_density = -log(forward_ub-forward_lb)
	w_new = runif(1,forward_lb, forward_ub)
	backward_lb = max(c(w_new - eps_change, 0))
	backward_ub = min(c(w_new+eps_change, 1))
	backward_density = -log(backward_ub-backward_lb)

	if (log(runif(1))<Compute_h_w(w_new)+backward_density- Compute_h_w(w)-forward_density){
		w = w_new
		accept =1
	}
	else{
		accept = 0
	}
	mu = ComputeMu(w, sigma2)
	return(c(w, mu,accept))
}

soft_thresholding<- function(x,a){

  sign(x)* (abs(x)-a)*((abs(x)-a)>0)

}



l1ball <- function(y, X, b_w = 1, steps = 3000, burnin=1000, b_lam=1E-3, sigma2_ub=Inf){
	n = nrow(X)
	p = ncol(X)


	trace_Sigma2 = numeric()
	trace_NonZero = matrix(0, nrow = steps, ncol = p)
	trace_theta = matrix(0, nrow = steps, ncol = p)
	trace_Lam = matrix(0, nrow = steps, ncol = p)

	X2 = t(X) %*% X
	Xy = t(X) %*% y
	theta = solve(X2+diag(1,p), Xy)
	sigma2 = 0.1#sum((y-X%*%theta)^2)/n
	w = 1./p

	mu = quantile( abs(theta),1-w) #runif(1)
	lam = rep(1, p)
	NonZero_indicator = (abs(theta)>mu)

	accept_sigma2 = 0
	eps_sigma2 = 1E-1
	accept_w = 0
	eps_w = 1E-2

	for (k in 1:(steps+burnin)){
		if (k%%100==0){
			print(k)
		  # print(sigma2)
		  }
		a = Sample_a(theta, lam, sigma2, NonZero_indicator, p)

		NonZero_indicator = SampleNonZeroIndicator(p, mu, lam, a, theta, sigma2, NonZero_indicator, a_eps = 1E-10)

		# update mu
		wmu_and_accept = SampleMu(p, lam, NonZero_indicator, sigma2, w, b_w, eps_change = eps_w, b_lam=b_lam)
		w = wmu_and_accept[1]
		mu = wmu_and_accept[2]
		accept_w = accept_w+wmu_and_accept[3]

		t = SampleT(mu, lam, NonZero_indicator, theta, sigma2,p)

		# print(sum(NonZero_indicator))

		theta = SampleTheta(X2, Xy, lam, a, sigma2, NonZero_indicator, p)

		lam = SampleLam(t, mu, sigma2, p, b_lam= b_lam)
		#sigma2 = 0.1**2#
		sigma2_and_accept = SampleSigma2(X, y, theta, sigma2, t, mu, lam, n, p, ub = sigma2_ub, eps_change = eps_sigma2)

		sigma2 = sigma2_and_accept[1]
		accept_sigma2 = accept_sigma2+ sigma2_and_accept[2]

		if (k< burnin/3){
		  # if (TRUE){

		    adapting_steps= 50

		  if( k %%adapting_steps == 0 ){



		    eps_sigma2 = eps_sigma2* exp( ((accept_sigma2/adapting_steps) - 0.234))

		    eps_w = eps_w* exp( ((accept_w/adapting_steps) - 0.234))

		    print ("Adapting the Metropolis-Hastings step size...the acceptance rates are")
		    print (c(accept_sigma2/adapting_steps, accept_w/adapting_steps))
		    accept_w = 0
		    accept_sigma2 = 0

		  }


		}


		if (k>burnin){
		  idx = k-burnin
			trace_theta[idx,] <- theta
			trace_NonZero[idx,] <- NonZero_indicator
			trace_Sigma2[idx] <- sigma2
			trace_Lam[idx,] <- c(lam)

		}



	}
	return(list(trace_theta=trace_theta, trace_NonZero = trace_NonZero, trace_Sigma2= trace_Sigma2, trace_Lam= trace_Lam ))
}
