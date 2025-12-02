## functions related to Entropy & Kullback estimation,
## on simulated copies of iid chain trajectories, or
## doing simulation & Entropy computation/slice of time ("parallel" in time)

# change for svn

## bandwidth matrix for multivariate kde
# Scott et al formula for gaussian multivariate data
# computed from a sample x (n,d) matrix
# where each row is a point in R^d 
# return a diagonal covariance matrix
mvbw <- function(x){
	n <- nrow(x); d <- ncol(x)
	ct <- (4/((d+2)*n))^(1/(d+4))
	h <- diag(ct*sd(x))^2
	}


####################################################
# theoretical value of entropy for the mv gaussian 
# = E_f(log f)
# Caution: sometimes defined by the opposite H(f) = - E_f(log f)
normEntropy <- function(target_param){
	d <- length(target_param$mean)
	Eflogf <- - (d + d*log(2*pi) + log(det(target_param$v)))/2
	Eflogf
	}

####################################################
# The root-mean-square error (RMSE)
#It computes the error between the theoretical value of entropy and the estimated vector one
RmseEnt = function(RealEnt, EstimatedEnt){
  RMSE <- sqrt(colMeans((RealEnt - EstimatedEnt)^2))
  RMSE
}


####################################################
# Euclidean distance between set of nrow(C) points in C, 
# and a set of nrow(P) points in P,  vectorized
# in d-dim; d = ncol(C) = ncol(P)
# returns a matrix
# D(nrow(C),nrow(P)), where D(i,j) = d(P_i,C_j)
dist.fun <- function(C, P){apply(C,1,function(x) colSums((t(P) - x)^2))}


#######################################################
## Kullback & entropy estimation from MCMCcopies simulation output
# mcmc input Ptheta= array(n+1,d,nmc) of simulated values
# where 1st iteration values are theta0's
# Hpt[t] = E_{p^t}[log(p^t)] entropy estimate of p^t
# REnt[t] = E_{p^t}[log(f)] relative entropy MC estimate
# Kb[t] = Estimated Kullback distance K(p^t,f) between p^t and the target

# plmc1 : a "plMCMC" object from MCMCcopies() 
# which contains all the simulations plus target def & parameters
# [plmc2 usage deprecated]
# if plmc2=NULL, target normalization must be known
#  so that the kullback computed from plmc1 is correct
# if plmc2 is supplied, then it is assumed that the normalization
# must be computed from the differences of non-normalized Kullback,
# when f is only known up to a multiplicative cte
# this function may be called for 2 mcmc_algo's to take the difference
# between the obtained Kullback estimates
# as in Chauveau & Vandekerkhove (HAL 2007) 
#
# eventual numeric pb in tails eg when computing log(10^-324)=-Inf
#
# method = "..." option defines the entropy estimate:
# "Resubst" : Resubstitution method ie 
#  whole sample used for mvkde and MC integration
# WARNING! the true Eggermont estimate is a numerical integration of the mvkde
# 	this implementation is provided without theoretical guarantee
# Actually it *seems* not consistent !!
#
# "Gyorfi.trim" : divide in 2 sub-samples, one for mvkde and one for MC int
# 	and remove trim % of the smallest f(X_i) in computing the 
# 	Monte-Carlo mean over the second subsample
#	all.f=FALSE means we compute REnt (the Monte-Carlo mean over log(f))
#	on the *same trimmed* subsample, to try removing the bias  
#	all.f=TRUE (the default) means REnt computed over the whole sample
#	Note: in Gyorfi sum(trimmed log(p^t))/n2  is used, so that the trimmed
#	log(p^t(x2_i)) are replaced by 0's instead of merely being removed from the
#	sample. It is implemented in that way
#
## "NearestNeighbor" as in Kozachenko and Leonenko (the default)
# NN estimate = - E_p(log p) so we take the negative of the estimate
#
#
#"kth NearestNeighbor" Brute-force Algorithms
#KNN estimate = - E_p(log p)  so we take the negative of the estimate
#k is  an integer to choose. When k=1, we obtains the NN method
#
#
#"A Nearest Neighbor" Approximation of the Knn
#
# This function simply call the appropriate Entropy.<method>() function which
# computes estimated entropy for each "slice of time" t=1,...n (see after)
#
# ToDo(?): method "Gyorfi" (does not work though, too high treshold!)
# apply a treshold ie keep only obs s.t. f > 1/log(nmc)*cte
#
# uselogtarget: if FALSE, log(target(theta)) is computed directly from target(), 
# if FALSE, a specific function logtarget must be provided, which is used instead, to solve numerical problem
# generation of Inf's, and a bit faster
# ToDo: check if this is compatible with all.f ?
## IN TESTING PHASE, JUST FOR DEFAULT "A.Nearest.Neighbor" METHOD YET
# note: EntropyHM = old name, deprecated

EntropyMCMC <- function(plmc1, # plmc2=NULL, 
                        method="A.Nearest.Neighbor", k=1, trim=0.02, eps=0,
                        all.f=TRUE, verb = FALSE, EntVect = FALSE,
                        uselogtarget = FALSE, logtarget = NULL){
  
  if (EntVect == TRUE && k==1) {stop("Error: multinearest neighbour version not available for k < 2\n")}
  # if (method=="A.Nearest.Neighbor") require(RANN) # removed since in Imports and NAMESPACE
  
  # retrieving parameters needed for all Entropy estimation methods
  mc1 <- plmc1$Ptheta # copies of MCMCs, first value is init theta0
  n <- dim(mc1)[1]    # nb of (time) iterations of each single chain	
  d <- dim(mc1)[2]    # space dimension of theta
  nmc <- dim(mc1)[3]  # nb of iid copies
  Kb1 <- Entp <- rep(0,n) # for storing results
  if(EntVect == TRUE) {Kb1k <- Entpk <- matrix(0,n,k)} # for storing results
  else {Kb1k <- Entpk <- matrix(0,n,1)}
  q=round(n/10); if (q==0) q <- 1 # for verbose print of % done
  
  if (method=="Gyorfi.trim") {  
    n1 <- ceiling(nmc/2) # subsampling done ONCE for all t=1,...,n
    ind1 <- sample(1:nmc,n1)
    ind2 <- setdiff(1:nmc,ind1); n2 <- length(ind2)
    ntrim <- round(trim*n2) + 1 # at least 1 value removed!
    for (it in 1:n){
      # x <- t(mc1[it,,])   # nmc x d = sample iid~p^t at time t
      if (d==1) x <- mc1[it,,] else x <- t(mc1[it,,])
      
      ep <- Entropy.Gf(x, ind1, ind2, plmc1$target, plmc1$f_param)
      Kb1[it] <- ep$Kb; Entp[it] <- ep$Entp
      # % done printed as dots...
      b=round(it/q); r=it-b*q; if (r==0 & verb) cat(".")
    }
    if (verb) {
      cat("\n")
      cat("Gyorfi.trim: size of trimmed subsample:",n2-ntrim,"\n")
      cat("Sample bandwidth matrix:\n"); print(mvbw(x))
    }		
  }
  
  if (method=="Resubst")  # Re-substitution without trimming and subsampling
    for (it in 1:n){
      # x <- t(mc1[it,,])   # nmc-sample iid~p^t at time t
      if (d==1) x <- mc1[it,,] else x <- t(mc1[it,,])
      ep <- Entropy.Resubst(x, plmc1$target, plmc1$f_param)
      Kb1[it] <- ep$Kb; Entp[it] <- ep$Entp
      # % done printed as dots...
      b=round(it/q); r=it-b*q; if (r==0 & verb) cat(".")
    }
  
  if (method=="Nearest.Neighbor") 
    for (it in 1:n){
      if (d==1) x <- mc1[it,,] else x <- t(mc1[it,,])
      ep <- Entropy.NN(x, plmc1$target, plmc1$f_param)
      Kb1[it] <- ep$Kb; Entp[it] <- ep$Entp		# % done printed as dots...
      b=round(it/q); r=it-b*q; if (r==0 & verb) cat(".")
    }
  
  if (method=="k.Nearest.Neighbor") 
    for (it in 1:n){
      if (d==1) x <- mc1[it,,] else x <- t(mc1[it,,]) 
      if(EntVect == TRUE) ep <- Entropy.kNN.vect(x, plmc1$target, plmc1$f_param, k)
     else  ep <- Entropy.kNN(x, plmc1$target, plmc1$f_param, k)
      Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
      b=round(it/q); r=it-b*q; if (r==0 & verb) cat(".")
    }
  if (method=="A.Nearest.Neighbor") 
    for (it in 1:n){
      if (d==1) x <- mc1[it,,] else x <- t(mc1[it,,]) 
      if(EntVect == TRUE) ep <- Entropy.ANN.vect(x, plmc1$target, plmc1$f_param, k, eps)
      else  ep <- Entropy.ANN(x, plmc1$target, plmc1$f_param, k, eps, uselogtarget, logtarget)
      Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
      b=round(it/q); r=it-b*q; if (r==0 & verb) cat(".")
    }
  
  # Output object			
  cat("\n") # for ending dots
  
  
  if (method=="k.Nearest.Neighbor" || method=="A.Nearest.Neighbor" ) {
    return(structure(list(Kullback=Kb1k, Entp=Entpk, nmc=nmc, dim=d,
                          algo=plmc1$algo, target=plmc1$target, method=method, k=k,
                          f_param=plmc1$f_param, q_param=plmc1$q_param),
                     class="KbMCMC"))	  
  }
  else{
    return(structure(list(Kullback=Kb1, Entp=Entp, nmc=nmc, dim=d,
                          algo=plmc1$algo, target=plmc1$target, method=method,
                          f_param=plmc1$f_param, q_param=plmc1$q_param),
                     class="KbMCMC"))
  }	
}
	

######################################################
## Set of functions for computing Entropy and Kullback 
## for the implemented methods
# just from a nmc- iid sample in R^d, typically iid~p^t at time t
# these functions are not for the end user, but intended to be called by 
#		- EntropyMCMC() which computes entropies from full MCMCcopies() output
#		- EntropyParallel() which simulates in parallel and computes entropies 
#			at each time, without storing all the trajectories unless EntropyMCMC
# x = matrix(nmc,d) where each row is a simulated value in R^d
# 		and d = space dimension of theta
# target = target density function definition (in R) as in plMCMC$target
# target_param = target parameter as in plMCMC$f_param objects
# nmc = nb of iid copies ~ "some" pdf p
# Entp = E_{p}[log(p)] entropy estimate of p
# REnt = E_{p}[log(f)] relative entropy iid MC estimate
# Kb = Kullback distance K(p,target)

# Entropy for method=="Gyorfi.trim"
# subs1, subs2 = subsamples index computed once for all iterations
# and passed by the calling function
Entropy.Gf <- function(x, subs1, subs2, target, target_param,
						trim=0.02, all.f=TRUE){
	nmc <- dim(x)[1]; d <- dim(x)[2]
	n1 <- length(subs1); n2 <- length(subs2)
	ntrim <- round(trim*n2) + 1 # at least 1 value removed!
	x1 <- x[subs1,] # subsample for the mvkde 
	x2 <- x[subs2,] # subsample for the Monte Carlo integration
	# Trimming trim % of the lower hatp(x2)
	h <- mvbw(x1) # mv bandwidth matrix using the mvkde sample
	hatp <- mvkde(x1, u=x2, bw=h) 	# hat p at x2_i's based on data x1
	o <- order(hatp)  				# hatp[o] is hat p(x2) sorted
	hatptr <- hatp[o][(ntrim+1):n2]
	x2tr <- x2[o,][(ntrim+1):n2,] 	# corresponding trimmed sample x2
	Entp <- sum(log(hatptr))/n2  # divided by the full x2 size
	if (all.f) 
		REnt <- mean(log(target(x, target_param))) else {
		REnt <- mean(log(target(x2, target_param))) 
		} 	
	Kb <- Entp - REnt
	return( list(Kb=Kb, Entp=Entp))	
	}



# method=="Nearest.Neighbor"
# SEE ALSO: the parallel version 
Entropy.NN <- function(x, target, target_param){
  if (d == 1) {nmc <- NROW(x); d <- NCOL(x)} 
  else {nmc <- nrow(x); d <- ncol(x)}	
	# CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du # in C
	# logc1 <- log(pi^(d/2)/gamma(d/2 + 1)) # first version
	logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())

	# C call for computing NN entropy with C loops
	# add PACKAGE="" option when moving to package
	z <- .C("entropyNNC", as.integer(nmc), as.integer(d),
			x=as.double(x), logc1=as.double(logc1), result=double(1))
	Entp <- z$result
	if (d == 1){
	  yy = matrix(1, nmc, d)
	  for(i in 1:nmc) yy[i,] = x[i]
	  REnt <- mean(log(target(yy, target_param))) }# E_{p}(log f)
	  else REnt <- mean(log(target(x, target_param))) # E_{p}(log f)
	  Kb <- Entp - REnt
	return( list(Kb=Kb, Entp=Entp))	
	}


# 1. method=="kth-Nearest.Neighbor", it returns only the kth value of the entropy
# SEE ALSO: the parallel version
Entropy.kNN <- function(x, target, target_param, kmax){
  if (d == 1) {nmc <- NROW(x); d <- NCOL(x)} 
  else {nmc <- nrow(x); d <- ncol(x)}	
  CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du # in C
  logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())
  
  if (d == 1){
    kNNdist <- matrix(0,nrow = nmc, ncol = kmax);
    for(i in 1:nmc) kNNdist[i,] <- t(sort(abs(x[i]-x), method="quick"))[2:(kmax+1)];
  }
  else {
    D <- sqrt(dist.fun(x, x)) # distance matrix between the nmc d-dim points
    kNNdist <- t(apply(D, 1, sort, method="quick"))[,(kmax+1)] # just k-th NN column
  }
  if (kmax>1) Lkm1 <- sum(1/(1:(kmax-1)))  else Lkm1 <- 0 # L_{k-1}
  Entp <- d*mean(log(kNNdist)) + logc1 + log(nmc) - Lkm1 + CEuler
  Entp <- - Entp # convention \int p \log(p)
  if (d == 1){
  yy = matrix(1, nmc, d)
  for(i in 1:nmc) yy[i,] = x[i]
  REnt <- mean(log(target(yy, target_param)))} # E_{p}(log f)
  else REnt <- mean(log(target(x, target_param))) # E_{p}(log f)
  Kb <- Entp - REnt
  return( list(Kb=Kb, Entp=Entp))	 
}

# 2. method=="kth-Nearest.Neighbor", it returns a vector of dimension k for all
# entropy estimates from k=1,...,k_max
# since what is cpu-demanding is sorting the distances !
# x = matrix(nmc,d), nmc rows iid~pdf, each row is a d-dim point. 
# SEE ALSO: the parallel version 

Entropy.kNN.vect <- function(x, target, target_param, kmax){
  if (d == 1) {nmc <- NROW(x); d <- NCOL(x)} 
  else {nmc <- nrow(x); d <- ncol(x)}	
  CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du # in C
  logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())
  
  if (d == 1){
    kNNdist <- matrix(0,nrow = nmc, ncol = kmax);
    for(i in 1:nmc) kNNdist[i,] <- sort(abs(x[i]-x), method="quick")[2:(kmax+1)];
  }
  else {
    D <- sqrt(dist.fun(x, x)) # distance matrix between the nmc d-dim points
    kNNdist <- t(apply(D, 1, sort, method="quick"))[,2:(kmax+1)] # 1.. kmax NN columns
  }
  Lc <- rep(0,kmax) # constants L_k 's
  for (k in 2:kmax) Lc[k] <- sum(1/(1:(k-1)))
  # computes entropy for one k per column
  Entp <- d*colMeans(log(kNNdist)) + logc1 + log(nmc) - Lc + CEuler
  Entp <- - Entp # convention \int p \log(p)
  if (d == 1){
    yy = matrix(1, nmc, d)
    for(i in 1:nmc) yy[i,] = x[i]
    REnt <- mean(log(target(yy, target_param)))# E_{p}(log f)
     } 
  else REnt <- mean(log(target(x, target_param))) # E_{p}(log f)
  Kb <- Entp - REnt
  return( list(Kb=Kb, Entp=Entp))	
}

# 3. method=="A.Nearest.Neighbor", it returns only the kth value of the entropy
# SEE ALSO: the parallel version
# New 2018: uselogtarget option
Entropy.ANN <- function(x, target, target_param, kmax, eps = 0, 
                        uselogtarget = FALSE, logtarget = NULL){
  if (d == 1) {nmc <- NROW(x); d <- NCOL(x)} 
  else {nmc <- nrow(x); d <- ncol(x)}
  CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du # in C
  logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())
  
  #the ANN method: returns a matrix of dimension nmc x k which represents the sorted distances
  # NB: uses pkg::ft to avoid require etc, and RANN is in Imports field already.
  k.nearest <- RANN::nn2(x,x, k=kmax+1, eps=eps)
  
  if (kmax>1) Lkm1 <- sum(1/(1:(kmax-1)))  else Lkm1 <- 0 # L_{k-1} 
  Entp <- d*mean(log(k.nearest$nn.dists[,kmax+1])) + logc1 + log(nmc) - Lkm1 + CEuler
  Entp <- - Entp # convention \int p \log(p)
  
  if (d == 1){
    yy = matrix(1, nmc, d)
    for(i in 1:nmc) yy[i,] = x[i]
    if (uselogtarget) REnt <- mean(logtarget(yy, target_param))
    else REnt <- mean(log(target(yy, target_param)))
    } # E_{p}(log f)
  else { # true multidim case d>1
    if (uselogtarget) REnt <- mean(logtarget(x, target_param))
    else REnt <- mean(log(target(x, target_param)))
  } 
  Kb <- Entp - REnt
  return( list(Kb=Kb, Entp=Entp)) 
}

# 4. method=="A.Nearest.Neighbor", it returns a vector of dimension k for all
# entropy estimates from k=1,...,k_max
# since what is cpu-demanding is sorting the distances !
# x = matrix(nmc,d), nmc rows iid~pdf, each row is a d-dim point. 
# SEE ALSO: the parallel version 

Entropy.ANN.vect <- function(x, target, target_param, kmax, eps = 0){
  if (d == 1) {nmc <- NROW(x); d <- NCOL(x)} 
  else {nmc <- nrow(x); d <- ncol(x)}	
  CEuler <- 0.57721566490153286 # Euler Cte int_R^+ exp(-u)log(u) du # in C
  logc1 <- (d/2)*log(pi) - lgamma(d/2 + 1) # lgamma better than log(gamma())
  
  #the ANN method: comput returs a matrix of dimension nmc x k which represents the sorted distances
  k.nearest <- RANN::nn2(x,x, k=kmax+1, eps=eps)
  
  Lc <- rep(0,kmax) # constants L_k 's
  for (k in 2:kmax) Lc[k] <- sum(1/(1:(k-1)))
  # computes entropy for one k per column
  Entp <- d*colMeans(log(k.nearest$nn.dists[,2:(kmax+1)])) + logc1 + log(nmc) - Lc + CEuler
  Entp <- - Entp # convention \int p \log(p)
  if (d == 1){
    yy = matrix(1, nmc, d)
    for(i in 1:nmc) yy[i,] = x[i]
    REnt <- mean(log(target(yy, target_param)))# E_{p}(log f)
  } 
  else REnt <- mean(log(target(x, target_param))) # E_{p}(log f)
  Kb <- Entp - REnt
  return( list(Kb=Kb, Entp=Entp))	
}



# method=="Resubst" Re-substitution
Entropy.Resubst <- function(x, target, target_param){
	nmc <- dim(x)[1]; d <- dim(x)[2]
	h <- mvbw(x) 	# multivariate bandwidth matrix
	hatp <- mvkde(x, bw=h) # kde of p^t at x_i, i=1,...,nmc
	Entp <- mean(log(hatp)) # entropy
	REnt <- mean(log(target(x, target_param))) # E_{p}(log f)
	Kb <- Entp - REnt
	return( list(Kb=Kb, Entp=Entp))	
	}


####################### EntropyParallel #######################
# simulation of MCMC "in parallel" i.e. for each "slices of time" t
# the nmc next steps for each copy of the chain are generated,
# then the Entropy is computed, and the past is forgotten
# so probably only non-adaptive Markov chains or adaptive chains for which
# the past can be summarized by some sufficient statistics will be eligible 
# unless the option keep.all=TRUE, in which case the entire "cube of simulation"
# is saved in the array Ptheta(n,d,nmc), such as the one
# returned by MCMCcopies()
# list of input parameters matches that of MCMCcopies for consistency
#
# d = space dimension of theta
# n = length of (time) iterations of each single chain
# nmc = nb of iid copies
# Ptheta0 = (nmc,d) matrix of "P"arallel theta0 values
#
# mcmc_algo is the algorithm definition, with name, chain, step, etc
#
# Note : when f is only known up to a multiplicative cte
# this function must be called for 2 mcmc_algo to take the difference
# between 2 Kullback estimates
#
# ToDo: permettre de continuer des simulations ?? partir de
# l'??tat n pour aller de n ?? n' qd on constate que la convergence 
# n'a pas eu lieu ?? n;
# possible aussi pour MCMCcopies, en passant la derniere ??tape
# comme Ptheta0 et ??crire une ft de concat??nation d'objets
# de classe plMCMC (cf bootstrap pour Waterdata...)
#
# Note: currently only "Nearest.Neighbor"  and "k.Nearest.Neighbor" methods are imlemented 

EntropyParallel <- function(mcmc_algo, n=100, nmc=10, Ptheta0,
						target, f_param, q_param, 
						method="A.Nearest.Neighbor", k=1, trim=0.02, 
						keep.all=FALSE, verb=TRUE, EntVect = FALSE) {
  # if (method=="A.Nearest.Neighbor") require(RANN) # removed since in Depends
	d <- dim(Ptheta0)[2]
	# all.f=TRUE # probably deprecated option for Gyorfi method
	if (keep.all) Ptheta <- array(0,c(n,d,nmc)) else Ptheta <- NULL
	if (method != "Nearest.Neighbor" && method != "k.Nearest.Neighbor" && method != "A.Nearest.Neighbor" ) {
		cat("only methods = Nearest.Neighbor, k.Nearest.Neighbor and A.Nearest.Neighbor are imlemented,\n")
		cat("running with that method.\n")
		method <- "A.Nearest.Neighbor"
		}
	q=round(n/10); pc <- 0 # for verbose print of % done over n
	if (q==0) q <- 1
	if (n < 20 && verb) {
		cat("verbose mode disabled for n<20 iterations.\n")
		verb <- FALSE}
		
	Stheta <- Ptheta0	# "Slice" of theta's at any time t (nmc,d)
	Kb1 <- rep(0,n); Entp <- rep(0,n) # for storing entropy results
	if(EntVect == TRUE) {Kb1k <- Entpk <- matrix(0,n,k)} # for storing results
	else {Kb1k <- Entpk <- matrix(0,n,1)}
	
	# new.Stheta <- array(0,nmc,d)	# updated slice at it+1
	pba <- 0 # overall chains proba & # of acceptation, when meaningful
	nba=0
	for (it in 1:n) {	# time-dependent loop over time iterations
		for (i in 1:nmc) {	# loop over parallel chains
			# NOTE: these independent nmc processes may be parallelized!!
			theta <- Stheta[i,]
			upd <- mcmc_algo$step(Stheta[i,], target,
							mcmc_algo$q_pdf, mcmc_algo$q_proposal,
							f_param, q_param, nba)
			Stheta[i,] <- upd$theta_new # update for the ith chain
			nba <- upd$nba
			}
		if (keep.all) { # stores slice for time it; remove for parallel version
			if (d==1) Ptheta[it,,] <- Stheta else Ptheta[it,,] <- t(Stheta)
			}
		## now entering Entropy & Kullback estimation on "slice it"
		if (method=="Nearest.Neighbor") {
			ep <- Entropy.NN(Stheta, target, f_param)
			Kb1[it] <- ep$Kb; Entp[it] <- ep$Entp
		}	
	  if (method == "k.Nearest.Neighbor"){
	    if(EntVect == TRUE) ep <- Entropy.kNN.vect(Stheta, target, f_param, k)
	    else  ep <- Entropy.kNN(Stheta, target, f_param, k)
	    Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
	  }
	  if (method == "A.Nearest.Neighbor"){
	    if(EntVect == TRUE) ep <- Entropy.ANN.vect(Stheta, target, f_param, k)
	    else  ep <- Entropy.ANN(Stheta, target, f_param, k)
	    Kb1k[it,] <- ep$Kb; Entpk[it,] <- ep$Entp
	  }

		# switch(method,) # ToDO! pb avec Gyorfi et subsampling...
		# once in indices or for each time iteration?

		# % done printout
		b=round(it/q); r=it-b*q
		if (r==0 & verb) {
		  pc <- pc+10; 
		  if (EntVect == FALSE) cat(pc,"% done, Kullback at ", k, "th Nearest Neighbor = ", Kb1k[it,],"\n")
		  else  cat(pc,"% done, Kullback at ", k, "th Nearest Neighbor = ", Kb1k[it,k],"\n")}
		} # it loop
	pba <- nba/(n*nmc)
	
	# Output object	of class KbMCMC like EntropyMCMC
	# with Ptheta set a NULL if keep.all=FALSE

	if (method=="k.Nearest.Neighbor" || method=="A.Nearest.Neighbor" ) {
	  return(structure(list(Kullback=Kb1k, Entp=Entpk, nmc=nmc, dim=d,
	                        algo=mcmc_algo$name, target=target, method=method, k=k,
	                        f_param=f_param, q_param=q_param, prob.accept=pba, Ptheta=Ptheta),
	                   class="KbMCMC"))	  
	}
	else{
	  return(structure(list(Kullback=Kb1, Entp=Entp, nmc=nmc, dim=d,
	                        algo=mcmc_algo$name, target=target, method=method,
	                        f_param=f_param, q_param=q_param, prob.accept=pba, Ptheta=Ptheta),
	                   class="KbMCMC"))
	}	
	}
