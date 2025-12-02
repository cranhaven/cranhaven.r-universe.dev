## simulate copies of MCMC, chain by chain, ie loop is on nb of chains
## see also the multicore version of this in MCMCcopies.mc
## Note: a more "Parallel" MCMC simulation structure 
## simulating by time slices t=1,...,n and computing entropy(t)
## is defined in EntropyParallel()

# mcmc_algo is the user-defined MCMC to study, which must be
# defined like the ones in Algo_defs.R, i.e. a list holding
# definitions for the required functions, such as
# mcmc_algo$chain(theta0,n, target, f_param, q_param)
# see RWHM_algo for an example; functions from the MCMCpack 
# package can also be called by complying to this definition
# d = space dimension of theta
# n = nb of (time) iterations of each single chain
# nmc = nb of iid copies
# Ptheta0 = (nmc,d) matrix of "P"arallel theta0 values
# output = object of class "plMCMC", in which
# Ptheta holds the nmc copies of chains in an 
# array(n,d,nmc) of simulated values
# where 1st value (1,d,nmc) is Ptheta0

# the finally retained name is MCMCcopies, 
# whence EntropyParallel will be for 
# the "parallel code" forgetting the past after entropy calculation
# so just a Entropy computation ft will be needed for the end user

MCMCcopies <- function(mcmc_algo, n=100, nmc=10, Ptheta0,
						target, f_param, q_param, verb=TRUE){
	d <- dim(Ptheta0)[2]
	Ptheta <- array(0,c(n,d,nmc))
	Ptheta[1,,] <- Ptheta0 # stores inits in output array
	pba <- 0 # overall chains proba of acceptation
	# loop over nb of // chains
	q=round(nmc/10); pc <- 0 # for verbose print of % done
	if (q==0) q <- 1
	if (nmc < 20 && verb) {
		cat("verbose mode disabled for nmc<20 chains.\n")
		verb <- FALSE}
	for (i in 1:nmc){
		theta0 <- Ptheta0[i,]
		simu <- mcmc_algo$chain(theta0, n, target, f_param, q_param)
		# if (verb) cat("mcmc",i,"done\n")
		Ptheta[,,i] <- simu$theta # stores (n,d) matrix of sim
		pba <- pba + simu$paccept
		# % done
		b=round(i/q); r=i-b*q
		if (r==0 & verb) {pc <- pc+10; cat(pc,"% done\n")}
		}
	return(structure(list(Ptheta=Ptheta, prob.accept=pba/nmc,
						algo=mcmc_algo$name, target=target,
						f_param=f_param, q_param=q_param),
						class="plMCMC"))
	}


## random draws for initialization
# nmc = nb of parallel chains = nb of init points
# d = space dimension
# initpdf = R random generator 
# for now must be in ("rnorm" , "runif")
# ... parameters passed to initpdf
# return: a matrix(nmc,d) where each row is a d-dim point
DrawInit <- function(nmc=1, d=2, initpdf="rnorm",...){
	if (initpdf=="rnorm"){
		theta0 <- matrix(rnorm(nmc*d,...),nrow=nmc,ncol=d)
		}
	if (initpdf=="runif"){
		theta0 <- matrix(runif(nmc*d,...),nrow=nmc,ncol=d)
		}
	theta0
	}
## ToDo: add a way to pass a given point for mean,
## other than mean=c(m,m)


## summary method for plMCMC object
# stats = TRUE to print additional summary statistics
# ... to comply to CRAN guidelines
summary.plMCMC <- function(object, stats=FALSE, ...){
  NextMethod("summary")
	n <- dim(object$Ptheta)[1]
	d <- dim(object$Ptheta)[2]
	nmc <- dim(object$Ptheta)[3]
	cat("-- Summary for Parallel MCMC (plMCMC) object --\n")
	cat("MCMC algorithm called:", object$algo,"\n")
	cat("state space dimension:",d,"\n")
	cat("number of iterations (including starting point):",n,"\n")
	cat("number of parallel chains:",nmc,"\n")
	cat("overall empirical rate of acceptation:", object$prob.accept,"\n")
	if (stats) {
		# concatenates all simulations together in a d-col matrix
		cat("-- summary statistics over all chains --\n")
		alltheta <- CollectChains(object)
		print(summary(alltheta))
		sd <- apply(alltheta, 2, sd)
		cat("sd :", sd, "\n")
		}
	}
	
## concatenates all simulated copies together in a (n,d) matrix
# s = plMCMC object
CollectChains <- function(s){
	n <- dim(s$Ptheta)[1]
	d <- dim(s$Ptheta)[2]
	nmc <- dim(s$Ptheta)[3]
	t2 <- aperm(s$Ptheta,c(2,1,3))
	alltheta <- matrix(t2, nrow=nmc*n, ncol=d, byrow=T)
	}