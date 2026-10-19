#' This function reduces the sample to its effective size
#'
#' When plotting, we want to show only a few representative
#' lines or points derived from a sample. This function will determine
#' the auto-correlation length very roughly and use that number to
#' thin out the sample to a minimal size that still represents the
#' original sample well.
#'
#' @param S an MCMC sample
#' @param L the log-likelihood values of S
#' @param verbose when TRUE the acf plot option is set to TRUE, and
#'     the found auto-correlation length is printed.
#' @examples
#' S <- matrix(rnorm(300),100,3)
#' ## the next line fakes auto-correlation:
#' attr(S,"logLikelihood") <- cos(seq(0,1,length.out=100)) + rnorm(100,sd=0.05)
#' print(dim(S))
#' print(dim(small(S)))
#' @export
#' @return a smaller version of S
small <- function(S,L=attr(S,"logLikelihood"),verbose=getOption("uqsa.verbose",default=interactive())){
	if (is.numeric(L)){
		ACF <- acf(L,plot=verbose)$acf
	} else {
		ACF <- rowMeans(apply(S,2,\(s) acf(s,plot=FALSE)$acf))
	}
	tau <- ceiling(sum(ACF[ACF>exp(-2)]))
	if (verbose) cli::cli_alert_info("auto-correlation: {tau}")
	N <- NROW(S)
	i <- seq(1,N,by=2*tau)
	S <- S[i,]
	if (is.numeric(L)) attr(S,"logLikelihood") <- L[i]
	return(S)
}

#' This function can be used to specify default values
#'
#' When attributes are missing, the `base::attr()` function returns
#' NULL. In those cases this function can be used to find an
#' alternative value in one expression:
#' `attr(x,"dim") %otherwise% length(x)`
#' @param a value to check for NULL
#' @param b value to substitute
#' @return a, or b if a is NULL
#' @export
#' @examples
#' x <- numeric(10)
#' l <- dim(x) %otherwise% c(length(x),1)
#' ## example with attributes:
#' attr(x,"logLikelihood") <- -980
#' ## elsewhere:
#' logLF <- attr(x,"logLikelihood") %otherwise% -Inf
`%otherwise%` <- function(a,b){
	if (is.null(a) || any(is.na(a)) || length(a)==0) {
		return(b)
	} else {
		return(a)
	}
}

#' Determine a prefix from a character vector str of similar contents
#'
#' The result is such that `all(startsWith(str,determinePrefix(str)))`
#' is `TRUE`.
#'
#' By default, the strings are assumed to be '-' separated words, and
#' a series of words is found to be the prefix if all entries start
#' with that set of words.
#'
#' The normal case is c("abc-1","abc-2b","abc-2a") maps to "abc"
#' @export
#' @param str a character vector
#' @param split the token to use for strsplit instead of '-', this
#'     should be `character(0)` if you want to split letter by letter
#' @param collapse the words constituents in the input that are found
#'     to be uniform in the input are connected via [paste] and this "collapse"
#'     value.
#' @return the prefix common to all entries of str.
#' @examples
#' files <- sprintf("smmala-sample-%i-of-3.RDS",seq(1,3))
#' pref <- determinePrefix(files)
#' print(pref)
determinePrefix <- function(str,split="-",collapse="-"){
return(paste(
	Reduce(
		function(a,b) {
			m<-seq(min(length(a),length(b)));
			a<-a[m]; b<-b[m];
			i<-which(unlist(mapply(identical,a,b)));
			return(a[i])
		},
		strsplit(str,split)),
		collapse=collapse)
	)
}

#' checks whether a variable has the named attributes
#'
#' @param var a variable to check for attributes
#' @param attrNames named attributes
#' @return TRUE if all attributes are present
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAP79"))
#' x <- values(m$Compound)
#' x %has% "unit"
#' print(x %@% "unit")
`%has%` <- function(var,attrNames){
	return(all(attrNames %in% names(attributes(var))))
}

#' Initialize the Markov chain
#'
#' This function must append all required attributes to the MCMC
#' variable, for the Markov chain to update correctly.
#'
#' @param beta inverse temperature for the Markov chain (parallel
#'     tempering)
#' @param parMCMC a plain starting value for the Markov chain
#' @param simulate a closure that maps the MCMC variable to simulation
#'     results (the simulation experiments are enclosed in this
#'     function).
#' @param logLikelihood a function that maps simulations to
#'     logLikelihood values
#' @param dprior density of the prior distribution
#' @param gradLogLikelihood the gradient function of the logLikelihood
#'     (optional) -- only if the algorithm requires it
#' @param gprior the gradient pf the log-prior (for SMMALA and similar
#'     algorithms).
#' @param fisherInformation a function that calculates the Fisher
#'     Information matrix
#' @return the same starting parameter vector, but with attributes.
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' p0 <- values(m$Parameter)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simfi(ex,o)
#' dprior <- dNormalPrior(p0,m$Parameter$stdv)
#' p <- mcmc_init(1.0,p0,s,dprior=dprior)
#' print(names(attributes(p))) ## now has attributes necessary for MCMC
mcmc_init <- function(beta,parMCMC,simulate,logLikelihood=ll,dprior=\(x) prod(rnorm(x)),gradLogLikelihood=NULL,gprior=NULL,fisherInformation=NULL){
	if (is.matrix(parMCMC)) parMCMC <- as.numeric(parMCMC)
	attr(parMCMC,"beta") <- beta
	attr(parMCMC,"simulations") <- simulate(parMCMC)
	attr(parMCMC,"prior")  <- dprior(parMCMC)
	attr(parMCMC,"logLikelihood") <- logLikelihood(parMCMC)
	if(!is.null(fisherInformation))
		attr(parMCMC,"fisherInformation") <- fisherInformation(parMCMC)
	if(!is.null(gradLogLikelihood))
		attr(parMCMC,"gradLogLikelihood") <- gradLogLikelihood(parMCMC)
	if(!is.null(gprior))
		attr(parMCMC,"gradLogPrior") <- gprior(parMCMC)
	class(parMCMC) <- "mcmcVariable"
	return(parMCMC)
}

#' Should 2 Markov chains exchange their temperatures
#'
#' This function makes a Boolean choice about changes in temperature,
#' based on the log(likelihood) values of two Markov chains in a
#' parallel tempering setting. The outcome is stochastic.
#'
#' This function is useful if `mpi.send()` and `mpi.recv()` are used.
#'
#' @param b1 the inverse temperature of chain 1
#' @param ll1 the log-likelihood of chain 1
#' @param b2 the inverse temperature of chain 2
#' @param ll2 the log-likelihood of chain 2
#' @return TRUE is the chains should swap their temperatures
#' @export
#' @examples
#' b <- c(1.0,0.5)
#' if (change_temperature(b[1],-850,b[2],-600)){ # with some randomness
#'   message(sprintf("yes, swapping temperature %f <=> %f",b[1],b[2]))
#' } else {
#'   message(sprintf("no, temperature %f, and %f stay unchanged",b[1],b[2]))
#' }
change_temperature <- function(b1,ll1,b2,ll2){
	a <- exp((b2-b1)*(ll1-ll2))
	r <- runif(1)
	return(r<a)
}

#' Markov Chain Monte Carlo
#'
#' This function creates an MCMC function for a given set of
#' experiments. The Markov chains have no communication between them
#' if more than one is created using this mechanism.
#'
#' The algorithm is entirely determined by the update function.  Any
#' intermediate values that updates requires aside from simulation
#' results have to be attributes of the MCMC variable: parMCMC.
#'
#' The update function: update(parGiven) -> parUpdate depends only on
#' the given parameters, all other dependencies have to be either
#' implicit (as a closure) or attributes of parGiven.
#'
#' @param update and update function
#' @param verbose prints a progress bar when TRUE
#' @return M(initPar,N), a function of initial starting values and
#'     number of Markov chain steps
#' @export
#' @examples
#'  m <- model_from_tsv(uqsa_example("AKAP79"))
#'  rwm <- high_level_metropolis(m) # "random walk", metropolis algorithm
#'  p <- rwm %@% "init"             # a valid starting point
#'  if (interactive()){
#'    smallSample <- rwm(rwm %@% "init",500,1e-4)
#'    pairs(smallSample[,seq(6)])
#'  } else {
#'    smallSample <- rwm(rwm %@% "init",10,1e-4)
#'  }
mcmc <- function(update, verbose=getOption("uqsa.verbose", interactive())){
	M <- function(parMCMC,N=1000,eps=1e-4){
		sample <- matrix(NA,nrow=N,ncol=length(parMCMC))
		colnames(sample) <- names(parMCMC)
		ll <- numeric(N)
		b <- numeric(N)
		a <- logical(N)
		A <- 0
		if (verbose) {
			cli::cli_progress_bar("mcmc",total=N)
		}
		for (i in seq(N)){
			parMCMC <- update(parMCMC,eps)
			ll[[i]] <- attr(parMCMC,"logLikelihood")
			sample[i,] <- as.numeric(parMCMC)
			b[i] <- attr(parMCMC,"beta")
			a[i] <- attr(parMCMC,"accepted")
			A <- ((i-1)*A + a[i])/i
			if (verbose && i%%10==0) {
				cli::cli_progress_update(inc=10, status=sprintf("a: %i %%",round(A*100)))
			}
		}
		if (verbose) cli::cli_progress_done()
		attr(sample,"acceptanceRate") <- mean(a)
		attr(sample,"acceptance") <- a
		attr(sample,"logLikelihood") <- ll
		attr(sample,"lastPoint") <- parMCMC
		attr(sample,"beta") <- b
		attr(sample,"stepSize") <- eps
		return(sample)
	}
	comment(M) <- "function(parMCMC,N=1000,eps=1e-4) where eps is the step-size (numeric scalar)"
	return(M)
}

#' Broadcast to other ranks and swap temperatures with any of them
#'
#' Using this function, at most two ranks will swap.
#'
#' Given a current log-likelihood, temperature and step-size, this
#' function will broadcast a log-likelihood value to all other ranks
#' and they can each decide to swap temperatures with the root
#' process. Root is cycled around all ranks (round-robin).
#'
#' Each other rank is allowed to make the
#' offer to swap. The root process decides which rank to swap with.
#'
#' @param i MCMC iteration (for round-robin rank selection)
#' @param B inverse temperature (parallel tempering)
#' @param LL log-likelihood value of current point
#' @param H algorithm's step size (often called epsilon in literature)
#' @param r MPI rank
#' @param comm MPI communicator
#' @param cs MPI comm size
#' @noRd
#' @return a list: `list(B,LL,H)` with the new (or unchanged) values
#' @examples
#' ## only in an MPI context
#' \dontrun{
#'   pbdMPI_bcast_reduce_temperatures(
#'     i,
#'     betaValue,
#'     LogLikelihoodValueL,
#'     CurrentStepSize,
#'     rank,
#'     mpi_comm,
#'     mpi_comm_size
#'   )
#' }
pbdMPI_bcast_reduce_temperatures <- function(i, B, LL, H, r, comm, cs){
	if (!requireNamespace("pbdMPI")) stop("this functions requires that pbdMPI is installed.")
	root <- (i %% cs) # rank of root process
	rootLL <- pbdMPI::bcast(LL,root,comm)
	rootB  <- pbdMPI::bcast(B,root,comm)
	rootH  <- pbdMPI::bcast(H,root,comm)
	if (root != r && change_temperature(B,LL,rootB,rootLL)){ # each _other_ process can propose this
		msg <- r
	} else {
		msg <- -1
	}
	xr <- pbdMPI::allreduce(msg,op="max",comm=comm) # now everyone knows which two will swap
	ret <- list(B=B,LL=LL,H=H)
	if (xr>=0 && xr < cs){ # do the swap
		if (r == root){
			xrB  <- pbdMPI::recv(rank.source = xr, tag = 1, comm = comm)
			xrLL <- pbdMPI::recv(rank.source = xr, tag = 2, comm = comm)
			xrH  <- pbdMPI::recv(rank.source = xr, tag = 3, comm = comm)
			ret <- list(B=xrB,LL=xrLL,H=xrH)
		} else if (r == xr){
			pbdMPI::send(B,rank.dest = root, comm = comm, tag = 1)
			pbdMPI::send(LL,rank.dest = root, comm = comm, tag = 2)
			pbdMPI::send(H,rank.dest = root, comm = comm, tag = 3)
			ret <- list(B=rootB,LL=rootLL,H=rootH)
		}
	}
	return(ret)
}


#' The MPI version of the mcmc function
#'
#' this version of the MCMC function returns a Markov chain closure
#' that assumes that it is being run in an MPI context: R was launched
#' in an MPI context, e.g. using
#' ```
#' mpirun -H localhost:8 -N 8 Rscript ...
#' ```
#' and the pbdMPI package is installed. The chains shall
#' communicate using the provided `comm` object.
#'
#' This function is intended for use within a parallel tempering
#' approach and MPI. For trivial parallelization (many chains), this
#' is not at all required, only a random number seed for each worker.
#'
#' It is possible to supply a custom swap function, with the interface:
#' ```
#' swapFunc <- function(i, B, LL, H, r, comm, cs)
#' ```
#' where `i` is the current iteration (for round robin rank choices),
#' `B` is the current beta value, `LL` the current log-likelihood (scalar)
#' and `H` the current step-size (scalar); `r`, `comm`, and `cs` are
#' the MPI rank, comm, and comm-size. The swap function returns a list:
#' `list(B=,LL=,H=)` with the updated values (after swapping) or the
#' old values if the swap was rejected.
#'
#' @param update an update function
#' @param comm an mpi comm which this function will use for
#'     send/receive operations
#' @param swapDelay swaps will be attempted every 2*swapDelay+1
#'     iterations [deprecated]
#' @param swapFunc can be a custom function that does the MPI
#'     communication and decides whether or nopt to swap temperatures
#' @return an mcmc closure m(parMCMC,N,eps) that implicitly uses the
#'     supplied update function
#' @export
#' @examples
#' ## works only in an MPI context (R session started with `mpirun Rscript ...`)
#' ## similar to mcmc without _mpi prefix
#' \dontrun{
#'   ## prepare the update functions
#'   pt_mcmc <- mcmc_mpi(update, comm, swapDelay=0, swapFunc=pbdMPI_bcast_reduce_temperatures)
#' }
mcmc_mpi <- function(update, comm, swapDelay=0, swapFunc=pbdMPI_bcast_reduce_temperatures){
	if (!requireNamespace("pbdMPI")) stop("this functions requires that pbdMPI is installed.")
	D <- max(2*swapDelay+1,1)
	M <- function(parMCMC,N=1000,eps=1e-4){
		r <- attr(comm,"rank") # 0..n-1
		cs  <- attr(comm,"size")
		sample <- matrix(nrow=N,ncol=length(parMCMC))
		colnames(sample) <- names(parMCMC)
		ll <- numeric(N)
		b <- numeric(N)
		a <- logical(N)
		swaps <- logical(N)
		h <- numeric(N)
		for (i in seq(N)){
			parMCMC <- update(parMCMC,eps)
			sample[i,] <- as.numeric(parMCMC)
			LL <- attr(parMCMC,"logLikelihood")
			B <- attr(parMCMC,"beta")
			a[i] <- attr(parMCMC,"accepted")
			ll[i] <- LL
			b[i]  <- B
			h[i] <- eps
			for (j in seq(0,cs-1)){
				res <- swapFunc(j,B,LL,eps,r,comm,cs)
				B <- res$B
				LL <- res$LL
				eps <- res$H
			}
			attr(parMCMC,"beta") <- B
			attr(parMCMC,"logLikelihood") <- LL
			swaps[i] <- (B != b[i])
		}
		attr(sample,"acceptanceRate") <- mean(a)
		attr(sample,"acceptance") <- a
		attr(sample,"logLikelihood") <- ll
		attr(sample,"lastPoint") <- parMCMC
		attr(sample,"beta") <- b
		attr(sample,"swapRate") <- mean(swaps)
		attr(sample,"swaps") <- swaps
		attr(sample,"stepSize") <- h
		return(sample)
	}
	comment(M) <- "function(parMCMC, N=1000, eps=1e-4) where eps is the step size, and N the sample size"
	return(M)
}

#' This function merges mpi-samples into one
#'
#' When using MPI, we save the sample immediately into a file, each
#' rank saves to its own file. This function is basically a wrapper
#' with several calls to `Reduce`, it collects all of these smaller
#' samples into one.
#'
#' The samples should have been saved with `saveRDS()`. This function
#' extracts the attributes that MPI sampling typically attaches to a
#' sample. The sample itself and all of these attributes are returned
#' as a list.
#'
#' If the samples contain different temperatures, then no attempt is
#' made to untangle or sort them.
#'
#' NOTE: If the big result-sample doesn't fit into memory, this
#' function will crash. Samples can be quite large, depending on the
#' problem size.
#'
#' @export
#' @param files the rds files where the individual samples are stored
#' @param verbose logical, when FALSE nothing will be printed on screen
#' @return a list of named items, with `$Sample` representing one
#'     matrix where all file-samples are concatenated (with rbind).
#' @examples
#' rprior <- rNormalPrior(seq(3),seq(4,5)) # some nonsense
#' N <- 100
#' f <- c(tempfile(),tempfile())
#'
#' ## first fake sample
#' X <- rprior(N)
#' attr(X,"beta") <- sample(1/seq(2)^2,N,replace=TRUE)
#' attr(X,"acceptanceRate") <- 0.23
#' attr(X,"swapRate") <- 0.1
#' attr(X,"logLikelihood") <- rnorm(N,-100,30)
#' saveRDS(X,file=f[1])
#'
#' ## second fake sample
#' X <- rprior(N)
#' attr(X,"beta") <- sample(1/seq(2)^2,N,replace=TRUE)
#' attr(X,"acceptanceRate") <- 0.23
#' attr(X,"swapRate") <- 0.1
#' attr(X,"logLikelihood") <- rnorm(N,-100,30)
#' saveRDS(X,file=f[2])
#'
#' Z <- loadSample_mpi(f)
#' print(dim(Z$Sample))
#' print(names(Z))
loadSample_mpi <- function(files,verbose=getOption("uqsa.verbose", interactive())){
	s <- lapply(files,readRDS)
	betaTrace <- Reduce(function(a,b) c(a,attr(b,"beta")),s,init=NULL)
	uB <- sort(unique(betaTrace),decreasing=TRUE)
	bSelection <- lapply(uB,function(b) abs(betaTrace-b)<1e-10)
	acc <- Reduce(function(a,b) c(a,attr(b,"acceptanceRate")),s,init=NULL)
	sR <- Reduce(function(a,b) c(a,attr(b,"swapRate")),s,init=NULL)
	ll <- Reduce(function(a,b) c(a,attr(b,"logLikelihood")),s,init=NULL)
	if (verbose){
		cli::cli_alert_info("loading sample files with acceptances: {acc}")
	}
	Sample <- Reduce(rbind,s)
	return(list(Sample=Sample,beta=betaTrace,acceptanceRate=acc,swapRate=sR,logLikelihood=ll,betaSelection=bSelection,uB=uB))
}

#' gatherSample collects all sample points, from all files, with the
#' given temperature
#'
#' This function assumes that each supplied RDS file contains a matrix
#' of model MCMC parameters, with an attribute called "beta" that
#' lists the temperature of each row.
#'
#' This function selects and collects all rows, from all files with
#' the same (given) temperature.
#'
#' This function should be used if you need to inspect only one of the
#' temperatures, not all of them. This function is similar to
#' [loadSample_mpi], which returns all temperatures. But, whereas
#' [loadSample_mpi] returns a list, this function returns the
#' sample-matrix itself (because the result of this function is conceptually
#' similar to sampling on one node, with one temperature).
#'
#' @export
#' @param files a list of file names
#' @param beta the inverse temperature to extract sample for
#' @param size a size the is smaller than the actual sample size, if
#'     left unchanged, all sampled points are returned
#' @return a matrix of sampled points, all with the same temperature
#' @examples
#' rprior <- rNormalPrior(seq(3),seq(4,5)) # some nonsense
#' N <- 100
#' f <- c(tempfile(),tempfile())
#'
#' ## first fake sample
#' X <- rprior(N)
#' attr(X,"beta") <- sample(1/seq(2)^2,N,replace=TRUE)
#' attr(X,"acceptanceRate") <- 0.23
#' attr(X,"swapRate") <- 0.1
#' attr(X,"logLikelihood") <- rnorm(N,-100,30)
#' saveRDS(X,file=f[1])
#'
#' ## second fake sample
#' X <- rprior(N)
#' attr(X,"beta") <- sample(1/seq(2)^2,N,replace=TRUE)
#' attr(X,"acceptanceRate") <- 0.23
#' attr(X,"swapRate") <- 0.1
#' attr(X,"logLikelihood") <- rnorm(N,-100,30)
#' saveRDS(X,file=f[2])
#'
#' Z <- gatherSample(f,beta=1)
#' print(N)
#' print(dim(Z)) ## should be c(2*N,3)
#' print(names(attributes(Z)))
gatherSample <- function(files,beta=1.0,size=NA){
	x <- numeric(0)
	lL <- numeric(0)
	H <- numeric(0)
	for (f in files){
		s <- readRDS(f)
		b <- attr(s,"beta")
		l <- attr(s,"logLikelihood")
		h <- attr(s,"stepSize")
		i <- which(abs(b - beta) <= 1e-9*beta + 1e-15)
		s <- s[i,,drop=FALSE]
		l <- l[i]
		if (!is.null(h) && length(h) == length(b)) {
			h <- h[i]
		}
		x <- rbind(x,s)
		lL <- c(lL,l)
		H <- c(H,h)
		if (!any(is.na(size)) && NROW(x)>=size) break
	}
	attr(x,"beta") <- beta
	attr(x,"logLikelihood") <- lL
	attr(x,"stepSize") <- H
	return(x)
}


#' Collect statistical Replicas
#'
#' `gatherReplicas` collects all sample-points, from all files, which
#' are assumed to be exact replicas. Replicas have different random
#' number seeds (and possibly sample sizes).
#'
#' This function uses mclapply to process the files, which may be
#' quicker than `gatherSample`.  The temperature `beta` is
#' disregarded, assuming that no parallel tempering was used.  To
#' facilitate the loading of a very big sample, this function will
#' analyze the auto-correlation within each file and returned a
#' thinned sub-sample of size `N/(2*tau_int)` (returning the effective
#' sample size). The value of tau_int is calculated on the likelihood
#' values, either with the hadron package, or the bultin `acf`
#' function. There is no need to further reduce the result.
#'
#' For small samples, it is better to load the entire sample and
#' analyze it in full. This function is intended for samples that are
#' so big that they challenge the memory of the machine.
#'
#' This function is quicker if you have used trivial parallelism,
#' _without_ MPI communication between the ranks (or another method of
#' obtaining several replicas, like forking or sequential repetition).
#'
#' This function assumes that each supplied RDS file contains a matrix
#' of model MCMC parameters. The returned value `X` will be similar to effect of
#' `Reduce(...,rbind)` of all the smaller samples contained in the
#' individual files. The value `X` will have several attributes attached
#' to it:
#'
#' - logLikelihood: log(likelihood(X\[i,\])), one value per row of X
#' - stepSize: the MCMC step size used in each given file#'
#'
#' @export
#' @param files a list of file names
#' @return a Sample matrix, with effective sample size
#'     (auto-correlation thinned)
#' @examples
#' rprior <- rNormalPrior(seq(3),seq(4,5)) # some nonsense
#' N <- 100
#' f <- c(tempfile(),tempfile())
#'
#' ## first fake sample
#' X <- rprior(N)
#' attr(X,"acceptanceRate") <- 0.23
#' ## fake auto-correlation
#' attr(X,"logLikelihood") <- sqrt(seq(N)) + rnorm(N,-100,3)
#' saveRDS(X,file=f[1])
#'
#' ## second fake sample
#' X <- rprior(N)
#' attr(X,"acceptanceRate") <- 0.23
#' attr(X,"logLikelihood") <- sqrt(seq(N)) + rnorm(N,-100,3)
#' saveRDS(X,file=f[2])
#'
#' Z <- gatherReplicas(f)
#' print(N)
#' print(dim(Z))
#' print(names(attributes(Z)))
gatherReplicas <- function(files){
	tau <- lapply(
		lapply(
			parallel::mclapply(files,readRDS),
			attr,"logLikelihood"
		),
		function(l) {
			res <- acf(l,lag.max=round(length(l)/2),plot=FALSE)
			j <- min(which(res$acf < 0.2))
			tauint <- sum(res$acf[seq(j)])
			return(tauint)
		}
	)
	x <- Reduce(
		rbind,
		parallel::mcmapply(
			function(x,tau){
				return(x[seq(1,NROW(x),by=ceiling(tau)),])
			},
			parallel::mclapply(files,readRDS),
			tau
		),
		init=numeric(0)
	)
	l <- unlist(
		parallel::mcmapply(
			function(l,tau){
				return(l[seq(1,length(l),by=ceiling(tau))])
			},
			lapply(parallel::mclapply(files,readRDS),attr,"logLikelihood"),
			tau
		)
	)
	h <- sapply(parallel::mclapply(files,readRDS),attr,"stepSize")
	attr(x,"logLikelihood") <- l
	attr(x,"stepSize") <- h
	attr(x,"tau") <- as.data.frame(Reduce(c,tau))
	return(x)
}


#' checks whether a given matrix is a valid, invertible fisherInformation
#'
#' This matrix has to be symmetric and invertible. But, because the
#' matrix has a perhaps sketchy origin, it could be defective in all
#' possible ways.
#'
#' @noRd
#' @param G a matrix
#' @param abs_tol absolute tolerance for the reciprocal condition number of G
#' @return TRUE or FALSE
#' @examples
#' A <- matrix(rnorm(9),3,3)
#' print(is.invertible(A))
#' B <- t(A) + A
#' print(is.invertible(B))
is.invertible <- function(G=NULL,abs_tol=1e-11){
	return(!is.null(G) && is.numeric(G) && is.matrix(G) && all(dim(G)>0) && !any(is.na(G)) && all(is.finite(G)) && isSymmetric(G) && rcond(G) > abs_tol)
}

dmvnorm <- function(x,mean,precision){
	mu <- mean
	stopifnot(!is.null(x) && is.numeric(x) && is.numeric(mu) && length(x) == length(mu))
	stopifnot(!is.null(precision) && is.matrix(precision))
	k <- length(mu)
	stopifnot(dim(precision) == c(k,k))
	xm <- (x-mu)
	xmPxm <- (x-mu) %*% (precision %*% (x-mu))
	C <- (2*pi)^(-0.5*k) * sqrt(abs(det(precision))) * exp(-0.5 * xmPxm)
	return(C)
}

rmvnorm <- function(mean,precision){
	mu <- mean
	stopifnot(!is.null(precision) && !is.null(mu))
	stopifnot(is.numeric(mu) && is.matrix(precision))
	stopifnot(all(is.finite(mu)))
	stopifnot(all(is.finite(precision)))
	k <- length(mu)
	P <- chol(precision) # chol(0.5*(t(precision) %*% precision))
	x <- solve(P,rnorm(k,0,1))+mu
	return(x)
}

#' SMMALA move
#'
#' The Simiplified Manifold Metropolis Adjusted Langevin Algorithm uses a
#' move instriction that uses a Gaussian kernel that is shifted away
#' from the current point
#'
#' @noRd
#' @param beta inverse temperature (parallel tempering)
#' @param parGiven given point
#' @return SMMALA proposal point
smmala_move <- function(beta,parGiven,fisherInformationPrior,eps=1e-2){
	stopifnot(parGiven %has% c("fisherInformation","gradLogLikelihood","gradLogPrior"))
	stopifnot(!is.null(beta) && !any(is.na(beta)) && is.finite(beta))
	fiGiven <- parGiven %@% "fisherInformation"
	gradLGiven <- parGiven %@% "gradLogLikelihood"
	gradPGiven <- parGiven %@% "gradLogPrior"
	G0 <- fisherInformationPrior
	G <- (beta^2*fiGiven)+G0
	stopifnot(!is.null(G) && is.matrix(G))
	if (isTRUE(is.invertible(G))){
		g <- solve(G,as.numeric(beta*gradLGiven+gradPGiven))
	} else {
		stopifnot(is.matrix(G0) && isSymmetric(G0))
		g <- solve(G0,gradPGiven)
		G <- G0
	}
	parProposal <- rmvnorm(
		mean=parGiven+0.5*eps*g,
		precision=G/eps
	)
	names(parProposal) <- names(parGiven)
	return(as.numeric(parProposal))
}

#' SMMALA transition kernel density
#'
#' The Simiplified Manifold Metropolis Adjusted Langevin Algorithm uses a
#' move instriction that uses a Gaussian kernel that is shifted away
#' from the current point.
#'
#' @noRd
#' @param beta inverse temperature (parallel tempering)
#' @param parGiven given point
#' @return SMMALA proposal point
smmala_move_density <- function(beta,parProposal,parGiven,fisherInformationPrior,eps=1e-2){
	stopifnot(parGiven %has% c("fisherInformation","gradLogLikelihood","gradLogPrior"))
	stopifnot(!is.null(beta) && !any(is.na(beta)) && is.finite(beta))
	fiGiven <- parGiven %@% "fisherInformation"
	gradLGiven <- parGiven %@% "gradLogLikelihood"
	gradPGiven <- parGiven %@% "gradLogPrior"
	G0 <- fisherInformationPrior
	G <- (beta^2*fiGiven)+G0
	stopifnot(!is.null(G) && is.matrix(G))
	if (isTRUE(is.invertible(G))){ # isTRUE has some additional safety for breakage cases we didn't consider
		g <- solve(G,beta*gradLGiven+gradPGiven)
	} else {
		stopifnot(is.matrix(G0) && isSymmetric(G0))
		g <- solve(G0,gradPGiven)
		G <- G0
	}
	return(
		dmvnorm(
			as.numeric(parProposal),
			mean=as.numeric(parGiven+0.5*eps*g),
			precision=G/eps
		)
	)
}

#' Metropolis Update is an MCMC update function
#'
#' During Markov chain Monte Carlo a given parameter needs to be
#' updated, the model needs to be simulated at the updated point.
#'
#' Using the simulations, and an acceptance rule, the proposed update
#' is either accepted or rejected.
#'
#' This function returns a closure `metropolis`, with only `parMCMC`
#' as it's sole argument: `parProposal <- metropolis(parGiven)`
#'
#' An optional argument to this function is `parAcceptable`, during
#' sampling, when `metropolis` is called as the update function, and
#' `parAcceptable(parProposal)` returns `FALSE`, then metropolis
#' shortcuts to `return(parGiven)` without performing simulations.
#'
#' This function can be used to weed out parameter combinations that
#' would result in obviously nonsensical simulations without wasting
#' CPU-time.
#'
#' The return value is a function (closure) that operates on mcmc
#' variables, these variables are numeric vectors with some necessary
#' attributes. The attributes record this vector's simulation results,
#' and other derived quantities.
#'
#' @export
#' @param simulate a function that simulates the model
#' @param logLikelihood a function that returns the log-likelihood
#'     value given the parameter value, with simulations attached to
#'     the parameter as an attribute (probably a closure)
#' @param dprior a function that returns the prior density of the
#'     given parameter vector
#' @param Sigma the transition kernel's covariance matrix.
#' @param parAcceptable a function that can be used to reject a
#'     proposal based on the values of the parameters alone (shortcut
#'     to rejection, sans simulation)
#' @return a closure with the arguments: (parGiven, eps=1e-4), where
#'     `parGiven` is the current position of the Markov chain (a
#'     numeric vector, with attributes), and `eps` the current step
#'     size.
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=0)
#' dprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' p <- mcmc_init(1.0,values(m$Parameter),s,ll,dprior)
#' UP <- metropolis_update(s,ll,dprior=dprior,Sigma=diag(m$Parameter$stdv)^2)
#' p2 <- UP(p)
#' ## updated value:
#' print(p2)
#' print(sum(abs(p2-p)))
metropolis_update <- function(simulate, logLikelihood=ll, dprior=\(x) prod(dnorm(x)), Sigma=NULL, parAcceptable=\(p) {all(is.finite(p))}){
	if (!is.null(Sigma)){
		cSigma <- chol(Sigma)
	}
	if (is.null(Sigma)){
		U <- function(parGiven, eps=1e-4){
			stopifnot(is.numeric(parGiven) && length(parGiven)>0 && all(is.finite(parGiven)))
			stopifnot(parGiven %has% c("logLikelihood","prior"))
			beta <- attr(parGiven,"beta") %otherwise% 1.0
			llGiven <- attr(parGiven,"logLikelihood")
			priorGiven <- attr(parGiven,"prior")
			stopifnot(is.numeric(llGiven) && length(llGiven)==1 && is.finite(llGiven))
			stopifnot(is.numeric(priorGiven) && length(priorGiven)==1 && is.finite(priorGiven))
			stopifnot(!is.null(eps) && is.numeric(eps) && length(eps)==1 && is.finite(eps))
			parProposal <- parGiven + rnorm(length(parGiven),0,eps)
			stopifnot(is.numeric(parProposal) && all(is.finite(priorGiven)))
			names(parProposal) <- names(parGiven)
			priorProposal <- dprior(parProposal)
			attr(parProposal,"prior") <- priorProposal
			if (priorProposal == 0.0 || !parAcceptable(parProposal)) {
				attr(parGiven,"accepted") <- FALSE
				return(parGiven)
			}
			attr(parProposal,"simulations") <- simulate(parProposal)
			llProposal <- logLikelihood(parProposal)
			if (!is.numeric(llProposal) || length(llProposal)!=1){
				warning(sprintf("metropolis_update encountered an invalid likelihood value: %f\n",llProposal[1]))
			}
			attr(parProposal,"logLikelihood") <- llProposal
			class(parProposal) <- class(parGiven)
			L <- exp(beta*(llProposal - llGiven))
			if (is.null(L)) warning("Likelihood is NULL; llProposal: ",llProposal," llGiven: ",llGiven," beta: ",beta," L:",L,"\n")
			P <- priorProposal/priorGiven
			if (!is.null(L) && !is.null(P) && is.finite(L) && is.finite(P) && runif(1) < L*P){
				attr(parProposal,"accepted") <- TRUE
				return(parProposal)
			} else {
				attr(parGiven,"accepted") <- FALSE
				return(parGiven)
			}
		}
	} else {
		U <- function(parGiven,eps=1e-4){
			stopifnot(is.numeric(parGiven) && length(parGiven)>0 && all(is.finite(parGiven)))
			stopifnot(parGiven %has% c("logLikelihood","prior"))
			beta <- attr(parGiven,"beta") %otherwise% 1.0
			llGiven <- attr(parGiven,"logLikelihood")
			priorGiven <- attr(parGiven,"prior")
			parProposal <- parGiven + solve(cSigma,rnorm(length(parGiven),0,eps))
			stopifnot(is.numeric(parProposal) && all(is.finite(priorGiven)))
			names(parProposal) <- names(parGiven)
			priorProposal <- dprior(parProposal)
			attr(parProposal,"prior") <- priorProposal
			if (priorProposal < 1e-15*priorGiven+1e-15 || !parAcceptable(parProposal)) {
				attr(parGiven,"accepted") <- FALSE
				return(parGiven)
			}
			attr(parProposal,"simulations") <- simulate(parProposal)
			llProposal <- logLikelihood(parProposal)
			if (!is.numeric(llProposal) || length(llProposal)!=1){
				warning(sprintf("metropolis update encountered an invalid likelihood value: %f\n",llProposal[1]))
			}
			attr(parProposal,"logLikelihood") <- llProposal
			L <- exp(beta*(llProposal - llGiven))
			if (is.null(L)) warning("Likelihood is NULL; llProposal: ",llProposal," llGiven: ",llGiven," beta: ",beta," L:",L,"\n")
			P <- priorProposal/priorGiven
			if (!is.null(L) && !is.null(P) && is.finite(L) && is.finite(P) && runif(1) < L*P){
				attr(parProposal,"accepted") <- TRUE
				return(parProposal)
			} else {
				attr(parGiven,"accepted") <- FALSE
				return(parGiven)
			}
		}
	}
	attr(U,"algorithm") <- "Metropolis-Hastings"
	return(U)
}

#' Default Log-likelihood Function
#'
#' Extracts the `logLikelihood` value from the simulations attribute
#' of the parMCMC argument, requires:
#' - parMCMC has simulations attribute
#' - simulations list includes logLikelihood values (omit<3)
#'
#' This function will take the log-likelihood-values calculated by the
#' ode solver in this package, and return the sum of those values over
#' all experiments. The value the simulator returns is calculated
#' with the assumption of a normal distribution on measurement errors.
#'
#' This function does _almost no work_, it merely sums up the values
#' calculated during simulation.
#'
#' @param parMCMC a numeric vector, with attributes for MCMC, specifically smmala
#' @return a scalar value: log(likelihood(data|parMCMC))
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=2) # not 3
#' p <- values(m$Parameter)
#' attr(p,"simulations") <- s(p)
#' print(ll(p))
ll <- function(parMCMC){
	y <- parMCMC %@% "simulations"
	return(sum(sapply(y,\(sim) sim$logLikelihood)))
}

#' Default gradient-log-likelihood Function
#'
#' Extracts the `FisherInformation` values from the simulations attribute
#' of the parMCMC argument, requires:
#' - parMCMC has simulations attribute
#' - simulations list includes gradient values (omit <2)
#'
#' This function will take the log-likelihood gradient values
#' calculated by the ode solver in this package, and return the sum of
#' those vectors over all experiments. The gll-value the simulator
#' returns is calculated with the assumption of a normal distribution
#' on measurement errors, and uses the "identity" map between MCMC
#' parameters and model-parameters by default (i.e. no
#' transformation).
#'
#' Like [ll] this function does almost no work, it merely sums up the
#' gradient values calculated during simulation, but it also performs a
#' transformation of the gradient vector, taking the
#' parameter-mapping between the sampling-space and
#' model-parameter-space into account.
#'
#' The returned function takes one argument, the MCMC variable
#' `parMCMC` (a numeric vector). This variable requires all smmala
#' specific attributes.
#'
#' @param parMapJac a function; maps parameter vectors to the Jacobian
#'     of the parameter transformation.
#' @return a numeric vector: grad(log(likelihood(data|parMCMC)))
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=1) # not 3
#' p <- values(m$Parameter)
#' attr(p,"simulations") <- s(p)
#' print(ll(p))
#' trivialJac <- \(x) diag(1,length(x),length(x)) # the default
#' gll <- gllf(parMapJac=trivialJac)
#' print(gll(p))
gllf <- function(parMapJac=\(x) diag(1,length(x),length(x))) {
	return(
		function(parMCMC){
			g <- Reduce(
				\(a,b) a+as.numeric(b$gradLogLikelihood[seq_along(parMCMC)]),
				parMCMC %@% "simulations",
				init=0.0
			)
			return(
				as.numeric(g %*% parMapJac(parMCMC))
			)
		}
	)
}

#' Default gradient-Log-likelihood Function
#'
#' Extracts the `gradLogLikelihood` values from the simulations attribute
#' of the parMCMC argument, requires:
#' - parMCMC has simulations attribute
#' - simulations list includes Fisher Information values (omit=0)
#'
#' This function will take the Fisher-Information-matrices calculated
#' by the ode solver in this package, and return the sum of those
#' values over all experiments. The gll-value the simulator returns is
#' calculated with the assumption of a normal distribution on
#' measurement errors, and uses the `identity` map between the MCMC
#' variable and the model's parameters by default (i.e. no
#' transformation).
#'
#' Like [ll] and [gllf] this function does almost no work, it merely
#' sums up the FI values calculated during simulation, but it also
#' performs a transformation of the Fisher Information Matrix, taking
#' the parameter-mapping between the sampling-space and
#' model-parameter-space into account.
#'
#' The only argument is a function that takes the current MCMC
#' variable, `parMCMC` (a numeric vector), with all necessary
#' attributes for smmala to work (e.g. through initialization).
#'
#' @param parMapJac a function; maps parameter vectors to the Jacobian
#'     of the parameter transformation.
#' @return a scalar value: log(likelihood(data|parMCMC))
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=0)
#' p <- values(m$Parameter)
#' attr(p,"simulations") <- s(p)
#' ### without parameter transformations
#' gll <- gllf()
#' FI <- fi()
#' if (interactive()){
#'   print(ll(p))
#'   print(gll(p))
#'   print(FI(p))
#' }
fi <- function(parMapJac=\(x) diag(1,length(x),length(x))){
	return(
		function(parMCMC) {
			i <- seq_along(parMCMC)
			f <- Reduce(
				\(a,b) a + b$FisherInformation[i,i,1],
				parMCMC %@% "simulations",
				init = 0.0
			)
			J <- parMapJac(parMCMC)
			return(
				t(J) %*% f %*% J
			)
		}
	)
}

#' SMMALA Update is an MCMC update function
#'
#' During Markov chain Monte Carlo a given parameter needs to be
#' updated, the model needs to be simulated at the updated point.
#'
#' Using the simulations, and an acceptance rule, the proposed update
#' is either accepted or rejected.
#'
#' This function returns a closure `smmala`, with only `parMCMC`
#' as it's sole argument: `parProposal <- smmala(parGiven)`
#'
#' An optional argument to this function is `parAcceptable`, during
#' sampling, when `metropolis` is called as the update function, and
#' `parAcceptable(parProposal)` returns `FALSE`, then metropolis
#' shortcuts to `return(parGiven)` without performing simulations.
#'
#' This function can be used to weed out parameter combinations that
#' would result in obviously nonsensical simulations without wasting
#' CPU-time.
#'
#' The argument `fisherInformationPrior` is really the precision of
#' the prior (a constant matrix). It's role is additive to the
#' fisherInformation and is used to regularize the _final_ Fisher
#' Information Matrix (makes it invertible).
#'
#' @export
#' @param simulate a function that simulates the model
#' @param logLikelihood a function that returns the log-likelihood
#'     value given the parameter value, with simulations attached to
#'     the parameter as an attribute (probably a closure)
#' @param dprior a function that returns the prior density of the
#'     given parameter vector
#' @param gradLogLikelihood any function that calculates or estimates
#'     the gradient of the log-likelihood function, for the chosen
#'     parameter mapping. Function must take one argument (the MCMC
#'     variable)
#' @param gprior a function that returns the gradient of the log-prior
#'     distribution.
#' @param fisherInformation a function that estimates the Fisher
#'     Information for a given MCMC variable (parMCMC).
#' @param fisherInformationPrior a constant fisherInformation of the
#'     prior distribution (or rather, the precision of the prior)
#' @param parAcceptable a function that can be used to reject a
#'     proposal based on the values of the parameters alone (shortcut
#'     to rejection, sans simulation)
#' @return a closure with the arguments: (parGiven, eps=1e-4), where
#'     `parGiven` is the current position of the Markov chain (a
#'     numeric vector, with attributes), and `eps` the current step
#'     size.
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=0)
#' ### without parameter transformations
#' gll <- gllf()
#' FI <- fi()
#' dprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' gprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' p <- mcmc_init(1.0,values(m$Parameter),s,ll,dprior,gll,gprior,FI)
#' UP <- smmala_update(s,ll,dprior=dprior,gll,gprior=gprior,FI,solve(diag(m$Parameter$stdv)))
#' p2 <- UP(p)
#' ## updated value:
#' print(p2)
#' print(sum(abs(p2-p)))
smmala_update <- function(simulate, logLikelihood=ll, dprior=\(x) prod(dnorm(x)), gradLogLikelihood=gllf(log10ParMapJac), gprior=\(x) (-x), fisherInformation=fi(log10ParMapJac), fisherInformationPrior=0, parAcceptable=\(p) all(is.finite(p))){
	U <- function(parGiven, eps=1e-4){
		stopifnot(parGiven %has% c("logLikelihood","prior","fisherInformation","gradLogLikelihood","gradLogPrior"))
		fp <- fisherInformationPrior
		stopifnot(is.matrix(fp) && isSymmetric(fp))
		beta <- attr(parGiven,"beta") %otherwise% 1.0
		llGiven <- attr(parGiven,"logLikelihood")
		priorGiven <- attr(parGiven,"prior")
		n <- length(parGiven)
		## the very important step: suggest a successor to parGiven and simulate the model
		parProposal <- smmala_move(beta,parGiven,fp,eps)
		priorProposal <- dprior(parProposal)
		if (priorProposal < 1e-15*priorGiven+1e-15 || !parAcceptable(parProposal)){
			attr(parGiven,"accepted") <- FALSE
			return(parGiven)
		}
		attr(parProposal,"simulations") <- simulate(parProposal)
		llProposal <- logLikelihood(parProposal)
		if (!is.numeric(llProposal) || length(llProposal)!=1){
			warning(sprintf("smmala update encountered an invalid likelihood value: %f\n",llProposal[1]))
		}
		attr(parProposal,"beta") <- beta
		attr(parProposal,"logLikelihood") <- llProposal
		attr(parProposal,"prior") <- priorProposal
		attr(parProposal,"fisherInformation") <- fisherInformation(parProposal)
		attr(parProposal,"gradLogLikelihood") <- gradLogLikelihood(parProposal)
		attr(parProposal,"gradLogPrior") <- gprior(parProposal)
		class(parProposal) <- class(parGiven)
		fwdDensity <- smmala_move_density(beta,parProposal,parGiven,fp,eps)
		bwdDensity <- smmala_move_density(beta,parGiven,parProposal,fp,eps)
		L <- exp(beta*(llProposal - llGiven)) %otherwise% 0.0
		P <- priorProposal/priorGiven %otherwise% 0.0
		K <- bwdDensity/fwdDensity %otherwise% 0.0
		flush.console()
		if (is.finite(L) && is.finite(K) && is.finite(P) && runif(1) < L * P * K){
			attr(parProposal,"accepted") <- TRUE
			return(parProposal)
		} else {
			attr(parGiven,"accepted") <- FALSE
			return(parGiven)
		}
	}
	attr(U,"algorithm") <- "smmala"
	return(U)
}


#' This function proposes an MCMC candidate variable, and either
#' accepts or rejects the candidate
#'
#' This function selects the update method based on the provided
#' ingredients.
#'
#' This function receives a current MCMC variable, then calculates a
#' possible successor and returns it in the case of acceptance. It
#' returns the (old) current state upon rejection of the candidate.
#'
#' The Markov chain has a current state (the MCMC variable, often x in
#' literature), but in the context of sampling the MCMC variables are
#' used as the parameters to a scientific model of some sort (and
#' these often have state variables, also x, or y). This is why we
#' call the variables parMCMC (parABC), or
#' par{Current|Given|Proposal}, and similar (with a "par" prefix).
#'
#' @noRd
#' @param simulate a function that simulates the model for a given
#'     parMCMC
#' @param logLikelihood a function that calculates log-likelihood
#'     values for given parMCMC
#' @param dprior prior density function
#' @param gradLogLikelihood a function that calculates the gradient of
#'     the log-likelihood for given parMCMC
#' @param gprior gradient of the prior density
#' @param fisherInformation a function that calculates approximates
#'     Fisher information matrices
#' @param fisherInformationPrior a constant matrix, the prior
#'     distributions fisher information
#' @param Sigma alternatively, `Sigma=solve(fisherInformationPrior)`,
#'     \[the inverse to fisherInformationPrior\] can be specified for
#'     the metropolis algorithm
#' @param parAcceptable a user shaped function that returns a Boolean
#'     scalar indicating whether a proposed parameter satisfies any
#'     user chosen constraints. A value of FALSE will trigger an early
#'     return and the model will not be simulated.
#' @return an automatically selected update function
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=0)
#' ### without parameter transformations
#' gll <- gllf()
#' FI <- fi()
#' dprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' gprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' p <- mcmc_init(1.0,values(m$Parameter),s,ll,dprior,gll,gprior,FI)
#' UP <- automatic_update(s,ll,dprior=dprior,gll,gprior=gprior,FI,solve(diag(m$Parameter$stdv)))
#' p2 <- UP(p)
#' ## updated value:
#' print(p2)
#' print(sum(abs(p2-p)))
automatic_update <- function(simulate, logLikelihood=ll, dprior=\(x) prod(dnorm(x)), gradLogLikelihood=NULL, gprior=NULL, fisherInformation=NULL, fisherInformationPrior=NULL, Sigma=NULL, parAcceptable=\(p) {TRUE}){
	if (is.null(Sigma) && !is.null(fisherInformationPrior)) {
		Sigma <- solve(fisherInformationPrior)
	}
	if (is.null(gradLogLikelihood)) { # Metropolis Hastings
		return(metropolis_update(simulate, logLikelihood, dprior, Sigma=Sigma, parAcceptable=parAcceptable))
	} else {
		return(smmala_update(simulate, logLikelihood, dprior, gradLogLikelihood, gprior, fisherInformation, fisherInformationPrior, parAcceptable))
	}
}

#' Default log-likelihood function
#'
#' This returns a function f(simulations), which maps simulation
#' results to log(likelihood) values. The experiments are used
#' implicitly; simulations is a list as returned by
#' rgsl::r_gsl_odeiv2_outer().
#'
#' @param experiments will be compared tp the simulation results
#' @param perExpLLF (optional) a user supplied function with the
#'     interface `perExpLLF(p,s,e)`, where `p` are the parameters, `s`
#'     are the simulations and `e` are the experiments (with
#'     data). Supply this function if some of your experiments need to
#'     be normalized by the other experiments (and other complex
#'     cases).
#' @param simpleUserLLF (optional) a user supplied function that is
#'     used instead of the default sum of ((y-h)/stdv)^2 terms. The
#'     interface is: `simpleUserLLF(y,h,stdv,name=NULL)`, where each
#'     of them is an N-M-matrix where N is the dimensionality of the
#'     model output and M the number of data time-points.  Here, `y`
#'     is `t(experiments[[i]]$data)` and may contain NA
#'     values.  This function should also accept an optional _name_
#'     argument (this is the name of the experiment this function is
#'     currently called for).
#' @return `llf(parMCMC)`, a closure (function) of the mcmc-variable:
#'     parMCMC; returns a scalar log-likelihood value. Alternatively,
#'     the user can define such a function:
#'     `parMCMC -> log(Likelihood(parMCMC))`,
#'     and use that during sampling. A test simulation of `p`:
#'     `y <- simulate(p)` will reveal which values the simulator produces.
#'     These values will be attached to p during sampling, as an
#'     attribute.  [mcmc_init] will attach the same values for the
#'     initial Markov chain state.  The log-likelihood function can
#'     use these attributes.
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o,omit=0)
#' p <- values(m$Parameter)
#' attr(p,"simulations") <- s(p)
#' ## this function is fairly flexible and accepts some user settings
#' llf <- logLikelihoodFunc(ex)
#' print(llf(p))
#' ## this function uses the values from the solver:
#' print(ll(p))
logLikelihoodFunc <- function(experiments,perExpLLF=NULL,simpleUserLLF=NULL){
	N <- length(experiments)
	n.out <- sum(unlist(lapply(experiments,\(e) sum(!is.na(e$data))))) # total number of valid values
	if (!is.null(simpleUserLLF)){
		llf <- function(parMCMC){
			if (!("simulations" %in% names(attributes(parMCMC)))) {
				return(-Inf)
			} else if (any(is.na(attr(parMCMC,"simulations")))) {
				return(-Inf)
			} else {
				simulations <- attr(parMCMC,"simulations")
			}
			n <- NCOL(parMCMC)
			L <- rep(0,n)
			for (i in seq(N)){
				if (!("func" %in% names(simulations[[i]]))) {
					warning("simulations contain no 'func' values (output function values, i.e. observables).")
					return(-Inf)
				} else if (any(is.na(simulations[[i]]$func))) {
					warning("simultions contain NA values which shouldn't happen, normally.")
					return(-Inf)
				}
				dimFunc <- dim(simulations[[i]]$func)
				m <- head(dimFunc,2)
				y <- experiments[[i]]$data %otherwise% t(experiments[[i]]$measurements)
				stdv <- standard_error_matrix(y) %otherwise% t(experiments[[i]]$standardError)
				for (k in seq(n)){
					h <- simulations[[i]]$func[,,k]
					dim(h) <- m
					stopifnot(all(dim(h)==dim(y)) && all(dim(y)==dim(stdv)))
					L[k] <- L[k] + simpleUserLLF(y,h,stdv,names(experiments)[i])
				}
			}
			return(L)
		}
	} else if (!is.null(perExpLLF)){
		llf <- function(parMCMC){
			if (!("simulations" %in% names(attributes(parMCMC))) || any(is.na(attr(parMCMC,"simulations")))) {
				warning("no simulations attached to parameter vector: attr(parMCMC,'simulations') is missing.")
				return(-Inf)
			} else {
				simulations <- attr(parMCMC,"simulations")
			}
			simulations <- attr(parMCMC,"simulations")
			L <- perExpLLF(parMCMC,simulations,experiments)
			return(L)
		}
	} else {
		llf <- function(parMCMC){
			if (!(parMCMC %has% "simulations")) {
				warning("no simulations attached to parameter vector: attr(parMCMC,'simulations') is missing.")
				return(-Inf)
			} else if (any(is.na(attr(parMCMC,"simulations")))) {
				warning("simulated values contain NA, which shouldn't happen normally.")
				return(-Inf)
			} else {
				simulations <- attr(parMCMC,"simulations")
			}
			simulations <- attr(parMCMC,"simulations")
			n <- NCOL(parMCMC)
			L <- rep(-0.5*n.out*log(2*pi),n)
			for (i in seq(N)){
				if (!("func" %in% names(simulations[[i]])) || any(is.na(simulations[[i]]$func))){
					return(-Inf)
				}
				dimFunc <- dim(simulations[[i]]$func)
				m <- head(dimFunc,2)
				y <- experiments[[i]]$data
				stdv <- standard_error_matrix(y) %otherwise% t(experiments[[i]]$standardError)
				for (k in seq(n)){
					h <- simulations[[i]]$func[,,k]
					dim(h) <- m
					stopifnot(all(dim(h)==dim(y)) && all(dim(y)==dim(stdv)))
					L[k] <- L[k] - 0.5*sum(((errors::drop_errors(y) - h)/stdv)^2,na.rm=TRUE) - sum(log(stdv),na.rm=TRUE)
				}
			}
			return(L)
		}
	}
	comment(llf) <- sprintf("The experiments contain %i non-missing values in total.",n.out)
	return(llf)
}

#' LOG10 parameter mapping used by the MCMC module
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters. This function is an example for the
#' `parMap` slot in sampling functions. A `parMap` function, like this
#' one, must transform an MCMC variable (vector) to a parameter vector
#' that the scientific model we simulate can work with.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @return a numeric vector intended for the simulator.
#' @export
log10ParMap <- function(parMCMC){
	return(10^(parMCMC))
}

#' LOG10 parameter mapping, jacobian
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters. As we often calculate sensitivities, we
#' also need the Jacobian of the map, due to the chain rule of
#' differentiation.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @export
#' @return a numeric matrix (dim: `c(length(parMCMC),length(parMCMC))`)
#' @examples
#' p <- c(-1,0,1)
#' parMap <- log10ParMap
#' parMpJ <- log10ParMapJac
#' print(parMap(p))
#' print(parMpJ(p))
log10ParMapJac <- function(parMCMC){
	return(diag(10^(parMCMC) * log(10)))
}

#' LOG2 parameter mapping used by the MCMC module
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @export
#' @return a numeric vector intended for the simulator.
#' @examples
#' p <- c(-1,0,1)
#' parMap <- log2ParMap
#' print(parMap(p))
log2ParMap <- function(parMCMC){
	return(2^(parMCMC))
}

#' LOG2 parameter mapping, jacobian
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters. As we often calculate sensitivities, we
#' also need the jacobian of the map, due to the chain rule of
#' differentiation.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @export
#' @return a numeric matrix (dim: `c(length(parMCMC),length(parMCMC))`).
#' @examples
#' p <- c(-1,0,1)
#' parMap <- log2ParMap
#' parMpJ <- log2ParMapJac
#' print(parMap(p))
#' print(parMpJ(p))
log2ParMapJac <- function(parMCMC){
	return(diag(2^(parMCMC) * log(2)))
}


#' NATURAL LOG parameter mapping used by the MCMC module
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @export
#' @return a numeric vector intended for the simulator.
#' @examples
#' p <- c(-1,0,1)
#' parMap <- logParMap
#' print(parMap(p))
logParMap <- function(parMCMC){
	return(exp(parMCMC))
}

#' NATURAL LOG parameter mapping, jacobian
#'
#' This map is used by the simulator to transform sampling variables
#' into ODE-model parameters. As we often calculate sensitivities, we
#' also need the jacobian of the map, due to the chain rule of
#' differentiation.
#'
#' @param parMCMC the sampling variables (numeric vector)
#' @export
#' @return a numeric matrix (dim: `c(length(parMCMC),length(parMCMC))`).
#' @examples
#' p <- c(-1,0,1)
#' parMap <- logParMap
#' parMpJ <- logParMapJac
#' print(parMap(p))
#' print(parMpJ(p))
logParMapJac <- function(parMCMC){
	return(diag(exp(parMCMC)))
}

#' High Level SMMALA function
#'
#' This function uses default assumption everywhere and returns a
#' function that will sample from the given model. This function will
#' generate code, compile the code, create an ODE solver for it, infer
#' the sampling space from the scale of the parameters, create all
#' necessary functions to move in parameter space (gradients of
#' likelihood and prior), as well as Fisher Information functions.
#'
#' @export
#' @param m the model's TSV representation read via `model_from_tsv`
#' @param o (optional) ode representation of `m`
#' @param ex experiments of `m`, with simulation instructions for `o`.
#' @param x initial point of the Markov chain, pre in initialized to
#'     have the right attributes.
#' @param verbose prints extra messages when TRUE
#' @return `smmala` a function of three arguments: p0, N, eps; where
#'     p0 is the starting point, N is the desired sample-size, and eps
#'     is the step size. This function has an attribute called "init",
#'     with a pre-initialized starting point.
#' @examples
#'   m <- model_from_tsv(uqsa_example("AKAP79"))
#'   rwm <- high_level_smmala(m) # "random walk", metropolis algorithm
#'   p <- rwm %@% "init"          # a valid starting point
#'   N <- 100
#'   if (interactive()){
#'     smallSample <- rwm(rwm %@% "init",N,1e-4)
#'     plot(
#'       smallSample %@% "logLikelihood",
#'       type='l',
#'       main=sprintf("%i iterations",N),
#'       xlab="iterations",
#'       ylab="log-likelihood"
#'     )
#'   } else {
#'     smallSample <- rwm(rwm %@% "init",1,1e-4)
#'   }
high_level_smmala <- function(m,o=as_ode(m,cla=TRUE),ex=experiments(m,o), x=values(m$Parameter), verbose=getOption("uqsa.verbose", interactive())){
	if (is.null(o$c_path) || is.null(o$so_path) || !file.exists(o$so_path)){
		C <- generate_code(o)
		c_path(o) <- write_c_code(C)
		so_path(o) <- shlib(o)
	}
	if (all(m$Parameter$scale == "log10")){
		if (verbose) message("The parameters are given in log10-scale, so the simulator will do the reverse transformation: 10^p.")
		parMap <- log10ParMap
		parMapJac <- log10ParMapJac
	} else if (all(m$Parameter$scale == "log2") || all(m$Parameter$scale == "ld")) {
		if (verbose) message("The parameters are given in log2-scale, so the simulator will do the reverse transformation: 2^p.")
		parMap <- log2ParMap
		parMapJac <- log2ParMapJac
	} else if (all(m$Parameter$scale == "log") || all(m$Parameter$scale == "ln")){
		if (verbose) message("The parameters are given in log-scale, so the simulator will do the reverse transformation: exp(p).")
		parMap <- logParMap
		parMapJac <- logParMapJac
	} else {
		if (verbose) message("The parameters are given in linear scale, they will be used as is.")
		parMap <- identity
		parMapJac <- diag(rep(1.0,length(x)))
	}
	s <- simfi(ex,o,parMap=parMap,omit=0)
	dprior <- dNormalPrior(
		mean=m$Parameter$median %otherwise% values(m$Parameter),
		sd=m$Parameter$stdv %otherwise% m$Parameter$sd
	)
	gprior <- gNormalPrior(
		mean=m$Parameter$median %otherwise% values(m$Parameter),
		sd=m$Parameter$stdv %otherwise% m$Parameter$sd
	)
	parMCMC <- mcmc_init(
		beta=1.0,
		x,
		simulate=s,
		dprior=dprior,
		gradLogLikelihood=gllf(parMapJac),
		gprior=gprior,
		fisherInformation=fi(parMapJac)
	)
	smmala <- mcmc(
		smmala_update(
			simulate=s,
			gradLogLikelihood=gllf(parMapJac),
			dprior=dprior,
			gprior=gprior,
			fisherInformation=fi(parMapJac),
			fisherInformationPrior=diag(1.0/m$Parameter$stdv^2)
		)
	)
	attr(smmala,"sim") <- s
	attr(smmala,"init") <- parMCMC
	return(smmala)
}

#' High Level Metropolis function
#'
#' This function uses default assumption everywhere and returns a
#' function that will sample from the given model. This function will
#' generate code, compile the code, create an ODE solver for it, infer
#' the sampling space from the scale of the parameters, create all
#' necessary functions to move in parameter space (gradients of
#' likelihood and prior), as well as Fisher Information functions.
#'
#' @export
#' @param m the model's TSV representation read via `model_from_tsv`
#' @param o (optional) ode representation of `m`
#' @param ex experiments of `m`, with simulation instructions for `o`.
#' @param x initial point of the Markov chain, pre in initialized to
#'     have the right attributes.
#' @param beta for parallel tempering, the log-likelihood will have a
#'     factor of `beta` applied to it
#' @param verbose prints extra messages when TRUE
#' @return `smmala` a function of three arguments: p0, N, eps; where
#'     p0 is the starting point, N is the desired sample-size, and eps
#'     is the step size. This function has an attribute called "init",
#'     with a pre-initialized starting point.
#' @examples
#'   m <- model_from_tsv(uqsa_example("AKAP79"))
#'   rwm <- high_level_metropolis(m) # "random walk", metropolis algorithm
#'   p <- rwm %@% "init"             # a valid starting point
#'   N <- 100
#'   if (interactive()){
#'     smallSample <- rwm(rwm %@% "init",N,1e-6)
#'     plot(
#'       smallSample %@% "logLikelihood",
#'       type="l",
#'       main=sprintf("%i iterations",N),
#'       xlab="iterations",
#'       ylab="log-likelihood"
#'     )
#'   } else {
#'     smallSample <- rwm(rwm %@% "init",N/4,1e-6)
#'   }
high_level_metropolis <- function(m,o=as_ode(m,cla=FALSE),ex=experiments(m,o), x=values(m$Parameter), beta=1.0, verbose=getOption("uqsa.verbose", interactive())){
	if (is.null(so_path(o)) || !file.exists(so_path(o))){
		C <- generate_code(o)
		c_path(o) <- write_c_code(C)
		so_path(o) <- shlib(o)
	}
	if (all(m$Parameter$scale == "log10")){
		if (verbose) message("The parameters are given in log10-scale, so the simulator will do the reverse transformation: 10^p.")
		parMap <- log10ParMap
	} else if (all(m$Parameter$scale == "log2") || all(m$Parameter$scale == "ld")) {
		if (verbose) message("The parameters are given in log2-scale, so the simulator will do the reverse transformation: 2^p.")
		parMap <- log2ParMap
	} else if (all(m$Parameter$scale == "log") || all(m$Parameter$scale == "ln")){
		if (verbose) message("The parameters are given in log-scale, so the simulator will do the reverse transformation: exp(p).")
		parMap <- logParMap
	} else {
		if (verbose) message("The parameters are given in linear scale, they will be used as is.")
		parMap <- identity
	}
	s <- simfi(ex,o,parMap=parMap,omit=2)
	stdv <- m$Parameter$stdv %otherwise% m$Parameter$sd %otherwise% m$Parameter$sigma
	if (is.null(stdv)) {
		stop("no parameter standard deviation (sigma) provided in parameter table")
	}
	p0 <- m$Parameter$median %otherwise% m$Parameter$mean %otherwise% m$Parameter$mu %otherwise% values(m$Parameter)
	dprior <- dNormalPrior(
		mean=p0,
		sd=stdv
	)
	parMCMC <- mcmc_init(
		beta=beta,
		x,
		simulate=s,
		dprior=dprior
	)
	metropolis <- mcmc(
		metropolis_update(
			simulate=s,
			logLikelihood=ll,
			dprior=dprior,
			Sigma=diag(stdv^2)
		)
	)
	attr(metropolis,"sim") <- s
	attr(metropolis,"init") <- parMCMC
	return(metropolis)
}

#' Find a good Step-Size for a given MCMC Algorithm
#'
#' Given a closure `MCMC(p,N,eps)`, where `p` is the initial
#' Markov-chain position, `N` a sample-size, and `eps` a step-size,
#' this function finds a good value for eps.
#'
#' It will take 100 sample points repeatedly, until an acceptance of
#' `target_acceptance` is reached (defaults to 25%). The step-size is
#' decreased if acceptance is very low and increased when it is too
#' high.
#'
#' When verbose
#' This function will do at most
#'
#' @param MCMC a Markov chain Monte Carlo closure (function)
#' @param parMCMC initial position of the Markov chain, has to be
#'     initialized with [mcmc_init].
#' @param target_acceptance a scalar value for the desired acceptance
#'     rate, some algorithms are most efficient with 20% to 30%
#'     acceptance, some work well with a very high acceptance.
#' @param iter.max maximum number of iterations until the function has
#'     to return.
#' @param h initial guess for the MCMC step size
#' @param N size of test-samples for acceptance rate estimate
#' @param verbose when TRUE, this function prints a progress bar, 'a:'
#'     reports current acceptance rate, and 'h:' reports the current
#'     step-size.
#' @return optimal step size
#' @export
#' @examples
#'   opt <- options(mc.cores=2)
#'   m <- model_from_tsv(uqsa_example("AKAP79"))
#'   rwm <- high_level_metropolis(m) # "random walk", metropolis algorithm
#'   p <- rwm %@% "init"             # a valid starting point
#'   N <- 100
#'   if (interactive()){
#'     h <- tune_step_size(rwm,p)
#'     smallSample <- rwm(rwm %@% "init",N,h)
#'     print(h)
#'     plot(
#'       smallSample %@% "logLikelihood",
#'       type="l",
#'       main=sprintf("step size: %g",h),
#'       xlab="iterations",
#'       ylab="log-likelihood"
#'     )
#'   } else {
#'     h <- tune_step_size(rwm,p,N=20,iter.max=1)
#'   }
#'   options(opt)
tune_step_size <- function(MCMC,parMCMC=attr(MCMC,"init"),target_acceptance=0.25, iter.max=6, h=1e-4, N=100, verbose=getOption("uqsa.verbose", interactive())){
	A <- target_acceptance
	for (i in seq(iter.max)){
		if (verbose) cli::cli_progress_step("iteration {i}/{iter.max}")
		X <- MCMC(parMCMC,N,h)
		a <- X %@% "acceptanceRate"
		if (verbose){
			cli::cli_alert_info(
				paste(
					"(a)cceptance: {format(a,digits=2)};",
					"step size: log2(h)={format(log2(h),digits=3)};"
				)
			)
			cli::cli_progress_update()
		}
		if (abs(a-A) < 3e-2) {
			break
		} else {
			h <- h*max(2*a^2/(A^2 + a^2),0.001)
		}
		if (verbose) cli::cli_progress_done()
	}
	return(h)
}
