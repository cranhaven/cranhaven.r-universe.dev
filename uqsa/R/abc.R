#' Performs and Approximate Bayesian Computation Sampling of Model Parameters
#'
#' ABC replaces the need for an exact likelihood function and uses a
#' distance function instead: the distance between data and
#' simulation. This distance function is very similar to a likelihood
#' function but lacks a statistical justification. Nevertheless, this
#' distance function, like the likelihood function of a deterministic
#' model, performs a simulation of the scientific model, be it fully
#' stochastic or a stochastically embedded, but deterministic in its
#' core.
#'
#' The distance of the ABC setting is compared to a threshold value
#' \eqn{\\delta}{delta}. The threshold doesn't need to be explicitly
#' provided. You can however provide a span of acceptable values in
#' any order, the smaller value will be used as a lower bound, the
#' larger value will be used initially.
#'
#' The ABC procedure will attempt to converge first, using the initial
#' delta value, and decreasing it slowly using observed distance
#' values.
#'
#' This function always operates on a bundle of Markov chains. The
#' size of this bundle can be determined through `startPar` (a matrix
#' of column vectors). Each column will be used as the initial point
#' of a Markov chain. The chains will be resampled at each step during
#' the convergence phase, and decouple from one another once the
#' burn-in is complete. When `startPar` is a vector, the batchSize
#' will be set to `100*length(startPar)`, if not provided explicitly.
#'
#' ABC methods (distance function, threshold delta) can be combined
#' with several other methods (like particle filters). Here we use
#' several parallel Markov chains to sample from the approximate
#' posterior.
#'
#' Since this sampler works in batches, it stores its return value in
#' batches along a 3rd dimension of an array: `ret$draws[,,1]` is the
#' first iteration of MCMC, `ret$draws[,,N]` the last iteration. One
#' sampled model parameter vector is a column vector:
#' `ret$draws[,1,1]` is something that can be passed to the
#' simulator. Because the simulator accepts batches of parameters,
#' this will cause a batch of simulations: `s(ret$draws[,,N])`.  This
#' structure is useful when determining the auto-correlation length
#' along the 3rd dimension: `ret$draws[i,j,]` is auto-correlated along
#' the 3rd dimension for any choice of `i` and `j`. To obtain a
#' classic sample, you can first flatten the third dimension:
#' `dim(ret$draws) <- c(np,batchSize*N)`, and then transpose for
#' functions like `cov`.
#'
#' @export
#' @param objectiveFunction function that, given a parameter matrix as
#'     input, simulates the model, and outputs the distance between
#'     experimental data and data simulated from the model with the
#'     parameter provided in input. This has to be a closure that
#'     contains the experimental data within itself. The closure must
#'     be vectorized over the columns of its matrix argument.
#' @param startPar starting values for the parameter vector, can (and
#'     should) be a matrix with n columns, where each column is a
#'     valid parameter vector; the number of columns determines the
#'     batch size.
#' @param N requested number of batches to return, the sample will be
#'     of size `batchSize*N` (batch size is the number of parallel
#'     Markov chains).
#' @param burnIn number of batches where the transition kernel will be
#'     adjusted to achieve an acceptance rate of below 10%.
#' @param Sigma0 multivariate normal covariance of Markov chain
#'     transition kernel, defaults to the covariance of the initial
#'     parameters. If startPar is one vector, this matrix must be
#'     provided explicitly.
#' @param dprior a function that returns prior probability density
#'     values.
#' @param deltaSpan either an initial and final value for the ABC
#'     threshold delta, or a fixed value for delta that will never
#'     change.
#' @param batchSize the size of each batch, this should be a number
#'     that could be sufficient to calculate the covariance of in the
#'     given parameter space.
#' @param parAcceptable a function that can reject a parameter vector
#'     early based on user-requirements. Has to return a scalar
#'     Boolean. Use this to test for inequalities that you find
#'     difficult to encode in the prior.
#' @param verbose when TRUE, a progress bar is printed during burn-in
#'     and actual sampling.
#' @return a list containing a sample matrix and a vector of observed distances
#'     (values that compare to delta for each sample). The sample (draws) is stored
#'     as a 3d-array, with these dimensions: \eqn{n_p\\times m\\times
#'     N}{c(np,m,N)}, where np is the number of model parameters, m
#'     the batch-size, and N the number of MCMC iterations.
#' @examples
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   o <- as_ode(m)
#'   ex <- experiments(m,o)
#'   C <- generate_code(o)
#'   c_path(o) <- write_c_code(C)
#'   so_path(o) <- shlib(o)
#'   s <- simulator.c(ex,o,parMap=log10ParMap)
#'   objFunc <- makeObjective(ex,s)
#'   p0 <- log10(values(m$Parameter))
#'   lowerBound <- p0 - 3
#'   upperBound <- p0 + 3
#'   dprior <- dUniformPrior(lowerBound,upperBound)
#'   rprior <- rUniformPrior(lowerBound,upperBound)
#'   X <- rprior(96)    # increase this number ...
#'   ## this always takes more than 5s to run:
#'   if (interactive()){
#'       abcSample <- abc_mcmc(
#'       objFunc,
#'       startPar=t(X),
#'       N=128,           # ... and this number
#'       Sigma0=cov(X),
#'       dprior=dprior
#'     )
#'   }
abc_mcmc <- function(objectiveFunction, startPar, N, burnIn=ceiling(sqrt(N)), Sigma0=cov(t(startPar)), dprior=NULL, deltaSpan=NULL,batchSize=NCOL(startPar), parAcceptable=\(p){all(is.finite(p))}, verbose=getOption("uqsa.verbose", interactive())){
	show_progress <- verbose && interactive()
	if (is.null(deltaSpan)){
		delta <- 2*max(objectiveFunction(startPar))
		delta_LB <- 0 # lower bound
	} else {
		delta <- max(deltaSpan)
		delta_LB <- min(deltaSpan)
	}
	if (missing(batchSize) && is.vector(startPar)) {
		batchSize <- 100*length(startPar)
		cli::cli_alert_warning("Starting parameters are not a matrix; picking batch size automatically: {batchSize}.")
	}
	np <- nrow(Sigma0)
	b <- sample.int(NCOL(startPar),size=batchSize,replace=TRUE)
	## make startPar _batch shaped_ (with batchSize columns), and add a little noise to it, in case it was exactly one vector
	curPar <- as.matrix(startPar)[,b] + matrix(rnorm(np*batchSize,0,norm(Sigma0)*0.01),np,batchSize)
	if (missing(dprior) || is.null(dprior)) { # construct something useful
		warning("[abc_mcmc] prior probability density not specified; will infer from start values (uniform distribution).")
		LB <- apply(curPar,1,min)
		UB <- apply(curPar,1,max)
		if (any(UB<=LB)){
			MD <- apply(curPar,1,median)
			LB <- MD - 2
			UB <- MD + 2
		}
		dprior <- dUniformPrior(LB,UB)
	}
	curPrior <- dprior(t(curPar))
	curDistance <- NULL
	mu <- rowMeans(curPar)                # the posterior's mean (tracking)
	S <- cov(t(curPar)) * (batchSize - 1) # the posterior's covariance (tracking)
	I <- diag(nrow=nrow(Sigma0))
	Sigma1 <- diag(diag(Sigma0))
	batchSample <- matrix(nrow=batchSize,ncol=length(startPar))
	acceptanceRate <- NULL
	deltaRecord <- NULL
	n <- 1
	if (show_progress) {
		cli::cli_progress_bar(name="abc", total = N+burnIn+1)
	}
	startTime <- Sys.time()
	draws <- array(NA,dim=c(np,batchSize,N))
	distanceRecord <- matrix(NA,nrow=batchSize,ncol=N)
	for (i in seq(-burnIn,N)) {
		L <- chol(Sigma0)
		Z <- matrix(rnorm(batchSize*np),np,batchSize) # this shape is expected by the Objective
		canPar <- curPar + L %*% Z                    # canPar is a multivariate normal batch derived from the previous batch
		canPrior <- dprior(t(canPar))                 # a vector
		canWeight <- canPrior/curPrior                # element-wise division
		canWeight[!is.finite(canWeight)] <- 0
		l <- as.logical((runif(batchSize) <= canWeight) & apply(canPar,2,parAcceptable))   # l marks parameters that can be investigated further
		canWeight[!l] <- 0
		canDistance <- rep(Inf,batchSize)
		if (any(l)){
			canDistance[l] <- colMeans(objectiveFunction(canPar[,l,drop=FALSE])) # Obj is a matrix: nExperiment x length(l)
			canWeight <- canWeight * (canDistance <= delta) * is.finite(canDistance)
		}
		a <- mean(canWeight>0,na.rm=TRUE)                      # acceptance rate
		if (any(canWeight>0,na.rm=TRUE)){
			canWeight[is.na(canWeight)] <- 0
			if (i<=0){
				j <- sample.int(batchSize,size=batchSize,replace=TRUE,prob=canWeight)
				curDistance <- canDistance[j]
				curPrior <- canPrior[j]
				curPar <- canPar[,j]
			} else {
				j <- which(canWeight>0)
				curDistance[j] <- canDistance[j]
				curPrior[j] <- canPrior[j]
				curPar[,j] <- canPar[,j]
			}
		} else {
			a <- 0
		}
		if (show_progress){
			cli::cli_progress_update(inc=1,status=ifelse(i<=0,"burn-in","recording"))
		}
		if (i <= 0) {
			if (delta>delta_LB &&  a>0.1) { # adjust delta down
				md <- median(curDistance,na.rm=TRUE)
				delta <- max(delta_LB,md)
			}
			A <- a^2/(0.1^2 + a^2) + 0.5 # or exp(2.5 * (a - 0.10))
			w <- a/(0.1 + a)
			## calculate a new value for Sigma0, by tracking global covariance S (unscaled)
			mu_batch <- rowMeans(curPar)
			S_batch  <- cov(t(curPar)) * (batchSize - 1)
			Dmu <- (mu_batch - mu)
			S <- S + S_batch + (Dmu %*% t(Dmu)) * ((n/(n+1))*batchSize)
			mu <- mu + Dmu/(n+1)
			n <- n+1
			C <- S/(n*batchSize-1) # C is now the updated global covariance
			if (abs(det(C)) < 1e-6 + 1e-6*norm(C)) C <- I*norm(Sigma1) # reset if in danger
			Sigma0 <- A*((1-w)*Sigma0 + w*C + 1e-8*Sigma1)
		} else {
			draws[,,i]  <- curPar
			distanceRecord[,i] <- curDistance
			acceptanceRate <- c(acceptanceRate,a)
			deltaRecord <- c(deltaRecord,delta)
		}
	}
	endTime <- Sys.time()
	cli::cli_progress_done()
	return(
		list(
			draws = draws,
			distances = distanceRecord,
			acceptanceRate = acceptanceRate,
			delta = deltaRecord,
			Sigma = Sigma0,
			time = difftime(endTime,startTime)
		)
	)
}

#' Performs and Approximate Bayesian Computation as a Particle Filter
#'
#' Given a set of simulation experiments (list), a model, parameter
#' boundaries, this function will draw a sample of parameters from the
#' posterior probability density of the given problem.
#'
#' This is a variant of ABC where the entire batch is simulated with
#' one call to the simulator. startPar is the initial batch to be
#' simulated: it is a matrix where columns are different parameter
#' vectors (e.g. prior sample members). In other words: `startPar[,i]`
#' must be a valid argument for the objectiveFunction.
#'
#' The objective function is a closure
#'
#' The Objective-Function `objectiveFuntion(P)` should return a matrix
#' with `n` rows, where `n` is the number of simulation experiments
#' (and thus data-sets), and `m` columns, where `m` is the number of
#' parameterizations `NCOL(P)`.
#'
#' @export
#' @param objectiveFunction a function that can simulate the model for
#'     a batch of parameter vectors provided as a matrix of columns
#'     (batches)
#' @param startPar a matrix that has the same shape as the desired
#'     sample, but transposed, this can be a sample from the prior or a
#'     pre-conditioned sample that approximates the posterior, e.g.: t(rprior(1000))
#' @param Sigma multivariate normal covariance of Markov chain
#'     transition kernel
#' @param delta ABC acceptance threshold, either a scalar, then it is
#'     the initial value of delta, or a pair of values, then it is the
#'     starting value and the final value of delta:
#'     `c(initialDelta,finalDelta)`
#' @param dprior a function that returns prior probability density
#'     values
#' @param parAcceptable is a rejection-shortcut function; if
#'     `parAcceptable(p)` returns `FALSE` for a specific value of `p`,
#'     it means that simulations shouldn't even be attempted.
#' @param verbose a logical value indicating whether log messages should be printed
#' @return a list containing a sample matrix and a vector of scores
#'     (values of delta for each sample)
#' @examples
#'   library(parallel)
#'   opt <- options(mc.cores=2) # use [detectCores()] here
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   ex <- experiments(m,as_ode(m,cla=FALSE))
#'   G <- as_cme(m)         # for Gillespie solver
#'   C <- generate_code(G)
#'   c_path(G) <- write_c_code(C)
#'   so_path(G) <- shlib(G)
#'   muX <- m$Parameter$value
#'   sdX <- m$Parameter$stdv
#'   rprior <- rNormalPrior(log(muX^2/(muX^2+sdX^2)),sqrt(log(1+sdX^2/muX^2)))
#'   dprior <- dNormalPrior(log(muX^2/(muX^2+sdX^2)),sqrt(log(1+sdX^2/muX^2)))
#'   s <- simstoch(ex,G,logParMap)
#'   O <- makeObjective(ex,s)
#'   X <- rprior(100)
#'   colnames(X) <- rownames(m$Parameter)
#'   if (interactive()) {
#'      posterior <- ABCSMC(O,t(X),Sigma=cov(X),dprior=dprior,delta=c(0.4,1.5))
#'   }
#'   options(opt) # restore original options
ABCSMC <- function(objectiveFunction, startPar, Sigma=2*cov(t(startPar)), dprior, delta=c(2,0.5),  parAcceptable=\(p){all(is.finite(p))}, verbose=getOption("uqsa.verbose", interactive())){
	delta <- sort(delta,decreasing=TRUE) # in case someone enters a range for delta, e.g. c(0.1,0.9) rather than c(initial,final)
	initialDelta <- delta[1]
	if (length(delta)>1){
		finalDelta <- delta[2]
	} else {
		finalDelta <- 0 # automatic lower bound
	}
	deltaLowerBound <- finalDelta
	if (verbose) cli::cli_alert_info(sprintf("allowed range for delta: [%g,%g]",deltaLowerBound,initialDelta))
	## initial delta, subject to change:
	delta <- initialDelta
	## prepare starting values
	stopifnot(is.matrix(startPar))
	batchSize <- NCOL(startPar)
	## calculate initial distances
	curPar  <- startPar
	curDistance <- colMeans(objectiveFunction(curPar))
	#stopifnot(all(is.finite(curDistance)))
	curPrior <- dprior(t(curPar))
	curWeight <- 1.0/curDistance # init
	curWeight <- curWeight/sum(curWeight)
	acceptanceRate <- 1.0                 # startPar has 100% acceptance, we don't reject any of them
	cli::cli_progress_bar(name="abc smc")
	while (delta > deltaLowerBound && acceptanceRate > 0.03) {
		newPar <- matrix(NA,NROW(startPar),0)
		newPrior <- numeric(0)
		newDistance <- numeric(0)
		accepted <- 0
		proposed <- 0
		# This while loop aggregates a new batch of points, using the current delta
		if (verbose) {
			cli::cli_progress_step(
				paste(
					"delta: {format(delta,digits=3)},",
					"accepted {accepted}/{proposed} point{?s}",
					"({round(100*(accepted/(proposed+1e-16)))}%)."
				),
				spinner=TRUE
			)
		}
		while (NCOL(newPar) < batchSize) {
			n <- max(100,batchSize - NCOL(newPar)) # don't bother suggesting anything too small
			proposed <- proposed + n
			## resample from previous batch:
			k <- sample(seq_along(curWeight),n,replace=TRUE,prob=curWeight)
			canPar <- curPar[,k]
			## jitter, using a Gaussian transition kernel
			canPar <- canPar + t(mvtnorm::rmvnorm(NCOL(canPar),numeric(NROW(startPar)),Sigma))
			l <- apply(canPar,2,parAcceptable) # hard-rejection
			if (!any(l)) next
			canPar <- canPar[,l,drop=FALSE]
			canPrior <- dprior(t(canPar)) # prior values
			l <- (canPrior>0.0)
			if (!any(l)) next
			canPar <- canPar[,l,drop=FALSE]
			canPrior <- canPrior[l]
			canDistance <- colMeans(objectiveFunction(canPar)) # vectorized
			l <- (canDistance < delta)
			if (any(l)){
				newPar <- cbind(newPar,canPar[,l,drop=FALSE])
				newPrior <- c(newPrior,canPrior[l])
				newDistance <- c(newDistance,canDistance[l])
				accepted <- accepted + sum(l)
			}
			if (verbose) cli::cli_progress_update()
		}
		if (verbose) cli::cli_progress_done()
		## newPar is now an aggregate new batch, possibly bigger than batchSize
		## we now calculate weights for newPar:
		t_curPar <- t(curPar)
		if (prod(dim(newPar))==0) stop("newPar is broken.")
		sum_W_K <- apply(newPar,2,\(z) {sum(curWeight*mvtnorm::dmvnorm(t_curPar,z,Sigma))})
		newWeight <- newPrior/sum_W_K
		newWeight <- newWeight/sum(newWeight)
		## save the generated batch as current values
		curPar <- newPar
		curDistance <- newDistance
		curWeight <- newWeight
		## update Sigma
		WSigma <- stats::cov.wt(t(curPar))
		Sigma <- 2*WSigma$cov
		## update delta
		k <- sample(seq_along(curWeight),batchSize,prob=curWeight,replace=TRUE)
		delta <- median(curDistance[k])
		acceptanceRate <- accepted/proposed
	}
	draws <- t(curPar[,k])
	colnames(draws) <- rownames(startPar)
	return(
		list(
			draws = draws,
			distances = curDistance[k],
			acceptanceRate = acceptanceRate
		)
	)
}

