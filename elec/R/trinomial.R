#source( "~/Documents/elec_R/PPEB.R" )




#' Auditing with the Trinomial Bound: trinomial.bound and trinomial.audit
#' 
#' This method makes a contour plot of the optimization problem.
#' 
#' Note: alphas are multiplied by 100 to get in percents.
#' 
#' @param n Size of the sample (not precincts, but samples which could
#' potentially be multiple samples of the same precinct).
#' @param k The number of positive taints found in sample.
#' @param d The maximum size of a small taint. This is the threshold for being
#' in the middle bin of the trinomial.  All taints larger than d would be in
#' the largest error bin.
#' @param e.max The size of the largest error bin.  Typically 100 (for percent)
#' or 1.
#' @param xlim Range of possible values of p0 worth considering
#' @param ylim Range of possible values of pd worth considering
#' @param alpha.lvls List of alphas for which bounds should be calculated. The
#' first is the one that will be returned.  The others will be graphed.
#' @param zero.threshold Since the method calculates on a numerical grid, what
#' difference between alpha and the calculated probabilty should be considered
#' no difference.
#' @param tick.lines A list of bounds.  For these bound levels, add tick-lines
#' (more faint lines) to graph
#' @param alpha.lwd Line width for alpha line.
#' @param bold.first TRUE/FALSE.  Should first alpha line be in bold.
#' @param plot Should a plot be generated.
#' @param p.value.bound What is the bound (1/U) that would correspond to the
#' entire margin.  Finding the alpha corresponding to this bound is a method
#' for finding the p-value for the trinomial bound test.
#' @param grid.resolution How many divisions of the grid should there be?  More
#' gives greater accuracy in the resulting p-values and bounds.
#' @param \dots Extra arguments passed to the plot command.
#' 
#' @return List with characteristics of the audit and the final results.
#' \item{n}{ Size of sample.} \item{k}{Number of non-zero taints.}
#' \item{d}{Threshold for what a small taint is.} \item{e.max}{The worst-case
#' taint.} \item{max}{ The upper confidence bound for the passed alpha-level.}
#' \item{p}{A length three vector.  The distribution (p0, pd, p1) that achieves
#' the worst case.} \item{p.value}{ The p.value for the test, if a specific
#' worst-case bound 1/U was passed via p.value.bound.}
#' 
#' @seealso
#' 
#' See \code{\link{elec.data}} for information on the object that holds vote
#' counts.  See \code{\link{tri.sample}} for drawing the actual sample.  See
#' \code{\link{tri.calc.sample}} for figuring out how many samples to draw.
#' See \code{\link{tri.audit.sim}} for simulating audits using this method.
#' See \link{CAST.audit} for an SRS audit method.
#' @references See Luke W. Miratrix and Philip B. Stark.  (2009) Election
#' Audits using a Trinomial Bound.  https://www.stat.berkeley.edu/~stark/Vote/index.htm
#' 
#' @examples
#' 
#' 
#' # The reported poll data: make an elec.data object for processing
#' data(santa.cruz)
#' Z = elec.data(santa.cruz, C.names=c("leopold","danner"))
#' Z
#'
#' # Make a plan
#' plan = tri.calc.sample( Z, beta=0.75, guess.N = 10, p_d = 0.05,
#'                swing=10, power=0.9, bound="e.plus" )
#'
#' # Conduct the audit
#' data(santa.cruz.audit)
#' res = trinomial.audit( Z, santa.cruz.audit )
#' res
#' 
#' # Compute the bound.  Everything is scaled by 100 (i.e. to percents) for easier numbers. 
#' trinomial.bound(n=res$n, k = res$k, d=100*plan$d, e.max=100, p.value.bound=100/plan$T,
#'            xlim=c(0.75,1), ylim=c(0.0,0.25),
#'            alpha.lvls=c(25), asp=1,
#'            main="Auditing Santa Cruz with Trinomial Bound" )
#' 
#' @export trinomial.bound
trinomial.bound = function( n = 11, k = 2, d=40, e.max=100, 
				xlim=c(0.4,1), ylim=c(0,0.55),
				alpha.lvls=c(10),
				zero.threshold = 0.3,
				tick.lines=NULL,
				alpha.lwd = 2, 
				bold.first=FALSE,
				plot=TRUE,
				p.value.bound = NULL,
				grid.resolution=300,
				... ) {
					
					
					
## chance of getting something as or even less extreme
## than k errors of size d and no errors larger.
## (Step-Down Set)
prob.S = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 0:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * ( p_0 ^ (n - x) ) * ( (p_d) ^ x ) 
		} ) )
	}	
}

## Derivative of above w.r.t p_0
d.prob.S.d.p_0 = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 0:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * (n - x) * ( p_0 ^ (n - x - 1) ) * ( (p_d) ^ x ) 
		} ) )
	}
	
}

## Derivative of above w.r.t p_d
d.prob.S.d.p_d = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 1:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * (x) * ( p_0 ^ (n - x) ) * ( (p_d) ^ (x-1) ) 
		} ) )
	}
}


## calc prob.S on vector of p_0s
prob.Ss = function( x, ... ) { sapply( x, prob.S, ... ) }

	
## the bound given the probability vector
## with, if desired, the lambda penalty for 
## the alpha constraint.
err.bound = function( p_0, p_d, n = 11, k = 2, d=40, 
					e.max=100, 
					penalize=FALSE, alpha=0.05, ... ) {
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		b = p_d * d + (1 - p_0 - p_d) * e.max
		if ( penalize ) {
			b = b - 100000 * (prob.S( p_0, p_d, n, k ) - alpha)^2
		}
		b
	}
}

## gradient of error bound function, with d/dp_0 and d/dp_d
grad.err.bound = function( x, d=40, e.max=100, 
						penalize=TRUE, alpha=0.05, ... ) {
	p_0 = x[1]
	p_d = x[2]
	
	if ( p_0 + p_d > 1 ) {
		c( NA, NA )
	} else {
		prS = prob.S( p_0, p_d )
		
		c( -e.max - 200000 * (prS - alpha) * d.prob.S.d.p_0(p_0, p_d, ... ),
			d - e.max - 200000 * (prS - alpha) * d.prob.S.d.p_d(p_0, p_d, ... ) )
	}
}



## the bound function that takes probs as a list
err.bound.pen = function( x, ... ) {
   err.bound( x[1], x[2], ... )
}

					
					
					
	p0s = seq( xlim[[1]], xlim[[2]], length.out = grid.resolution )
	pds = seq( ylim[[1]], ylim[[2]], length.out = grid.resolution )

	## PLOT THE ALPHA LINES
	vSS <- Vectorize(prob.S, c("p_0", "p_d"))

	# outer product, make 2D array applying vSS to all values of p0s and pds
	alphas = 100 * outer( p0s, pds, vSS, n=n, k=k )
	
	if ( plot ) {
		lwd = rep( alpha.lwd, length( alpha.lvls ) )
		if ( bold.first ) {
			lwd[1] = 2 * lwd[1]
		}
		contour( p0s, pds, alphas, levels=alpha.lvls, lwd=lwd, frame=FALSE, 
				xlab=expression(pi['0']), drawlabels=FALSE,
				ylab=bquote( pi['d'] ~ ", d =" ~ .(round(d,digits=2)) ),
				method="edge", ... )
	
		## BOUNDING BOX
		if ( ylim[2] < 1 ) {
			y0 = ylim[2]
		} else {
			y0 = 1
		}
		x0 = 1 - y0
		
		if ( ylim[1] > 0 ) {
			y1 = ylim[1]
		} else {
			y1 = 0	
		}
		x1 = 1 - y1

		if ( x0 < xlim[1] ) {
			x0 = xlim[1]
			y0 = 1 - x0
		}
		if ( x1 > xlim[2] ) {
			x1 = xlim[2]
			y1 = 1 - x1
		}
		
		segments( x0, y0, x1, y1 )
		segments( x0, 0, x1, 0 )
		
	}
	
	## THE BOUND CONTOUR LINES
	vBND = Vectorize( err.bound, c("p_0","p_d") )

	bounds = outer( p0s, pds, vBND, n=n, k=k, d=d)
	
	if ( plot ) {
		contour( p0s, pds, bounds, lty=3, add=TRUE )
		if ( !is.null( tick.lines ) ) {
			contour( p0s, pds, bounds, lty=5, levels=tick.lines, add=TRUE )
		}
	}
	
	## Finding the max along the main alpha line

	alpha = alpha.lvls[[1]]
	del = abs( alphas - alpha ) < zero.threshold
	zt = zero.threshold

	while( sum( del, na.rm=TRUE ) < 75 ) {
		zt = zero.threshold + zt
		warning( "zero threshold too small--increasing it to ", zt )
		del = abs( alphas - alpha ) < zt
	}

	del = !is.na(del) & del
	b = bounds
	b[ !del ] = 0
	loc = which.max(b) - 1
	loc.y = pds[ 1 + floor( loc / ncol(b) ) ]
	loc.x = p0s[ 1 + loc %% ncol(b) ]
	if ( plot ) {
		points( loc.x, loc.y, pch=19 )
	}
	
	## find the p.value corresponding to the passed bound
	if (!is.null( p.value.bound ) ) {
		
		del = abs( bounds - p.value.bound ) < zero.threshold / 5
		del = !is.na(del) & del
		a = alphas
		a[ !del ] = 0
		loc = which.max(a) - 1
		p.value = max(a)
		loc.y = pds[ 1 + floor( loc / ncol(a) ) ]
		loc.x = p0s[ 1 + loc %% ncol(a) ]
		if ( plot ) {
			points( loc.x, loc.y, pch=20, col="red" )
		}
	} else {
		p.value = NULL
	}

		
	list( n=n, k=k, d=d, e.max=e.max, max=max(b), 
			p=c(loc.x, loc.y, 1-loc.x-loc.y),
			p.value=p.value ) 
}






## SIMULATION FUNCTION
## Given a matrix of votes, calculate the weights for all precincts
## and do a sample.  
## Then, if sim.audit=TRUE, assume that p_d percent of the precincts
## (at random) have error, and the errors are due to vote miscounts of
## size 'swing'.
## Param  n: Sample size to draw.
##     sim.audit: pretend that auditing occurs at passed rates.
## Return:   List of taints found in such a circumstance OR precincts selected
##      with relevant attributes (including simulated errors, if asked) OR 
##      the number of non-zero taints and the size of largest taint.


#' tri.audit.sim
#' 
#' 
#' This is a SIMULATION FUNCTION, and is not used for actual auditing of
#' elections.
#' 
#' Given a matrix of votes, calculate the weights for all precincts and then
#' draw a sample (using tri.sample).  Then, assuming that p\_d percent of the
#' precincts (at random) have error, and the errors are due to vote miscounts
#' of size 'swing', conduct a simulated ``audit'', returning the found
#' descrepancies.
#' 
#' 
#' @param Z elec.data object.
#' @param n Sample size to draw.
#' @param p_d The probability of a precinct having an error.
#' @param swing The size of the error, in votes.
#' @param return.type What kind of results to return: "statistics","taints", or
#' "precinct"
#' @param seed Random seed to use.
#' @param PID Column name of column holding unique precinct IDs
#' @param \dots Extra arguments passed to tri.sample
#' @return List of taints found in such a circumstance OR precincts selected
#' with relevant attributes (including simulated errors, if asked) OR the
#' number of non-zero taints and the size of largest taint.
#' @author Luke W. Miratrix
#' @seealso \code{\link{elec.data}} for the object that holds vote data.  See
#' \code{\link{tri.calc.sample}} for computing sample sizes for trinomial bound
#' audits.
#' @examples
#' 
#' 
#'   data(santa.cruz)
#'   Z = elec.data(santa.cruz, C.names=c("leopold","danner"))
#'   Z$V$e.max = maximumMarginBound( Z )
#'   ## Sample from fake truth, see how many errors we get.
#'   tri.audit.sim( Z, 10,  p_d=0.25, swing=10, return.type="precinct" )
#' 
#'   ## what does distribution look like?
#'   res = replicate( 200, tri.audit.sim( Z, 10,  p_d=0.25, swing=10 ) )
#'   apply(res,1, summary) 
#'   hist( res[2,], main="Distribution of maximum size taint" )
#' 
#' @export tri.audit.sim
tri.audit.sim = function( Z, n, p_d = 0.1, 
						swing = 5, 
						return.type = c("statistics","taints","precinct"),
						seed=NULL,
						PID = "PID",
						... )    {
	stopifnot( !is.null( Z$V[[PID]] ) )
	stopifnot( p_d > 0 && p_d < 1 )
	
	return.type = match.arg( return.type )	

	# make the truth.
	Z$V$swing.ep = ifelse( runif(Z$N) < p_d, 
			calc.pairwise.e_p( Z, Z$V, err.override=swing ), 0 )
	
	aud = tri.sample( Z, n, PID=PID, simplify=(return.type=="precinct"), ... )

	aud$taint =  aud$swing.ep / aud$e.max
	
	switch( return.type,
		"precinct" = aud,
		"statistics" = c( k=sum( aud$taint > 0 ), d=max( aud$taint ) ),
		"taints" = aud$taint )
}


## Generate sample.
##
## Param: n -- number of samples desired OR an audit.plan.tri object
## Return: List of precincts to audit (with weights--the number of times that
##        precinct was selected).


# @aliases tri.sample tri.sample.stats

#' Sample from List of Precincts PPEB
#' 
#' tri.sample selects a sample of precincts PPEB.  Namely, samples n times,
#' with replacement, from the precincts proportional to the weights of the
#' precincts.
#' 
#' The weights, if passed, are in the ``e.max'' column of Z\$V.
#' 
#' @param Z elec.data object
#' @param n Either a audit.plan.tri object (that contains n) or an integer
#' which is the size of the sample
#' @param seed Seed to use.
#' @param print.trail Print diagnostics and info on the selection process.
#' @param simplify If TRUE, return a data frame of unique precincts sampled,
#' with counts of how many times they were sampled.  Otherwise return
#' repeatedly sampled precincts seperately.
#' @param return.precincts Return the precincts, or just the precint IDs
#' @param PID The name of the column in Z\$V holding unique precinct IDs
#' @param known Name of column in Z\$V of TRUE/FALSE, where TRUE are precincts
#' that are considered ``known'', and thus should not be sampled for whatever
#' reason.
#' @return a sample of precincts. 
#' @author Luke W. Miratrix
#' @seealso \code{\link{trinomial.bound}} \code{\link{elec.data}}
#' \code{\link{tri.calc.sample}}
#' @examples
#' 
#' data(santa.cruz)
#' Z = elec.data( santa.cruz, C.names=c("danner","leopold") )
#' samp = tri.calc.sample( Z, beta=0.75, guess.N = 10, p_d = 0.05,
#'                swing=10, power=0.9, bound="e.plus" )
#' tri.sample( Z, samp, seed=541227 )
#' 
#' @export tri.sample
tri.sample = function( Z, n, seed=NULL, 
						print.trail=FALSE,
						simplify=TRUE,
						return.precincts = TRUE, 
						PID = "PID",
						known="known" ) {
	
	if ( is.audit.plan.tri( n ) ) {
		Z$V$e.max = switch( n$bound,
		 	"passed" = {
				if ( is.null( Z$V$e.max ) ) {
					stop( "No e.max column, and told to not calculate it" )
				}
				Z$V$e.max },
			"e.plus" = maximumMarginBound( Z ),
			"WPM" = 0.4 * Z$V[[Z$tot.votes.col]] / Z$margin
			)
		n = n$n 
	} else {
		if ( is.null(Z$V$e.max) ) {
			warning( "Computing e.max values to sample from" )
			Z$V$e.max = maximumMarginBound( Z )
		}
	}	
		
	stopifnot( n > 0 && n <= Z$N )
	
		
	if ( !is.null(seed) ) {
		cat( "setting seed to ", seed )
		set.seed( seed )	
	}
	
	stopifnot( !is.null( Z$V$e.max ) )
	
	## Note the replace=TRUE--technically, we should sample a SRS of vote units
	## but since we are looking at margins, "vote units" are abstract, arb small 
	## pieces of the margin.  So just sample with replacement.
	sample = sample( 1:nrow(Z$V), n, prob=Z$V$e.max, replace=TRUE )
	
	A = Z$V[sample,]
	if ( simplify ) {
		A = A[ order(A[[PID]]), ]
		A$count = table(A$PID)[ A$PID ]
		A = A[ !duplicated(A$PID), ]
	}
	if ( return.precincts ) {
		A
	} else {
		tri.sample.stats( A )
	}
}


#' Utility function for tri.sample
#'
#' A utility function returning the total number of unique precincts
#' and ballots given a sample.
#'
#' @param samp A sample, such as one returned from tri.sample
#' @return  the total number of unique precincts and ballots given a
#'   sample.
#' @export
tri.sample.stats = function( samp ) {
	
	c( sampled = nrow( samp ),
		votes = sum( samp$tot.votes ),
		mean.e.max = mean( samp$e.max ) )
		
}







## Calculate estimated sample size to do a trinomial bound that would certify
## assuming a given estimate of low-error error rate
##
## Param: bound -- e.plus, WPM, or use the passed, previously computed, e.max values in the Z object.


#' Calculate needed sample size for election auditing using the
#' Trinomial Bound
#'
#' Calculate an estimated sample size to do a trinomial bound that
#' would have a specified power (the chance to certify assuming a
#' given estimate of low-error error rate), and a specified maximum
#' risk of erroneously certifying if the actual election outcome is
#' wrong.
#'
#'
#' @param Z elec.data object
#' @param beta 1-beta is the acceptable risk of failing to notice that
#'   a full manual count is needed given an election with an actual
#'   outcome different from the semi-official outcome.
#' @param guess.N The guessed needed sample size.
#' @param p_d For the alternate: estimate of the proportion of
#'   precincts that have error.
#' @param swing For the alternate: estimate of the max size of an
#'   error in votes, given that error exists.
#' @param power The desired power of the test against the specified
#'   alternate defined by p\_d and swing.
#' @param bound e.plus, WPM, or use the passed, previously computed,
#'   e.max values in the Z object.
#' @return An \code{audit.plan.tri} object.  This is an object that
#'   holds information on how many samples are needed in the audit,
#'   the maximum amount of potential overstatement in the election,
#'   and a few other things.
#'   
#' @seealso
#'
#' See \code{\link{elec.data}} for information on the object that
#' holds vote counts.  See \code{\link{tri.sample}} for drawing the
#' actual sample.  The \code{audit.plan.tri} object holds the audit
#' plan information (e.g., number of draws, estimated work in ballots
#' to audit, etc.).  See \code{\link{trinomial.bound}} for analyzing
#' the data once the audit results are in.  See
#' \code{\link{tri.audit.sim}} for simulating audits using this
#' method.  See \link{CAST.audit} for an SRS audit method.
#'
#' @references See Luke W. Miratrix and Philip B. Stark.  (2009)
#'   Election Audits using a Trinomial Bound.
#'   http://www.stat.berkeley.edu/~stark
#' @examples
#'
#' data(santa.cruz)
#' Z = elec.data( santa.cruz, C.names=c("danner","leopold") )
#' tri.calc.sample( Z, beta=0.75, guess.N = 10, p_d = 0.05,
#'                swing=10, power=0.9, bound="e.plus" )
#'
#' @export tri.calc.sample
tri.calc.sample = function( Z, beta=0.75, guess.N = 20, p_d = 0.1, swing = 5, 
							power=0.90,
							bound = c("e.plus", "WPM", "passed") ) {

	bound = match.arg(bound)
	
	if ( bound == "passed" ) {
		if ( is.null( Z$V$e.max ) ) {
			stop( "No e.max column, and told to not calculate it" )
		}
	} else if ( bound == "e.plus" ) {
		Z$V$e.max = maximumMarginBound( Z )
	} else {
		Z$V$e.max = 0.4 * Z$V[[Z$tot.votes.col]] / Z$margin
	}
	
	## Compute the taints that would be due to swings as large as 
	## passed.
	Z$V$swing.ep = calc.pairwise.e_p( Z, Z$V, err.override=swing )
	Z$V$taint = Z$V$swing.ep / Z$V$e.max
	
	V = Z$V[ order( Z$V$e.max, decreasing=TRUE ), ]
	cumMass = cumsum(V$e.max)
	
	T = sum( V$e.max )
	if ( p_d > 0 ) {
		P = (power^(1/guess.N) - 1 + p_d) / p_d
		cut = sum( cumMass <= P * T)
		d.plus=max(V$taint[1:cut])
	} else { 
		stopifnot( p_d == 0 )
		P = 1
		cut = nrow(V)
		d.plus=0
	}
	
	n = ceiling( log( 1 - beta ) / log( 1 + d.plus * p_d - 1/T ) )
	
	## calculated expected amount of work
	Ep = nrow(V) - sum( (1-V$e.max/T)^n )
	Evts =  sum( V[[Z$tot.votes.col]] * ( 1 - (1-V$e.max/T)^n ) )
	
	res <- list( beta=beta, stages=1, beta1=beta, 
				P=P, cut=cut, swing=swing, d.plus=d.plus, p_d = p_d, T=T, n=n,
				E.p = Ep, E.vts = Evts,
				bound=bound,
				Z = Z )
				
	class(res) <- "audit.plan.tri"
	res	
}





#' Conduct trinomial audit
#' 
#' \code{trinomial.audit} converts the audited total counts for candidates to
#' overstatements and taints. \code{trinomial.bound} calculates the trinomial
#' bound given the size of an audit sample, the number of non-zero errors, and
#' the size of the small-error threshold. It can also plot a contour of the
#' distribution space, bounds, and alpha lines.
#' 
#' Right now the p-value is computed in a clumsy, bad way.  A grid of points
#' over (0, xlim) X (0, ylim) is generated corresponding to values of p0 and
#' pd, and for each point the mean of that distribution and the chance of
#' generating an outcome as extreme as k is calculated.  Then the set of points
#' with an outcome close to alpha is extrated, and the corresponding bound is
#' optimized over this subset.  Not the best way to do things.
#'
#' @param Z An elec.data object that is the race being audited.
#' @param audit A data.frame with a column for each candidate and a row for
#' each audited precinct, holding the audit totals for each candidate. An
#' additional column, \code{count}, holds the number of times that precinct was
#' sampled (since sampling was done by replacement).
#' @export
trinomial.audit = function( Z, audit ) {

	audit$e.max = maximumMarginBound( Z, audit )
	audit = audit.totals.to.OS(Z, audit)
	errs = compute.audit.errors( Z, audit=audit, 
					w_p=weight.function("taint"), 
					bound.col="e.max"  )
			
	audit$MOS = errs$err * Z$margin
	audit$taint = errs$err.weighted
	k = sum( (audit$taint > 0) * (audit$count) )
	d.max = max(audit$taint)

	list( n=sum(audit$count), k=k, d.max=d.max, audit=audit )
}
