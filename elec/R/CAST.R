# Implementation of CAST system of Stark




##### CAST ######








#' @title
#' Construct a sample for auditing using CAST
#' 
#' @description 
#' Collection of functions for planning and evaluating results of a CAST
#' election audit.  CAST is a system devised by Dr. Philip B., Stark, UC
#' Berkeley Department of Statistics.
#' 
#' \code{CAST.calc.sample} determines what size SRS sample should be drawn to
#' have a reasonable chance of certification if the election does not have
#' substantial error.  It returns an \code{audit.plan}. \code{CAST.sample}
#' takes the audit.plan and draws a sample to audit. \code{CAST.audit} takes
#' audit data (presumably from the audit of the sample drawn in previous step)
#' and analyzes it.
#' 
#' Make an audit.plan given reported results for an election.  It gives back what to
#' do for a single stage.  If stages is > 1, then it adjusts beta appropriately.
#' 
#' 
#' @param Z elec.data object (voter matrix)
#' @param beta the confidence level desired - overall chance of correctly escalating a bad election to full recount
#' @param stages number of auditing stages. Each stage will have the same
#' confidence level, determined by a function of beta.  A value of 1 is a
#' single-stage audit.
#' @param t The maximum amount of error, in votes, expected. Threshold error for escalation -- if >= 1 then number of votes, otherwise
#'                fraction of margin.
#' @param as.taint Boolean value.  TRUE means interpret $t$ as a taint in
#' $[0,1]$ by batch (so the threshold error will be batch-specific).  FALSE
#' means interpret $t$ as a proportion of the margin or as number of votes (as
#' described above).
#' @param small.cut Cut-off in votes--any precincts with potential error
#' smaller than this value will not be audited and be assumed to be worst case
#' error.
#' @param strata Name of the stratification column of Z.  Not needed if audit
#' plan also being passed in case of CAST.sample. NULL means single strata.
#' @param drop Vector of precincts to drop for whatever reasons (such as they
#' are already known).  This is a vector of TRUE/FALSE.
#' @param method Method of calculation.
#' @param calc.e.max Should the e.max be taken as given, or recalculated?
#' @param bound.function What function should be used to calculate worst-case
#' potential error of precincts.
#' @author Luke W. Miratrix
#' @seealso \code{\link{elec.data}} for a description of the object that holds
#' precinct-level vote records.  See \code{\link{tri.calc.sample}} for a PPEB
#' auditing method.  See \code{\link{CAST.calc.opt.cut}} for calculating
#' optimal cut-offs to keep needed sample size low. Also see
#' \code{\link{sim.race}}, \code{\link{do.audit}}, \code{\link{make.sample}},
#' and \code{\link{make.truth}} for doing simulation studies of this method.
#' 
#' @references Philip B. Stark. CAST: Canvass Audits by Sampling and Testing.
#' University of California at Berkeley Department of Statistics, 2009. URL:
#' http://statistics.berkeley.edu/~stark/Preprints/cast09.pdf.  Also see
#' http://www.stat.berkeley.edu/~stark/Vote/index.htm for other relevant
#' information.
#' @examples
#' 
#'         ## Make an example cartoon race (from Stark paper)
#' 	Z = make.cartoon()
#' 
#'         ## What should we do?
#' 	samp.info = CAST.calc.sample( Z )
#' 	samp.info
#' 
#'         ## Draw a sample.
#' 	samp = CAST.sample( Z, samp.info$ns )
#'         samp
#' 
#'         ## Analyze what a CAST audit of santa cruz would entail
#'         data(santa.cruz)
#'         Z = elec.data( santa.cruz, C.names=c("leopold","danner") )
#'         CAST.calc.sample( Z, beta=0.75, stages=1, t=5, small.cut=60)
#' @export
CAST.calc.sample = function( Z, beta = 0.9, stages=1, t=3, as.taint=FALSE,
				small.cut = NULL,
				strata=NULL, drop=NULL, 
				method=c("select", "binomial","hypergeometric"),
				calc.e.max = TRUE,
				bound.function = maximumMarginBound ) {

	method=match.arg(method)
	
	if ( !is.null(strata) && is.null( Z$V[[strata]] ) ) {
		warning( "No strata column of name '", strata, "'--assuming single strata" )
		strata=NULL
	}
	
	if ( is.null( drop ) ) {
		Z$V$known = FALSE
		drop="known"
	}
	
	beta1 = beta^(1/stages)
	
	# convert t to fraction of margin
	if ( t >= 1 ) { t = t / Z$margin }
	
	# Ignore small precincts?  Even if not,
	# there is no point in auditing precincts smaller than
	# t.
	if ( as.taint ) {
		# do nothing---in the taint world, nothing is too small (unless
		# it is an empty precint)
		small.cut = 0
	} else if ( is.null( small.cut ) ) {
		small.cut = t
	} else if ( small.cut >= 1 ) {
		small.cut = small.cut / Z$margin
	}
	
	if ( calc.e.max ) {
		Z$V$e.max = bound.function( Z )
	}
	
	Z$V$small = Z$V$e.max <= small.cut
	
	# do not consider all precincts that are either known or too small to pay
	# attention to.
	skip = Z$V[[drop]] | Z$V$small
	Z$V$skip = skip
	
	if ( is.null(strata) ) {
		strat.size = sum( !skip )
	} else {
		strat.size = table( Z$V[ !skip, strata ] )
	}
	
	# Reduce the threshold by all non-known precincts that we are skipping,
	# as we must assume the error in them is as large as possible.
	# Also calc new N (number of remaining batches to select from).
	threshold = 1 - sum( Z$V$e.max[ !Z$V[[drop]] & Z$V$small ] )
	N = Z$N - sum(skip)
	
	if ( threshold <= 0 ) {
		# We are doomed.  Must audit everything.
		q = -1
		n = Z$N
		
	} else {	
		if ( as.taint ) {
			q = find.q( Z$V, drop=skip, t, "e.max", threshold=threshold,
							w_p = weight.function("taint") )
		} else {
			q = find.q( Z$V, drop=skip, t, Z$tot.votes.col, threshold=threshold )
		}
		
		stopifnot( N >= q )
	
	
		if ( method=="select") {
			method = ifelse( length(strat.size)==1, "hypergeometric","binomial")
		}
	
		if ( q == 0 ) {
			n = N
		} else if ( method=="hypergeometric" ) {
			n = ceiling( uniroot( function(x) { 
				lchoose( N - q, round(x) ) - lchoose( N, round(x) ) - log(1-beta1) }, 
								  c(0,N-q) )$root )
		} else {
			n = ceiling( log( 1-beta1 ) / log( ( N - q ) / N ) )
		}
	}
	
	if ( n > N ) {
		n = N
	}

	ns = ceiling( n * strat.size / N )
	
	# calculate estimated work
	V2 = Z$V[ !skip, ]
	if ( is.null( strata ) ) {
		E.votes = mean(V2$tot.votes) * ns
	} else {
		mns = tapply( V2$tot.votes, V2[[strata]], mean )
		E.votes = ns * mns[names(ns)]
	}
	
	res = list( beta=beta, beta1 = beta1, stages=stages, t=t, as.taint=as.taint, 
				q=q, N=N, n=n, 
				stratas=strat.size, strata=strata, ns=ns, method=method, Z=Z, 
				threshold=threshold, skipped = sum(skip), 
				bound.function=bound.function,
				E.votes=E.votes )
	class(res) = "audit.plan"
	res
}




#' Calculate Optimal CAST plan
#' 
#' With CAST, it is sometimes advantageous to set aside small precincts and
#' assume they are entirely in error so as to reduce the total number of
#' precincts in the pool that we sample from.  This trade-off can increase the
#' power of the audit or, in other terms, allow us to sample fewer precincts as
#' the chance of nabbing the large, dangerous ones is larger.
#' 
#' Of all cuts that produce the smallest \code{n}, it returns the smallest cut
#' (since sometimes multiple cut-offs lead to the same sample size).
#' 
#' This function also plots the trade-off of sample size for a specific cut, if
#' the plot flag is TRUE.
#' 
#' This function iteratively passes increasing values of \code{small.cut} to
#' \code{\link{CAST.calc.sample}} and examines the resulting \code{n}.
#' 
#' @param Z The elec.data object
#' @param beta 1-\code{beta} is the risk of the audit failing to notice the
#' need to go to a full manual count if it should.
#' @param stages Number of stages in the audit.
#' @param t The allowed vote swing that is not considered a material error.
#' @param plot TRUE/FALSE.  Plot the trade-off curve.
#' @param \dots Extra arguments to the plot command.
#' @return Returns a list.  \item{cut}{ Size of the optimal cut.  All precincts
#' with an error smaller than or equal to cut would not be audited, and instead
#' be assumed to be in full error. } \item{n}{ Corresponding needed sample size
#' given that cut. } \item{q}{ The number of tainted precincts that would be
#' needed to throw the election, beyond the ones set aside due to being smaller
#' than \code{cut}.}
#' @author Luke W. Miratrix
#' @examples
#' 
#' 
#'         ## Find optimial cut for  determining which small precincts that
#'         ## we would set aside and not audit in Santa Cruz
#'         data(santa.cruz)
#'         Z = elec.data( santa.cruz, C.names=c("leopold","danner") )
#' 
#'         CAST.calc.opt.cut( Z, beta=0.75, stages=1, t=5, plot=TRUE )
#' 
#' @export CAST.calc.opt.cut
CAST.calc.opt.cut = function( Z, beta = 0.9, stages=2, t=3, plot=FALSE, ... ) {

	cuts = t:max(2*Z$V$tot.votes)
	ns = rep( NA, length(cuts) )
	qs = rep( NA, length(cuts) )
	cnt = 1
	done = FALSE
	while( !done ) {
		
		s = CAST.calc.sample( Z, beta, stages, t, small.cut=cuts[[cnt]], ... )
		if ( s$q < 0 ) {
			done = TRUE
		} else {
			ns[cnt] = s$n
			qs[cnt] = s$q
		#	cat( "on ", cuts[[cnt]], " - ", ns[cnt], "\n" )
			cnt = cnt+1
		}
	}
	names(ns) = cuts
	ns = ns[1:(cnt-1)]
	qs = qs[1:(cnt-1)]
	cuts = cuts[1:(cnt-1)]
	
	if ( plot ) {
		plot( names(ns), ns, ylim=c(0,max(ns)), type="s", main="Sample Size by Small Cut", pch=19,
		ylab="sample size needed", xlab="cut-off for cutting out small precincts" )
		scale = max(qs) / max(ns) 
		qsU = unique( qs )
		points( names(ns), qs / scale, type="s", col="red" )
		axis( side=4, at=qsU / scale, labels=qsU )
	}
	cuts = data.frame( cut=cuts, n=ns, q=qs )
	v = which.min( cuts$n )
	cuts[v, ]
}


				

#' Sample from the various strata according to the schedule set by 'ns'.
#' Ignore all precincts that are known (i.e., have been previously audited).
#'
#' @inheritParams CAST.calc.sample
#' 
#' @param ns EITHER an audit.plan or a vector of sample sizes for the strata.
#' Names must correspond ot the names of the strata.  If ns is an audit plan,
#' then the strata variable should not be passed as well.
#' @param seed Seed to use--for reproducability.
#' @param print.trail Print out diagnostics.
#' @param known The column of known precincts that should thus not be selected.
#' Similar to "drop", above.
#'
#' @return: List of precincts to be audited.
#' @examples
#' Z = make.cartoon()
#' samp.info = CAST.calc.sample( Z )
#' samp.info
#' samp = CAST.sample( Z, samp.info )
#' 
#' @export
CAST.sample = function( Z,  ns,  strata=NULL, seed=NULL, 
						print.trail=FALSE, known="known" ) {
	pt = function( ... ) { if ( print.trail ) { cat( ..., "\n" ) } }
		
	if ( is.audit.plan(ns) ) {
		strata = ns$strata
		ns = ns$ns
	}
	
	if ( !is.null( seed ) ) {
          set.seed( seed )
          pt( "Setting seed to ", seed )
	}
	
	if ( !is.null( Z$V[[known]] ) ) {
		V = Z$V[ !Z$V$known, ]
	} else {
		V = Z$V
	}

	if ( is.null( strata ) ) {
		stopifnot( length(ns) == 1 )
	
		ids = sample( nrow(V), ns )
		pt( "Full sample: ", ids )
		
		V$PID[ids]

	} else {
		strat = split( V, V[[strata]] )
	
		l = lapply( 1:length(ns), function( X ) {
			st = names(ns)[[X]]
			pids = sort( strat[[ st ]]$PID )
			ids = sample( length(pids), ns[[X]] )
			pt( "Strata", st, ": ", ids )
		
			pids[ids]
		} )
		unlist( l )
	}
}


#' Given audit data, compute p.values and all that.
#'
#'
#' @inheritParams CAST.calc.sample
#' @param plan An audit.plan object that the audit was conducted
#'   under.
#' @param audit A data.matrix holding the audit data, if the Z object
#'   does not have one, or if it is desirable to override it.  If both
#'   the Z object has an audit object and audit is not null, it will
#'   use this parameter and ignore the one in Z.
#' @param ...  Passed to CAST.calc.sample if plan is null and needs to
#'   be regenerated.
#' @export
CAST.audit = function( Z, audit=NULL, plan=NULL, ... ) {
	if ( is.null( plan ) ) {
		plan = CAST.calc.sample( Z, ... )
	}

	if ( is.null(audit) ) {
		plan$Z$audit = Z$audit
	} else {
		plan$Z$audit = audit
	}
		
	res = stark.test.Z( plan$Z, drop="skip", max_err=plan$bound.function,
			bound.col=Z$tot.votes.col,
			strat.col=plan$strata )
	res$certify = res$p.value < (1 - plan$beta1)
	
	cat( "Certify election: ", res$certify, "\n" )
	res
}



## Given the reported vote table, Z, and the actual truth (simulated)
## (a Z matrix with same precincts), and a list of precincts to audit,
## do the audit.  If audit.names
## is null and the ns is not null, it will sample from precincts via
## CAST.sample automatically.
##
## Return:   Overstatments for each candidate for each precinct.


#' do.audit
#' 
#' Given a list of precincts to audit, the truth (as an elec.data object), and
#' the original votes (also as an elec.data object), do a simulated CAST audit
#' and return the audit frame as a result.
#' 
#' Given the reported vote table, Z, and the actual truth (simulated) (a Z
#' matrix with same precincts), and a list of precincts to audit, do the audit.
#' If audit.names is null and the ns is not null, it will sample from precincts
#' via CAST.sample automatically.
#' 
#' @param Z elec.data object
#' @param truth another elec.data object--this one's vote counts are considered
#' "true"
#' @param audit.names name of precincts to audit.  Correspond to rownames of
#' the Z and truth elec.data objects.
#' @param ns List of sample sizes for strata. If this is passed, this method
#' will randomly select the precincts to audit.  In this case audit.names
#' should be set to NULL.
#' @return Overstatments for each candidate for each precinct.
#' @author Luke W. Miratrix
#' @seealso \link{CAST.audit} for how to run the CAST auditing method.  See
#' \code{\link{make.sample}} and \code{\link{make.truth}} for generating fake
#' situations for doing simulation studies of the CAST method.  See
#' \link{AuditErrors} and \code{\link{audit.totals.to.OS}} for utility
#' functions handing processing of audit data.
#' @examples
#' 
#' Z = make.cartoon(n=200)
#' truth = make.truth.opt.bad(Z, t=0, bound="WPM")
#' samp.info=CAST.calc.sample(Z, beta=0.75, stages=1, t=5 )
#' audit.names = CAST.sample( Z, samp.info )
#' do.audit( Z, truth, audit.names )
#' 
#' 
#' @export do.audit
do.audit = function( Z, truth, audit.names, ns=NULL ) {
	
	if ( is.null( audit.names ) ) {
		audit.names = CAST.sample( ns )
	}
	rownames(truth$V) = truth$V$PID
	truth$V = truth$V[ Z$V$PID, ]  # get in same order
	
	os = truth$V
	os[Z$C.names] =  Z$V[Z$C.names] - os[Z$C.names]
	os$clear = apply( os[Z$C.names], 1, function(X) { sum( abs(X) ) == 0 } )
	os = os[ os$PID %in% audit.names, ]
	
	os
}





#' Simulate CAST audits to assess performance
#' 
#' Simulate a race (using the \code{\link{make.cartoon}} method) and run a CAST
#' audit on that simulation.  CAST is a system devised by Dr. Philip B., Stark,
#' UC Berkeley Department of Statistics.
#' 
#' 
#' @param beta the confidence level desired
#' @param stages number of auditing stages. Each stage will have the same
#' confidence level, determined by a function of beta.
#' @param print.trail Print out diagnostics.
#' @param n Desired sample size.
#' @param truth.maker Function to generate "truth"
#' @return A vector of 3 numbers.  The first is the stage reached.  The second
#' is the total number of precincts audited.  The third is 0 if the audit
#' failed to certify (i.e. found large error in the final stage), and 1 if the
#' audit certified the election (did not find large error in the final stage).
#' @author Luke W. Miratrix
#' @seealso See \code{\link{CAST.audit}} and \code{\link{CAST.calc.opt.cut}} for
#' methods regarding CAST audits. Also see \code{\link{do.audit}},
#' \code{\link{make.sample}}, and \code{\link{make.truth}} for doing other
#' simulation studies of this method.
#' @references See http://www.stat.berkeley.edu/~stark/Vote/index.htm for
#' relevant information.
#' @examples
#' 
#'      ## See how many times the CAST method fails to catch a wrong
#'      ##  election in 20 trials.
#'      replicate( 20, sim.race( beta=0.75, stages=2, truth.maker=make.truth.opt.bad) )
#' 
#'      ## Now see how much work the CAST method does for typical elections.
#'      replicate( 20, sim.race( beta=0.75, stages=2, truth.maker=make.ok.truth) )
#' 
#' @export sim.race
sim.race = function( n=800, beta=0.75, stages=2, 
						truth.maker=make.truth.opt.bad,
						print.trail=FALSE) {

	Z = make.cartoon(n=n)
	Z$V$known = FALSE
	rownames(Z$V) = Z$V$PID
	Z
	
	truth = truth.maker(Z)
	rownames(truth$V) = truth$V$PID
	truth
			
	s = 0
	tots = rep( 0, stages )

	while ( s < stages && (s == 0 || t > samp.info$t) ) {
		s = s + 1
		samp.info = CAST.calc.sample( Z, stages=stages, beta=beta, drop="known" )
		
		tots[s] = sum( samp.info$ns )
		
		## add entropy and then use this function to generate table of audits
		audit.names = CAST.sample( Z, samp.info, 
							print.trail=print.trail, known="known" )
		aud = do.audit( Z, truth, audit.names )
		Z$audit = aud
		
		t = compute.stark.t( Z, "tot.votes" )
		
		if ( t > samp.info$t ) { # escalate!
			# replace count information with the 'true' audit
			# information for all audited precincts.
			Z$V[audit.names,] = truth$V[audit.names,]
			# mark these precincts as known so that future stages will
			# not audit again.
			Z$V[ audit.names, "known" ] = TRUE
			# rebuild Z to update margin totals, etc.
			Z = elec.data( Z$V, Z$C.names )  
		}
	}
	c( s, sum(tots), t < samp.info$t )
}



