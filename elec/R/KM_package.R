## This is to run simulations on the Kaplan-Markov methods
## April-May, 2009


# Given reported results (Z), make a new data.frame which is the
# truth (that can be 'audited' by looking at relevant precincts).
# 
# This is the generic small error generation used in trinomial paper
# and elsewhere as a baseline "normal" mode of operations.
#
# Param: p_d   Percent of precicnts with error
#       swing  Vote error.
#      uniform Sample amount of swing randomly from [-swing:swing], excluding 0.  
#              If false, the error is +/-swing
#
# Return: elec.data object holding the 'truth'.


#' making fake truth for electios
#' 
#' Make a random truth that is with the reported outcome, but has random error
#' scattered throughout.
#' 
#' Given reported results (Z), make a new data.frame which is the truth (that
#' can be 'audited' by looking at relevant precincts).
#' 
#' This is the generic small error generation used in trinomial paper and
#' elsewhere as a baseline "normal" mode of operations.
#' 
#' @param Z elec.data object.  The original reported results.
#' @param p_d chance a batch has error
#' @param swing max amount of error in votes.
#' @param uniform if yes, then error is from 1 to swing.  If no, then error is
#' swing.
#' @param seed random seed to ease replication
#' @param PID which column has batch IDs.
#' @return # Return: elec.data object holding the 'truth'.
#' @export make.random.truth
make.random.truth = function( Z, 
						p_d = 0.1, 
						swing = 10, 
						uniform = TRUE,
						seed=NULL,
						PID = "PID" )    {
	pwinners = Z$winners
	stopifnot( !is.null( Z$V[[PID]] ) )
	stopifnot( p_d > 0 && p_d < 1 )
	
	# calculate the swings
	if ( uniform ) {
		err.ovr = sample( c(-swing:-1, 1:swing), Z$N, replace=TRUE )
	} else {
		err.ovr = sample( c( -swing, swing), Z$N, replace=TRUE )
	}
	
	# make the truth.
	Z$V$swing = err.ovr
	Z$V$swing.ep = ifelse( runif(Z$N) < p_d, 
			calc.pairwise.e_p( Z, Z$V, err.override=err.ovr ), 0 )
	Z$V$taint = Z$V$swing.ep / Z$V$e.max
	
	Z = countVotes(Z)
	
	# make sure we did not flip the election!
	stopifnot( Z$winners == pwinners )
	
	Z
}



## Make an audit data.frame with the error being exactly 1 margin, and packed
## into a small number of precincts (with some potential for binding amount of
## error per precinct).
##
## Warning: error is not necessarily achievable as the discrete nature of 
## whole votes is disregarded.
##
## Param: max.taint - maximum amount of taint in a given precinct
##
## Return the vote matrix (a data.frame) with tot.votes, e.max, and taint computed (NOT the elec data object).


#' make.truth.opt.bad
#' 
#' Generate a ``truth'' that is optimally bad in the sense of the margin in
#' error is packed into as few precints as possible.
#' 
#' Make an audit data.frame with the error being exactly 1 margin, and packed
#' into a small number of precincts (with some potential for binding amount of
#' error per precinct).
#' 
#' Warning: error is not necessarily achievable as the discrete nature of whole
#' votes is disregarded.
#' 
#' @param Z elec.data object to make bad truth for.
#' @param max.taint max taint for any batch
#' @param max.taint.good max taint in good direction for any batch
#' @param WPM Use WPM bound on error.
#' @param add.good add this amount of margin in good error (i.e. for the
#' winner)
#' @param add.random add a random tweak to error
#' @return Return the vote matrix (a data.frame) with tot.votes, e.max, and
#' taint computed (NOT the elec data object).
#' @export make.opt.packed.bad
make.opt.packed.bad = function( Z,  max.taint = 1, 
						max.taint.good=max.taint, 
						WPM = FALSE, add.good = 0, add.random=FALSE ) {
	
	# Make sure we have the proper parameters.
	stopifnot( is.elec.data(Z) )
	stopifnot( !is.null(Z$V$e.max ) )
	
	# save since we will tweak this to pack error
	Z$V$e.max.original = Z$V$e.max
	
	# Compute maximum amount of error per precinct (and sampling weights)
	if ( WPM ) {
		Z$V$e.max = pmin( fractionOfVotesBound(Z, frac = 0.4),
								Z$V$e.max * max.taint )
	} else {
		Z$V$e.max = Z$V$e.max * max.taint
		if ( add.random ) {
			Z$V$e.max = Z$V$e.max * runif( length( Z$V$e.max ), 0, 1 )
		}
	}
	
	ord = order( Z$V$e.max, decreasing=TRUE )
	tot = 1 + add.good
	Z$V$error = 0
	# starting with biggest precinct and moving downward, 
	# start packing in error.  
	for ( i in ord ) {
		if ( Z$V$e.max[i] <= tot  && Z$V$e.max[i] > 0 ) {
			tot = tot - Z$V$e.max[i]
			Z$V$error[i] = Z$V$e.max[i]
		}
	}
	# At the end of the loop, all (nonzero size) precincts without error
	# have more space than the remaining total.

	# Put last bit of slop in.
	for ( i in rev(ord) ) {
		if ( Z$V$error[i] == 0 && Z$V$e.max[i] > 0 ) {
			Z$V$error[i] = tot
			break
		}
	}
	
	# Now add in good direction stuff to desired level
	if ( add.good > 0 ) {
		if ( WPM ) {
			Z$V$e.max = pmin( fractionOfVotesBound(Z, frac = 0.4),
								Z$V$e.max.original * max.taint.good )
		} else {
			Z$V$e.max = Z$V$e.max.original * max.taint.good
		}
		for ( i in ord ) {
			if ( Z$V$error[i] == 0 && Z$V$e.max[i] <= add.good  && Z$V$e.max[i] > 0 ) {
				add.good = add.good - Z$V$e.max[i]
				Z$V$error[i] = -Z$V$e.max[i]
			}
		}
	}

	# replace bounds to get taints right
	Z$V$e.max = Z$V$e.max.original
	Z$V$e.max.original = NULL
	
	Z$V$taint = Z$V$error / Z$V$e.max
	Z$V$taint[is.nan(Z$V$taint)] = 0
	
	Z$V
} # end make.opt.packed.bad




# Given the structure of some large election, make a small election by
# sampling batches (with replacement) from the full list.
# This first samples N precincts (and gets the totals
# from them) and then builds the 'truth' as normal using the make.audit() 
# method.
# Note different calls to this will produce different margins based on precincts
# selected.  
#
# WARNING: It is conceivable that the winner will flip due to the sampling, if the 
#    sample has too many batches for the loser.
# 
# Param: Z: The large election, holding precincts with size, votes, etc., that get
#        sampled.
#        N: The desired size of the new election.
#
# Return: Data frame with precinct information for the race.  NOTE- The reported vote
#    totals are just that, reported.

#' @rdname make.audit
#' @export
make.audit.from.Z = function( Z, N = 400, ... ) {
	Z$V = Z$V[ sample(1:nrow(Z$V), N, replace=TRUE), ]
	Z$audit=NULL
	Z = countVotes(Z) # regenerate totals.
	make.audit( Z=Z, N=N, ... )
}
	

# Make the election results that can be sampled from with the simulator.
# This method generates the true taint and sampling weights of all precincts
# in the race.  
# The taint is in column 'taint', sampling weights in 'e.max'
# Param:  method - if "tweak" (the default), then add random amounts of swing to some precincts,
#				and call that the "truth".
#               The other methods generate the truth according to various metrics.
# Return: Data frame with precinct information for the race.  NOTE- The reported vote
#    totals are just that, reported.


#' Make a fake audit given specified error for simulations
#' 
#' Functions that make fake audits given a specified error mechanism and a
#' elec.data object holding reported outcomes.
#' 
#' 
#' make.audit is to make the election results that can be sampled from with the
#' simulator.  This method generates the true taint and sampling weights of all
#' precincts in the race.  The taint is in column 'taint', sampling weights in
#' 'e.max'
#' 
#' make.audit.from.Z Given the structure of some large election, make a small
#' election by sampling batches (with replacement) from the full list.  This
#' first samples N precincts (and gets the totals from them) and then builds
#' the 'truth' as normal using the make.audit() method.  Note different calls
#' to this will produce different margins based on precincts selected.
#' 
#' WARNING: It is concievable that the winner will flip due to the sampling, if
#' the sample has too many batches for the loser.
#' 
#' @param Z elec.data object.  For make.audit.from.Z, this is the large
#' election, holding precincts with size, votes, etc., that get sampled to make
#' an election of a requested number of batches.
#' @param method the method of error generation.  if "tweak" (the default),
#' then add random amounts of swing to some precincts, and call that the
#' "truth".  The other methods generate the truth according to various metrics.
#' @param p_d percent chance of error in precinct (for ok method)
#' @param swing vote swing if batch has error (for ok method)
#' @param max.taint maximum taint allowed in batch
#' @param print.race print info on race to command line?
#' @param N The desired size of the new election.
#' 
#' @param \dots other arguments to the method functions
#' @return
#' 
#' Data frame with precinct information for the race.  NOTE- The reported vote
#' totals are just that, reported.
#' @author Miratrix
#' @seealso \link{truth.looker}
#' @export make.audit
make.audit = function( Z = NULL, 
					method = c( "tweak", "opt.bad", "opt.bad.WPM", "opt.bad.packed", "opt.bad.packed.WPM", "ok", "no error" ),
					p_d = 0.20, swing=20, max.taint=1, print.race=FALSE, ... ) {
						
	# Make sure we have the proper parameters.
	stopifnot( is.elec.data(Z) )
	
	if ( print.race ) {
		cat( "The Reported results: " )
		print(Z)
		print( tri.calc.sample(Z,guess.N=15) )
	}
		
	method = match.arg(method)
	if ( method != "tweak" ) {
		# make the truth
		truth = switch( method,
				"opt.bad" = make.truth.opt.bad(Z, ...)$V,
				"opt.bad.WPM" = make.truth.opt.bad(Z, bound="WPM", ...)$V,
				"opt.bad.packed" = make.opt.packed.bad(Z, max.taint=max.taint, ...),
				"opt.bad.packed.WPM" = make.opt.packed.bad(Z, max.taint=max.taint, WPM=TRUE, ...),
				"ok" = make.ok.truth( Z, num.off=16, amount.off=30, ... )$V,
				"no error" = Z$V )

		if ( length( grep("packed", method ) ) == 0 ) {
			audit = audit.totals.to.OS(Z,truth)

			## Compute the true e.p for all audit units, given the truth
			audit = compute.audit.errors(Z, audit, 
							w_p=weight.function("taint"), 
							bound.col="e.max")
			names(audit)[pmatch("err.weighted", names(audit))] = "taint"
		} else {
			audit = truth
		}
	} else {
		
		truth = make.random.truth(Z, p_d, swing)
		
		audit = truth$V
	}
	
	audit$taint[is.nan(audit$taint)] = 0
	audit$taint[audit$e.max==0] = 0
	
	if ( print.race ) {
		truth.looker( audit )
	}

	audit
}



# Given a list of all precincts and their true taints and their sampling weights (in data, 
# a data.frame), do a sequential audit at the specified alpha.
#
# Param: data, a data frame, one row per patch, with: tot.votes, e.max, taint
#        M - the maximum number of samples to draw before automatically escalating to 
#        a full recount.
#        return.Ps - Return the sequence of p-values all the way up to N.
#
# Return: stopPt - number of draws drawn
#         n - number of unique precincts audited


#' simulate KM audits
#' 
#' This takes an election and a truth and conducts a KM audit.
#' 
#' 
#' Given a list of all precincts and their true taints and their sampling
#' weights (in data, a data.frame), do a sequential audit at the specified
#' alpha.
#' 
#' @param data a data frame, one row per patch, with: tot.votes, e.max, taint
#' @param M the maximum number of samples to draw before automatically
#' escalating to a full recount.
#' @param alpha level of risk.
#' @param plot plot a chart?
#' @param debug debug diag printed?
#' @param return.Ps Return the sequence of p-values all the way up to N.
#' @param truncate.Ps Return Ps only up to where audit stopped.
#' @return stopPt - number of draws drawn n - number of unique precincts
#' audited
#' @export simulateIt
simulateIt = function( data, M = 50, alpha = 0.25, 
					   plot = FALSE,
					   debug=FALSE, return.Ps=FALSE, truncate.Ps=TRUE ) {
	#browser()
	lalpha=log(alpha)
	U = sum( data$e.max )
	U.factor = 1 - 1/U
	
	# Order the sequence of samples up to the possible max.
	sample = sample(1:nrow(data), M, prob = data$e.max, replace = TRUE)
	taints = data$taint[sample]

	# Calculate the individual step terms.
	Xs = log( U.factor / ( 1 - taints ) )

	# The P-values P_j are the sums of the Xs up to X_j.
	Ps = cumsum( Xs )
 	
	if ( debug ) {	print(summary(Ps))   }
	
	# find out whether audit would stop
	if ( min( Ps ) < lalpha ) {
		# Audit stopped... but where?
		stopPt = min( which( Ps < lalpha ) )
		
		ns = unique( sample[1:stopPt] )  # The list of precincts audited
		n = length(ns)
		ballots = sum( data$tot.votes[ns] )
		balPer = 100 * ballots / sum( data$tot.votes )
	} else {
		# We automatically escalated to a full recount.
		stopPt = M + 1# Inf
		n = nrow(data)
		ballots = sum( data$tot.votes )
		balPer = 100
	}
	
	if ( plot ) {
		PsP = ifelse( is.infinite(Ps), NA, Ps )
		#browser()
		ylim = range(PsP,lalpha, na.rm=TRUE)  #c(-2, log(20))
		plot( PsP, ylim=ylim, type="l", bty="n", yaxt="n" )
		abline( h=lalpha, col="red" )
		
		abline( v = ceiling( lalpha / log(U.factor) ) )
		
		# plot going to infinity, if that occured
		if ( any( is.na( PsP ) ) ) {
			pos = min( which( is.na( PsP ) ) )
			points( pos, ifelse( pos==1, 0, PsP[pos-1] ), pch=24 )
		}
		
		# Label point of certifying race.
		if ( stopPt <= M ) {  # < Inf
			segments( stopPt, ylim[1], stopPt, lalpha )#abline( v = stopPt )
			text( stopPt, lalpha, stopPt, pos=3 )
		}
	}


	if ( return.Ps ) {
		if ( truncate.Ps & stopPt < M ) {
			Ps = Ps[1:stopPt]
		}
		list( stopPt=stopPt, n=n, ballots=ballots, balPer=balPer, 
				Ps=Ps, low=min(Ps), high=max(Ps) )
	} else {
		list( stopPt=stopPt, n=n, ballots=ballots, balPer=balPer )
	}
}


####################################################################################
# For actual data
####################################################################################




#' KM Audit Sample Size Calc
#' 
#' Calc KM Optimal Sample Size
#' 
#' This is how many steps would be needed if no error was found with each step.
#' Obviously a bit idealistic, but still useful.
#' 
#' @param Z elec.data object
#' @param beta risk
#' @return Single number of batches to sample.
#' @export opt.sample.size
opt.sample.size = function( Z, beta=0.25 ) {
	U = sum( Z$V$e.max )
	ceiling( log( beta ) / log( 1 - 1/U ) )
}

# Order of audited precincts is assumed to be as given.


#' KM Audit Calculator
#' 
#' Do a KM audit given a specified list of audited batches for a specified
#' election.
#' 
#' This will do a single-stage KM audit as a consequence of doing the stepwise
#' version (since the single-stage is the same as the stepwise up to the number
#' of batches audited).
#' 
#' WARNING: This function is not fully debugged!
#' 
#' @param data Data frame holding audit data with taint and tot.votes as two
#' columns.
#' @param U Maximum total error bound (sum of e.max for all batches in race).
#' @param Z elec.data object for the race---the original reported results.
#' @param alpha Risk.
#' @param plot Plot the audit?
#' @param debug Print debugging info
#' @param return.Ps Return the stepwise P-values
#' @param truncate.Ps Return the stepwise P-values only up to the audit stop
#' point.
#' @return List of various things, including final p-value.
#' @author Miratrix
#' @references Stark, Miratrix
#' @export KM.audit
KM.audit = function( data, U, Z, alpha = 0.25, 
					   plot = FALSE,
					   debug=FALSE, return.Ps=FALSE, truncate.Ps=TRUE ) {
		
	lalpha=log(alpha)
	U.factor = 1 - 1/U
	
	taints = data$taint

	# Calculate the individual step terms.
	Xs = log( U.factor / ( 1 - taints ) )

	# The P-values P_j are the sums of the Xs up to X_j.
	Ps = cumsum( Xs )
 	
	if ( debug ) {	print(summary(Ps))   }
	
	# find out whether audit would stop
	if ( min( Ps ) < lalpha ) {
		# Audit stopped... but where?
		stopPt = min( which( Ps < lalpha ) )
		
		ns = !duplicated(data$PID) # The list of precincts audited
		n = sum(ns)
		ballots = sum( data$tot.votes[ns] )
		balPer = 100 * ballots / Z$total.votes
	} else {
		# We automatically escalated to a full recount.
		stopPt = Inf
		n = Z$N
		ballots = Z$total.votes
		balPer = 100
	}
	
	if ( plot ) {
		PsP = ifelse( is.infinite(Ps), NA, Ps )
		#browser()
		ylim = range(PsP,lalpha, na.rm=TRUE)  #c(-2, log(20))
		plot( PsP, ylim=ylim, type="l", bty="n", yaxt="n" )
		abline( h=lalpha, col="red" )
		
		abline( v = ceiling( lalpha / log(U.factor) ) )
		
		# plot going to infinity, if that occured
		if ( any( is.na( PsP ) ) ) {
			pos = min( which( is.na( PsP ) ) )
			points( pos, ifelse( pos==1, 0, PsP[pos-1] ), pch=24 )
		}
		
		# Label point of certifying race.
		if ( stopPt < Inf ) {
			segments( stopPt, ylim[1], stopPt, lalpha )#abline( v = stopPt )
			text( stopPt, lalpha, stopPt, pos=3 )
		}
	}


	if ( return.Ps ) {
		if ( truncate.Ps & stopPt < nrow(data) ) {
			Ps = Ps[1:stopPt]
		}
		list( stopPt=stopPt, n=n, ballots=ballots, balPer=balPer, Ps=Ps, low=min(Ps), high=max(Ps) )
	} else {
		list( stopPt=stopPt, n=n, ballots=ballots, balPer=balPer )
	}
}







#' Looking at fake ``truths'' for election simulations
#' 
#' This prints out total error in a fake truth for an election, and some other
#' info.
#' 
#' Utility function for debugging and understanding stuff.
#' 
#' Look at a specific "truth" and print out what total error, etc.  is.
#' 
#' @param data The data.frame returned from such things as make.audit
#' @return None.  Just does printout.
#' @export truth.looker
truth.looker = function( data ) {
	cat( "* Summary Statistics for True Error *\nBatches with taint:\n" )
	a = data[ data$taint != 0, ]
	if(nrow(a) < 20 ) {
		print(a)
	}
	print( summary( data$taint[data$taint != 0] ) )
	perT =  100*mean(data$taint !=0) 
	cat( sprintf( "# tainted = %d (%d%%)   total error = %.3f   U=%.3f\n", 
				nrow(a), round(perT), sum(a$taint * a$e.max ), sum(data$e.max) ) )

	cat( sprintf( "E[Taint] = %.3f\n", 
				sum( data$taint * data$e.max / sum(data$e.max) ) ) )
				
}





#' Pretty print KM audit plan
#' 
#' @param x A audit.plan.KM object, such as one returned by KM.calc.sample.
#' @param ... ignored
#' @export
print.audit.plan.KM = function (x, ...) 
{
  cat(sprintf("Audit plan: beta=%.2f   met=PPEB (KM) w/ %s\n", 
  						x$beta, x$bound))
  cat(sprintf("\tt=%.3f\t \t N=%d / %d\t n=%d\n", 
  						x$taint, x$N, x$Z$N, x$n ) )
  cat(sprintf("\tU=%.1f\t 1/U=%.4f\n", 
  						x$U, 1/x$U ) )
  cat(sprintf("\tskipped=%d\t mar lost=%.0f%%\n",
  						x$Z$N - x$N, 0 ) )
  cat(sprintf("\tE[# pcts audited]=%.1f\t\t E[votes audited]=%.1f\n",
  						x$E.p, x$E.vts ) )
}


# Taint is assumed to be the taint for _all_ batches (very conservative).
# If taint=0 then we have a good baseline.


#' Calculate sample size for KM-audit.
#' 
#' Calculate the size of a sample needed to certify a correct election if a KM
#' audit is planned.
#' 
#' 
#' @param Z elec.data object
#' @param beta Desired level of confidence.  This is 1-risk, where risk is the
#' maximum chance of not going to a full recount if the results are wrong.
#' Note that in Stark's papers, the value of interest is typically risk,
#' denoted $alpha$.
#' @param taint Assumed taint.  Taint is assumed to be the taint for all
#' batches (very conservative).  If taint=0 then we produce a good baseline.
#' @param bound Type of bound on the maximum error one could find in a batch.
#' 
#' @return A audit.plan.KM object.
#' @author Based on the KM audit by Stark.
#' @seealso KM.audit
#' @examples
#' 
#'   data(santa.cruz)
#'   Z = elec.data( santa.cruz, C.names=c("danner","leopold") )
#'   KM.calc.sample( Z, beta=0.75, taint=0 )
#' 
#' @export KM.calc.sample
KM.calc.sample = function( Z, beta=0.75, taint=0,
					bound = c("e.plus", "WPM", "passed"))
{
	 stopifnot( beta > 0 && beta < 1 )
	 alpha = 1 - beta  # alpha is the risk

    bound = match.arg(bound)
    if (bound == "passed") {
        if (is.null(Z$V$e.max)) {
            stop("No e.max column, and told to not calculate it")
        }
    }
    else if (bound == "e.plus") {
        Z$V$e.max = maximumMarginBound(Z)
    }
    else {
        Z$V$e.max = 0.4 * Z$V[[Z$tot.votes.col]]/Z$margin
    }
	 U = sum( Z$V$e.max )

	 if ( taint >= 1/U ) {
	 	n = Inf
	 } else {
	 	n = ceiling( log(alpha) / log( (1 - 1/U)/(1 - taint) ) )
    }
    V = Z$V

    if ( n == Inf ) {
    	Ep = Z$N
    	Evts = Z$total.votes
	 } else {
	 	Ep = nrow(V) - sum((1 - V$e.max/U)^n)
    	Evts = sum(V[[Z$tot.votes.col]] * (1 - (1 - V$e.max/U)^n))
    }
    res <- list(beta = beta, taint=taint, 
        U = U, n = n, E.p = Ep, E.vts = Evts, bound = bound, 
        Z = Z,
        N = sum(Z$V$e.max > 0) )
    class(res) <- "audit.plan.KM"
    res
}



