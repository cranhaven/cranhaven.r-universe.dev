### election functions

### Testing using simple random sample of precincts or stratified SRS of precincts.
### Miratrix Mar, 08
### From 4.1 - Stark





#' @title Functions that Compute Error Levels Given Audit Data
#' @name AuditErrors
#' 
#' @description 
#' Calculate the error amounts for all precincts in Z that were audited from
#' the audit data, given as overstatement errors for all candidates.
#' 
#' compute.audit.errors uses the calc functions and the weight functions in a
#' 1-2 combination.
#' 
#' calc.pairwise.e\_p() is often used with an err.override for simulation
#' studies and whatnot to see what a fixed vote impact would have on taints for
#' trinomial.
#' 
#' @param Z elec.data object
#' @param err.override Assume a baserate of this amount of error everywhere,
#' ignoring audit data. If non-null, use this as the found error in votes
#' rather than the actual errors found in the audit.
#' @param audit The audit object, if it is not in the Z object, or if some
#' other object other than the one in the Z object is desired to be considered
#' as the audit object.  Used by the simulation functions to generate errors
#' for some fixed amount of error in conjunction with the err.override.
#' @return compute.audit.errors returns a new audit table from Z with two new
#' columns, err and err.weighted, corresponding to the errors found in each
#' audited precinct before and after the weight function has been applied to
#' them.
#' @note Z must have an audit component, or one must be passed, for this
#' function to make sense!  Remember that audit objects have overstatements,
#' NOT total votes for candidates.  With err.override being set this is less
#' relevant as the actual votes are usually ignored.
#' @author Luke W. Miratrix
#' @seealso See \code{\link{audit.totals.to.OS}} for a utility function that
#' handles processing of audit data.
NULL





### The various weight functions, packaged in a single object


#' weight functions
#' 
#' This function produces weight functions to reweight found audit miscounts.
#' 
#' The functions are no weighting, weighted by size of precint, weight by size,
#' after a slop of 2 votes has been taken off, and weighing for pairwise margin
#' tests, and finally, the taint weight function that takes maximum error in
#' precincts and gives a ratio of actual error to maximum error.
#' 
#' @param name name of function desired
#' @return A two-element list of two functions, the second being the inverse of
#' the first.  All the functions have three parameters, x, b\_m, and M, which
#' are the things to weight, the bound on votes (or maximum error in
#' precincts), and the (smallest) margin.
#' @author Luke W. Miratrix
#' @export weight.function
weight.function = function( name=c("no.weight","weight","weight.and.slop", "margin.weight","taint") ) {
  name = match.arg( name )
  switch( name,
     
         ## no weighting
         no.weight= c( function( x, b_p, M ) { x + 0 * b_p }, 
           function( x, b_p, M ) { x + 0 * b_p } ),
         
         ## weighted by size of precint
         weight = c( function( x, b_p, M ) { x / b_p }, 
           function( x, b_p, M ) { x * b_p } ),
         ## weight by size, after a slop of 2 votes has been taken off
         weight.and.slop = c( function( x, b_p, M ) { pmax( x - 2, 0 ) / b_p },
           function( t, b_p, M ) { t * b_p + 2 } ),
         ## for pairwise margin tests
         margin.weight = c( function( x, b_p, M ) { pmax(x - 2/M, 0) / b_p },
           function( x, b_p, M ) { x * b_p + 2/M } ),
         ## taint -- divide by maximum error in the precinct
         taint = c( function( x, b_p, M ) { x / b_p },
            function( x, b_p, M ) { x * b_p } )
           )
}



#' @title Election Audit Error Bound Functions
#'
#' @description This is one of the various bounding functions used to
#' bound the maximum amount of error one could see in a single audit
#' unit.
#'
#' maximumMarginBound returns the maximum margin reduction for each
#' precint by computing all margin reductions between pairs of winners
#' & losers and then scaling by that pair's total margin to get a
#' proportion and then taking the max of all such proportions (usually
#' will be the last winner to the closest loser).
#'
#'
#' @param Z The elec.data object.
#' @param votes The data.frame to compute the maximumMarginBounds for.
#'   If null, will return all bounds for all precincts in Z.
#' @return Vector (of length of precincts) of maximum possible error
#'   for each precinct.
#' @author Luke W. Miratrix
#' @export
maximumMarginBound = function( Z, votes=NULL ) {
## return the maximum margin reduction for each precint by computing
## all margin reductions between pairs of winners & losers and then
## scaling by that pair's total margin to get a proportion and then
## taking the max of all such proportions (usually will be the last 
## winner to the closest loser).
## Return:   Vector (of length of precincts) of maximum possible error for 
##           each precinct.
    
  if ( is.null( Z$winners ) ) {
    stop( "Need to count votes before computing bouns in maximumMarginBound()" )
  }
  if ( is.null( Z$Ms ) ) {
    stop( "Need to compute pairwise margins before computing error bounds." )
  }
  if ( is.null( Z$V[[Z$tot.votes.col]] ) ) {
    stop( "Vote total column, ", Z$tot.votes.col, " not found." )
  }
  
  
  err.bound = data.frame( row.names = row.names(Z$V) )
  
  for ( i in Z$winners ) {
    for ( j in Z$losers ) {	
      mrg = paste(i,j,sep="_")
      err.bound[[mrg]] = (Z$V[[Z$tot.votes.col]] + Z$V[[i]] - Z$V[[j]])/Z$Ms[[i,j]]		
    }
  }

  res = apply( err.bound, 1, max )
  
  if ( !is.null( votes ) ) {
  	stopifnot( !is.null( votes$PID ) && is.character( votes$PID ) )
  	res[ votes$PID ]
  } else {
  	res
  }
  
}


#' Fraction of votes bound
#' 
#' 
#' @description 
#' WPM.  The maximum error of the unit is a fixed
#' percentage of the total votes cast in the unit.  Typically the 20\% WPM is
#' used--meaning a swing of 40\% is the largest error possible as 20\% of the
#' votes go from the winner to the loser.
#' 
#' @param frac Fraction of total votes that could be a winner
#'   overstatement/loser understatement.  So if the worst-case is a
#'   20\% flip then enter 0.4
#'  
#' @seealso maximumMarginBound
#' @inheritParams maximumMarginBound
#' @export
fractionOfVotesBound = function( Z, frac=0.4 ) {
  ## This is the 0.4b bound function.  It returns frac * total votes.
  ## Return:   Vector (of length of precincts) of maximum error for 
  ##           each precinct.
  
  Z$V[[Z$tot.votes.col]] * frac / Z$margin
}




#' countVotes
#' 
#' Given a elec.data object, count the votes as reported and determine
#' winner(s) and loser(s).
#' 
#' 
#' @param Z the elec.data object.
#' @return Updated 'Z' matrix with the total votes as components inside it.
#' @author Luke W. Miratrix
#' @examples
#' 
#'   Z = make.cartoon()
#'   ## Take away 20 percent of C1's votes.
#'   Z$V$C1 = Z$V$C1 * 0.8
#'   ## Count again to find winner.
#'   Z = countVotes(Z)
#'   Z
#' 
#' @export countVotes
countVotes = function( Z ) {
  ## Count the total votes for various candidates.
  ## Return:  Updated 'Z' matrix with the total votes as components
  ##          inside it.
  
  
    computeMargins = function( Z ) {
    ## Used by elec.data
    ##
    ## Return: the pairwise margins (as # of votes) as a data.frame with the rows being
    ## the winners and the columns being the losers.
    
    if ( is.null( Z$winners ) ) {
      stop( "Need to count votes before computing margins in computeMargins()" )
    }
    Ms = data.frame( row.names = Z$winners )
    
    for ( j in Z$losers ) {
      Ms[[j]] = rep(0, length(Z$winners))	
      
      for ( i in Z$winners ) {
        
        Ms[[ i, j ]] = Z$totals[[i]] - Z$totals[[j]]
        
      }
    }

    Ms
  }


  ## Recalculate N based on V
  Z$N = nrow( Z$V )
                                        # total votes in given precinct
  if ( is.null( Z$V[[Z$tot.votes.col]] ) ) {
    warning("Totalling votes from candidates in countVotes()\n" )
    Z$V[[Z$tot.votes.col]] = apply( Z$V[Z$C.names], 1, sum )
  }
  grand.tot = sum(Z$V[[Z$tot.votes.col]])
  
  tots = sapply(Z$V[Z$C.names],sum)
  nt = sort(tots, decreasing=TRUE)
  M = nt[Z$f] - nt[Z$f+1]   # smallest margin (last winner - best loser)
  

  winners = names(nt[1:Z$f])
  losers = setdiff( names(nt), winners )

  Z[c("total.votes", "margin", "margin.per", "totals","winners","losers")] = list(grand.tot, M, M/grand.tot, tots, winners, losers )
  
  ## Compute pairwise margins between all winners and losers
  Z$Ms = computeMargins( Z )

  Z
  #c( Z, list( margin=M, totals=tots, winners=winners, losers=losers ) )
}


## Functions to calculate error amount in audit sample ##

#' @title Calc overstatement
#'
#' One way of calculating the errors for a collection of audited precints.
#' This one is the sum of all winner overcounts plus the sum of all 
#' loser undercounts (for each precinct)
#' @return  calc.overstatement.e_p: Vector (of length of audited precincts) of found errors by precinct. 
#' @rdname AuditErrors
#' 
#' @export
calc.overstatement.e_p = function( Z ) {

  apply( Z$audit, 1, 
        function(p) {sum( pmax(p[Z$winners],0) ) + sum( pmax(-1*p[Z$losers],0) )}  )
}


#' @title Calc pairwise
#'
#' @rdname AuditErrors
#' 
#' @export
calc.pairwise.e_p = function( Z, audit=NULL, err.override=NULL ) {
  ## Calculate the error by finding the maximum margin reduction for each precint
  ## by computing all margin reductions between pairs of winners & losers
  ## (scaling by that pair's total margin to get a proportion) and then
  ## taking the max of all such proportions (usually will be the last 
  ## winner to the closest loser).
  ## Param err.override:  If non-null, use this as the found error in votes rather than
  ##                      the actual errors found in the audit.
  ## Return: Vector (of length of audited precincts) of found errors by precinct. 

 
  if ( is.null( audit ) ) {
 	audit = Z$audit
  }
  if ( is.null(audit) ) {
    stop( "No audit to calculate e_p values for." )
  }

  stopifnot( !is.null(audit$PID ) )
  rownames(audit) = audit$PID
  stopifnot( Z$C.names %in% names(audit) )
  
  # If there is an error override, we need to make sure not to
  # give more error to a precinct than it can hold. Thus we need
  # a bound on maximum error--hopefully it is passed.
  if ( !is.null( err.override ) ) {
  	if ( is.null( audit$e.max ) ) {
   		if ( is.null( audit[[Z$tot.votes.col]] ) ) {
		    warning( "No '", Z$tot.votes.col, "' or e.max in audit matrix in calc.pairwise.e_p" )
        	audit$e.max = Inf
        } else {
	        warning( "No e.max column in calc.pairwise.e_p with a set error override.  Using tot votes / M." )
    		audit$e.max = audit[[Z$tot.votes.col]] / Z$M
        }
     }
  }
	
  err.bound = data.frame( row.names = row.names(audit) )
  
  for ( i in Z$winners ) {
    for ( j in Z$losers ) {
      
      mrg = paste(i,j,sep="_")

      if ( is.null(err.override) ) {
        err.bound[[mrg]] =  (audit[[i]]-audit[[j]]) / Z$Ms[[i,j]]
      } else {
        err.bound[[mrg]] = pmin( audit$e.max, err.override/Z$Ms[[i,j]] )
      
      }
    }
  }

  apply( err.bound, 1, max )
}


#' Calculate the measured error in each of the audited precicnts.
#'
#' @param Z Elec.data object holding the originally reported results
#' @param audit A data.frame with one column per candidate that holds
#'   the totals from the audit.  Each row corresponds to a precinct.
#'   Object needs a PID column with precinct ids that match the ones
#'   in Z.
#' @param bound.col This is the vector (in audit) containing the
#'   maximum number of votes (or error) possible in the various
#'   precincts.
#' @param err.override  If non-null, use this as the found error in
#'   votes rather than the actual errors found in the audit.
#' @param bound.col This is the vector (in audit) containing the
#'   maximum number of votes possible in the various precincts.
#' @param calc.e_p Calculate e\_p or take as given.
#' @param w_p The weight function to use to reweight the errors of
#'   precincts.
#'
#' @return    Orig audit table from Z with two new columns, err and
#'   err.weighted, corresponding to the errors found in each audited
#'   precinct before and after the weight function has been applied to
#'   them.
#' @export
compute.audit.errors = function( Z, audit=NULL,
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  bound.col="tot.votes",
  err.override = NULL ) {
  
  if ( !is.null(audit) ) {
  	Z$audit = audit
  }
                                        # calculate the errors
  Z$audit$err = calc.e_p( Z, err.override=err.override )
  
                                        # weight the errors
  Z$audit$err.weighted = w_p[[1]]( Z$audit$err, Z$audit[[bound.col]], Z$margin )

  Z$audit
}						



#' compute.stark.t
#' 
#' Compute the test statistic for election audits, essentially the largest
#' error found in the audit, as measured by the passed functions and methods.
#' 
#' This is an older method that other methods sometime use---it is probably
#' best ignored unless you have a good reason not to.
#' 
#' 
#' @param Z If it already has an audit table with err and err.weighted then it
#' will use those errors, otherwise it will compute them with compute.stark.err
#' @param bound.col This is the vector containing the maximum number of votes
#' possible in the various precincts.
#' @param calc.e_p Function to compute e_p.  Default is calc.pairwise.e_p.
#' @param w_p The weight function to be applied to the precinct error.
#' @param err.override If non-null, use this as the found error in votes rather
#' than the actual errors found in the audit.
#' @param return.revised.audit Return the updated audit frame with the error
#' and weighted errors calculated.
#' @return The test statistic, i.e. the maximum found error in the audit
#' sample, as computed by calc.e\_p and weighted by w\_p.
#' @author Luke W. Miratrix
#' @seealso \code{\link{find.q}} \code{\link{stark.test}}
#' @export compute.stark.t
compute.stark.t = function( Z,
  bound.col,
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  err.override = NULL,
  return.revised.audit = FALSE
  ) {
  ## Compute the error statistic given audit data and relevant functions
  ##
  ## Param Z              If it already has an audit table with err and err.weighted
  ##                      then it will use those errors, otherwise it will compute them
  ##                      with compute.stark.err
  ##
  ## Param bound.col:     This is the vector containing the maximum number of votes
  ##                      possible in the various precincts.
  ## Param err.override:  If non-null, use this as the found error in votes rather than
  ##                      the actual errors found in the audit.

  ## Return: the test statistic, i.e. the maximum found error in the audit 
  ##         sample, as computed by calc.e_p and weighted by w_p.
  
  if ( is.null( Z$audit[[bound.col]] ) ) {
    warning( "Assuming audit's rownames are unique and correspond to Z$V's row names" )
    Z$audit[[bound.col]] = Z$V[rownames(Z$audit), bound.col]
  }
  if ( is.null( Z$audit$err ) ) {
    Z$audit = compute.audit.errors( Z, bound.col=bound.col, 
                   calc.e_p=calc.e_p, w_p=w_p, err.override=err.override )
  }
  
  t.stat = max( Z$audit$err.weighted )
  
  if ( return.revised.audit ) {
    list( t.stat, Z$audit )
  } else {
    t.stat
  }
}



#' find.q
#' 
#' Find q, the minimum number of precints with w\_p's greater than given t.stat
#' that can hold an entire election shift in them.
#' 
#' This number is behind the SRS methods such as CAST.  If we know how many
#' precincts, at minimum, would have to hold substantial error in order to have
#' the reported outcome be wrong, we can compute the chance of finding at least
#' one such precinct given a SRS draw of size n.
#' 
#' 
#' Find the number of precints that need to have "large taint" in order to flip
#' the election.  This is, essentially, finding a collection of precints such
#' that the max error (e.max) plus the background error (the w\_p-inverse of
#' the t.stat) for the rest of the precints is greater than the margin (or 1 if
#' done by proportions).
#' 
#' @param V The data.frame of votes--the subwing of a elec.data object,
#' usually.
#' @param t.stat The worst error found in the audit (weighted, etc.)
#' @param bound.col The name of the column in V to be used for the passed size
#' (max number of votes, total votes, incl undervotes, etc.) to the error
#' function.
#' @param M The margin to close.  Usually 1 for proportional.  Can be less if
#' error from other sources is assumed.
#' @param threshold The total amount of error to pack in the set of tainted
#' precincts
#' @param w_p The weight function for errors.
#' @param drop Drop precincts with this column having a "true" value--they are
#' previously audited or otherwise known, and thus can't hold error.  Can also
#' pass a logical T/F vector of the length of nrow(V)
#' @return integer, number of badly tainted precints needed to hold 'threshold'
#' error
#' @author Luke W. Miratrix
#' @export find.q
find.q = function( V, t.stat, bound.col, M, threshold=1.0,
			 w_p = weight.function("no.weight"),
 			 drop=NULL ) {
  ## Find q, the minimum number of precints with w_p's greater than given t.stat
  ## that can hold an entire election shift in them.

  ## I.e., find the number of precints that need to have "large taint" in order to
  ## flip the election.  This is, essentially, finding a collection of precints
  ## such that the max error (e.max) plus the background error (the w_p-inverse of the
  ## t.stat) for the rest of the precints is greater than the margin (or 1 if done
  ## by proportions).
  ##
  ##
  ## Param bound.col:  The name of the column in V to be used for the passed size 
  ##                   (max # votes, total votes, incl undervotes, etc.) to the error 
  ##                   function.
  ##       threshold:  The total amount of error to pack in the set of tainted precincts
  ##            drop:  Drop precincts with this column having a "true" value--they are
  ##                   previously audited or otherwise known, and thus can't hold error.
  ##                   Can also pass a logical T/F vector of the length of nrow(V)
  ##
  ## Return:  integer, number of badly tainted precints needed to hold 'threshold' error
  
  stopifnot( is.null( V$e.max ) == FALSE )
  
  if ( !is.null( drop ) ) {
    if ( length(drop) == nrow(V) ) {
      V = V[ !drop, ]
    } else {
      V = V[ !V[[drop]], ]
    }
  }
  
  sortErr = data.frame( e.max=V$e.max, wp.inv=w_p[[2]](t.stat, V[[bound.col]], M  ) )
  
  sortErr$bkg = pmin(sortErr$e.max, sortErr$wp.inv )

  sortErr$importance = sortErr$e.max - sortErr$bkg
  sortErr = sortErr[order( sortErr$importance, decreasing=TRUE ),]

  q = 0
  totErr = sum( sortErr$bkg )
  if ( sum( sortErr$importance ) + totErr < threshold ) {
    q = nrow(sortErr)
  } else {
    while( totErr < threshold ) {
      q = q + 1;
      totErr = totErr + sortErr$importance[q]
    }
  }
  q
}




#' find.stark.SRS.p
#' 
#' Find the p-value for a given q, n, and N.  Helper function for a simple
#' hypergeometric calculaton--see reports.
#' 
#' 
#' @param N total number of precints
#' @param n total number of audited precints (must be less than N)
#' @param q min number of precints that could hold taint to flip election
#' @return Chance that 1 or more of the q 'bad' things will be seen in a size n
#' SRS draw from the N sized bucket.
#' @author Luke W. Miratrix
#' @export find.stark.SRS.p
find.stark.SRS.p = function( N, n, q ) {
                                        ## Find the p-value for a given q, n, and N
  
                                        ## N = total number of precints
                                        ## n = total number of audited precints (must be less than N)
                                        ## q = min number of precints that could hold taint to flip election
  
  if ( n >= N ) stop( "Audit size is equal or greater than population size." )
  
                                        #cat( "N=",N, "n=",n,"q=",q,"\n")
  if ( N-q < n ) {
    0
  } else {
                                        # q bad precints, N-q good ones, n draws -> chance of getting
                                        # no bad precints in audit
    phyper( 0, q, N-q, n )
  }
}


#' Workhorse driver for stark.test
#' 
#' @rdname stark.test
#' @export
stark.test.Z = function( Z, 
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  max_err = maximumMarginBound,
  bound.col = Z$tot.votes.col,
  strat.col = NULL,
  drop=NULL,
  strat.method = NULL,
  err.override = NULL,
  n = NULL, t = NULL, q = NULL
  ) {
  
  ## Param Z: The object holding all the voting information
  ##    In particular: Z$V:     The table of reported votes
  ##                   Z$audit: The table of audits as differences from 
  ##                            recorded votes
  ## Param calc.e_p: The Function used to calculate maximum error bounds
  ## Param      w_p: The function used to calculate weights of error
  ##                 (A list of two functions)
  ##   	    max_err: Function to compute _max_ error bounds for each precint
  ##      strat.col: Name of column that determines how to stratify
  ##          t,n,q: Elements of the test statistic.  Can pass to avoid computation
  ##                 if those values are already known (e.g., for a simulation)
  
   ##stopifnot( !is.null( strat.col) && is.null(strat.method) )
  stopifnot( is.null(drop) || !is.null(Z$V[[drop]]) )
  
  if ( is.null( t ) ) { 	
    res = compute.stark.t( Z, bound.col, calc.e_p, w_p, err.override=err.override,
      return.revised.audit = TRUE)
    Z$audit = res[[2]]
    t = res[[1]]
  }
  
                                        #cat( "weight function = " )
                                        #print( w_p )
  
  if ( is.null( q ) ) {
    Z$V$e.max = max_err( Z )
    q = find.q( Z$V, t, bound.col, Z$margin, w_p=w_p, drop=drop )
  }
  
  
                                        # computing sample size.
  if ( is.null( n ) ) {
    n = length(Z$audit[[1]])
    passed_n = NULL
  } else {
    ## compute scaling of n
    passed_n = n / length(Z$audit[[1]])
  }
  
  
  ## Once q has been computed, calculate final p-values.
  
  
  if ( is.null( strat.col ) ) {
	if ( !is.null(drop) ) {
 	 	eff.N = Z$N - sum(Z$V[[drop]])
 	} else {
  		eff.N = Z$N
  	}
    p.value = find.stark.SRS.p( eff.N, n, q )
    method = "Stark's Election Audit Test (SRS Audit)"
    DAT = paste( "# precincts = ", eff.N, " of ", Z$N, ", f/C=", Z$f, " of ", 
      Z$C, ", n=",n, sep="")
    
  } else {
	## Analysis as if the minimum sampling fraction applied
	##  everywhere.  Bound from sampling *with* replacement

	if ( !is.null(drop) ) {
		stop( "dropped column and stratification code not working right--no drop col allowed" )
	}
	
	## Find stratification levels ###
    strat = find.stratification( Z$V, Z$audit, strat.col )
        	
    ## The following takes out completely missed strata for which there
    ## is no audit data, and assigns maximum error to it
    not.audited = strat[[strat.col]][strat$sample<0.001]
    big.N = eff.N
    if ( length( not.audited ) > 0 ) {
      
      skipped = Z$V[[strat.col]] %in% not.audited
      cat( "# precincts missed", length(Z$V[skipped,]$e.max), " prec in ", length( not.audited ), "\n")
      M.red = sum( Z$V[skipped,]$e.max )
      
                                        #print( Z$V[skipped,] )
      
      qp = find.q( Z$V[!skipped, ], t, bound.col, Z$margin, threshold=(1-M.red), w_p=w_p )
                                        #               warning( "Some strata have no audit data--will subtract max error to get B=", 
                                        #               round( (1-M.red), digits=3), ", and q reduced to ", qp, " from ", q )
      
      big.N = big.N - (q - qp) 
      cat( " del q ", (q-qp), round( M.red, digits=3 ), "\n" )
      q = qp
    }
    
    ## find effective n by taking smallest audit percentage of strata (other than 0).
    n <- floor(big.N*min(strat$sample[strat$sample>=0.001]))
    if ( !is.null(passed_n) ) { 
      n = floor( passed_n *big.N*min(strat$sample[strat$sample>=0.01]))
    }
    p.value = pbinom(0,n,q/big.N)
    method = "Stark's Election Audit Test (Conservative Stratafication)"
    DAT = paste( "# precincts = ", Z$N, ", f/C=", Z$f, " of ", 
      Z$C, ", n=",length(Z$audit[[1]]), 
      " (", n, ")", sep="")
    if ( big.N != Z$N ) {
      DAT = paste( DAT, " N'=", big.N )
    }
  } # end stratification block
  
  names(t) = "max err";
  names(q) = "q";
  
  structure(list(statistic = t, p.value = p.value,
                 method=method, parameter=q,
                 data.name = DAT,
                 Z=Z, n=n), class = "htest")
}




# @aliases stark.test stark.test.Z stark.pairwise.test

#' Conduct old-style test of election data
#' 
#' These main methods conduct the test of the election audit and returns a
#' p-value and other related info on that test.
#' 
#' It is an older method.  Most likely \code{\link{CAST.audit}} or
#' \code{\link{trinomial.audit}} should be used instead.
#' 
#' stark.test() will do the entire test. It is basically a driver function that
#' sets up 'Z' matrix and passes buck to the stark.test.Z
#' 
#' The Z object, in particular has: Z\$V: The table of reported votes Z\$audit:
#' The table of audits as differences from recorded votes
#' 
#' @param votes data.frame of votes. Each row is precinct.
#' @param audits data.frame of audits. Each row is precinct.  Table reports
#' overstatement by candidate.
#' @param C.names Names of candidates (and names of cor columns in votes and
#' audits tables.  If NULL will derive from cols 2 on of votes
#' @param f The number of winners
#' @param pool If TRUE, combine small candidates into single pseudo-candidates
#' to increase power
#' @param pairwise if TRUE then do a pairwise test for all pairs and return
#' highest p-value
#' 
#' @param Z The object holding all the voting information.  See below for
#' details.
#' @param calc.e_p The Function used to calculate maximum error bounds
#' @param w_p The function used to calculate weights of error (A list of two
#' functions)
#' @param max_err Function to compute max error bounds for each precint
#' @param bound.col Name (or column index) of column in the vote matrix
#' corresponding to maximum number of votes allowed in precinct.
#' @param strat.col Name of column that determines how to stratify if NULL will
#' not stratify
#' @param strat.method Not currently implemented.
#' @param err.override If non-null, use this as the found error in votes rather
#' than the actual errors found in the audit.
#' @param n Elements of the test statistic.  Can pass to avoid computation if
#' those values are already known (e.g., for a simulation)
#' @param t Elements of the test statistic.  Can pass to avoid computation if
#' those values are already known (e.g., for a simulation)
#' @param q Elements of the test statistic.  Can pass to avoid computation if
#' those values are already known (e.g., for a simulation)
#' @param drop Either a vector of TRUE/FALSE or a name of a column in Z\$V of
#' T/F values.  Precincts identified by drop will be dropped from calculations.
#' 
#' @param \dots Extra arguments passed directly to the work-horse method
#' stark.test.Z
#' 
#' @return Return an htest object with pvalue, some relevant statistics, and
#' the Z object used (possibly constructed) that produced those results.
#' @author Luke W. Miratrix
#' @seealso See \code{\link{elec.data}} for description of the main object.
#' See \code{\link{find.q}} and \code{\link{compute.stark.t}} for the main
#' components of this test.  \code{\link{find.stark.SRS.p}} is a utility
#' function for computing a p-value for a specific situation.  See
#' \link{weight.function} for functions used to weight audit errors.  See
#' MaximumBound for a bound on error that one might use for
#' these tests.  See \code{\link{find.stratification}} for a utility for
#' stratification.
#' @examples
#' 
#' ## pretending that santa cruz audit was a SRS audit (which it was not)
#' data(santa.cruz)
#' Z = elec.data(santa.cruz, C.names=c("leopold","danner"))
#' data(santa.cruz.audit)
#' ## do some work to get the audit totals to overstatements
#' rownames(santa.cruz.audit) = santa.cruz.audit$PID
#' Z$audit = audit.totals.to.OS(Z, santa.cruz.audit)
#' Z$audit
#' stark.test.Z(Z)
#' 
#' 
#' @export stark.test
stark.test = function( votes, audits, C.names=NULL, f=1, pool=TRUE, pairwise=FALSE, ... ) {
  ## Do the entire test. Basically a driver function that sets up 'Z' matrix and passes buck
  ## to the stark.test.Z
  ## 
  ## param       votes:  Table of reported votes.  Each row is precinct.
  ## param      audits:  Table of audits.  Each row is precinct.  Table reports overstatement by
  ##                     candidate.
  ## param     C.names:  Names of candidates (and names of cor columns in votes and audits tables.
  ## param           f:  The number of winners
  ## param    pairwise:  if TRUE then do a pairwise test for all pairs and return
  ##                     highest p-value
  ## param     C.names:  if NULL will derive from cols 2 on of votes
  ## param   strat.col:  if NULL will not stratify

  if ( pairwise ) {
    stark.pairwise.test( votes, audits, C.names=C.names, f=f, pool=pool, ... )
  } else {
    Z = elec.data(votes, C.names, f, audit=audits, pool=pool )

    T = stark.test.Z( Z, ... )
    
    T
  }
}




#' find.stratification
#' 
#' Find how audit covered the strata for a given table of votes and audits.
#' 
#' 
#' @param D Table of votes
#' @param aud Table of audit data
#' @param strat.col The column to use that identifies the stratification levels
#' @return Table of strata. For each stratum (row) the table has the name of
#' the stratam, the number of precincts in the stratum, the number of audited
#' precincts and percent of precincts audited.
#' @author Luke W. Miratrix
#' @export find.stratification
find.stratification = function( D, aud, strat.col ) {
  ## Finding how audit interacted with stratification levels for a table of votes and audits
  ## param     D:    Table of votes
  ##         aud:    Table of audit data
  ##     strat.col:  The column to use that identifies the stratification levels
  ## Return: Table of strata, for each strata (row) the name of the strata,
  ##         the number of precincts, the number of audited precincts
  ##         and percent of precincts audited is returned.
  if ( !(strat.col %in% names(D) ) ) {
    stop( "Cannot find stratification since strat.col '", strat.col, "' is not in vote data object." )
  }
  
  if ( !( strat.col %in% names(aud) ) ) {
  	stop( "Cannot find stratification since strat.col '", strat.col, "' is not in audit data object." )
  }
  
  nPrecincts <- table( D[[strat.col]] )
  audPrecincts = table( aud[[strat.col]] )
  tbl = merge( nPrecincts, audPrecincts, by="Var1", all.x=TRUE )

  names(tbl) = c(strat.col, "n", "audit" )
  tbl$audit[is.na(tbl$audit)] = 0
  tbl$sample = tbl$audit / tbl$n
  
  tbl
}

stark.pairwise.test = function( votes, audits, C.names=NULL, f=1, pool=TRUE, ... ) {
  ## Pairwise test: Look at all pairs of winners and losers, compute the p-value
  ## according to methods defined by passed parameters, and then return the
  ## worst p-value, using the bound that the chance that _all_ the statistics would be
  ## that small under their respective H0 is bounded by the chance that the hardest-to-
  ## detect statistic would be that small.
  ## param     C.names:  if NULL will derive from cols 2 on of votes
  ## param   strat.col:  if NULL will not stratify
  
  all.Z= elec.data(votes, C.names, f, audit=audits, pool=pool )
  
  max.pv = -1
  best.T = NULL

  for ( w in all.Z$winners ) {
    for ( l in all.Z$losers ) {
      ##cat( "Examining ", w, " beating ", l, "\n" )
      Z = elec.data( all.Z$V, C.names=c(w, l), 1, audit=all.Z$audit, pool=FALSE )
      T = stark.test.Z( Z, ... )
      ##print(T)
      if ( T$p.value > max.pv ) {
        max.pv = T$p.value
        best.T = T
      }
      ##print( "finished iteration" )
    }
  }

  best.T$method = paste( best.T$method, "(pairwise)" )
  best.T$data.name = paste( best.T$data.name, " (overall F/C=",all.Z$f, "/", all.Z$C, ")", sep="" )
  
  best.T
}




#' Converting total vote counts to Over Statements
#' 
#' This utility function takes a collection of total votes from an audit and
#' subtracts the originally reported totals from them to give overstatement
#' errors (i.e., how many votes more than actual a candidate had).  I.e., the
#' overstatement error is REPORTED - ACTUAL.
#' 
#' Make sure the audit's PID column is a character vector and not a factor.  If
#' needed, convert via \code{audit\$PID = as.character(audit\$PID)}.
#' 
#' @param Z Elec.data object holding the originally reported results
#' @param audit A data.frame with one column per candidate that holds the
#' totals from the audit.  Each row corresponds to a precinct.  Object needs a
#' PID column with precinct ids that match the ones in Z.
#' @return A new data.frame with overstatement errors.
#' @author Luke W. Miratrix
#' @seealso See \link{AuditErrors} for different ways of summarizing audit
#' errors.
#' @examples
#' 
#' 
#' ## Generate a fake race, a fake audit, and then compute overstatements
#' Z = make.sample(0.08, 150, per.winner=0.4, R=2.01)
#' Z
#' Zb = make.ok.truth(Z, num.off=150, amount.off=5)
#' Zb
#' aud = Zb$V[ sample(1:Zb$N, 10), ]
#' aud
#' audit.totals.to.OS(Z, aud )
#' 
#' @export audit.totals.to.OS
audit.totals.to.OS = function( Z, audit ) {
	stopifnot( !is.null( Z$V$PID ) )
	stopifnot( !is.null( audit$PID ) )
	stopifnot( all( Z$C.names %in% colnames(audit) ) )
	stopifnot( all( audit$PID %in% rownames(Z$V) ) )
	audit$PID = as.character(audit$PID)
	
	audit[ Z$C.names ] = Z$V[ audit$PID, Z$C.names ] - audit[ Z$C.names ]  
	if ( is.null( audit$tot.votes ) ) {
		audit$tot.votes = Z$V[ audit$PID, "tot.votes" ]
	}
	
	audit
}




