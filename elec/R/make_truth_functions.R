# make.truth functions


#' Make the cartoon example from the CAST paper as a voter data
#' matrix.
#'
#' This makes the sample scenario described in P. B. Stark's CAST
#' paper.
#'
#' @param vote.dist reported votes for C1, C2, and C3 in order for all
#'   precincts.prompt
#' @param n Size of sample.
#' @param stratify Should the sample be stratified?
#'
#' @export
make.cartoon = function( n = 400, vote.dist=c(125,113,13), stratify=TRUE ) {
    n = n - n %% 4
    
    V = data.frame( strata = rep( c(rep(c("S1","S1.VBM"), 3),"S2","S2.VBM"), n/4 ),
                    tot.votes = rep( 255, 2*n ),
                    C1 = rep( vote.dist[[1]], 2*n ),
                    C2 = rep( vote.dist[[2]], 2*n ),
                    C3 = rep( vote.dist[[3]], 2*n ) )
    
    
    if ( !stratify ) {
        V$strata = "S1"
    }
    PID = paste( V$strata, rownames(V), sep="-" )
    names(PID) = "PID"
    V = cbind( PID, V )
    V$PID = as.character(V$PID)
    
    
    elec.data( V, c( "C1", "C2", "C3" ) )
}



#' @title Make baseline truth for simulations
#' @name make.truth
#'
#' @description 
#' For simulations.  These methods, given an elec.data object, make a
#' ``truth''---i.e. a different vote count---that meets the same precinct and
#' tot.votes structure, but has potentially different results and outcomes.
#' 
#' \code{make.truth.opt.bad} makes the ``optimally worse truth'', where the
#' error needed to flip the winner and runner-up is packed into as a few
#' precincts as possible.
#' 
#' \code{make.ok.truth} makes the truth have the same outcome as the reported,
#' but some errors here and there.
#' 
#' 
#' @param Z The elec.data to build from.
#' @param strata name of column holding strata, if any.
#' @param bound What sort of maximum error can be held in a precinct.
#' @param t Number of per-precinct vote "background" error that can occur
#' without triggering escalation if seen.
#' @param shuffle.strata Should the error be randomly put in the strata?
#' @param num.off Number of precincts that should have small errors.  Direction
#' of errors split 50-50 positive and negative.
#' @param amount.off Size of the small errors that should be imposed.
#' @return Another elec.data matrix with the same candidates and total ballot
#' counts as the passed frame, but with different candidate totals and
#' by-precinct votes.  Can be used to test the power or actual confidence of
#' the various auditing procedures.
#' 
#' WARNING: make.ok.truth randomly adds votes and can thus sometimes exceed the
#' allowed ballot count for a precinct by small amounts.
#' 
#' WARNING: If the desired bound is WPM, the error in make.opt.bad.truth is
#' made by simply adding the maximum allowed amount of error in votes to the
#' first loser's total (so that total votes may in this case exceed the total
#' votes of the precinct)--this could potentially cause trouble.  Be careful!
#' 
#' WARNING: \code{make.truth.ex.bad} and \code{make.truth.opt.bad.strat} only
#' work in conjunction with the \code{\link{make.cartoon}} method.
#' @author Luke W. Miratrix
#' @seealso \code{\link{elec.data}} \code{\link{make.sample}}
#' \code{\link{do.audit}} \code{\link{make.cartoon}}
#' @examples
#' 
#' ## First make a fake election.
#' Z = make.sample(0.08, 150, per.winner=0.4, R=2.2)
#' Z
#' 
#' ## Now make a fake truth, which has a lot of small errors:
#' Zb = make.ok.truth(Z, num.off=150, amount.off=5)
#' Zb
#' 
#' ## Finally, make the hardest to detect (via SRS) ``wrong'' election:
#' Zw = make.truth.opt.bad( Z, t=4 )
#' Zw 



#' @rdname make.truth
#' @export
make.truth.ex.bad = function( Z ) {
    n = nrow(Z$V)
    stopifnot( n %% 8 == 0 )
    C1 = c( rep( 80, n/8 ), rep( 124, 7*n/8 ) )
    C2 = c( rep( 160, n/8 ), rep( 113, 7*n/8 ) )
    C3 = c( rep( 13, n/8 ), rep( 15, 7*n/8 ) )
    reord = sample( 1:n, n )
    
    V = data.frame( strata = rep( c(rep(c("S1","S1.VBM"), 3),"S2","S2.VBM"), n/8 ),
                    tot.votes = rep( 255, n ),
                    C1=C1[reord], C2=C2[reord], C3=C3[reord] )
    
    PID = paste( V$strata, rownames(V), sep="-" )
    names(PID) = "PID"
    V = cbind( PID, V )
    V$PID = as.character(V$PID)
    
    elec.data( V, c( "C1", "C2", "C3" ) )
}


#' Given a collection of counted votes, make a theoretical bad truth by packing all error 
#' possible in the largest precincts
#'
#' Warning: if bound is WPM this error is made by simply adding the max amount of error
#' to the first loser's total (so that total votes may in this case exceed the total votes
#' of the precinct)--this could potentially cause trouble.  Be careful!
#'
#' @rdname make.truth
#' @param t an allowed backgound level of error for all precincts
#' @export      
make.truth.opt.bad = function(Z, strata="strata", 
                              bound=c("margin","WPM"), t=0 ) {
    
    bound = match.arg( bound )
    
    if ( bound=="WPM" ) {
        s = CAST.calc.sample(Z, t=t, bound.function=fractionOfVotesBound )
    } else {
        s = CAST.calc.sample(Z, t=t )
    }
    
    winner = Z$winner[[Z$f]]
    
    stopifnot( all( s$Z$V$PID == Z$V$PID ) )
    V = Z$V[ order(s$Z$V$e.max, decreasing=TRUE), ]
    baddies = 1:nrow(V) <= s$q
    V[winner] = pmax( 0,  V[[winner]] - t )
    
    tots = V$tot.votes[baddies]
    if ( bound=="margin" ) {
        newvotes = as.data.frame( matrix( 0, nrow=s$q, ncol=length(Z$C.names) ) )
        names(newvotes) = Z$C.names
        newvotes[[ Z$losers[[1]] ]] = tots
        V[ baddies, Z$C.names ] = newvotes 
    } else {
        V[ baddies, Z$losers[[1]] ] = V[ baddies, Z$losers[[1]] ] + round( 0.4 * V[ baddies, ]$tot.votes )
    }
    
    
    nZ = elec.data( V[ Z$V$PID, ], Z$C.names )
    stopifnot( nZ$winner != Z$winner )
    nZ$num.tweaked = s$q
    nZ
    
}

#' Make optimally bad truth 
#' 
#' @description 
#'  make bad truth as described in Stark's paper (assuming fixed precinct size)
#'  
#' @rdname make.truth
#' @export
make.truth.opt.bad.strat = function(Z, strata="strata", t=3, shuffle.strata=FALSE) {
    s = CAST.calc.sample(Z, t=t)
    
    if ( shuffle.strata ) {
        pids = sample(Z$V$PID, s$q )
    } else {
        strats = split( Z$V$PID, Z$V[strata] )
        pids = lapply( names(s$stratas), function( ST ) {
            sample( strats[[ST]], floor( s$q * s$stratas[[ST]] / s$N ) )
        } )
        pids = unlist( pids )
        if ( ( ext <- ( s$q - length( pids ) ) ) > 0 ) {
            pids = c( pids, sample( setdiff( Z$V$PID, pids ), ext ) )
        }
    }
    
    Z$V["C1"] = Z$V["C1"] - t
    baddies = Z$V$PID %in% pids
    Z$V[ baddies, Z$C.names ] = cbind( rep( 0, s$q ), rep(255,s$q), rep(0,s$q) ) 
    elec.data( Z$V, Z$C.names )
}



#' @rdname make.truth
#' @export
make.ok.truth = function( Z, num.off = 8, amount.off = 5 ) {
    cut = floor(num.off / 2)
    off = sample( 1:nrow(Z$V), num.off )
    offUp = off[1:cut]
    offDown = off[(cut+1):num.off]
    CW = Z$winners[Z$f]
    CL = Z$losers[1]
    maxL = pmin( Z$V[ offUp, CL ], amount.off )
    Z$V[ offUp, CL ] = Z$V[ offUp, CL ] - maxL
    Z$V[ offUp, CW ] = Z$V[ offUp, CW ] + maxL
    maxL = pmin( Z$V[ offDown, CW ], amount.off )
    Z$V[ offDown, CW ] = Z$V[ offDown, CW ] - maxL
    Z$V[ offDown, CL ] = Z$V[ offDown, CL ] + maxL
    
    Z = countVotes(Z)
    
    Z
}

