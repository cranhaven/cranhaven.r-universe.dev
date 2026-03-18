







#' @title Generate fake election results for simulation studies
#' 
#' @description 
#' These methods are for SIMULATION STUDIES.  These functions will build a
#' sample, i.e. simulated, record of votes given certain parameters.
#' 
#' 
#' @param M The margin desired between the winner and loser (as a percent).
#' @param N Number of precincts desired.
#' @param strata Number of strata desired.
#' @param per.winner The percent of votes the winner should receive.
#' @param worst.e.max The worst e.max possible for any precinct.
#' @param R The "dispersion" a measure of how unequal in size precincts should
#' be.  R needs to be greater than 0.  NULL indicates equal size.  For R
#' between 0 and 1, the precincts are distributed 'linearly', i.e., the size of
#' precinct i is proportional to i.  At 2, the smallest precint will be near 0
#' and the largest twice the average votes per precinct.  After 2, the
#' precincts are distributed in a more curved fashion so that the smaller
#' precincts do not go negative.
#' @param tot.votes The total votes desired.
#' @return A elec.data object meeting the desired specifications.
#' @author Luke W. Miratrix
#' @seealso \code{\link{elec.data}} \code{\link{make.truth}}
#' \code{\link{do.audit}}
#' @references See http://www.stat.berkeley.edu/~stark/Vote/index.htm for
#' relevant information.
#' @examples
#' 
#' Z = make.sample(0.08, 150, per.winner=0.4)
#' Z
#' 
#' Z2 = make.sample(0.08, 150, per.winner=0.4, R=2.2)
#' Z2
#' 
#' ## Note how they have different precinct sizes.
#' 
#' summary(Z$V$tot.votes)
#' summary(Z2$V$tot.votes)
#' 
#' 
#' 
#' @export make.sample
make.sample = function( M, N, strata=1, per.winner=NULL, 
                        worst.e.max = NULL, 
                        R = NULL, 
                        tot.votes=100000 ) {
    if ( !is.null( per.winner ) ) {
        v = c( per.winner, per.winner - M, 1 - 2 * per.winner + M )
        names(v) = c("WNR", "LSR", "OTR" )
        stopifnot( min(v) >= 0 && max(v) < 1 )
        stopifnot( v[[1]] == max(v) )
    } else {
        stopifnot( M < 1 && M > 0 )
        v = c( (1+M) / 2, (1-M)/2 )
        names(v) = c("WNR", "LSR" )
    }
    
    st = paste( "ST", 1:strata, sep="-" )
    V = round( (tot.votes / N) * matrix( rep( v, N ), ncol=length(v), byrow=TRUE ) )
    V = data.frame( V )
    V = cbind( paste("P", 1:N, sep="-"), rep( st, length.out=N ), apply( V, 1, sum ), V )
    V[[1]] = as.character( V[[1]] )
    names(V) = c( "PID", "strata", "tot.votes", names(v) )
    
    if ( !is.null( worst.e.max ) ) {
        # calc how bad worst prec is, and then use p(x) = c/sqrt(x)
        # as density function of precincts.  Invert the CDF, and eval
        # at orders (1:N)/N to get the new precinct sizes.  Scale
        # and go!
        t1 = V[1, ]
        e.max = t1$tot.votes - t1$LSR + t1$WNR
        stopifnot( is.null( R ) )
        R = worst.e.max / e.max
    }
    if ( !is.null(R) ) {   # using the ratio of bad to baseline, go.
        stopifnot( R >= 0 )
        if ( R <= 2 ) {
            m = R / N
            wts = 1:N * m - N*m / 2 + 1
        } else {
            p = (R - 2)/(R - 1)
            C = (1 - p)/R^(1 - p)
            
            wts = (((1:N)/N)/(C * (R - 1)))^(R - 1)
        }
        
        V[names(v)] = round( V[names(v)] * wts )
        V["tot.votes"] = apply( V[names(v)], 1, sum )
    }
    
    elec.data( V, C.names=names(v) )
}


#' Make sample from vote totals (for simulations)
#' 
#' Given a vector of precinct totals and the total votes for the winner
#' and the loser, make a plausible precinct-by-precinct vote count that
#' works. 
#' Note: the margins of the precincts will all be the same as the margin
#' of the overall race.
#' 
#' @param vote.W Total votes for winner.
#' @param vote.L Total votes for loser.
#' @param totals Vector of total votes for precincts.
#'
#' @export
make.sample.from.totals = function( vote.W, vote.L, totals ) {
    GT = sum(totals)
    v = c(vote.W/GT, vote.L/GT)
    names(v) = c("WNR", "LSR")
    stopifnot(min(v) >= 0 && max(v) < 1)
    stopifnot(v[[1]] == max(v))
    N = length(totals)
    V = matrix(rep(v, N), ncol = length(v), byrow = TRUE)
    V = data.frame(V)
    V = cbind(rep("ST-1", length.out = N), totals, V)
    names(V) = c("strata", "tot.votes", names(v))
    V[names(v)] = round(V[names(v)] * totals)
    
    totW = vote.W - sum( V$WNR )
    totL = vote.L - sum( V$LSR )
    
    # make total match (gets off due to rounding)
    tweak = function( votes, flex, tot ) {
        cntr = 1
        while ( tot != 0 && cntr <= length(votes) ) {
            if ( tot > 0 ) {
                if ( flex[cntr] > 0 ) {
                    votes[cntr] = votes[cntr] + 1
                    tot = tot - 1
                }
            } else {
                if ( votes[cntr] > 0 ) {
                    votes[cntr] = votes[cntr] - 1
                    tot = tot + 1
                }
            }
            cntr = cntr + 1
        }
        votes
    }
    
    
    flex = with( V, tot.votes - WNR - LSR )
    V$WNR = tweak( V$WNR, flex, totW )
    flex = with( V, tot.votes - WNR - LSR )
    V$LSR = tweak( V$LSR, flex, totL )
    
    Z =    elec.data(V, C.names = names(v))
    stopifnot( Z$total.votes == sum( totals ) )
    stopifnot( sum(Z$V$WNR) == vote.W )
    stopifnot( sum(Z$V$LSR) == vote.L )
    
    Z
}




make.sample.from.totals.margin = function( M,  totals, per.winner=NULL ) {
    GT = sum(totals)
    vote.W = round( GT* (0.5 + M/2) )
    vote.L = round( GT * (0.5 - M/2) )
    make.sample.from.totals( vote.W, vote.L, totals )
}



