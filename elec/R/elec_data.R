



#' core election audit data structure
#' 
#' Makes an object (often called a `Z' object in this documentation) that holds
#' all the vote totals, etc., as well as some precomputed information such as
#' vote margins between candidates, the theoretical winners, and so on.
#' 
#' \code{elec.data} does some cleaning and renaming of the passed data
#' structure.  In particular it will rename the tot.votes column to "tot.votes"
#' if it is not that name already.
#' 
#' 
#' 
#' @param V Voter matrix OR 2-element list with Voter Matrix followed by
#' Candidate names
#' @param C.names List of candidate names.  Also names of columns in V
#' @param f Number of winners
#' @param audit The audit data---must have columns that match C.names.  Columns
#' are overstatements of votes found for those candidates.
#' @param pool Combine small candidates into single pseudo-candidates to
#' increase power
#' @param tot.votes.col Name of column that has the total votes for the
#' precincts.
#' @param PID.col Name of column that identifies unique PIDs for precincts.
#' @param ...  The collection of arguments that are passed directly to
#' elec.data, or (in the case of print), unused.
#' @param x For print() and is.elec.data(). An elec.data object
#' @param n For print(). number of sample precincts to print
#' @return
#' 
#' A ``elec.data'' data structure.  Note: Will add PID (precinct ID) column if
#' no PID provided (and generate unique PIDs).  It will rename the PID column
#' to PID.  Also, rownames are always PIDs (so indexing by PID works).
#' @author Luke W. Miratrix
#' @seealso See \link{CAST.audit} for the CAST method.  See
#' \code{\link{tri.calc.sample}} for the trinomial bound method.  See
#' \code{\link{countVotes}} for counting the votes listed in Z.
#' @examples
#' 
#' data(santa.cruz)
#' elec.data( santa.cruz, C.names=c("danner","leopold") )
#' 
#' @export elec.data
elec.data = function( V, C.names=names(V)[2:length(V)], f = 1, 
                      audit=NULL, pool=TRUE, tot.votes.col="tot.votes", PID.col="PID" ) {
    ## Make the 'Z' matrix that holds all the vote totals, etc., as well as some
    ## precomputed information such as vote margins between candidates, the theoretical
    ## winners, and so on.
    ##
    ## elec.data does some cleaning and renaming of the passed data structure.  In particular
    ## it will rename the tot.votes column to "tot.votes" if it is not that name already.
    ##
    ## Param    V: Voter matrix OR 2-element list with Voter Matrix followed 
    ##             by Candidate names
    ##       pool: Combine small candidates into single pseudo-candidates to increase 
    ##             power
    ##
    ## Return:  A "elec.data" data structure. 
    ##         Note: Will _add_ PID (precinct ID) column (and generate PIDs)
    ##               if no PID provided.  Also, rownames
    ##               are always PIDs (so indexing by PID works)
    
    
    
    
    Z = list()
    class( Z ) = "elec.data"
    
    if ( length(V) == 2 ) {
        C.names = V[[2]]
        V = V[[1]]
    }
    
    Z$N = length(V[[1]])     # number of precincts
    Z$C = length(C.names)    # number of candidates
    Z$f = f                  # number of possible winners
    
    Z$V = V
    Z$C.names = C.names
    
    ##	Z$alpha = 0.10
    ##	Z$a_s = rep(0.5,10)^(1:10)
    
    stopifnot( all( Z$C.names %in% names(Z$V) ) )
    
    if ( is.numeric(tot.votes.col) ) {
        tot.votes.col = names(V)[[tot.votes.col]]
    }
    if ( is.na(l <- match( tot.votes.col, names(V) ) ) ) {
        warning( "No tot.votes.col found in vote matrix--will be recounting" )
    } else {
        names(Z$V)[l] = "tot.votes"
    }
    Z$tot.votes.col = "tot.votes"
    
    Z = countVotes(Z)
    
    if( !is.null( audit ) ) {
        stopifnot( all( Z$C.names %in% names(audit) ) )
        Z$audit = audit
    }
    
    ## Pooling!
    ln = length(Z$losers)
    if ( pool && 
         ln > 2 && ( (Z$totals[Z$losers[ln]] + Z$totals[Z$losers[ln-1]]) < Z$totals[Z$losers[1]] )) {
        
        grb = ln - 1
        while( grb > 1 &&  sum( Z$totals[Z$losers[grb:ln]] ) < Z$totals[Z$losers[1]] ) {
            grb = grb - 1
        }
        grb = grb + 1
        to.pool = Z$losers[grb:ln]
        
        Z$V[["pool"]] = apply(Z$V[to.pool],1,sum)
        Z$V = Z$V[setdiff(names(Z$V),to.pool)]
        if ( !is.null( audit )) {  # pool the audit info too.
            Z$audit[["pool"]] = apply(Z$audit[to.pool],1,sum)
            Z$audit = Z$audit[setdiff(names(Z$audit),to.pool)]
        }
        Z$C.names = c( setdiff(Z$C.names, to.pool ), "pool" )
        
        Z$C = length(Z$C.names)    # number of candidates
        Z = countVotes(Z)
        ln = ln - 1
        
    }
    
    
    if ( is.numeric(PID.col) ) {
        names(Z$V)[PID.col] = "PID"
    } else if ( is.na(l <- match( PID.col, names(V) ) ) ) {
        warning( "No PID.col found in vote matrix--will be generated" )
    } else {
        names(Z$V)[l] = "PID"
    }
    Z$PID.col = "PID"
    
    if ( is.null( Z$V$PID ) ) {
        PID = paste( "P", rownames(V), sep="-" )
        names(PID) = "PID"
        Z$V = cbind( PID, Z$V )
    }	
    Z$V$PID = as.character(Z$V$PID)
    stopifnot( sum(duplicated(Z$V$PID)) == 0 )
    rownames(Z$V) = Z$V$PID
    
    Z
}




#' Check if object is elec.data object
#' 
#' @return is.elec.data: TRUE if object is an elec.data object.
#'
#' @export
#' @param x  object to test.
#'
#' @rdname is.elec.data
#' 
is.elec.data = function( x ) {
    inherits(x, "elec.data")
}


#' @title Pretty print elec.data object
#'
#' @param n Number to print
#'   
#' @return print: No return value; prints results.  
#'   
#' @rdname elec.data
#' 
#' @export
#' 
print.elec.data = function( x, n=4, ... ) {
    # "N"           "C"           "f"           "V"           "C.names"     "total.votes" "margin"      "margin.per"  "totals"      "winners"     "losers"      "audit"       "Ms"         
    Z = x
    cat( "Z frame:  N = ", Z$N, "\tC/f = ", Z$C, "/", Z$f, "\ttotal votes = ", 
         Z$total.votes, "    M = ", Z$margin,
         " (", round(100*Z$margin.per), "%)\n",
         "\t\tNames = ", paste(Z$C.names, " (", round(100*Z$totals/Z$total.votes), "%)", sep="", collapse=", "), "\t  Winners = ", 
         paste( Z$winners, collapse=", " ), "    Losers = ", 
         paste( Z$losers, collapse=", "), "\n", sep="")
    if ( Z$C > 2 ) { 
        cat( "Pairwise margins:\n" )
        print( Z$Ms )
    }
    
    cat( "Sample votes (", length( rownames(Z$V) ), " records)\n", sep="" )
    print( head( Z$V, n ) , fill=TRUE, labels=c("\t"))
    if ( !is.null(Z$audit ) ) {
        cat( "Sample Audits (", length( rownames(Z$audit) ), " records)\n", sep="" )
        print(	head( Z$audit, n ) )
        
        #fill=TRUE, labels=c("\t")
    } else {
        cat( "(No audit information)\n" )
    }
    
    invisible( Z )
    
}





