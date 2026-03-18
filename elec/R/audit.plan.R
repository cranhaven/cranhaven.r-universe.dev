
# Code for the audit.plan S3 class




#' @title
#' Audit Plans for CAST and Trinomial Methods
#' @name audit.plan
#'
#' @description 
#' An \code{audit.plan} is returned by CAST.calc.sample, containing details of
#' how to audit for a desired level of confidence.  It has a print method for
#' pretty output.
#' 
#' The \code{audit.plan.tri}, similarly, is an object that holds information
#' about conduting a PPEB election audit, in particular an audit that will use
#' the trinomial bound to analyze resultant audit data.  It is what is returned
#' by the tri.calc.sample method.
#' 
#' Theoretically, auditors will use the plan and go out and generate actual
#' audit data.  (You can fake it with simulations--see \link{make.truth}.)  The
#' audit data should be stored in a new data frame with new vote totals, or
#' overstatements, for the candidates in the audited precincts. To convert from
#' totals to overstatements, use \code{\link{audit.totals.to.OS}}.  You can
#' store that in a elec.data object under ``audit'', or keep it seperate.
#' 
#' @param x An audit plan (or trinomial audit plan).
#' @param ...  Unused.
#' @author Luke W. Miratrix
#' @seealso \link{CAST.calc.sample} \link{tri.calc.sample}
#' @keywords manip
NULL




#' @return is.audit.plan: TRUE if object is an audit.plan object.
#'
#' @export
#' @param x Object to check
#'
#' @rdname audit.plan
#' 
is.audit.plan = function( x ) {
    inherits( x, "audit.plan" )
}



#' @title Pretty print audit plan
#'
#' @param audit.plan to print.
#' @param ... No extra options passed.
#'   
#' @return print: No return value; prints results.  
#'   
#' @rdname audit.plan
#' 
#' @export
print.audit.plan = function( x, ... ) {
    P = x
    if ( x$as.taint ) {
        met = paste( P$method, "(taint)", collapse="")
    } else {
        met = P$method
    }
    cat(sprintf("Audit plan: beta=%.2f  stages=%d   beta1=%.2f   met=%s\n", 
                P$beta, P$stages, P$beta1, met))
    if ( P$t < 1 ) {
        cat(sprintf("\tt=%.3f\t q=%d\t N=%d / %d\t n=%d\n", 
                    P$t, P$q, P$N, P$Z$N, P$n ) )
    } else {
        cat(sprintf("\tt=%d\t q=%d\t N=%d / %d\t n=%d\n", 
                    P$t, P$q, P$N, P$Z$N, P$n ) )
    }
    cat(sprintf("\tskipped=%d\t mar lost=%.0f%%\n",
                P$skipped, 100*(1-P$threshold) ) )
    cat(sprintf("\tE[# pcts audited]=%.1f\t\t E[votes audited]=%.1f\n",
                P$n, sum(P$E.votes) ) )
    if ( length(P$stratas) > 1 ) {
        tb =  rbind( P$stratas, P$ns, P$E.votes )
        rownames(tb) = c( "N", "n", "E.vts" )
        colnames(tb) = names(P$ns)
        print(tb)
    }
}




#' @return is.audit.plan.tri: TRUE if object is an audit.plan.tri object.
#'
#' @export
#' @param x object to check
#'
#' @rdname audit.plan
#' 
is.audit.plan.tri = function( x ) {
    inherits( x, "audit.plan.tri" )
}



#' @title Pretty print audit plan (tri version)
#'
#' @param audit.plan.tri to print.
#' @param ... No extra options passed.
#'   
#' @return print: No return value; prints results.  
#'   
#' @rdname audit.plan
#' 
#' @export
print.audit.plan.tri = function( x, ... ) {
    cat( "Audit plan: beta=", x$beta, "  stages=", x$stages, "  beta1=", x$beta1, 
         "\n\t\td^+=", x$d.plus, " (vote swing of ", x$swing, ")    p_d=", x$p_d, 
         "\n\t\tP=", x$P, "  cut=", x$cut, "  T=", x$T, "  1/T=", (1/x$T),
         "\n\t\tn=", x$n, "  met=", "  PPEB w/ ", x$bound, sep="",
         "\n\t\tE[# pcts audited]=", round( x$E.p, digits=1), "   E[votes audited]=", round(x$E.vts), "\n" )
    #			"\n\t\tskipped=", x$skipped, " %mar lost =", (1-x$threshold), "\n" )
    #	if ( length(x$stratas) > 1 ) {
    #		print( rbind( x$stratas, x$ns ) )
    #	}
}




