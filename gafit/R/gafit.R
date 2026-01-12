gafit <- function( target, start, thermal=0.1, maxiter=50, samples=10, step=1e-3, tolerance=-Inf )
{
    env <- new.env()
    for( j in names( start ))  # Put list names into environment
    {
	#  FIXME: we want to copy the VALUE as it goes into the environment (and also force any
	#  arguments to evaluate at the same time). However standard behaviour for R is to just
	#  copy a reference here, which is not good enough ... when we get into gafit_C it will
	#  CERTAINLY modify those values in place, so we MUST trigger a copy now.
	#
	#  Without triggering a copy, values inside "start" will be modified including wherever
	#  those values came from, which is bad.
	#
	#  There's probably a better method, but as a workaround just add zero and force a new
	#  value equal to the old value... however this will also coerce the input, maybe that's good??

	tmp <- 0 + start[ j ][[ 1 ]];
        assign( j, tmp, envir=env )
    }
    gafit.workspace <- .Call( "gafit_C", target, env, thermal, maxiter, samples, step, tolerance, PACKAGE="gafit" );
    # Environment can coerce back into a list
    result <- as.list( env );
    attr( result, "score" ) <- gafit.workspace[ 1 ]
    attr( result, "count" ) <- gafit.workspace[ 5 ]
    result # Return the list containing our best guess
}
