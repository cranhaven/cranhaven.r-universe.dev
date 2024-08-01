AROC.f <-
function(x1 = NULL, by = NULL, K = 0) {   
	args <- match.call()
	if(is.null(args$x1) & is.null(args$x2))
		stop("x1 or x2 must be indicated")
	if(!is.null(args$x1) & is.null(args$by)) { # Smooth effect			   
		cov <- c("-1", deparse(args$x1, backtick = TRUE, width.cutoff = 500))
	} else if (!is.null(args$x1) & !is.null(args$by)) {	  
		cov <- c(deparse(args$by, backtick = TRUE, width.cutoff = 500), deparse(args$x1, backtick = TRUE, width.cutoff = 500))		
	} else {
		stop("Invalid expression")
	}
	res <- list(cov = cov, K = K)
	res
}
