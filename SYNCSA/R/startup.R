.onLoad <- function(libname, pkgname){
	op <- options()
	op.SYNCSA <- list(SYNCSA.speed = TRUE)
	toset <- !(names(op.SYNCSA) %in% names(op))
	if(any(toset)){
		options(op.SYNCSA[toset])	
	} 
	invisible()
}