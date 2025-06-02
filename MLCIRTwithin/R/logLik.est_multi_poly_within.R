logLik.est_multi_poly_within <-function(object, ...){
	
# preliminaries
	out = object
# print output	
    cat("\nLog-likelihood:\n")
    print(round(out$lk,4))
    cat("\n")
# output
	lk = out$lk	    
	
}