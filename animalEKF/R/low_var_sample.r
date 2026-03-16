low_var_sample <- function(wts, M=length(wts)) {

	#M should be number of particles
   xr <- runif(1, min=0, max=1/M)
   u <- (xr + ((1:M) -1)/M)*sum(wts)
   cs <- cumsum(wts)
   indices <- .bincode(x=u, breaks=c(0,cs))
   indices
}
