retour_op <- function
### This function is used by the Fpop function to recover the best
### segment ends from 1:n from the C output.
(path
### the path vector of the "colibri_op_R_c C" function
){
   chaine <- integer(1)
   chaine[1] <- length(path)
   j <- 2
   while(chaine[j-1] > 0){
     chaine[j] <- path[chaine[j-1]]
     j=j+1	
   }
   rev(chaine)[-1]
### a vector with the best segment ends.
}

Fpop <- structure(function
### Function calling the fpop algorithm, use functional pruning and
### optimal partionning to recover the best segmentation with respect
### to the L2 loss with a per change-point penalty of lambda. More
### precisely, this function computes the solution to argmin_m
### sum_{i=1}^n (x_i-m_i)^2 + lambda * sum_{i=1}^{n-1} I(m_i !=
### m_{i+1}), where the indicator function I counts the number of
### changes in the mean vector m.
(x, 
### A vector of double : the signal to be segmented
lambda, 
### Value of the penalty
mini=min(x), 
### Min value for the mean parameter of the segment
maxi=max(x)
### Max value for the mean parameter of the segment
){
  n <- length(x)
  A <- .C("colibri_op_R_c", signal=as.double(x), n=as.integer(n), 
		lambda=as.double(lambda),   min=as.double(mini), 
		max=as.double(maxi), path=integer(n), cost=double(n)
	, PACKAGE="fpop")
    A$t.est <- retour_op(A$path)
    A$K <- length(A$t.est)
    A$J.est <- A$cost[n] - (A$K+1)*lambda + sum(x^2)
    return(A);	
### Named list with the following elements: input data (signal, n,
### lambda, min, max), path (best previous segment end up to each data
### point), cost (optimal penalized cost up to each data point), t.est
### (vector of overall optimal segment ends), K (optimal number of
### segments), J.est (total un-penalized cost of optimal model). To
### see how cost relates to J.est, see definition of J.est in the R
### source code for this function.
}, ex=function(){
  set.seed(1)
  N <- 100
  data.vec <- c(rnorm(N), rnorm(N, 2), rnorm(N))
  fit <- Fpop(data.vec, N)
  end.vec <- fit$t.est
  change.vec <- end.vec[-length(end.vec)]
  start.vec <- c(1, change.vec+1)
  segs.list <- list()
  for(seg.i in seq_along(start.vec)){
    start <- start.vec[seg.i]
    end <- end.vec[seg.i]
    seg.data <- data.vec[start:end]
    seg.mean <- mean(seg.data)
    segs.list[[seg.i]] <- data.frame(
      start, end,
      mean=seg.mean,
      seg.cost=sum((seg.data-seg.mean)^2))
  }
  segs <- do.call(rbind, segs.list)
  plot(data.vec)
  with(segs, segments(start-0.5, mean, end+0.5, mean, col="green"))
  with(segs[-1,], abline(v=start-0.5, col="green", lty="dotted"))
})

fpop_analysis <- function
### A function to count the number of intervals and or candidate
### segmentation at each step of fpop (under-developpemment)
(x,
### A vector of double : the signal to be segmented
lambda, 
### Value of the penalty
mini=min(x), 
### Min value for the mean parameter of the segment
maxi=max(x)
### Max value for the mean parameter of the segment
){
  n <- length(x)
  A <- .C("colibri_op_R_c_analysis", signal=as.double(x), n=as.integer(n), lambda=as.double(lambda),   min=as.double(mini), max=as.double(maxi), path=integer(n), cost=double(n), nbCandidate=integer(n)
	, PACKAGE="fpop")
  A$t.est <- retour_op(A$path)
  return(A);	
### return a list with a vector containing the position of the change-points t.est
} 


