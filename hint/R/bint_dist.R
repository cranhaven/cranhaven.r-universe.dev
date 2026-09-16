###########################################################################
#
# Functions for calculating binomial intersection distributions.
#
# Author: Alex T. Kalinka (alex.t.kalinka@gmail.com)
#
###########################################################################


#### Main user-callable functions that do the dispatching to user-invisible worker functions below. ####
#
# Split into two sets: distribution, quantile, etc functions, and a more formal test function that will
#  take more flexible user input for testing (objects, which will be placed into categories internally).
#
# 1. Density, distribution (probability mass function), quantile, and random generation functions:


.bint.check.params <- function(n, A)
	{
	la <- length(A)
	if(!n>0 ){
		stop("the following constraint must be met:\nn > 0\n", call. = FALSE)
		}
	ll <- list()
	for(i in 1:la){
		if(!A[i]>=0 || !A[i]<=n){
			stop("the following constraints must be met:\n0 <= a <= n\n", call. = FALSE)
			}
		ll[[i]] <- A[i]
		}
	vmin <- max(sum(A)-(la-1)*n,0)
	vmax <- Reduce(min, ll)
	vrange <- vmin:vmax
	return(vrange)
	}


#' The Binomial Intersection Distribution
#' 
#' Density, distribution function, quantile function and random generation for the binomial intersection distribution.
#' 
#' @param n An integer specifying the number of categories in the urns.
#' @param A A vector of integers specifying the numbers of balls drawn from each urn. The length of the vector equals the number of urns.
#' @details The binomial intersection distribution is given by \deqn{  P(X = v|N) = {b \choose v} \left(\prod_{i=1}^{N-1} p_{i}\right)^{v} \left(1 - \prod_{i=1}^{N-1} p_{i}\right)^{b-v} }{ P(X = v|N) = choose(b,v)*((prod_{i=1}^{N-1} p_i)^v) * (1 - prod_{i=1}^{N-1} p_i)^(b-v) } where b gives the sample size which is smallest. This is an approximation for the hypergeometric intersection distribution when \eqn{n}{n} is large and \eqn{b}{b} is small relative to the samples taken from the \eqn{N-1}{N-1} other urns.
#' @name Binomialintersection
NULL
#> NULL

#' @rdname Binomialintersection
#' @param range A vector of integers specifying the intersection sizes for which probabilities (dhint) or cumulative probabilites (phint) should be computed (can be a single number). If range is NULL (default) then probabilities will be returned over the entire range of possible values.
#' @param log Logical. If TRUE, probabilities p are given as log(p). Defaults to FALSE.
#' @export
#' @examples 
#' ## Generate the distribution of intersections sizes:
#' dd <- dbint(20, c(10, 12, 11, 14))
#' ## Restrict the range of intersections.
#' dd <- dbint(20, c(10, 12), range = 0:5)
dbint <- function(n, A, range = NULL, log = FALSE)
	{
	# range is a vector giving intersection sizes for which the user wishes to retrieve probabilities.
	# A is a vector of integers giving sample sizes from each urn (N = length(A)).
	vrange <- .bint.check.params(n, A)
	if(is.null(range)){
		range <- vrange
		}
	la <- length(A)
	rn <- intersect(range, vrange)
	if(length(rn)==0){
		dist <- data.frame(v=range, p=rep(0,length(range)))
	}else{
		dist <- .bint.multi.N(n, A, range = rn)
		}
	if(log){
		dist[,2] <- log(dist[,2])
		}
	return(dist)
	}


#' @rdname Binomialintersection
#' @param vals A vector of integers specifying the intersection sizes for which probabilities (dhint) or cumulative probabilites (phint) should be computed (can be a single number). If range is NULL (default) then probabilities will be returned over the entire range of possible values.
#' @param log.p Logical. If TRUE, probabilities p are given as log(p). Defaults to FALSE.
#' @param upper.tail Logical. If TRUE, probabilities are P(X >= v), else P(X <= v). Defaults to TRUE.
#' @export
#' @examples 
#' ## Generate cumulative probabilities.
#' pp <- pbint(29, c(15, 8), vals = 5)
#' pp <- pbint(29, c(15, 8), vals = 2, upper.tail = FALSE)
pbint <- function(n, A, vals, upper.tail = TRUE, log.p = FALSE)
	{
	# vals are the values of v for which we want cumulative probabilities.
	dist <- dbint(n, A)
	if(log.p){
		dist[,2] <- log(dist[,2])
		}
	vrange <- .bint.check.params(n, A)
	rn <- intersect(vals, vrange)
	if(length(rn)==0){
		pp <- NULL
		for(i in 1:length(vals)){
			if(vals[i] < min(vrange)){
				if(upper.tail){
					pp[i] <- 1
				}else{
					pp[i] <- 0
					}
			}else{
				if(upper.tail){
					pp[i] <- 0
				}else{
					pp[i] <- 1
					}
				}
			}
		pval <- data.frame(v=vals, cum.p=rep(pp, length(vals)))
	}else{
		pv <- NULL
		for(i in 1:length(rn)){
			if(upper.tail){
				inds <- which(dist[,1]==rn[i]):nrow(dist)
			}else{
				inds <- 1:which(dist[,1]==rn[i])
				}
			pv[i] <- sum(dist[inds, 2])
			}
		pval <- data.frame(v=rn, cum.p=pv)
		}
	if(log.p){
		pval[,2] <- log(pval[,2])
		}
	return(pval)
	}


#' @rdname Binomialintersection
#' @param p A probability between 0 and 1.
#' @export
#' @examples 
#' ## Extract quantiles:
#' qq <- qbint(0.15, 23, c(12, 10))
qbint <- function(p, n, A, upper.tail = TRUE, log.p = FALSE)
	{
	# p is a probability.
	if(!p>=0 || !p<=1){
		stop("p must be between 0 and 1\n", call. = FALSE)
		}
	vrange <- .bint.check.params(n, A)
	vals <- vrange
	dist <- dbint(n, A, range=vals)
	pvals <- pbint(n, A, upper.tail=upper.tail, vals=vals)
	inds <- which(pvals[,2]<=p)
	pv <- sum(dist[inds, 2])
	qq <- pvals[max(inds),1]
	if(log.p){
		pv <- log(pv)
		}
	ret <- data.frame(v=qq, cum.p=pv)
	return(ret)
	}


#' @rdname Binomialintersection
#' @param num An integer specifying the number of random numbers to generate. Defaults to 5.
#' @export
#' @examples 
#' ## Generate random samples from Binomial intersection distributions.
#' rr <- rbint(num = 10, 18, c(9, 14))
rbint <- function(num = 5, n, A)
	{
	vrange <- .bint.check.params(n, A)
	probs <- dbint(n, A, range=vrange)
	samp <- sample(vrange, num, prob = probs[,2], replace = TRUE)
	return(samp)
	}


# Worker functions. #

# Binomial approximation for large n.
.bint.multi.N <- function(n, A, range)
	{
	ll <- list(); P <- NULL
	for(i in 1:length(A)){
		ll[[i]] <- A[i]
		P <- append(P, (A[i]/n))
		}
	b <- Reduce(min, ll)
	P <- P[-which(P==(b/n))[1]]
	dist <- NULL
	for(v in range){
		qs <- choose(b,v)*((1-prod(P))^(b-v))*(prod(P))^v		
		
		dist <- append(dist, qs)
		}
	ret <- data.frame(v=range, p=dist)
	return(ret)
	}

