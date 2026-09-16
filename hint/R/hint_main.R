###########################################################################
#
# Functions for calculating hypergeometric intersection distributions.
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


.hint.check.params <- function(n, A, q)
	{
	la <- length(A)
	if(la>2 && q!=0){
		stop("q must be 0 if there are more than 2 urns\n", call. = FALSE)
	}else if(la==2 && q!=0){
		if(!A[2]<=(n+q)){
			stop("the following constraint must be met:\n0 <= b <= (n + q)\n", call. = FALSE)
			}
		}
	if(!n>0 || !q>=0 || !q<=n){
		stop("the following constraints must be met:\nn > 0\n0 <= q <= n\n", call. = FALSE)
		}
	ll <- list()
	for(i in 1:la){
		if(!A[i]>=0 || !A[i]<=n){
			stop("the following constraints must be met:\n0 <= a <= n\n", call. = FALSE)
			}
		ll[[i]] <- A[i]
		}

	if(la==2 && q>0){
		vmin <- max(sum(A)-n-min(floor(A[2]/2),q),0)
	}else{
		vmin <- max(sum(A)-(la-1)*n,0)
		}
	
	vmax <- Reduce(min, ll)
	vrange <- vmin:vmax
	return(vrange)
	}


#' The Hypergeometric Intersection Family of Distributions
#'
#' @param n An integer specifying the number of categories in the urns.
#' @param A A vector of integers specifying the numbers of balls drawn from each urn. The length of the vector equals the number of urns.
#' @param q An integer specifying the number of categories in the second urn which have duplicate members. If q is 0 (default) then the symmetrical, singleton case is computed, otherwise the asymmetrical, duplicates case is computed (see Details).
#' @name Hyperintersection
#' @details The hypergeometric intersection distributions describe the distribution of intersection sizes when sampling without replacement from two separate urns in which reside balls belonging to the same n object categories. In the simplest case when there is exactly one ball in each category in each urn (symmetrical, singleton case), then the distribution is hypergeometric: \deqn{P(X=v)=\frac{{a \choose v}{n-a \choose b-v}}{{n \choose b}}}{P(X=v) = (choose(a,v)*choose(n-a,b-v))/choose(n,b)} When there are three urns, the distribution is given by \deqn{P(X=v) = \frac{ {a \choose v} \sum_{i} {a-v \choose i} {n-a \choose b-v-i} {n-v-i \choose c-v} }{ {n \choose b} {n \choose c} } }{P(X=v) = choose(a,v) sum_i choose(a-v,i)*choose(n-a,b-v-i)*choose(n-v-i,c-v)/choose(n,b)*choose(n,c)} If, however, we allow duplicates in \eqn{q \leq n}{q <= n} of the categories in the second urn, then the distribution of intersection sizes is described by the following variant of the hypergeometric: \deqn{P(X=v) = \sum_{m=0}^{\alpha} \sum_{l=0}^{\beta} \sum_{j=0}^{l} {n-q \choose v-l} {q \choose l} {q-l \choose m} {n-v-q+l \choose a-v-m}  {l \choose j} {n+q-a-m-j \choose b-v} / {n \choose a}{n+q \choose b}}{P(X=v) = sum_m sum_l sum_j choose(n-q,v-l)*choose(q,l)*choose(q-l,m)*choose(n-v-q+l,a-v-m)*choose(l,j)*choose(n+q-a-m-j,b-v)/ choose(n,a)*choose(n+q,b)}
#' @return `dhint`, `phint`, and `qhint` return a data frame with two columns: v, the intersection size, and p, the associated p-values. `rhint` returns an integer vector of random samples based on the hypergeometric intersection distribution.
#' @references Kalinka, A. T. (2013). The probability of drawing intersections: extending the hypergeometric distribution. \href{https://arxiv.org/abs/1305.0717}{arXiv.1305.0717}
#' @rawNamespace useDynLib(hint, .registration = TRUE)
NULL
#> NULL

#' @rdname Hyperintersection
#' @param range A vector of integers specifying the intersection sizes for which probabilities (dhint) or cumulative probabilites (phint) should be computed (can be a single number). If range is NULL (default) then probabilities will be returned over the entire range of possible values.
#' @param approx Logical. If TRUE, a binomial approximation will be used to generate the distribution.
#' @param log Logical. If TRUE, probabilities p are given as log(p). Defaults to FALSE.
#' @param verbose Logical. If TRUE, progress of calculation in the asymmetric, duplicates case is printed to the screen.
#' @export
#' @examples 
#' ## Generate the distribution of intersections sizes without duplicates:
#' dd <- dhint(20, c(10, 12))
#' ## Restrict the range of intersections.
#' dd <- dhint(20, c(10, 12), range = 0:5)
#' ## Allow duplicates in q of the categories in the second urn:
#' dd <- dhint(35, c(15, 11), 22, verbose = FALSE)
dhint <- function(n, A, q = 0, range = NULL, approx = FALSE, log = FALSE, verbose = TRUE)
	{
	# range is a vector giving intersection sizes for which the user wishes to retrieve probabilities.
	# A is a vector of integers giving sample sizes from each urn (N = length(A)).
	# If approx is TRUE then a binomial approximation will be applied (with warning(s) if assumptions are not met).
	vrange <- .hint.check.params(n, A, q)
	if(is.null(range)){
		range <- vrange
		}
	la <- length(A)
	if(q==0){
		rn <- intersect(range, vrange)
		if(length(rn)==0){
			dist <- data.frame(v=range, p=rep(0,length(range)))
			}
		if(la==2){
			dist <- .hint.symm.sing(n, A[1], A[2], range = rn)
		}else if(approx){
			dist <- .bint.multi.N(n, A, range = rn)
		}else if(la == 3){
			dist <- .hint.multi.urn.3(n, A, range = rn)
		}else if(la == 4){
			dist <- .hint.multi.urn.4(n, A, range = rn, verbose = verbose)
		}else if(la > 4 && !approx){
			stop("no exact distribution known; to use a binomial approximation set approx = TRUE\n", call. = FALSE)
			}
	}else{
		rn <- intersect(range, vrange)
		if(length(rn)==0){
			dist <- data.frame(v=range, p=rep(0,length(range)))
			}
		dist <- .hint.asymm.dup(n, A[1], A[2], q, range = rn, verbose = verbose)
		}
	if(log){
		dist[,2] <- log(dist[,2])
		}
	return(dist)
	}


#' @rdname Hyperintersection
#' @param vals A vector of integers specifying the intersection sizes for which probabilities (dhint) or cumulative probabilites (phint) should be computed (can be a single number). If range is NULL (default) then probabilities will be returned over the entire range of possible values.
#' @param log.p Logical. If TRUE, probabilities p are given as log(p). Defaults to FALSE.
#' @param upper.tail Logical. If TRUE, probabilities are P(X >= c), else P(X <= c). Defaults to TRUE.
#' @export
#' @examples
#' ## Generate cumulative probabilities.
#' pp <- phint(29, c(15, 8), vals = 5)
#' pp <- phint(29, c(15, 8), vals = 2, upper.tail = FALSE)
#' pp <- phint(29, c(15, 8), 23, vals = 2)
phint <- function(n, A, q = 0, vals, upper.tail = TRUE, log.p = FALSE)
	{
	# vals are the values of v for which we want cumulative probabilities.
	dist <- dhint(n, A, q)
	if(log.p){
		dist[,2] <- log(dist[,2])
		}
	vrange <- .hint.check.params(n, A, q)
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


#' @rdname Hyperintersection
#' @param p A probability between 0 and 1.
#' @export
#' @examples
#' ## Extract quantiles:
#' qq <- qhint(0.15, 23, c(12, 10))
#' qq <- qhint(0.15, 23, c(12, 10), 18)
qhint <- function(p, n, A, q = 0, upper.tail = TRUE, log.p = FALSE)
	{
	# p is a probability.
	if(!p>=0 || !p<=1){
		stop("p must be between 0 and 1\n", call. = FALSE)
		}
	vrange <- .hint.check.params(n, A, q)
	vals <- vrange
	dist <- dhint(n, A, q, range=vals)
	pvals <- phint(n, A, q, upper.tail=upper.tail, vals=vals)
	inds <- which(pvals[,2]<=p)
	pv <- sum(dist[inds, 2])
	qq <- pvals[max(inds),1]
	if(log.p){
		pv <- log(pv)
		}
	ret <- data.frame(v=qq, cum.p=pv)
	return(ret)
	}


#' @rdname Hyperintersection
#' @param num An integer specifying the number of random numbers to generate. Defaults to 5.
#' @export
#' @examples
#' ## Generate random samples from Hypergeometric intersection distributions.
#' rr <- rhint(num = 10, 18, c(9, 14))
#' rr <- rhint(num = 10, 22, c(11, 17), 12)
rhint <- function(num = 5, n, A, q = 0)
	{
	vrange <- .hint.check.params(n, A, q)
	probs <- dhint(n, A, q, range=vrange)
	samp <- sample(vrange, num, prob = probs[,2], replace = TRUE)
	return(samp)
	}


############################################################
#
# 2. Functions to perform tests of enrichment or depletion.
#    Includes distance distribution.
#
##

#' print.hint.test
#' 
#' Prints the resuls of `hint.test`.
#' 
#' @param x An object of class `hint.test`.
#' @param ... Additional arguments to be passed to `print`.
#' 
#' @return Prints output to the console.
#' @export
print.hint.test <- function(x, ...)
	# S3 method for generic function "print".
	# x is a "hint.test".
	{
	md <- match("d",names(x$parameters))
	if(is.na(md)){
		rv <- "v"
	}else{
		rv <- "d"
		}
	if(x$alternative == "greater"){
		p.test <- paste("P(X >= ",rv,")",sep="")
	}else if(x$alternative == "less"){
		p.test <- paste("P(X <= ",rv,")",sep="")
	}else{
		p.test <- "P(two-sided)"
		}
	cat("\nHypergeometric intersection test\n\nParameters:\n")
	print(x$parameters)
	cat("\n",p.test," = ",x$p.value,"\n\n")
	}


#' hint.test
#' 
#' Apply the hypergeometric intersection test to categorical data to test for enrichment or depletion of intersections between two samples.
#' 
#' @param cats A data frame or matrix with 3 columns; the first gives the category identifier, and the second and third give the number of balls belonging to this category in the first and second urns respectively.
#' @param draw1 A vector of objects corresponding to the categories given in cats drawn from the first urn.
#' @param draw2 A vector of objects corresponding to the categories given in cats drawn from the second urn.
#' @param alternative A characer string specifying the hypothesis to be tested. Can be one of "greater", "less", or "two.sided".
#' 
#' @details The hypergeometric intersection distributions describe the distribution of intersection sizes when sampling without replacement from two separate urns in which reside balls belonging to the same n object categories (see \code{\link{Hyperintersection}}).
#' @return An object of class `hint.test`, which is a list containing the following components:
#' * `parameters` An integer vector giving the parameter values.
#' * `p.value` A numerical value giving the p-value associated with the test.
#' * `alternative` A character string naming the hypothesis that was tested.
#' @md
#' @references Kalinka, A. T. (2013). The probability of drawing intersections: extending the hypergeometric distribution. \href{https://arxiv.org/abs/1305.0717}{arXiv.1305.0717}
#' @export
hint.test <- function(cats, draw1, draw2, alternative = "greater")
	{
	# cats is a data frame or character matrix of category names and numbers for each urn:
	#    cats urn.1 urn.2
	#	a     1     1
	#	b     1     1
	#	c     1     1
	#	d     1     1
	#	e     1     1
	# draw1 and draw2 are samples of the categories from urn 1 and 2 respectively:
	# [1] "a" "b" "c" "d" "e"
	#
	# alternative can be: greater, less, or two.sided.
	# two-sided defined by doubling: 2*min(p.upper, p.lower)
	#
	n <- length(unique(cats[,1]))
	a <- length(draw1)
	b <- length(draw2)
	q <- length(which(cats[,3]==2))
	v <- length(intersect(draw1,draw2))
	if(alternative == "greater"){
		ut <- TRUE
	}else if(alternative == "less"){
		ut <- FALSE
	}else if(alternative == "two.sided"){
		p1 <- phint(n, A = c(a, b), q, vals = v, upper.tail = TRUE)
		p2 <- phint(n, A = c(a, b), q, vals = v, upper.tail = FALSE)
	}else{
		stop("alternative hypothesis must be one of: greater, less, or two.sided\n", call. = FALSE)
		}
	if(alternative == "two.sided"){
		pval <- 2*min(p1, p2)
	}else{
		pval <- phint(n, A = c(a, b), q, vals = v, upper.tail = ut)[,2]
		}
	ret <- list()
	params <- c(n,a,b,q,v)
	names(params) <- c("n","a","b","q","v")
	ret$parameters <- params
	ret$p.value <- pval
	ret$alternative <- alternative
	class(ret) <- "hint.test"
	return(ret)
	}


#' hint.dist.test
#' 
#' Tests whether the absolute distance between two intersection sizes would be expected by chance, i.e. whether they fall into opposite tails of their respective Hypergeometric Intersection distributions.
#' 
#' @param d A positive integer specifying the observed distance to be tested.
#' @param n1 An integer specifying the number of categories in the urns for the first distribution.
#' @param A1 An integer vector specifying the number of balls drawn from urns for the first distribution.
#' @param n2 An integer specifying the number of categories in the urns for the second distribution.
#' @param A2 An integer vector specifying the number of balls drawn from the urns for the second distribution.
#' @param q1 An integer specifying the number of categories with duplicates in the second urn of the first distribution. If 0 then the symmetric, singleton case is computed, otherwise the asymmetric, duplicates case is computed (see \code{\link{Hyperintersection}}).
#' @param q2 An integer specifying the number of categories with duplicates in the second urn of the second distribution. If 0 then the symmetric, singleton case is computed, otherwise the asymmetric, duplicates case is computed (see \code{\link{Hyperintersection}}).
#' @param alternative A characer string specifying the hypothesis to be tested. Can be one of "greater", "less", or "two.sided".
#' @details The distribution of absolute distances between two hypergeometric intersection sizes is given by \deqn{P(X=d) = \sum_{\{v_{1},v_{2}\}_{i} \in D_{d}}^{|D_{d}|} P(v_{1_i}|n_{1},a_{1},b_{1},...)\cdot P(v_{2_i}|n_{2},a_{2},b_{2},...) }{P(X=d) = sum_{v1,v2} P(v1)*P(v2) } where \eqn{D_{d}}{D_d} is the set of pairs of intersection sizes, \eqn{\{v_{1},v_{2}\}}{(v_1,v_2)}, with absolute differences of size \eqn{d}{d}.
#' @return An object of class `hint.dist.test`, which is a list containing the following components:
#' * `parameters` An integer vector giving the parameter values.
#' * `p.value` A numerical value giving the p-value associated with the test.
#' * `alternative` A character string naming the hypothesis that was tested.
#' @md
#' @export
hint.dist.test <- function(d, n1, A1, n2, A2, q1 = 0, q2 = 0, alternative = "greater")
	{
	# d is the distance we wish to test.
	# alternative can be: greater, less, or two.sided.
	# two-sided defined by doubling: 2*min(p.upper, p.lower)
	#
	vrange1 <- .hint.check.params(n1, A1, q1)
	vrange2 <- .hint.check.params(n2, A2, q2)
	dist <- .hint.dist.distr(n1, A1, n2, A2, q1, q2)
	if(alternative == "greater"){
		inds <- which(dist[,1] >= d)
		if(length(inds)==0){
			pval <- 0
		}else{
			pval <- sum(dist[inds, 2])
			}
	}else if(alternative == "less"){
		inds <- which(dist[,1] <= d)
		if(length(inds)==0){
			pval <- 0
		}else{
			pval <- sum(dist[inds, 2])
			}
	}else if(alternative == "two.sided"){
		inds1 <- which(dist[,1] >= d)
		inds2 <- which(dist[,1] <= d)
		if(length(inds1)==0){
			p1 <- 0
		}else{
			p1 <- sum(dist[inds1, 2])
			}
		if(length(inds2)==0){
			p2 <- 0
		}else{
			p2 <- sum(dist[inds2, 2])
			}
		pval <- 2*min(p1, p2)
	}else{
		stop("alternative hypothesis must be one of: greater, less, or two.sided\n", call. = FALSE)
		}
	ret <- list()
	params <- c(d,n1,A1,q1,n2,A2,q2)
	names(params) <- c("d","n1","a1","b1","q1","n2","a2","b2","q2")
	ret$parameters <- params
	ret$p.value <- pval
	ret$alternative <- alternative
	class(ret) <- "hint.test"
	return(ret)
	}



#### Worker functions: ####


# Symmetrical, singletons case.
.hint.symm.sing <- function(n, a, b, range)
	{
	dist <- NULL
	for(i in range){
		dist <- append(dist, exp(-1*lgamma((i+1)) - lgamma((b-i+1)) - lgamma((a-i+1)) - lgamma((n-b-a+i+1)) - lgamma((n+1)) + lgamma((b+1)) + lgamma((n-b+1)) + lgamma((a+1)) + lgamma((n-a+1))) )
		}
	ret <- data.frame(v=range, p=dist)
	return(ret)
	}


# Asymmetrical, duplicates case.
.hint.asymm.dup <- function(n, a, b, q, range, verbose = TRUE)
	{
	len <- length(range)
	dist <- rep(0, len)
	out <- .C("inters_distr_dup_opt",as.integer(n),as.integer(a),as.integer(b),as.integer(q),dist=as.double(dist),as.integer(range),as.integer(len),as.logical(verbose))
	if(verbose){
		cat("\n")
		}
	dist <- out$dist
	ret <- data.frame(v=range, p=dist)
	return(ret)
	}


# Hypergeometric intersection for 3 urns: lgamma.
.hint.multi.urn.3 <- function(n, A, range)
	{
	a <- A[1]; b <- A[2]; c <- A[3]
	dist <- NULL
	for(v in range){
		qs <- 0
		mni <- max(a+b-n-v,0)
		mxi <- min(a-v,b-v)
		for(i in mni:mxi){
			if(n-a-b+v+i+1 <= 0 || n-c-i+1 <=0 || n-v-i+1 <=0){
				qs <- qs
			}else{
				qs <- sum(qs, exp(lgamma(a+1) - lgamma(v+1) - lgamma(a-v-i+1) - lgamma(i+1) + lgamma(n-a+1) - lgamma(b-v-i+1) - lgamma(n-a-b+v+i+1) + lgamma(n-v-i+1) - lgamma(n-c-i+1) - lgamma(c-v+1) + lgamma(b+1) + lgamma(n-b+1) - lgamma(n+1) + lgamma(c+1) + lgamma(n-c+1) - lgamma(n+1)) )
				}
			}

		dist <- append(dist, qs)
		}
	ret <- data.frame(v=range, p=dist)
	return(ret)
	}


# Hypergeometric intersection for 4 urns: lgamma.
.hint.multi.urn.4 <- function(n, A, range, verbose = TRUE)
	{
	a <- A[1]; b <- A[2]; c <- A[3]; d <- A[4]
	dist <- NULL
	for(v in range){
		if(verbose){
			cat("   Completed...",round((v/max(range))*100,3),"%\r",collapse="")
			}
		qs <- 0
		mni <- max(a+b-n-v,0)
		mxi <- min(a-v,b-v)
		for(i in mni:mxi){
			mnl <- max(a+b+c-2*n-2*v,0)
			mxl <- i
			for(l in mnl:mxl){
				if(n-a-b+v+i+1 <= 0 || n-c-i+l+1 <=0 || n-l-d+1 <=0 || n-v-l+1 <=0 || c-v-l+1 <=0 || b-v-i+1 <=0){
					qs <- qs
				}else{
					qs <- sum(qs, exp( lgamma(a+1) - lgamma(v+1) - lgamma(a-v-i+1) - lgamma(i-l+1) - lgamma(l+1) + lgamma(n-a+1) - lgamma(n-a-b+v+i+1) - lgamma(b-v-i+1) + lgamma(n-v-i+1) - lgamma(n-i-c+l+1) - lgamma(c-v-l+1) + lgamma(n-v-l+1) - lgamma(n-l-d+1) - lgamma(d-v+1) - 3*lgamma(n+1) + lgamma(n-b+1) + lgamma(b+1) + lgamma(n-c+1) + lgamma(c+1) + lgamma(n-d+1) + lgamma(d+1) ) )
					}
				}
			}

		dist <- append(dist, qs)
		}
	cat("\n")
	ret <- data.frame(v=range, p=dist)
	return(ret)
	}


# Distribution of distances between independent intersection distributions.
.hint.dist.distr <- function(n1, A1, n2, A2, q1 = 0, q2 = 0)
	{
	dist <- NULL
	a1 <- A1[1]; b1 <- A1[2]; a2 <- A2[1]; b2 <- A2[2]
	m1 <- min(a1,b1)
	m2 <- min(a2,b2)
	m3 <- max(a1+b1-n1-min(floor(b1/2),q1),0)
	m4 <- max(a2+b2-n2-min(floor(b2/2),q2),0)
	if(m1 > m2){
		mxdist <- m1 - m4
		mndist <- max(m3 - m2,0)
	}else{
		mxdist <- m2 - m3
		mndist <- max(m4 - m1,0)
		}
	for(d in mndist:mxdist){
		cat("   Completed...",round((d/mxdist)*100,3),"%\r",collapse="")
		# Enumerate intersection pairs that produce distance d.
		rm <- m2 - d
		sm <- m1 - d
		A <- min(m1,rm)+1
		B <- min(m2,sm)+1
		if(A<0 && B>0){
			Q <- B
		}else if(A>0 && B<0 || d == 0){
			Q <- A
		}else{
			Q <- A + B
			}
		if(Q <= 0){dist[(d+1)] <- 0; next}
		pairs <- matrix(0,Q,2)
		lim <- A
		one <- 0; if(d==0){two <- 0}else{two <- d}
		if(lim>0){
			for(i in 1:lim){
				pairs[i,] <- c(one,two)
				one <- one + 1
				two <- two + 1
				}
			}
		one <- d; two <- 0
		if(d!=0){
			lim2 <- B
			if(lim<Q && lim2>0){
				for(i in (lim+1):Q){
					pairs[i,] <- c(one,two)
					one <- one + 1
					two <- two + 1
					}
				}
			}
		# Calculate probability for this d.
		sd <- 0 

		for(i in 1:Q){
			# First distribution.
			v <- pairs[i,1]
			if(v < m3){
				p1 <- 0
			}else{
				p1 <- dhint(n1, A = c(a1, b1), q1, range = v, verbose = FALSE)[,2]
				}
			# Second distribution.
			v <- pairs[i,2]
			if(v < m4){
				p2 <- 0
			}else{
				p2 <- dhint(n2, A = c(a2, b2), q2, range = v, verbose = FALSE)[,2]
				}
			sd <- sum(sd, p1*p2)
			}
		dist <- append(dist, sd)
		}
	cat("\n")
	ret <- data.frame(d=mndist:mxdist, p=dist)
	return(ret)
	}


### Simulation ###

# Two urns.
# Variable numbers in each category:
.sim.hypint <- function(n, A, sims = 10000, Na = NULL)
	{
	# Na is a list of vectors with the numbers in each category (if they're variable).
	if(!is.null(Na)){
		if(length(Na) != length(A)){
			stop("the length of A must equal the length of Na", call. = FALSE)
			}
	}else{
		Na <- list()
		for(i in 1:length(A)){
			Na[[i]] <- rep(1,n)
			}
		}

	sdist <- NULL; na <- list()
	for(i in 1:length(Na)){
		nn <- NULL
		for(j in 1:n){
			nn <- append(nn, rep(j,Na[[i]][j]))
			}
		na[[i]] <- nn
		}

	for(i in 1:sims){
		cat("\r   Completed...",round((i/sims)*100,3),"%",collapse="")
		samps <- list()
		for(j in 1:length(A)){
			samps[[j]] <- sample(na[[j]], A[j], replace = FALSE)
			}
		sdist[i] <- length(Reduce(intersect, samps))
		}
	cat("\n")
	return(sdist)
	}


# To overlay results of simulation on top of distribution point plot.
.overlay.sim <- function(sim, breaks, col = "red", pch = 1, lwd = 1)
	{
	# breaks is a vector giving the exact breaks as determined by the distribution.
	pts <- NULL
	len <- length(sim)
	for(i in breaks){
		pts <- append(pts, length(which(sim==i))/len )
		}

	points(breaks, pts, col=col, pch=pch, lwd=lwd)
	return(invisible())
	}

