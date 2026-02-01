#---------------------------------------------------------------------------------#
#From mvtnorm package: https://cran.r-project.org/web/packages/mvtnorm/index.html #
#---------------------------------------------------------------------------------#

isInf <- function(x) x > 0 & is.infinite(x)

rmvnorm = function(n, mu=NULL, sigma )
{
	if(is.null(mu)) mu = rep(0, n)
	p = nrow(sigma)
	y  <- matrix(rnorm(n*p),nrow = p, ncol= n)
	chl<- chol(sigma)
	z <- t( t(chl) %*% y + mu )
return(z)
}

dmvnorm <- function (x, mean = rep(0, p), sigma = diag(p), log = FALSE)
{
	p = ncol(x)
	if(is.null(mean)) mean = rep(0, p)
    if (is.vector(x))
	x <- matrix(x, ncol = length(x))
    p <- ncol(x)
    if(!missing(mean)) {
	if(!is.null(dim(mean))) dim(mean) <- NULL
	if (length(mean) != p)
	    stop("mean and sigma have non-conforming size")
    }
    if(!missing(sigma)) {
	if (p != ncol(sigma))
	    stop("x and sigma have non-conforming size")
	if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
			 check.attributes = FALSE))
	    stop("sigma must be a symmetric matrix")
    }

    
    dec <- tryCatch(chol(sigma), error=function(e)e)
    if (inherits(dec, "error")) {
        x.is.mu <- colSums(t(x) != mean) == 0
        logretval <- rep.int(-Inf, nrow(x))
        logretval[x.is.mu] <- Inf 
    } else {
	tmp <- backsolve(dec, t(x) - mean, transpose = TRUE)
	rss <- colSums(tmp ^ 2)
	logretval <- - sum(log(diag(dec))) - 0.5 * p * log(2 * pi) - 0.5 * rss
    }
    names(logretval) <- rownames(x)
    if(log) logretval else exp(logretval)
}


rmvt <- function(n, sigma = diag(2), df = 1,
                 delta = rep(0, nrow(sigma)),
                 type = c("shifted", "Kshirsagar"))
{
    if (length(delta) != nrow(sigma))
        stop("delta and sigma have non-conforming size")

    if (df == 0 || isInf(df))
	{
		return(rmvnorm(n, mu = delta, sigma = sigma))
	}	
    type <- match.arg(type)
    switch(type,
           "Kshirsagar" = {
               return(rmvnorm(n, mu = delta, sigma = sigma)/
                      sqrt(rchisq(n, df)/df))
           },
           "shifted" = {
               sims <- rmvnorm(n, sigma = sigma)/sqrt(rchisq(n, df)/df)
               return(sweep(sims, 2, delta, "+"))
           },
           stop("wrong 'type'"))
}
