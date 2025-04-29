

# this is a stripped down version of the original stats::ks.test function, R Under development (unstable) (2019-12-10 r77548)

# we only use x,y (two.sided and exact=FALSE are default), pvalue is returned only.
#  ks.test(x,y, exact=FALSE)$p.value  = ks_test(x,y)

ks_test <- function (x, y)
{
    x <- x[!is.na(x)]
    n <- length(x)

    y <- y[!is.na(y)]
    n.x <- as.double(n)
    n.y <- length(y)
    n <- n.x * n.y/(n.x + n.y)
    w <- c(x, y)
    z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
    STATISTIC <- max(abs(z))
    pkstwo <- function(x, tol = 1e-06) {
        if (is.numeric(x)) 
            x <- as.double(x)
        else stop("argument 'x' must be numeric")
        p <- rep(0, length(x))
        p[is.na(x)] <- NA
        IND <- which(!is.na(x) & (x > 0))
        if (length(IND)) 
            p[IND] <- .Call(pks2, p = x[IND], tol, PACKAGE="PRSim")
        p
    }
    PVAL <-   1 - pkstwo(sqrt(n) * STATISTIC)
    
    return( min(1, max(0, PVAL)) )
}
