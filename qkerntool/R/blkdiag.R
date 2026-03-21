##  Block diagonal concatenation of matrix input arguments.
##

blkdiag <- function (x)
{
        dots <- x
        if (!all(sapply(dots, is.matrix)) || !all(sapply(dots, is.numeric)))
                stop("All input arguments in x must be numeric matrices")
        nrows <- sapply(dots, nrow)
        ncols <- sapply(dots, ncol)
        if (any(nrows == 0) || any(ncols == 0))
                stop("All input matrices x must be non-empty.")
        n <- sum(nrows)
        N <- c(0, cumsum(nrows))
        m <- sum(ncols)
        M <- c(0, cumsum(ncols))
        res <- matrix(0, nrow = n, ncol = m)
        k <- length(dots)
        for (i in 1:k) {
                res[(N[i] + 1):N[i + 1], (M[i] + 1):M[i + 1]] <- dots[[i]]
        }
        return(res)
}
