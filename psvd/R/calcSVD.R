
#############################################################################
calcSVD <- function(X, r=2, eta=10, q=2, itmax=200, err=1e-8, mySeed=50) {
    d <- ncol(X)

    # Initial random orthonormal matrix
    set.seed(mySeed)
    w <- c(rnorm(d*r, 0,1))
    res <- mGSc(w, d, r)
    wp <- matrix(res$wp, ncol = r)

    # power q of the symmetric matrix to use in the iterative search
    imd <- diag(c(rep(1,d)))
    xtx <- t(X) %*% X
    t1 <-  xtx %*% xtx
    if (q>4 | q<=2) { tt <- imd + eta*t1 }
    else if (q==3) { tt <- imd + eta*t1 %*% xtx }
    else {  tt <- imd + eta*t1 %*% t1 }

    # calculation of the eigenvectors matrix
    res <- eigenVc(tt, wp, d, r, itmax, err)
    v <- matrix(res$wc, ncol = r)
    t <- res$iter

    # results extraction for the SVD
    xw <- X %*% v
    tt <- diag(t(xw) %*% xw)
    d <- sqrt(tt)
    u <- xw %*% diag(1/d)
    list(d=d,u=u,v=v,iter=t)
}
#############################################################################
