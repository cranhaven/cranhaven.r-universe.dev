
#############################################################################
calcPCA <- function(X, r=2, eta=10, itmax=200, err=1e-8, normed = TRUE,
                    mySeed=50) {
    d <- ncol(X)
    m <- nrow(X)

    # initial random orthonormal matrix
    set.seed(mySeed)
    w <- c(rnorm(d*r, 0,1))
    res <- mGSc(w, d, r)
    wp <- matrix(res$wp, ncol=r)

    # data centering
    moy <- apply(X, 2, mean)
    Xm <- X - matrix(rep(moy, each=m), ncol=d)

    # data standarization if normed is TRUE and compute the SVD
    if (normed) {
       std <- apply(Xm, 2, sd)*sqrt((m-1)/m)
       Si <- diag(1/std)
       Xs <- Xm %*% Si
       res <- calcSVD(Xs, r, eta, itmax, err)
    } else {
       Xs <- Xm
       res <- calcSVD(Xs, r, eta, itmax, err)
    }

    # PCA results from the SVD results
    vectors <- res$v
    values <- res$d^2/m
    t <- res$iter
    li <- Xs %*% vectors
    ut <- li %*% diag(1/sqrt(values))
    co <- (t(Xs) %*% ut)/m
    colnames(li) <- paste("Axis",1:r,sep=".")
    colnames(co) <- paste("Comp",1:r,sep=".")

    list(values=values,vectors=vectors,iter=t,li=li,co=co)
}
#############################################################################
