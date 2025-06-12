"makepd" <-
function(mat, eig.tol=1.0000e-06){
    cor.mat <- covtocor(mat)
    mat <- cor.mat$mat
    eig <- eigen(mat, symmetric = TRUE)
    D <- sort(eig$values)
    S <- eig$vectors[,order(sort(D, decreasing=TRUE ))]
    D[which(D<eig.tol)] <- eig.tol 
    for (i in 1:ncol(S)) S[,i] <- S[,i] * D[i]
    B <- t(sumsqscale(t(S)))
    A <- B %*% t(B)
    return (cortocov(A, cor.mat$sd))
}

