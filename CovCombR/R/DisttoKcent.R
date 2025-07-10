DisttoKcent <- function(D) {
    D <- as.matrix(D^2)
    Pmat <- diag(ncol(D)) - (1/(ncol(D))) * tcrossprod(rep(1, ncol(D)))
    Kcent <- -0.5 * (Pmat %*% (D) %*% Pmat)
    rownames(Kcent) <- colnames(Kcent) <- rownames(D)
    return(Kcent)
}