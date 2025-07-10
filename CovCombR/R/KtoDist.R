KtoDist <- function(K) {
    dK <- diag(K)
    ones <- matrix(1, nrow = length(dK), ncol = 1)
    
    D <- tcrossprod(ones, dK) - 2 * K + tcrossprod(dK, ones)
    rownames(D) <- colnames(D) <- rownames(K)
    
    return(as.dist(sqrt(D)))
}
