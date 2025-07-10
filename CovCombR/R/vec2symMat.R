vec2symMat <- function(x, diag = TRUE, byrow = FALSE) {
    m <- length(x)
    d <- if (diag) 
        1 else -1
    n <- floor((sqrt(1 + 8 * m) - d)/2)
    if (m != n * (n + d)/2) 
        stop("Length of input is incorrect.")
    mat <- diag(n)
    if (byrow) {
        mat[upper.tri(mat, diag = diag)] <- x
        index <- lower.tri(mat)
        mat[index] <- t(mat)[index]
    } else {
        mat[lower.tri(mat, diag = diag)] <- x
        index <- upper.tri(mat)
        mat[index] <- t(mat)[index]
    }
   return(mat)
}
