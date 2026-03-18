sn2ft2x2 <- function(s, n) {
    ls <- length(s)
    ln <- length(n)
    if (ls != ln) 
        stop("S and N have to be vectors of equal length\n")
    if (ls != 2) 
        stop("S and N have to be vectors of length 2\n")
    Q <- matrix(rep(NA, 2), 2, 2)
    Q[1, ] <- s
    Q[2, ] <- n - s
    dimnames(Q) <- list(A = c("Y", "N"), B = c("Y", 
        "N"))
    return(Q)
}
