dist2d <- function(a,b,c) {
# This function was obtained from https://stackoverflow.com/
# questions/35194048/using-r-how-to-calculate-the-distance-
# from-one-point-to-a-line .

    v1 <- b - c
    v2 <- a - b
    m  <- cbind(v1,v2)
    abs(det(m))/sqrt(sum(v1*v1))
}

distMat <- function(x,y,theta) {
    ncomp <- length(theta)
    n     <- length(x)
    dM    <- matrix(nrow=n,ncol=ncomp)
    for(j in 1:ncomp) {
        beta <- theta[[j]][["beta"]]
        b0   <- if(length(beta)==2) beta[1] else 0
        b1   <- if(length(beta)==2) beta[2] else beta[1]
        for(i in 1:n) {
            x1 <- (b1*y[i] + x[i] - b0*b1)/(1+b1^2)
            y1 <- b0+b1*x1
            dM[i,j] <- (x[i] - x1)^2 + (y[i] - y1)^2
        }
    }
    sqrt(dM)
}
