#' vmf.kerncontour2
#'
#' This function that calculates the Von Mises-Fisher kernel density estimator is a slight modification of function vmf.kerncontour
#' in Directional package. In this new version, it is possible to provide a numerical value for h
#'
#' @param u A two column matrix. The first column is the latitude and the second is the longitude
#' @param h Numeric value for smoothing parameter
#' @param full If FALSE (default), uses the range of positions from 'u' to calculate and optionally plot densities. If TRUE, calculates densities covering the entire sphere
#' @param ngrid Sets the resolution of the density calculation
vmf.kerncontour2<-function (u, h, full = FALSE, ngrid = 100){
    n <- dim(u)[1]
    x <- euclid(u)

    if (full) {
        x1 <- seq(0, 180, length = ngrid)
        x2 <- seq(0, 360, length = ngrid)
    }
    else {
        x1 <- seq(min(u[, 1]) - 5, max(u[, 1]) + 5, length = ngrid)
        x2 <- seq(min(u[, 2]) - 5, max(u[, 2]) + 5, length = ngrid)
    }
    cpk <- 1/((h^2)^0.5 * (2 * pi)^1.5 * besselI(1/h^2, 0.5))
    mat <- matrix(nrow = ngrid, ncol = ngrid)
    for (i in 1:ngrid) {
        for (j in 1:ngrid) {
            y <- euclid(c(x1[i], x2[j]))
            a <- as.vector(tcrossprod(x, y/h^2))
            can <- sum(exp(a + log(cpk)))/ngrid
            if (abs(can) < Inf)
                mat[i, j] <- can
        }
    }
    return(list(lat = x1, long = x2, h = h, den = mat))
}
#' @noRd
#' @keywords internal
