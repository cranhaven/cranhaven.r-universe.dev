bbase <-
function(x, xl, xr, ndx, deg){

    dx <- (xr - xl) / ndx
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    P <- outer(x, knots, tpower, deg)
    n <- dim(P)[2]
    D <- ndiff(n, deg + 1) / (gamma(deg + 1) * dx ^ deg)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
}
