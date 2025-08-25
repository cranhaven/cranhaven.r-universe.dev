
#############################################################################
eigenV <- function(xmat, wp, itmax=200, err=1e-8) {
    for (t in 1:itmax) {
        wt <- xmat %*% wp
        wc <- mGS(wt)
        if (norm(wp-wc,"F") < err) {
           break
        } else { wp <- wc}
    }
    list(wc=wc, iter=t)
}
#############################################################################
