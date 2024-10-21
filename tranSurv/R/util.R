#' @noRd
#' @keywords internal 
uniroot.all <- function (f, interval, lower= min(interval), upper= max(interval), ... ) {
    n = 100
    xseq <- seq(lower, upper, len = n + 1)
    mod  <- f(xseq,...)
    Equi <- xseq[which(mod==0)]
    ss   <- mod[1:n] * mod[2:(n+1)]
    ii   <- which(ss<0)
    if (length(ii) > 0) 
    for (i in ii) Equi <- c(Equi,uniroot(f,lower=xseq[i],upper=xseq[i+1],...)$root)
    else Equi <- NULL
    return(Equi)
}

