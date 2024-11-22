
# exponential cumulative hazard function
merlin_ch_exp <- function(gml) {
    if (gml$Nlevels) cumhaz <- matrix(0,gml$Nobs[gml$modelind],gml$Ndim[gml$Nlevels])
    else cumhaz <- matrix(0,gml$Nobs[gml$modelind],1)
    hq  <- gml$haznodes[[gml$modelind]]
    hw  <- gml$hazweights[[gml$modelind]]
    for (q in 1:gml$hazNnodes[gml$modelind]) {
        xbev   <- exp(merlin_util_xzb(gml,hq[,q]))
        cumhaz <- cumhaz + sweep(xbev,1,hw[,q],"*")
        #cumhaz <- cumhaz + hazweights[,q] %*% xbev
    }
    return(cumhaz)
}

# Weibull cumulative hazard function
merlin_ch_weibull <- function(gml,gamma)
{

    if (gml$Nlevels) cumhaz <- matrix(0,gml$Nobs[gml$modelind],gml$Ndim[gml$Nlevels])
    else cumhaz <- matrix(0,gml$Nobs[gml$modelind],1)
    hq     <- gml$haznodes[[gml$modelind]]
    hq2    <- gamma * hq ^ (gamma - 1) * gml$hazweights[[gml$modelind]]

    for (q in 1:gml$hazNnodes[gml$modelind]) {
        xbev   <- exp(merlin_util_xzb(gml,hq[,q]))
        cumhaz <- cumhaz + sweep(xbev,1,hq2[,q],"*")
    }
    return(cumhaz)

}

# Gompertz cumulative hazard function
merlin_ch_gompertz <- function(gml,gamma) {

    if (gml$Nlevels) cumhaz <- matrix(0,gml$Nobs[gml$modelind],gml$Ndim[gml$Nlevels])
    else cumhaz <- matrix(0,gml$Nobs[gml$modelind],1)
    hq  <- gml$haznodes[[gml$modelind]]
    hq2 <- exp(gamma * hq) * gml$hazweights[[gml$modelind]]
    for (q in 1:gml$hazNnodes[gml$modelind]) {
        xbev   <- exp(merlin_util_xzb(gml,hq[,q]))
        cumhaz <- cumhaz + sweep(xbev,1,hq2[,q],"*")
    }
    return(cumhaz)
}

# Weibull cumulative hazard function
merlin_ch_logh <- function(gml)
{

    if (gml$Nlevels) cumhaz <- matrix(0,gml$Nobs[gml$modelind],gml$Ndim[gml$Nlevels])
    else cumhaz <- matrix(0,gml$Nobs[gml$modelind],1)
    hq     <- gml$haznodes[[gml$modelind]]
    hq2    <- gml$hazweights[[gml$modelind]]

    for (q in 1:gml$hazNnodes[gml$modelind]) {
        xbev   <- exp(merlin_util_xzb(gml,hq[,q]))
        cumhaz <- cumhaz + sweep(xbev,1,hq2[,q],"*")
    }
    return(cumhaz)

}
