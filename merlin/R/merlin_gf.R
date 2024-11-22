
# merlin_gf
merlin_gf <- function(par,gml) {

    gml$par <- par
    gml     <- merlin_xb(gml)

    if (gml$Nlevels) lnfi <- merlin_logl_panels_nested(index=1,gml)
    else             lnfi <- merlin_logl_ob(gml)

    return(-sum(lnfi))
}

# numerically calculate gradients
merlin_gf_deriv <- function(par,gml) {

    smp  			   = .Machine$double.eps
    hstep 			 = rep_len(1,length(par))
    index        = abs(par) <= 1
    hstep[index] = abs(par)[index]
    hstep 			 = hstep * smp ^(1/3)
    res          = vector()
    for (p in 1:length(par)) {
        copypar = par
        copypar[p] = copypar[p] + hstep
        lh = merlin_gf(copypar,gml)
        copypar = par
        copypar[p] = copypar[p] - hstep
        rh = merlin_gf(copypar,gml)
        res = c(res,(lh - rh)/(2*hstep[p]))
    }
    return(res)
}

# merlin_logl_panels_nested
merlin_logl_panels_nested <- function(index,gml){

    # set up
    index2 <- index + 1
    res    <- matrix(0,gml$Npanels[[index]],gml$Ndim[index])

    if (index < gml$Nlevels) {
        # above lowest level
        # x   <- t(as.matrix(1:gml$Ndim[index]))
        # res <- apply(x,2,merlin_logl_clusters,gml,index,index2)
        for (q in 1:gml$Ndim[index]) {
            gml$qind[index2] <- q
            newres           <- merlin_logl_panels_nested(index2,gml)
            res[,q]          <- rowsum(newres,group=gml$panelid[[gml$modelind]][[index]])
        }
    }
    else {
        # observation level
        for (m in 1:gml$Nmodels) {
            gml$modelind <- gml$modtouse <- m
            if (gml$NotNull[m]) {
                res2         <- eval(parse(text=gml$logl[m]))
                if (gml$hassweights) res2 <- res2 * gml$swts[[gml$modtouse]][[index2]]
                newres <- rowsum(res2,group=gml$panelid[[gml$modelind]][[index]])
                res <- res + newres
            }
        }
    }

    # NI
    if (gml$intmethod[index]=="ghermite") lli <- log(exp(res) %*% gml$w[[index]])
    else                                  lli <- log(base::rowSums(exp(res)) / gml$Ndim[index])

    # sample weights
    if (gml$hassweights) lli <- gml$swts[[gml$modtouse]][[index]] * lli

    return(lli)
}

merlin_logl_clusters <- function(x,gml,index,index2) {
  gml$qind[index2] <- x[1]
  newres           <- merlin_logl_panels_nested(index2,gml)
  rowsum(newres,group=gml$panelid[[gml$modelind]][[index]])
}
