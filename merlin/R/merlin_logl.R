
# merlin_logl_ob
merlin_logl_ob <- function(gml) {

    res <- 0
    for (m in 1:gml$Nmodels) {
        gml$modelind <- m
        gml$modtouse <- m
        if (gml$NotNull[m]) {
            ret <- eval(parse(text=gml$logl[m]))
            res <- res + sum(ret)
        }
    }
    return(res)
}


# log-likelihood for gaussian model
merlin_logl_gaussian <- function(gml)
{

    y  = merlin_util_depvar(gml)
    xb = merlin_util_xzb(gml)
    mu = (sweep(xb,1,y,"-"))^2
    se = exp(gml$par[gml$dapbindex[[gml$modelind]][1]])
    ll = ((-0.5 * log(2*pi) - log(se)) - (mu/(2 * se^2)))
    return(ll)

}

# log-likelihood for weibull model
merlin_logl_weibull <- function(gml)
{
    m      = gml$modelind
    m2     = gml$modtouse
    y      = merlin_util_depvar(gml)
    gamma  = exp(gml$par[gml$dapbindex[[m]][1]])
    xzb    = merlin_util_xzb(gml,t=y[,1])
    logh   = xzb + log(gamma) + (gamma - 1) * log(y[,1])

    if (gml$hasbh[m]) {
        logh = log(sweep(exp(logh),1,gml$data[gml$datause[[m2]],gml$bhazard[m]],"+"))
    }

    if (gml$NI[[gml$modelind]]) ret <- sweep(logh,1,y[,2],"*") - merlin_ch_weibull(gml,gamma)
    else                        ret <- sweep(logh,1,y[,2],"*") - sweep(exp(xzb),1,y[,1]^gamma,"*")
    return(ret)

}

# log-likelihood for exponential model
merlin_logl_exponential <- function(gml)
{

    y    = merlin_util_depvar(gml)
    logh = merlin_util_xzb(gml,t=y[,1])

    if (gml$NI[[gml$modelind]]) ret <- sweep(logh,1,y[,2],"*") - merlin_ch_exp(gml)
    else                        ret <- sweep(logh,1,y[,2],"*") - sweep(exp(logh),1,y[,1],"*")
    return(ret)

}


# log-likelihood for gompertz survival model
merlin_logl_gompertz <- function(gml) {

    y    = merlin_util_depvar(gml)
    xzb  = merlin_util_xzb(gml=gml,t=y[,1])
    gam1 = gml$par[gml$dapbindex[[gml$modelind]][1]]
    logh = xzb + gam1 * y[,1]

    if (gml$NI[[gml$modelind]]) ret <- sweep(logh,1,y[,2],"*") - merlin_ch_gompertz(gml=gml,gamma=gam1)
    else  ret <- sweep(logh,1,y[,2],"*") - sweep(exp(xzb),1,((1/gam1)*(exp(gam1*y[,1])-1)),"*")
    return(ret)

}

# log-likelihood for RP survival model
merlin_logl_rp <- function(gml) {

    y   = merlin_util_depvar(gml)
    xzb = merlin_util_xzb(gml)
    suppressWarnings(ret <- y[,2] * (xzb + log(merlin_util_xzb_deriv(gml,y[,1]))) - exp(xzb))
    return(ret)

}

# log-likelihood for bernoulli model
merlin_logl_bernoulli <- function(gml){

  y    <- merlin_util_depvar(gml)
  p    <- merlin_util_ev(gml)
  logl <- sweep(log(p),1,y,"*") +  sweep(log(1 - p),1,(1 - y),"*")

}

# log-likelihood for log hazard model
merlin_logl_logh <- function(gml)
{

    y    <- merlin_util_depvar(gml)
    logh <- merlin_util_xzb(gml,t=y[,1])
    ret  <- sweep(logh,1,y[,2],"*") - merlin_ch_logh(gml)
    return(ret)

}

# log-likelihood for poisson model
merlin_logl_poisson <- function(gml){

    y    <- merlin_util_depvar(gml)
    xzb  <- merlin_util_xzb(gml)

    if (gml$hasexposure[gml$modelind]) xzb <- sweep(xzb,1,log(gml$data[gml$datause[[gml$modtouse]],gml$exposure[gml$modelind]]),"+")

    logl <- sweep(xzb,1,y,"*") - exp(xzb)
    logl <- sweep(logl,1,lgamma(y+1),"-")
    return(logl)

}

# log-likelihood for beta model
merlin_logl_beta <- function(gml){

    y    <- merlin_util_depvar(gml)
    mu   <- merlin_util_ev(gml)
    s    <- gml$par[gml$dapbindex[[gml$modelind]][1]]
    mus  <- mu * s

    logl <- lgamma(s) - lgamma(mus) - lgamma(s-mus)
    logl <- logl + sweep((mus-1),1,log(y),"*") + sweep((s-mus-1),1,log(1-y),"*")
    return(logl)

}

# log-likelihood for beta model
merlin_logl_negbinomial <- function(gml){

    y      <- merlin_util_depvar(gml)
    mu     <- merlin_util_xzb(gml)
    alpha1 <- exp(gml$par[gml$dapbindex[[gml$modelind]][1]])
    m1     <- 1/alpha1
    p      <- 1/(1+alpha1*log(mu))
    logl   <- lgamma(y+m1) - lgamma(y+1) - lgamma(m1) + m1 * log(p) + sweep(log(1-p),1,y,"*")
    return(logl)

}

