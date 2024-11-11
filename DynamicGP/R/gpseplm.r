newGPsepLm <- function(X, Z, d, g, dK, mtype=c("cmean","lmean"))
{
    n <- nrow(X)
    m <- ncol(X)
    if(is.null(n)) stop("X must be a matrix")
    if(length(Z) != n) stop("must have nrow(X) = length(Z)")
    if(length(d) == 1) d <- rep(d, m)
    else if(length(d) != m) stop("must have length(d) = ncol(X)")
    mtype=match.arg(mtype)
    H <- if(mtype=="cmean") rep(1,n) else cbind(rep(1,n),X)
    p <- if(mtype=="cmean") 1 else m+1
    out <- .C("newGPsepLm_R",
              m = as.integer(m),
              n = as.integer(n),
              X = as.double(t(X)),
              Z = as.double(Z),
              d = as.double(d),
              g = as.double(g),
              dK = as.integer(dK),
              p = as.integer(p),
              H = as.double(t(H)),
              gplmi = integer(1),
              PACKAGE="DynamicGP"
              )
    return(out$gplmi)
}

deleteGPsepLm <- function(gplmi)
{
    .C("deleteGPsepLm_R",
       gplmi = as.integer(gplmi),
       PACKAGE = "DynamicGP")
    invisible(NULL)
}
deleteGPsepLms <- function()
{
    .C("deleteGPsepLms_R",
       PACKAGE="DynamicGP")
    invisible(NULL)
}

getmGPsep <- function(gpsepi)
{
    .C("getmGPsep_R", gpsepi = as.integer(gpsepi), m = integer(1), PACKAGE="DynamicGP")$m
}

getmGPsepLm <- function(gplmi)
{
    .C("getmGPsepLm_R", gplmi = as.integer(gplmi), m = integer(1), PACKAGE = "DynamicGP")$m
}

jmleGPsepLm <- function(gplmi, drange=c(sqrt(.Machine$double.eps), 10),
                         grange=c(sqrt(.Machine$double.eps), 1),
                         dab=c(0,0), gab=c(0,0), maxit=100,
                         verb=0)
{
    ## sanity check tmin and tmax
    m <- getmGPsepLm(gplmi)
    if(length(drange) != 2) stop("drange should be a two vector for c(dmin, dmax)")
    dmin <- rep(drange[1], m)
    dmax <- rep(drange[2], m)
    if(length(grange) != 2) stop("grange should be a 2-vector for c(gmin, gmax)")

    ## sanity check ab
    if(length(dab) != 2 || any(dab < 0)) stop("dab should be a positive 2-vector")
    if(length(gab) != 2 || any(gab < 0)) stop("gab should be a positive 2-vector")

    ## call the C-side function
    r <- .C("jmleGPsepLm_R",
            gplmi = as.integer(gplmi),
            maxit = as.integer(maxit),
            verb = as.integer(verb),
            dmin = as.double(dmin),
            dmax = as.double(dmax),
            grange = as.double(grange),
            dab = as.double(dab),
            gab = as.double(gab),
            d = double(m),
            g = double(1),
            dits = integer(1),
            gits = integer(1),
            dconv = integer(1),
            PACKAGE = "DynamicGP")

    return(data.frame(d=t(r$d), g=r$g, tot.its=r$dits+r$gits,
                      dits=r$dits, gits=r$gits, dconv=r$dconv))
}
llikGPsepLm <- function(gplmi, dab=c(0,0), gab=c(0,0))
{
    r <- .C("llikGPsepLm_R",
            gplmi = as.integer(gplmi),
            dab = as.double(dab),
            gab = as.double(gab),
            llik = double(1),
            PACKAGE = "DynamicGP")

    return(r$llik)
}

predGPsepLm <- function(gplmi, XX, mtype=c("cmean","lmean"))
{
    nn <- nrow(XX)
    m <- ncol(XX)
    mtype <- match.arg(mtype)
    HH <- if(mtype=="cmean") rep(1,nn) else cbind(rep(1,nn),XX)
    p <- if(mtype=="cmean") 1 else m+1
    out <- .C("predGPsepLm_R",
              gplmi = as.integer(gplmi),
              m = as.integer(m),
              nn = as.integer(nn),
              p = as.integer(p),
              XX = as.double(t(XX)),
              HH = as.double(t(HH)),
              mean = double(nn),
              s2 = double(nn),
              df = double(1),
              llik = double(1),
              PACKAGE="DynamicGP")
    return(list(mean=out$mean, s2=out$s2, df=out$df, llik=out$llik))
}
