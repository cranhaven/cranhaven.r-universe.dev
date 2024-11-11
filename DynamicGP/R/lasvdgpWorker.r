lasvdgp <- function(X0, design, resp, n0, nn,
                    nfea = min(1000,nrow(design)),
                    nsvd = nn, nadd = 1,
                    frac = .9, gstart = 0.001,
                    resvdThres = min(5, nn-n0),
                    every = min(5,nn-n0),nstarts=1,
                    maxit=100, verb=0, errlog = "",
                    nthread=1, clutype="")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    out <- .C("lasvdGP_R", as.double(t(X0)), as.double(t(design)),
              as.double(resp), as.integer(M), as.integer(N),
              as.integer(m), as.integer(tlen), as.integer(nn),
              as.integer(n0), as.integer(nfea), as.integer(nsvd),
              as.integer(nadd), as.double(frac), as.double(gstart),
              as.integer(resvdThres), as.integer(every),
              as.integer(maxit), as.integer(verb), as.character(errlog),
              pmean=double(M*tlen), ps2=double(M*tlen), flags = integer(M),
              PACKAGE="DynamicGP")
    pmean <- out$pmean
    ps2 <- out$ps2
    ret <- list(pmean=matrix(pmean,nrow=tlen),ps2=matrix(ps2,nrow=tlen), flags=out$flags)
    return(ret)
}
lasvdgpParal <- function(X0, design, resp, n0, nn,
                         nfea = min(1000,nrow(design)),
                         nsvd = nn, nadd = 1,
                         frac = .9, gstart = 0.001,
                         resvdThres = min(5, nn-n0),
                         every = min(5,nn-n0),nstarts=1,
                         maxit=100, verb=0, errlog = "",
                         nthread = 1, clutype="PSOCK")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    blocks <- blockPar(M,nthread)
    X0par <- lapply(blocks,getRowBlock,X0)
    ret <- genericLapply(X0par,lasvdgp,design,resp,n0,nn,nfea,nsvd,nadd,frac,
                         gstart,resvdThres,every,nstarts,maxit,verb,
                         errlog, nthread, clutype,
                         nthread=nthread, clutype=clutype)
    pmean <- matrix(unlist(sapply(ret,`[`,"pmean")),nrow=tlen)
    ps2 <- matrix(unlist(sapply(ret,`[`,"ps2")),nrow=tlen)
    flags <- unlist(sapply(ret,`[`,"flags"))
    ret <- list(pmean=pmean,ps2=ps2,flags=flags)
    return(ret)
}
lasvdgpOMP <- function(X0, design, resp, n0, nn,
                       nfea = min(1000,nrow(design)),
                       nsvd = nn, nadd = 1,
                       frac = .9, gstart = 0.001,
                       resvdThres = min(5, nn-n0),
                       every = min(5,nn-n0),nstarts=1,
                       maxit=100, verb=0, errlog = "",
                       nthread = 1, clutype="OMP")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    out <- .C("lasvdGPomp_R", as.double(t(X0)), as.double(t(design)),
              as.double(resp), as.integer(M), as.integer(N), as.integer(m),
              as.integer(tlen), as.integer(nn), as.integer(n0), as.integer(nfea),
              as.integer(nsvd), as.integer(nadd), as.double(frac), as.double(gstart),
              as.integer(resvdThres), as.integer(every), as.integer(maxit),
              as.integer(verb), as.character(errlog), as.integer(nthread),pmean=double(M*tlen),
              ps2=double(M*tlen), flags = integer(M), PACKAGE="DynamicGP")
    ret <- list(pmean = matrix(out$pmean,nrow=tlen), ps2 = matrix(out$ps2, nrow=tlen),
                flags = out$flags)
    return(ret)
}
lasvdgpms <- function(X0, design, resp, n0, nn,
                      nfea = min(1000,nrow(design)),
                      nsvd = nn, nadd = 1,
                      frac = .9, gstart = 0.001,
                      resvdThres = min(5, nn-n0),
                      every = min(5,nn-n0),
                      nstarts = 5, maxit=100, verb=0,
                      errlog = "", nthread=1, clutype="")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    if(nstarts < 0) stop("illegal nstarts")
    out <- .C("lasvdGPms_R", as.double(t(X0)), as.double(t(design)),
              as.double(resp), as.integer(M), as.integer(N),
              as.integer(m), as.integer(tlen), as.integer(nn),
              as.integer(n0), as.integer(nfea), as.integer(nsvd),
              as.integer(nadd), as.double(frac), as.double(gstart),
              as.integer(resvdThres), as.integer(every), as.integer(nstarts),
              as.integer(maxit), as.integer(verb), as.character(errlog),
              pmean=double(M*tlen), ps2=double(M*tlen), flags=integer(M),
              PACKAGE="DynamicGP")
    pmean <- out$pmean
    ps2 <- out$ps2
    ret <- list(pmean=matrix(pmean,nrow=tlen),ps2=matrix(ps2,nrow=tlen), flags = out$flags)
    return(ret)
}
lasvdgpmsOMP <- function(X0, design, resp, n0, nn,
                         nfea = min(1000,nrow(design)),
                         nsvd = nn, nadd = 1,
                         frac = .9, gstart = 0.001,
                         resvdThres = min(5, nn-n0),
                         every = min(5,nn-n0),
                         nstarts = 5, maxit=100, verb=0,
                         errlog = "",nthread=1, clutype="OMP")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    if(nstarts < 0) stop("illegal nstarts")
    out <- .C("lasvdGPmsomp_R", as.double(t(X0)), as.double(t(design)),
              as.double(resp), as.integer(M), as.integer(N),
              as.integer(m), as.integer(tlen), as.integer(nn),
              as.integer(n0), as.integer(nfea), as.integer(nsvd),
              as.integer(nadd), as.double(frac), as.double(gstart),
              as.integer(resvdThres), as.integer(every), as.integer(nstarts),
              as.integer(maxit), as.integer(verb), as.character(errlog),as.integer(nthread),
              pmean=double(M*tlen), ps2=double(M*tlen), flags=integer(M), PACKAGE="DynamicGP")
    pmean <- out$pmean
    ps2 <- out$ps2
    ret <- list(pmean=matrix(pmean,nrow=tlen),ps2=matrix(ps2,nrow=tlen), flags = out$flags)
    return(ret)
}

lasvdgpmsParal <- function(X0, design, resp, n0, nn,
                           nfea = min(1000,nrow(design)),
                           nsvd = nn, nadd = 1,
                           frac = .9, gstart = 0.001,
                           resvdThres = min(5, nn-n0),
                           every = min(5,nn-n0),
                           nstarts = 5, maxit=100, verb=0,
                           errlog="",nthread = 1, clutype="PSOCK")
{
    N <- nrow(design)
    m <- ncol(design)
    tlen <- nrow(resp)
    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    if(nstarts < 0) stop("illegal nstarts")
    blocks <- blockPar(M,nthread)
    X0par <- lapply(blocks,getRowBlock,X0)
    ret <- genericLapply(X0par,lasvdgpms,design,resp,n0,nn,nfea,nsvd,nadd,frac,
                         gstart,resvdThres,every,nstarts,maxit,verb,errlog,nthread,
                         clutype, nthread=nthread, clutype=clutype)
    pmean <- matrix(unlist(sapply(ret,`[`,"pmean")),nrow=tlen)
    ps2 <- matrix(unlist(sapply(ret,`[`,"ps2")),nrow=tlen)
    flags <- unlist(sapply(ret,`[`,"flags"))
    ret <- list(pmean=pmean,ps2=ps2,flags=flags)
    return(ret)
}
