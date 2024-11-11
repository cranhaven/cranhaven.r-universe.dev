lasvdGP <- function(design, resp, X0=design, n0=10, nn=20,
                    nfea = min(1000,nrow(design)),
                    nsvd = nn, nadd = 1, frac = .95, gstart = 0.0001,
                    resvdThres = min(5, nn-n0), every = min(5,nn-n0),
                    nstarts = 5,centralize=FALSE, maxit=100,
                    errlog = "", nthread = 1, clutype="PSOCK")
{
    if(.Machine$sizeof.pointer != 8)
    {
        cat("the current version only supports 64-bit version of R\n")
        return(NULL)
    }
    if(!is.matrix(design)) stop("design must be a matrix")
    if(!is.matrix(resp)) stop("resp must be a matrix")
    N <- nrow(design)
    m <- ncol(design)
    if(ncol(resp) != N)
        stop("number of design points and responses are not consistent")
    tlen <- nrow(resp)
    if(!is.matrix(X0) && length(X0) != m)
        stop("illegal form of prediction set")
    if(!is.matrix(X0)) X0 <- matrix(X0,ncol=m)
    if(ncol(X0) != m) stop("dimensions of design and prediction set are not consistent")
    M <- nrow(X0)

    if(centralize)
    {
        rmean <- apply(resp,1,mean)
        resp <- (resp-rmean)
    }
    mssuf <- if(nstarts>1) "ms" else ""
    clsuf <- if(nthread <= 1) "" else if(clutype != "OMP") "Paral" else "OMP"
    workerstr <- paste("lasvdgp",mssuf,clsuf,sep="")
    workerfun <- get(workerstr)
    ret <- workerfun(X0,design,resp,n0,nn,nfea,nsvd,nadd,frac,gstart,resvdThres,
                     every,nstarts,maxit,0,errlog,nthread,clutype)
    if(centralize) ret$pmean <- ret$pmean+rmean
    return(ret)
}
