knnsvdGP <- function(design, resp, X0=design, nn=20, nsvd = nn, frac = .95,
                     gstart = 0.0001, nstarts = 5,centralize=FALSE, maxit=100,
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
    ret <- workerfun(X0,design,resp,nn,nn,frac=frac,gstart=gstart,nstarts=nstarts,
                     maxit=maxit,verb=0,errlog=errlog,nthread=nthread,clutype=clutype)
    if(centralize) ret$pmean <- ret$pmean+rmean
    return(ret)
}
