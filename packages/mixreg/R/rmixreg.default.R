rmixreg.default <- function (x,nobs,theta,seed=NULL,xNms=NULL,yNm="y",...) {
# Random (simulated) data from a mixreg (mixture of regressions)
# model.  (The default method.)

    if(is.list(theta)) {
        theta <- matrix(unlist(theta),nrow=length(theta),byrow=TRUE)
    } else if(!is.matrix(theta)) {
        stop("Argument \"theta\" should either be a list or matrix.\n")
    }
    ncoef <- ncol(theta) - 2
    if(is.null(x)) {
        if(missing(nobs))
            stop("Argument \"nobs\" must be supplied when \"x\" is NULL.\n")
        X <- as.matrix(rep(1,nobs))
        noPred <- TRUE
    } else {
        X     <- as.matrix(x)
        if(!ncol(X) %in% (ncoef+c(-1,0))) {
            stop("Inconsistency between \"theta\" and the number of predictors.\n")
        } else if(ncol(X) == ncoef - 1) {
            X <- cbind(1,X)
        }
        noPred <- FALSE
    }
    if(!isTRUE(all.equal(1,sum(theta[,ncol(theta)]))))
        stop("The \"lambda\" values must sum to 1.\n")
    theta <- t(theta)
    nr    <- nrow(theta)
    Beta  <- theta[-c(nr - 1, nr),]
    lamb  <- theta[nr,]
    sigsq <- theta[nr-1,]
    mu    <- X %*% Beta
    K     <- length(lamb)
    n     <- nrow(X)
    if(is.null(seed)) seed <- sample(1:1e5,1)
    set.seed(seed)
    state <- sample(1:K,n,TRUE,lamb)
    rrr   <- rnorm(n,0,sqrt(sigsq[state]))
    ysim  <- mu[cbind(1:n,state)] + rrr
    rslt  <- if(noPred) data.frame(ysim) else data.frame(x,ysim)
    npred <- if(noPred) 0 else ncol(as.matrix(x))
    if(!is.null(xNms)) {
        if(length(xNms) != npred)
            stop("Argument \"xNms\" has wrong length.\n")
        names(rslt)[1:npred] <- xNms
    }
    names(rslt)[1+npred] <- yNm
    attr(rslt,"seed") <- seed
    rslt
}
