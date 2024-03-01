rmixreg.mixreg <- function (x, semiPar = FALSE, conditional = semiPar,
                            seed = NULL, ...) {
# Random (simulated) data from a mixreg (mixture of regressions)
# model.  (The mixreg method.)

    if(conditional & ! semiPar)
        stop("Can only set \"conditional\" equal to TRUE when \"semiPar\" is TRUE.\n")

    theta <- x$theta
    fmla  <- x$formula
    nmy   <- as.character(fmla[[2]])
    y     <- x$data[[nmy]]
    X     <- model.matrix(fmla,data=x$data)
    m     <- t(x$parmat)
    nr    <- nrow(m)
    Beta  <- m[-c(nr - 1, nr), ]
    lamb  <- m[nr,]
    sigsq <- m[nr-1,]
    mu    <- X %*% Beta
    K     <- length(lamb)
    n     <- length(y)
    if(is.null(seed)) seed <- sample(1:1e5,1)
    set.seed(seed)
    if(conditional) {
        gamm  <- gfun(X, y, theta)$gamma
        state <- apply(gamm,1,function(x,K){sample(1:K,1,prob=x)},K=K) 
    } else {
        state <- sample(1:K,n,TRUE,lamb)
    }
    if (semiPar) {
        rrr   <- y - mu
        ind   <- sample(1:n,n,TRUE)
        ysim  <- mu[cbind(1:n,state[ind])] + rrr[cbind(1:n,state[ind])]
    } else {
        rrr  <- rnorm(n,0,sqrt(sigsq[state]))
        ysim <- mu[cbind(1:n,state)] + rrr
    }
    pnms <- names(x$data)
    pnms <- pnms[pnms!=nmy]
    preds <- x$data[,pnms,drop=FALSE]
    rslt <- cbind(preds,data.frame(ysim))
    names(rslt)[ncol(rslt)] <- nmy
    attr(rslt,"seed") <- seed
    rslt
}
