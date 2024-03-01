covMix <- function(object,useMC=c("ifNec","no"),...) {
#
# Function covMix.  To calculate the covariance matrix of the
# parameter estimates produced by mixreg().
#
useMC  <- match.arg(useMC)
theta  <- object$theta
fmla   <- object$formula
K      <- length(theta)
eqVar  <- object$eqVar
xm     <- model.matrix(fmla,data=object$data)
y      <- object$data[[fmla[[2]]]]
g      <- gfun(xm,y,theta)$gamma
info.1 <- info1(xm,y,theta,g)
info.2 <- info2(xm,y,theta,g)

nms   <- as.vector(outer(colnames(object$parmat),1:K,paste,sep="."))
nms   <- nms[-length(nms)]
finfo <- info.1-info.2
if(eqVar) {
    p     <- length(theta[[1]][["beta"]]) + 2
    ind   <- (1:K)*p - 1
    nms   <- c(nms[-ind],"sigsq")
    finfo <- aux3(finfo,ind)
}
sing <- any(eigen(finfo)$values <= 0)

if(sing) {
    if(useMC=="ifNec") {
        dotargs <- list(...)
        seed    <- dotargs[["cMseed"]]
        dotargs[["cMseed"]] <- NULL
        arghs   <- c(list(object=object,seed=seed),dotargs)
        covMat  <- do.call(covMixMC,arghs)
        covMat  <- covMixMC(object,...)
    } else {
        stop("Calculated Fisher info is not positive definite.\n")
    }
} else {
    covMat <- solve(finfo)
    dimnames(covMat) <- list(nms,nms)
    attr(covMat,"seed") <- NA
    attr(covMat,"MC")   <- FALSE
}
covMat
}
