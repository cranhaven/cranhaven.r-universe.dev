covMixMC <- function(object,nsim=100,progRep=TRUE,seed=NULL,...) {
#
# Function covMixMC.  To estimate the covariance matrix of the
# parameter estimates produced by mixreg() via a Monte Carlo method.
#
simpars <- vector("list",nsim)
data    <- object$data
fmla    <- object$formula
theta   <- object$theta
K       <- length(theta)
nmy     <- as.character(fmla[[2]])
nms     <- as.vector(outer(colnames(object$parmat),1:K,paste,sep="."))
nms     <- nms[-length(nms)]
eqVar   <- object$eqVar
kpind   <- 1:length(nms)
if(eqVar) {
    jlast <- length(kpind)
    p     <- length(theta[[1]][["beta"]]) + 2
    kpind <- setdiff(kpind,(1:K)*p - 1)
    nms   <- c(nms[kpind],"sigsq")
    kpind <- c(kpind,jlast)
}
if(is.null(seed)) seed <- sample(1:1e5,1)
set.seed(seed)
for(i in 1:nsim) {
    arghs        <- c(list(x=object),list(...))
    simdat       <- do.call(rmixreg,arghs)
    fit          <- update(object,data=simdat,thetaStart=theta,verb=FALSE,covMat=FALSE)
    simpars[[i]] <- unlist(fit$theta)[kpind]
    if(progRep) cat(i,"")
    if(progRep & i%%10 == 0) cat("\n")
}
X <- matrix(unlist(simpars),byrow=TRUE,nrow=nsim)
covMat <- var(X)
dimnames(covMat) <- list(nms,nms)
attr(covMat,"seed") <- seed
attr(covMat,"MC")   <- TRUE
covMat
}
