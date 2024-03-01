cint <- function(object,alpha=0.05,MC=FALSE,plot=FALSE,...) {
#
# Function cint.  To do the calculations to provide 100(1-alpha)%
# confidence and prediction intervals for a scalar (Gaussian)
# mixture model.
#

if(!inherits(object,"mixreg")) {
    stop("Argument \"object\" must be of class \"mixreg\".\n") 
}

vars <- all.vars(object$formula)
if(length(vars) != 1)
    stop("This function can be applied only to scalar mixtures.\n")

# Get the response name.
nmy <- vars[1]

theta  <- object$theta
K      <- length(theta)
eqVar  <- object$eqVar
covMat <- object$covMat
if(is.null(covMat)) {
    covMat <- if(MC) covMixMC(object,...) else covMix(object,...)
} else {
    if(!MC & attr(covMat,"MC")) {
        covMat <- covMix(object,useMC="no")
    } else if(MC & !attr(covMat,"MC")) {
         covMat <- covMixMC(object,...)
    }
}

alpha1 <- alpha
alpha2 <- alpha/2
tv1 <- qnorm(1-alpha1)
tv2 <- qnorm(1-alpha2)

bnds <- vector("list",K)
for(k in 1:K) {
    beta <- theta[[k]]$beta
    if(eqVar) {
        ind  <- 2*(k-1) + 1
    } else {
        ind  <- 3*(k-1) + 1
    }
    mn   <- beta[1]
    v    <- covMat[ind,ind]
    se   <- sqrt(v)
    ucb1 <- mn + tv1*se
    lcb1 <- mn - tv1*se
    upb1 <- mn + tv1*sqrt(theta[[k]]$sigsq + v)
    lpb1 <- mn - tv1*sqrt(theta[[k]]$sigsq + v)
    ucb2 <- mn + tv2*se
    lcb2 <- mn - tv2*se
    upb2 <- mn + tv2*sqrt(theta[[k]]$sigsq + v)
    lpb2 <- mn - tv2*sqrt(theta[[k]]$sigsq + v)
    bnds[[k]] <- cbind(lcb1,ucb1,lcb2,ucb2,lpb1,upb1,lpb2,upb2)
}

rslt <- list(bnds=bnds,theta=theta,alpha=alpha,nmy=nmy,y=object$data[[nmy]])
class(rslt) <- "cint"
if(plot) {
    if("cints" %in% names(list(...))) {
        cints <- list(...)[["cints"]]
    } else if("cbands" %in% names(list(...))) {
        cints <- list(...)[["cbands"]]
    } else cints <- TRUE
    if("pints" %in% names(list(...))) {
        pints <- list(...)[["pints"]]
    } else if("pbands" %in% names(list(...))) {
        pints <- list(...)[["pbands"]]
    } else pints <- TRUE
    type <- list(...)$type
    if(is.null(type)) type <- "both"
    plot(rslt,cints=cints,pints=pints,type=type)
    return(invisible(rslt))
} else return(rslt)
}
